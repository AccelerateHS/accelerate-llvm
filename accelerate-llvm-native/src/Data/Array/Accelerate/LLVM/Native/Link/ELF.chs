{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.ELF
-- Copyright   : [2017] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.ELF (

  loadObject,

) where

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.Native.Link.Object
import Data.Array.Accelerate.Lifetime
import qualified Data.Array.Accelerate.Debug              as Debug

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString                                    ( ByteString )
import Data.Char
import Data.Int
import Data.List
import Data.Serialize.Get
import Data.Vector                                        ( Vector )
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr                                     ( mallocPlainForeignPtrAlignedBytes )
import GHC.Prim                                           ( addr2Int#, int2Word# )
import GHC.Ptr                                            ( Ptr(..) )
import GHC.Word                                           ( Word64(..) )
import System.IO.Unsafe
import System.Posix.DynamicLinker
import Text.Printf
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import qualified Data.ByteString.Internal                 as B
import qualified Data.ByteString.Short                    as BS
import qualified Data.ByteString.Unsafe                   as B
import qualified Data.Vector                              as V
import Prelude                                            as P

#include <elf.h>
#include <sys/mman.h>


-- Dynamic object loading
-- ----------------------

-- Load an ELF object file and return pointers to the executable functions
-- defined within. The executable sections are aligned appropriately, as
-- specified in the object file, and are ready to be executed on the target
-- architecture.
--
loadObject :: ByteString -> IO (FunctionTable, ObjectCode)
loadObject obj =
  case parseObject obj of
    Left err                              -> $internalError "loadObject" err
    Right (secs, symbols, relocs, strtab) -> do
      -- Load the sections into executable memory
      --
      (funtab, oc) <- loadSegment obj strtab secs symbols relocs

      -- The executable pages were allocated on the GC heap. When the pages
      -- are finalised, unset the executable bit and mark them as
      -- read/write so that the memory can be reused.
      --
      objectcode <- newLifetime [oc]
      addFinalizer objectcode $ do
        Debug.traceIO Debug.dump_gc ("gc: unload module: " ++ show funtab)
        case oc of
          Segment vmsize oc_fp ->
            withForeignPtr oc_fp $ \oc_p ->
              mprotect oc_p vmsize ({#const PROT_READ#} .|. {#const PROT_WRITE#})

      return (funtab, objectcode)


-- Load the sections into memory.
--
-- Extra jump islands are added directly after the section data. On x86_64
-- PC-relative jumps and accesses to the global offset table are limited to
-- 32-bits (+-2GB). If we need to go outside of this range than we must do so
-- via the jump islands.
--
-- NOTE: This puts all the sections into a single block of memory. Technically
-- this is incorrect because we then have both text and data sections together,
-- meaning that data sections are marked as execute when they really shouldn't
-- be. These would need to live in different pages in order to be mprotect-ed
-- properly.
--
loadSegment
    :: ByteString
    -> ByteString
    -> Vector SectionHeader
    -> Vector Symbol
    -> Vector Relocation
    -> IO (FunctionTable, Segment)
loadSegment obj strtab secs symtab relocs = do
  let
      pagesize    = fromIntegral c_getpagesize

      -- round up to next multiple of given alignment
      pad align n = (n + align - 1) .&. (complement (align - 1))

      -- determine where each section should be placed in memory, respecting
      -- alignment requirements. SectionHeaders which do not correspond to
      -- program data (e.g. systab) just carry along the previous offset value.
      -- This is to avoid filtering the list of sections, so that section
      -- indices (e.g. in relocations) remain valid.
      --
      nsecs       = V.length secs
      offsets     = V.constructN (nsecs + 1) $ \v ->
                      case V.length v of
                        0 -> 0
                        n -> let this     = secs V.! n
                                 prev     = secs V.! (n-1)
                                 alloc s  = testBit (sh_flags s) 1  -- SHF_ALLOC: section occupies memory at execution?
                                 --
                                 align | n >= nsecs       = 16
                                       | not (alloc this) = 1
                                       | otherwise        = sh_align this
                                 --
                                 size  | alloc prev       = sh_size prev
                                       | otherwise        = 0
                             in
                             pad align (size + v V.! (n-1))

      -- The section at index `i` should place its data beginning at page boundary
      -- offset given by offsets!i.
      --
      vmsize'     = V.last offsets                                  -- bytes required to store all sections
      vmsize      = pad pagesize (vmsize' + (V.length symtab * 16)) -- sections + jump tables

  seg_fp  <- mallocPlainForeignPtrAlignedBytes vmsize pagesize
  funtab  <- withForeignPtr seg_fp $ \seg_p -> do

              -- Clear the segment data; this takes care of .bss sections
              fillBytes seg_p 0 vmsize

              -- Jump tables are placed directly after the segment data
              let jump_p = seg_p `plusPtr` vmsize'
              V.imapM_ (makeJumpIsland jump_p) symtab

              -- Copy over section data
              V.izipWithM_ (loadSection obj strtab seg_p) offsets secs

              -- Process relocations
              V.mapM_ (processRelocation symtab offsets seg_p jump_p) relocs

              -- Mark the page as executable and read-only
              mprotect seg_p vmsize ({#const PROT_READ#} .|. {#const PROT_EXEC#})

              -- Resolve external symbols defined in the sections into function
              -- pointers.
              --
              -- Note that in order to support ahead-of-time compilation, the generated
              -- functions are given unique names by appending with an underscore followed
              -- by a unique ID. The execution phase doesn't need to know about this
              -- however, so un-mangle the name to the basic "map", "fold", etc.
              --
              let funtab              = FunctionTable $ V.toList (V.map resolve (V.filter extern symtab))
                  extern Symbol{..}   = sym_binding == Global && sym_type == Func
                  resolve Symbol{..}  =
                    let name  = BS.toShort (B8.take (B8.length sym_name - 65) sym_name)
                        addr  = castPtrToFunPtr (seg_p `plusPtr` (fromIntegral sym_value + offsets V.! sym_section))
                    in
                    (name, addr)
              --
              return funtab
  --
  return (funtab, Segment vmsize seg_fp)


-- Add the jump-table entries directly to each external undefined symbol.
--
makeJumpIsland :: Ptr Word8 -> Int -> Symbol -> IO ()
makeJumpIsland jump_p symbolnum Symbol{..} = do
#ifdef x86_64_HOST_ARCH
  when (sym_binding == Global && sym_section == 0) $ do
    let
        target  = jump_p `plusPtr` (symbolnum * 16) :: Ptr Word64   -- addr
        instr   = target `plusPtr` 8                :: Ptr Word8    -- jumpIsland
    --
    poke target sym_value
    pokeArray instr [ 0xFF, 0x25, 0xF2, 0xFF, 0xFF, 0xFF ]  -- jmp *-14(%rip)
#endif
  return ()


-- Load the section at the correct offset into the given segment
--
loadSection :: ByteString -> ByteString -> Ptr Word8 -> Int -> Int -> SectionHeader -> IO ()
loadSection obj strtab seg_p sec_num sec_addr SectionHeader{..} =
  when (sh_type == ProgBits && sh_size > 0) $ do
    message (printf "section %d: Mem: 0x%09x-0x%09x         %s" sec_num sec_addr (sec_addr+sh_size) (B8.unpack (indexStringTable strtab sh_name)))
    let (obj_fp, obj_offset, _) = B.toForeignPtr obj
    --
    withForeignPtr obj_fp $ \obj_p -> do
      -- Copy this section's data to the appropriate place in the segment
      let src = obj_p `plusPtr` (obj_offset + sh_offset)
          dst = seg_p `plusPtr` sec_addr
      --
      copyBytes dst src sh_size


-- Process local and external relocations.
--
processRelocation :: Vector Symbol -> Vector Int -> Ptr Word8 -> Ptr Word8 -> Relocation -> IO ()
#ifdef x86_64_HOST_ARCH
processRelocation symtab sec_offset seg_p jump_p Relocation{..} = do
  message (printf "relocation: 0x%04x to symbol %d in section %d, type=%-14s value=%s%+d" r_offset r_symbol r_section (show r_type) (B8.unpack sym_name) r_addend)
  case r_type of
    R_X86_64_None -> return ()
    R_X86_64_64   -> relocate value

    R_X86_64_PC32 ->
      let offset :: Int64
          offset = fromIntegral (value - pc')
      in
      if offset >= 0x7fffffff || offset < -0x80000000
        then
          let jump'   = castPtrToWord64 (jump_p `plusPtr` (r_symbol * 16 + 8))
              offset' = fromIntegral jump' + r_addend - fromIntegral pc'
          in
          relocate (fromIntegral offset' :: Word32)
        else
          relocate (fromIntegral offset  :: Word32)

    R_X86_64_PC64 ->
      let offset :: Int64
          offset = fromIntegral (value - pc')
      in
      relocate (fromIntegral offset :: Word32)

    R_X86_64_32 ->
      if value >= 0x7fffffff
        then
          let jump'   = castPtrToWord64 (jump_p `plusPtr` (r_symbol * 16 + 8))
              value'  = fromIntegral jump' + r_addend
          in
          relocate (fromIntegral value' :: Word32)
        else
          relocate (fromIntegral value  :: Word32)

    R_X86_64_32S ->
      let values :: Int64
          values = fromIntegral value
      in
      if values > 0x7fffffff || values < -0x80000000
        then
          let jump'   = castPtrToWord64 (jump_p `plusPtr` (r_symbol * 16 + 8))
              value'  = fromIntegral jump' + r_addend
          in
          relocate (fromIntegral value' :: Int32)
        else
          relocate (fromIntegral value  :: Int32)

    R_X86_64_PLT32 ->
      let offset :: Int64
          offset  = fromIntegral (value - pc')
      in
      if offset >= 0x7fffffff || offset < -0x80000000
        then
          let jump'   = castPtrToWord64 (jump_p `plusPtr` (r_symbol * 16 + 8))
              offset' = fromIntegral jump' + r_addend - fromIntegral pc'
          in
          relocate (fromIntegral offset' :: Word32)
        else
          relocate (fromIntegral offset  :: Word32)

  where
    pc :: Ptr Word8
    pc  = seg_p `plusPtr` (fromIntegral r_offset + sec_offset V.! r_section)
    pc' = castPtrToWord64 pc

    value :: Word64
    value = symval + fromIntegral r_addend

    symval :: Word64
    symval =
      case sym_binding of
        Local   -> castPtrToWord64 (seg_p `plusPtr` (sec_offset V.! sym_section + fromIntegral sym_value))
        Global  -> sym_value
        Weak    -> $internalError "processRelocation" "unhandled weak symbol"

    Symbol{..} = symtab V.! r_symbol

    relocate :: Storable a => a -> IO ()
    relocate x = poke (castPtr pc) x

#else
precessRelocation =
  $internalError "processRelocation" "not defined for non-x86_64 architectures yet"
#endif


-- Object file parser
-- ------------------

-- Parse an ELF object file and return the set of section load commands, as well
-- as the symbols defined within the sections of the object.
--
-- Actually loading the sections into executable memory happens separately.
--
parseObject :: ByteString -> Either String (Vector SectionHeader, Vector Symbol, Vector Relocation, ByteString)
parseObject obj = do
  (p, tph, tsec, strix) <- runGet readHeader obj

  -- As this is an object file, we do not expect any program headers
  unless (tb_entries tph == 0) $ Left "unhandled program header(s)"

  -- Read the object file headers
  secs    <- runGet (V.replicateM (tb_entries tsec) (readSectionHeader p)) (B.drop (tb_fileoff tsec) obj)
  strtab  <- readStringTable obj (secs V.! strix)

  let symtab  = V.toList . V.filter (\s -> sh_type s == SymTab)
      reloc   = V.toList . V.filter (\s -> sh_type s == Rel || sh_type s == RelA)

  symbols <- V.concat <$> sequence [ readSymbolTable p secs obj sh | sh <- symtab secs ]
  relocs  <- V.concat <$> sequence [ readRelocations p      obj sh | sh <- reloc secs ]

  return (secs, symbols, relocs, strtab)


-- Parsing depends on whether the ELF file is 64-bit and whether it should be
-- read as big- or little-endian.
--
data Peek = Peek
    { is64Bit   :: !Bool
    , getWord16 :: !(Get Word16)
    , getWord32 :: !(Get Word32)
    , getWord64 :: !(Get Word64)
    }

data Table = Table
    { tb_fileoff    :: {-# UNPACK #-} !Int    -- byte offset to start of table (array)
    , tb_entries    :: {-# UNPACK #-} !Int    -- number of entries in the table (array)
    , tb_entrysize  :: {-# UNPACK #-} !Int    -- size in bytes per entry
    }

{--
data ProgramHeader = ProgramHeader
    { prog_vmaddr   :: {-# UNPACK #-} !Int    -- virtual address
    , prog_vmsize   :: {-# UNPACK #-} !Int    -- size in memory
    , prog_fileoff  :: {-# UNPACK #-} !Int    -- file offset
    , prog_filesize :: {-# UNPACK #-} !Int    -- size in file
    , prog_align    :: {-# UNPACK #-} !Int    -- alignment
    , prog_paddr    :: {-# UNPACK #-} !Int    -- physical address
    }
--}

data SectionHeader = SectionHeader
    { sh_name       :: {-# UNPACK #-} !Int    -- string table index
    , sh_addr       :: {-# UNPACK #-} !Word64 -- virtual memory address
    , sh_size       :: {-# UNPACK #-} !Int    -- section size in bytes
    , sh_offset     :: {-# UNPACK #-} !Int    -- file offset in bytes
    , sh_align      :: {-# UNPACK #-} !Int
    , sh_link       :: {-# UNPACK #-} !Int
    , sh_info       :: {-# UNPACK #-} !Int    -- additional section info
    , sh_entsize    :: {-# UNPACK #-} !Int    -- entry size, if section holds table
    , sh_flags      :: {-# UNPACK #-} !Word64
    , sh_type       :: !SectionType
    }
    deriving Show

{#enum define SectionType
    { SHT_NULL      as NullSection
    , SHT_PROGBITS  as ProgBits
    , SHT_SYMTAB    as SymTab
    , SHT_STRTAB    as StrTab
    , SHT_RELA      as RelA
    , SHT_HASH      as Hash
    , SHT_DYNAMIC   as Dynamic
    , SHT_NOTE      as Note
    , SHT_NOBITS    as NoBits
    , SHT_REL       as Rel
    , SHT_DYNSYM    as DynSym
    }
    deriving (Eq, Show)
#}

data Symbol = Symbol
    { sym_name      :: {-# UNPACK #-} !ByteString
    , sym_value     :: {-# UNPACK #-} !Word64
    , sym_section   :: {-# UNPACK #-} !Int
    , sym_binding   :: !SymbolBinding
    , sym_type      :: !SymbolType
    }
    deriving Show

{#enum define SymbolBinding
    { STB_LOCAL     as Local
    , STB_GLOBAL    as Global
    , STB_WEAK      as Weak
    }
    deriving (Eq, Show)
#}

{#enum define SymbolType
    { STT_NOTYPE    as NoType
    , STT_OBJECT    as Object     -- data object
    , STT_FUNC      as Func       -- function object
    , STT_SECTION   as Section
    , STT_FILE      as File
    , STT_COMMON    as Common
    , STT_TLS       as TLS
    }
    deriving (Eq, Show)
#}

data Relocation = Relocation
    { r_offset      :: {-# UNPACK #-} !Word64
    , r_symbol      :: {-# UNPACK #-} !Int
    , r_section     :: {-# UNPACK #-} !Int
    , r_addend      :: {-# UNPACK #-} !Int64
    , r_type        :: !RelocationType
    }
    deriving Show

#ifdef i386_HOST_ARCH
{#enum define RelocationType
    { R_386_NONE    as R_386_None
    , R_386_32      as R_386_32
    , R_386_PC32    as R_386_PC32
    }
    deriving (Eq, Show)
#}
#endif
#ifdef x86_64_HOST_ARCH
{#enum define RelocationType
    { R_X86_64_NONE   as R_X86_64_None      -- no relocation
    , R_X86_64_64     as R_X86_64_64        -- direct 64-bit
    , R_X86_64_PC32   as R_X86_64_PC32      -- PC relative 32-bit signed
    , R_X86_64_PC64   as R_X86_64_PC64      -- PC relative 64-bit
    , R_X86_64_32     as R_X86_64_32        -- direct 32-bit zero extended
    , R_X86_64_32S    as R_X86_64_32S       -- direct 32-bit sign extended
    , R_X86_64_PLT32  as R_X86_64_PLT32     -- 32-bit PLT address
    -- ... many more relocation types
    }
    deriving (Eq, Show)
#}
#endif

-- The ELF file header appears at the start of every file.
--
readHeader :: Get (Peek, Table, Table, Int)
readHeader = do
  p@Peek{..}            <- readIdent
  (_, phs, secs, shstr) <- case is64Bit of
                             True  -> readHeader64 p
                             False -> readHeader32 p
  return (p, phs, secs, shstr)


readHeader32 :: Peek -> Get (Int, Table, Table, Int)
readHeader32 _ = fail "TODO: readHeader32"

readHeader64 :: Peek -> Get (Int, Table, Table, Int)
readHeader64 p@Peek{..} = do
  readType p
  readMachine p
  skip {#sizeof Elf64_Word#}      -- e_version
  e_entry     <- getWord64        -- entry point virtual address (page offset?)
  e_phoff     <- getWord64        -- program header table file offset
  e_shoff     <- getWord64        -- section header table file offset
  skip ({#sizeof Elf64_Word#}+{#sizeof Elf64_Half#})    -- e_flags + e_ehsize
  e_phentsize <- getWord16        -- byte size per program header entry
  e_phnum     <- getWord16        -- #program header entries
  e_shentsize <- getWord16
  e_shnum     <- getWord16
  e_shstrndx  <- getWord16
  return ( fromIntegral e_entry
         , Table { tb_fileoff = fromIntegral e_phoff, tb_entries = fromIntegral e_phnum, tb_entrysize = fromIntegral e_phentsize }
         , Table { tb_fileoff = fromIntegral e_shoff, tb_entries = fromIntegral e_shnum, tb_entrysize = fromIntegral e_shentsize }
         , fromIntegral e_shstrndx
         )


readIdent :: Get Peek
readIdent = do
  ei_magic    <- getBytes 4
  unless (ei_magic == B8.pack [chr {#const ELFMAG0#}, {#const ELFMAG1#}, {#const ELFMAG2#}, {#const ELFMAG3#}]) $
    fail "invalid magic number"

  ei_class    <- getWord8
  is64Bit     <- case ei_class of
                   {#const ELFCLASS32#} -> return False
                   {#const ELFCLASS64#} -> return True
                   _                    -> fail "invalid class"
  ei_data     <- getWord8
  p           <- case ei_data of
                   {#const ELFDATA2LSB#} -> return $ Peek { getWord16 = getWord16le, getWord32 = getWord32le, getWord64 = getWord64le, .. }
                   {#const ELFDATA2MSB#} -> return $ Peek { getWord16 = getWord16be, getWord32 = getWord32be, getWord64 = getWord64be, .. }
                   _                     -> fail "invalid data layout"
  ei_version  <- getWord8
  unless (ei_version == {#const EV_CURRENT#}) $ fail "invalid version"
  skip (1+1+{#const EI_NIDENT#}-{#const EI_PAD#}) -- ABI, ABI version, padding
  return p


readType :: Peek -> Get ()
readType Peek{..} = do
  e_type    <- getWord16
  case e_type of
    {#const ET_REL#}  -> return ()
    _                 -> fail "expected relocatable object file"

readMachine :: Peek -> Get ()
readMachine Peek{..} = do
  e_machine <- getWord16
  case e_machine of
#ifdef i386_HOST_ARCH
    {#const EM_386#}    -> return ()
#endif
#ifdef x86_64_HOST_ARCH
    {#const EM_X86_64#} -> return ()
#endif
    _                   -> fail "expected host architecture object file"


{--
-- Program headers define how the ELF program behaves once it has been loaded,
-- as well as runtime linking information.
--
-- TLM: Since we are loading object files we shouldn't get any program headers.
--
readProgramHeader :: Peek -> Get ProgramHeader
readProgramHeader p@Peek{..} =
  case is64Bit of
    True  -> readProgramHeader64 p
    False -> readProgramHeader32 p

readProgramHeader32 :: Peek -> Get ProgramHeader
readProgramHeader32 _ = fail "TODO: readProgramHeader32"

readProgramHeader64 :: Peek -> Get ProgramHeader
readProgramHeader64 _ = fail "TODO: readProgramHeader64"
--}

-- Section headers contain information such as the section name, size, and
-- location in the object file. The list of all the section headers in the ELF
-- file is known as the section header table.
--
readSectionHeader :: Peek -> Get SectionHeader
readSectionHeader p@Peek{..} =
  case is64Bit of
    True  -> readSectionHeader64 p
    False -> readSectionHeader32 p

readSectionHeader32 :: Peek -> Get SectionHeader
readSectionHeader32 _ = fail "TODO: readSectionHeader32"

readSectionHeader64 :: Peek -> Get SectionHeader
readSectionHeader64 Peek{..} = do
  sh_name     <- fromIntegral <$> getWord32
  sh_type     <- toEnum . fromIntegral <$> getWord32
  sh_flags    <- getWord64
  sh_addr     <- getWord64
  sh_offset   <- fromIntegral <$> getWord64
  sh_size     <- fromIntegral <$> getWord64
  sh_link     <- fromIntegral <$> getWord32
  sh_info     <- fromIntegral <$> getWord32
  sh_align    <- fromIntegral <$> getWord64
  sh_entsize  <- fromIntegral <$> getWord64
  return SectionHeader {..}


indexStringTable :: ByteString -> Int -> ByteString
indexStringTable strtab ix = B.takeWhile (/= 0) (B.drop ix strtab)

readStringTable :: ByteString -> SectionHeader -> Either String ByteString
readStringTable obj SectionHeader{..} =
  case sh_type of
    StrTab -> Right $ B.take sh_size (B.drop sh_offset obj)
    _      -> Left "expected string table"


readRelocations :: Peek -> ByteString -> SectionHeader -> Either String (Vector Relocation)
readRelocations p@Peek{..} obj SectionHeader{..} = do
  unless (sh_type == Rel || sh_type == RelA) $ Left "expected relocation section"
  --
  let nrel = sh_size `quot` sh_entsize
  runGet (V.replicateM nrel (readRel p sh_type sh_info)) (B.drop sh_offset obj)


readRel :: Peek -> SectionType -> Int -> Get Relocation
readRel p@Peek{..} sh_type r_section =
  case is64Bit of
    True  -> readRel64 p sh_type r_section
    False -> readRel32 p sh_type r_section

readRel32 :: Peek -> SectionType -> Int -> Get Relocation
readRel32 _ _ _ = fail "TODO: readRel32"

readRel64 :: Peek -> SectionType -> Int -> Get Relocation
readRel64 Peek{..} sh_type r_section = do
  r_offset  <- getWord64
  r_info    <- getWord64
  r_addend  <- case sh_type of
                 RelA -> fromIntegral <$> getWord64
                 _    -> return 0
  let r_type    = toEnum (fromIntegral (r_info .&. 0xffffffff))
      r_symbol  = fromIntegral (r_info `shiftR` 32) - 1
  --
  return Relocation {..}


readSymbolTable :: Peek -> Vector SectionHeader -> ByteString -> SectionHeader -> Either String (Vector Symbol)
readSymbolTable p@Peek{..} secs obj SectionHeader{..} = do
  unless (sh_type == SymTab) $ Left "expected symbol table"

  let nsym    = sh_size `quot` sh_entsize
      offset  = sh_offset + sh_entsize  -- First symbol in the table is always null; skip it.
                                        -- Make sure to update relocation indices
  strtab  <- readStringTable obj (secs V.! sh_link)
  symbols <- runGet (V.replicateM (nsym-1) (readSymbol p secs strtab)) (B.drop offset obj)
  return symbols

readSymbol :: Peek -> Vector SectionHeader -> ByteString -> Get Symbol
readSymbol p@Peek{..} secs strtab =
  case is64Bit of
    True  -> readSymbol64 p secs strtab
    False -> readSymbol32 p secs strtab

readSymbol32 :: Peek -> Vector SectionHeader -> ByteString -> Get Symbol
readSymbol32 _ _ _ = fail "TODO: readSymbol32"

readSymbol64 :: Peek -> Vector SectionHeader -> ByteString -> Get Symbol
readSymbol64 Peek{..} secs strtab = do
  st_strx     <- fromIntegral <$> getWord32
  st_info     <- getWord8
  skip 1 -- st_other  <- getWord8
  sym_section <- fromIntegral <$> getWord16
  sym_value   <- getWord64
  skip 8 -- st_size   <- getWord64

  let sym_name
        | sym_type == Section = indexStringTable strtab (sh_name (secs V.! sym_section))
        | st_strx == 0        = B.empty
        | otherwise           = indexStringTable strtab st_strx

      sym_binding = toEnum $ fromIntegral ((st_info .&. 0xF0) `shiftR` 4)
      sym_type    = toEnum $ fromIntegral (st_info .&. 0x0F)

  case sym_section of
    -- External symbol; lookup value
    {#const SHN_UNDEF#} | not (B.null sym_name) -> do
        funptr <- resolveSymbol sym_name
        message (printf "%s: external symbol found at %s" (B8.unpack sym_name) (show funptr))
        return Symbol { sym_value = castPtrToWord64 (castFunPtrToPtr funptr), .. }

    -- Internally defined symbol
    n | n < {#const SHN_LORESERVE#} -> do
        message (printf "%s: local symbol in section %d at 0x%02x" (B8.unpack sym_name) sym_section sym_value)
        return Symbol {..}

    {#const SHN_ABS#} | sym_type == File -> return Symbol {..}
    {#const SHN_ABS#} -> fail "unhandled absolute symbol"
    _                 -> fail "unhandled symbol section"


-- Return the address binding the named symbol
--
resolveSymbol :: ByteString -> Get (FunPtr ())
resolveSymbol name
  = unsafePerformIO
  $ B.unsafeUseAsCString name $ \c_name -> do
      addr <- c_dlsym (packDL Default) c_name
      if addr == nullFunPtr
        then do
          err <- dlerror
          return (fail $ printf "failed to resolve symbol %s: %s" (B8.unpack name) err)
        else do
          return (return addr)


-- Utilities
-- ---------

-- Get the address of a pointer as a Word64
--
castPtrToWord64 :: Ptr a -> Word64
castPtrToWord64 (Ptr addr#) = W64# (int2Word# (addr2Int# addr#))


-- c-bits
-- ------

-- Control the protection of pages
--
mprotect :: Ptr Word8 -> Int -> Int -> IO ()
mprotect addr len prot
  = throwErrnoIfMinus1_ "mprotect"
  $ c_mprotect addr (fromIntegral len) (fromIntegral prot)

foreign import ccall unsafe "mprotect"
  c_mprotect :: Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "getpagesize"
  c_getpagesize :: CInt

#if __GLASGOW_HASKELL__ <= 708
-- Fill a given number of bytes in memory. Added in base-4.8.0.0.
--
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes dest char size = do
  _ <- memset dest (fromIntegral char) (fromIntegral size)
  return ()

foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
#endif


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> a -> a
trace msg = Debug.trace Debug.dump_ld ("ld: " ++ msg)

{-# INLINE message #-}
message :: Monad m => String -> m ()
message msg = trace msg (return ())


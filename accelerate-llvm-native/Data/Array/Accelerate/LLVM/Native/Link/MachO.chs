{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Link.MachO
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Link.MachO (

  loadObject,

) where

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.Native.Link.Object
import Data.Array.Accelerate.Error
import qualified Data.Array.Accelerate.Debug              as Debug

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString                                    ( ByteString )
import Data.Maybe                                         ( catMaybes )
import Data.Serialize.Get
import Data.Vector                                        ( Vector )
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Ptr                                            ( Ptr(..), FunPtr(..) )
import GHC.Prim                                           ( addr2Int#, int2Word# )
import GHC.ForeignPtr                                     ( mallocPlainForeignPtrAlignedBytes )
import GHC.Int                                            ( Int(..) )
import GHC.Word                                           ( Word64(..) )
import System.IO.Unsafe
import System.Posix.DynamicLinker
import Text.Printf
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import qualified Data.ByteString.Internal                 as B
import qualified Data.ByteString.Unsafe                   as B
import qualified Data.Vector                              as V
import Prelude                                            as P

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach/machine.h>
#include <sys/mman.h>


-- Load a Mach-O object file and return pointers to the executable functions
-- defined within. The executable sections are aligned appropriately, as
-- specified in the object file, and are ready to be executed on the target
-- architecture.
--
loadObject :: ByteString -> IO (FunctionTable, ObjectCode)
loadObject obj =
  case parseObject obj of
    Left err            -> $internalError "loadObject" err
    Right (symtab, lcs) -> loadSegments obj symtab lcs


-- Execute the load segment commands and return function pointers to the
-- executable code in the target memory space.
--
loadSegments :: ByteString -> Vector Symbol -> Vector LoadSegment -> IO (FunctionTable, ObjectCode)
loadSegments obj symtab lcs = do
  -- Load the segments into executable memory.
  --
  segs  <- V.mapM (loadSegment obj symtab) lcs

  -- Resolve the symbols defined in the sections of this object into function
  -- pointers
  let local Symbol{..}    = sym_extern && sym_segment > 0
      resolve Symbol{..}  =
        let Segment _ fp  = segs V.! (fromIntegral (sym_segment-1))
            name          = B8.unpack sym_name
            addr          = castPtrToFunPtr (unsafeForeignPtrToPtr fp `plusPtr` fromIntegral sym_value)
        in
        (name, addr)

      funtab              = FunctionTable $ V.toList $ V.map resolve (V.filter local symtab)
      objectcode          = V.toList segs

  -- The executable pages were allocated on the GC heap; unset the executable
  -- bit and mark them as read/write so that they can be reused.
  --
  objectcode' <- newLifetime objectcode
  addFinalizer objectcode' $ do
    Debug.traceIO Debug.dump_gc ("gc: unload module: " ++ show funtab)
    forM_ objectcode $ \(Segment vmsize oc_fp) -> do
      withForeignPtr oc_fp $ \oc_p -> do
        mprotect oc_p vmsize ({#const PROT_READ#} .|. {#const PROT_WRITE#})

  return (funtab, objectcode')

loadSegment :: ByteString -> Vector Symbol -> LoadSegment -> IO Segment
loadSegment obj symtab seg@LoadSegment{..} = do
  let pagesize              = fromIntegral c_getpagesize
  --
  seg_fp  <- mallocPlainForeignPtrAlignedBytes seg_vmsize pagesize
  _       <- withForeignPtr seg_fp $ \seg_p  -> do
              -- Process each of the sections of this segment
              V.mapM_ (loadSection obj symtab seg seg_p) seg_sections

              -- Mark the page as executable and read-only
              -- Should we instead use maxprot and initprot?
              mprotect seg_p seg_vmsize ({#const PROT_READ#} .|. {#const PROT_EXEC#})
  --
  return (Segment seg_vmsize seg_fp)

-- Load a section at the correct offset into the given segment, and apply
-- relocations.
--
loadSection :: ByteString -> Vector Symbol -> LoadSegment -> Ptr Word8 -> LoadSection -> IO ()
loadSection obj symtab seg seg_p sec@LoadSection{..} = do
  let (obj_fp, obj_offset, _) = B.toForeignPtr obj
  --
  withForeignPtr obj_fp $ \obj_p -> do
    -- Copy this section's data to the appropriate place in the segment
    let src = obj_p `plusPtr` (obj_offset + sec_offset)
        dst = seg_p `plusPtr` sec_addr
    --
    copyBytes dst src sec_size
    V.mapM_ (processRelocation symtab seg seg_p sec) sec_relocs

-- Process both local and external relocations. The former are probably not
-- necessary since we load all sections into the same memory segment.
--
processRelocation :: Vector Symbol -> LoadSegment -> Ptr Word8 -> LoadSection -> RelocationInfo -> IO ()
processRelocation symtab LoadSegment{..} seg_p sec RelocationInfo{..} = do
  let
      pc                = seg_p `plusPtr` (sec_addr sec + ri_address)
      val_pcrel         = val - castPtrToWord64 pc
      val | ri_extern   = sym_value (symtab V.! ri_symbolnum)
          | otherwise   = castPtrToWord64 (seg_p `plusPtr` sec_addr (seg_sections V.! ri_symbolnum))
  --
  when ri_extern $
    case ri_length of
      1 -> poke (castPtr pc :: Ptr Word8)  . fromIntegral $ if ri_pcrel then val_pcrel else val
      2 -> poke (castPtr pc :: Ptr Word16) . fromIntegral $ if ri_pcrel then val_pcrel else val
      4 -> poke (castPtr pc :: Ptr Word32) . fromIntegral $ if ri_pcrel then val_pcrel else val
      _ -> $internalError "processRelocation" "unhandled relocation size"


-- Parse the Mach-O object file and return the set of section load commands, as
-- well as the symbols defined within the sections of this object.
--
-- Actually _executing_ the load commands, which entails copying the pointed-to
-- segments into an appropriate VM image in the target address space, happens
-- separately.
--
parseObject :: ByteString -> Either String (Vector Symbol, Vector LoadSegment)
parseObject obj = do
  ((p, ncmd, _), rest)  <- runGetState readHeader obj 0
  cmds                  <- catMaybes <$> runGet (replicateM ncmd (readLoadCommand p obj)) rest
  let
      lc = [ x | LC_Segment     x <- cmds ]
      st = [ x | LC_SymbolTable x <- cmds ]
  --
  return (V.concat st, V.fromListN ncmd lc)


-- Data types
-- ----------

-- Parsing depends on whether the Mach-O file is 64-bit and whether it should be
-- read as big- or little-endian.
--
data Peek = Peek
    { is64Bit   :: !Bool
    , getWord16 :: !(Get Word16)
    , getWord32 :: !(Get Word32)
    , getWord64 :: !(Get Word64)
    }

-- Load commands directly follow the Mach-O header.
--
data LoadCommand
    = LC_Segment      !LoadSegment
    | LC_SymbolTable  !(Vector Symbol)

-- Indicates that a part of this file is to be mapped into the task's
-- address space. The size of the segment in memory, vmsize, must be equal
-- to or larger than the amount to map from this file, filesize. The file is
-- mapped starting at fileoff to the beginning of the segment in memory,
-- vmaddr. If the segment has sections then the section structures directly
-- follow the segment command.
--
-- For compactness object files contain only one (unnamed) segment, which
-- contains all the sections.
--
data LoadSegment = LoadSegment
    { seg_name      :: !ByteString
    , seg_vmaddr    :: !Int                     -- starting virtual memory address of the segment
    , seg_vmsize    :: !Int                     -- size (bytes) of virtual memory occupied by the segment
    , seg_fileoff   :: !Int                     -- offset in the file for the data mapped at 'seg_vmaddr'
    , seg_filesize  :: !Int                     -- size (bytes) of the segment in the file
    , seg_sections  :: !(Vector LoadSection)    -- the sections of this segment
    }

data LoadSection = LoadSection
    { sec_secname   :: !ByteString
    , sec_segname   :: !ByteString
    , sec_addr      :: !Int                     -- virtual memory address of this section
    , sec_size      :: !Int                     -- size in bytes
    , sec_offset    :: !Int                     -- offset of this section in the file
    , sec_align     :: !Int
    , sec_relocs    :: !(Vector RelocationInfo)
    }

data RelocationInfo = RelocationInfo
    { ri_address    :: !Int                     -- offset from start of the section
    , ri_symbolnum  :: !Int                     -- index into the symbol table (when ri_extern=True) else section number (??)
    , ri_pcrel      :: !Bool                    -- item containing the address to be relocated uses PC-relative addressing
    , ri_extern     :: !Bool
    , ri_length     :: !Int                     -- length of address (bytes) to be relocated
    }

-- A symbol defined in the sections of this object
--
data Symbol = Symbol
    { sym_name      :: !ByteString
    , sym_extern    :: !Bool
    , sym_segment   :: !Word8
    , sym_value     :: !Word64
    }


-- Object file parser
-- ------------------

-- The Mach-O file consists of a header block, a number of load commands,
-- followed by the segment data.
--
--   +-------------------+
--   |   Mach-O header   |
--   +-------------------+  <- sizeofheader
--   |   Load command    |
--   |   Load command    |
--   |        ...        |
--   +-------------------+  <- sizeofcmds + sizeofheader
--   |   Segment data    |
--   |   Segment data    |
--   |        ...        |
--   +-------------------+
--
readHeader :: Get (Peek, Int, Int)
readHeader = do
  magic       <- getWord32le
  p@Peek{..}  <- case magic of
                   {#const MH_MAGIC#}    -> return $ Peek False getWord16le getWord32le getWord64le
                   {#const MH_CIGAM#}    -> return $ Peek False getWord16be getWord32be getWord64be
                   {#const MH_MAGIC_64#} -> return $ Peek True  getWord16le getWord32le getWord64le
                   {#const MH_CIGAM_64#} -> return $ Peek True  getWord16be getWord32be getWord64be
                   m                     -> fail (printf "unknown magic: %x" m)
  skip ({#sizeof cpu_type_t#} + {#sizeof cpu_subtype_t#})
  filetype    <- getWord32
  case filetype of
    {#const MH_OBJECT#} -> return ()
    _                   -> fail "expected object file"
  ncmds       <- fromIntegral <$> getWord32
  sizeofcmds  <- fromIntegral <$> getWord32
  skip $ case is64Bit of
           True  -> 8 -- flags + reserved
           False -> 4 -- flags
  return (p, ncmds, sizeofcmds)


-- Read a segment load command from the Mach-O file.
--
-- The only thing we are interested in are the symbol table, which tell us which
-- external symbols are defined by this object, and the load commands, which
-- indicate part of the file is to be mapped into the target address space.
-- These will tell us everything we need to know about the generated machine
-- code in order to execute it.
--
-- Since we are only concerned with loading object files, there should really
-- only be one of each of these.
--
readLoadCommand :: Peek -> ByteString -> Get (Maybe LoadCommand)
readLoadCommand p@Peek{..} obj = do
  cmd     <- getWord32
  cmdsize <- fromIntegral <$> getWord32
  --
  let required = 0 /= cmd .&. {#const LC_REQ_DYLD#}
  --
  case cmd .&. (complement {#const LC_REQ_DYLD#}) of
    {#const LC_SEGMENT#}    -> Just . LC_Segment     <$> readLoadSegment p obj
    {#const LC_SEGMENT_64#} -> Just . LC_Segment     <$> readLoadSegment p obj
    {#const LC_SYMTAB#}     -> Just . LC_SymbolTable <$> readLoadSymbolTable p obj
    {#const LC_LOAD_DYLIB#} -> fail "unhandled LC_LOAD_DYLIB"
    this                    -> do if required
                                    then fail    (printf "unknown load command required for execution: 0x%x" this)
                                    else message (printf "skipping load command: 0x%x" this)
                                  skip (cmdsize - 8)
                                  return Nothing


-- Read a load segment command, including any relocation entries.
--
readLoadSegment :: Peek -> ByteString -> Get LoadSegment
readLoadSegment p@Peek{..} obj =
  if is64Bit
    then readLoadSegment64 p obj
    else readLoadSegment32 p obj

readLoadSegment32 :: Peek -> ByteString -> Get LoadSegment
readLoadSegment32 p@Peek{..} obj = do
  name      <- B.takeWhile (/= 0) <$> getBytes 16
  vmaddr    <- fromIntegral <$> getWord32
  vmsize    <- fromIntegral <$> getWord32
  fileoff   <- fromIntegral <$> getWord32
  filesize  <- fromIntegral <$> getWord32
  skip (2 * {#sizeof vm_prot_t#})
  nsect     <- fromIntegral <$> getWord32
  skip 4 -- flags
  message (printf "LC_SEGMENT:            Mem: 0x%09x-0x09%x" vmaddr (vmaddr + vmsize))
  secs      <- V.replicateM nsect (readLoadSection32 p obj)
  return $ LoadSegment name vmaddr vmsize fileoff filesize secs

readLoadSegment64 :: Peek -> ByteString -> Get LoadSegment
readLoadSegment64 p@Peek{..} obj = do
  name      <- B.takeWhile (/= 0) <$> getBytes 16
  vmaddr    <- fromIntegral <$> getWord64
  vmsize    <- fromIntegral <$> getWord64
  fileoff   <- fromIntegral <$> getWord64
  filesize  <- fromIntegral <$> getWord64
  skip (2 * {#sizeof vm_prot_t#}) -- maxprot, initprot
  nsect     <- fromIntegral <$> getWord32
  skip 4 -- flags
  message (printf "LC_SEGMENT_64:         Mem: 0x%09x-0x%09x" vmaddr (vmaddr + vmsize))
  secs      <- V.replicateM nsect (readLoadSection64 p obj)
  return $ LoadSegment name vmaddr vmsize fileoff filesize secs

readLoadSection32 :: Peek -> ByteString -> Get LoadSection
readLoadSection32 p@Peek{..} obj = do
  secname   <- B.takeWhile (/= 0) <$> getBytes 16
  segname   <- B.takeWhile (/= 0) <$> getBytes 16
  addr      <- fromIntegral <$> getWord32
  size      <- fromIntegral <$> getWord32
  offset    <- fromIntegral <$> getWord32
  align     <- fromIntegral <$> getWord32
  reloff    <- fromIntegral <$> getWord32
  nreloc    <- fromIntegral <$> getWord32
  skip 12 -- flags, reserved1, reserved2
  message (printf "  Mem: 0x%09x-0x%09x         %s.%s" addr (addr+size) (B8.unpack segname) (B8.unpack secname))
  relocs    <- either fail return $ runGet (V.replicateM nreloc (loadRelocation p)) (B.drop reloff obj)
  return $ LoadSection secname segname addr size offset align relocs

readLoadSection64 :: Peek -> ByteString -> Get LoadSection
readLoadSection64 p@Peek{..} obj = do
  secname   <- B.takeWhile (/= 0) <$> getBytes 16
  segname   <- B.takeWhile (/= 0) <$> getBytes 16
  addr      <- fromIntegral <$> getWord64
  size      <- fromIntegral <$> getWord64
  offset    <- fromIntegral <$> getWord32
  align     <- fromIntegral <$> getWord32
  reloff    <- fromIntegral <$> getWord32
  nreloc    <- fromIntegral <$> getWord32
  skip 16 -- flags, reserved1, reserved2, reserved3
  message (printf "  Mem: 0x%09x-0x%09x         %s.%s" addr (addr+size) (B8.unpack segname) (B8.unpack secname))
  relocs    <- either fail return $ runGet (V.replicateM nreloc (loadRelocation p)) (B.drop reloff obj)
  return $ LoadSection secname segname addr size offset align relocs

loadRelocation :: Peek -> Get RelocationInfo
loadRelocation Peek{..} = do
  addr    <- fromIntegral <$> getWord32
  val     <- getWord32
  let symbol  = val .&. 0xFFFFFF
      pcrel   = testBit val 24
      extern  = testBit val 27
      len     = 2 ^ ((val `shiftR` 28) .&. 0x3)
  --
  when (addr .&. {#const R_SCATTERED#} /= 0) $ fail "unhandled scatted relocation info"
  message (printf "    Reloc: 0x%02x to %s %d" addr (if extern then "symbol" else "section") symbol)
  return $ RelocationInfo addr (fromIntegral symbol) pcrel extern len

readLoadSymbolTable :: Peek -> ByteString -> Get (Vector Symbol)
readLoadSymbolTable p@Peek{..} obj = do
  symoff  <- fromIntegral <$> getWord32
  nsyms   <- fromIntegral <$> getWord32
  stroff  <- fromIntegral <$> getWord32
  strsize <- getWord32
  message "LC_SYMTAB"
  message (printf "  symbol table is at offset 0x%x (%d), %d entries" symoff symoff nsyms)
  message (printf "  string table is at offset 0x%x (%d), %d bytes" stroff stroff strsize)
  --
  let symbols = B.drop symoff obj
      strtab  = B.drop stroff obj
  --
  either fail return $ runGet (V.replicateM nsyms (loadSymbol p strtab)) symbols

loadSymbol :: Peek -> ByteString -> Get Symbol
loadSymbol Peek{..} strtab = do
  n_strx  <- fromIntegral <$> getWord32
  n_flag  <- getWord8
  n_sect  <- getWord8
  skip 2 -- n_desc
  n_value <- case is64Bit of
               True  -> fromIntegral <$> getWord64
               False -> fromIntegral <$> getWord32

  let -- Symbols with string table index zero are defined to have a null
      -- name (""). Otherwise, drop the leading underscore.
      str | n_strx == 0 = B.empty
          | otherwise   = B.takeWhile (/= 0) (B.drop n_strx strtab)
      name
          | B.length str > 0 && B8.head str == '_'  = B.tail str
          | otherwise                               = str

      -- Extract the four bit fields of the type flag
      -- n_pext  = n_flag .&. {#const N_PEXT#}  -- private external symbol bit
      n_stab  = n_flag .&. {#const N_STAB#}  -- if any bits set, a symbolic debugging entry
      n_type  = n_flag .&. {#const N_TYPE#}  -- mask for type bits
      n_ext   = n_flag .&. {#const N_EXT#}   -- external symbol bit

  unless (n_stab == 0) $ fail "unhandled symbolic debugging entry (stab)"

  case n_type of
    {#const N_UNDF#} -> do
        funptr <- resolveSymbol name
        message (printf "    %s: external symbol found at %s" (B8.unpack name) (show funptr))
        return $ Symbol name (n_ext /= 0) n_sect (castPtrToWord64 (castFunPtrToPtr funptr))
    {#const N_SECT#} -> do
        message (printf "    %s: local symbol in section %d at 0x%02x" (B8.unpack name) n_sect n_value)
        return $ Symbol name (n_ext /= 0) n_sect n_value
    {#const N_ABS#}  -> fail "unhandled absolute symbol"
    {#const N_PBUD#} -> fail "unhandled prebound (dylib) symbol"
    {#const N_INDR#} -> fail "unhandled indirect symbol"
    _                -> fail "unknown symbol type"


-- Get the address of a pointer as a Word64
--
castPtrToWord64 :: Ptr a -> Word64
castPtrToWord64 (Ptr addr#) = W64# (int2Word# (addr2Int# addr#))

-- Return the address binding the named symbol
--
resolveSymbol :: ByteString -> Get (FunPtr ())
resolveSymbol name
  = unsafePerformIO
  $ B.unsafeUseAsCString name $ \c_name -> do
      addr <- c_dlsym (packDL Next) c_name
      if addr == nullFunPtr
        then do
          err <- dlerror
          return (fail $ printf "failed to resolve symbol %s: %s" (B8.unpack name) err)
        else do
          return (return addr)

-- Control the protection of pages
--
mprotect :: Ptr Word8 -> Int -> Int -> IO ()
mprotect addr len prot
  = throwErrnoIfMinus1_ "mprotect"
  $ c_mprotect (castPtr addr) (fromIntegral len) (fromIntegral prot)

foreign import ccall unsafe "mprotect"
  c_mprotect :: Ptr () -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "getpagesize"
  c_getpagesize :: CInt


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> a -> a
trace msg = Debug.trace Debug.dump_ld ("ld: " ++ msg)

{-# INLINE message #-}
message :: Monad m => String -> m ()
message msg = trace msg (return ())


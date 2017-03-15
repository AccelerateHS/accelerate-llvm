{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
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
import qualified Data.Array.Accelerate.Debug              as Debug

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString                                    ( ByteString )
import Data.List                                          ( partition )
import Data.Maybe                                         ( catMaybes )
import Data.Serialize.Get
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.ForeignPtr                                     ( mallocPlainForeignPtrAlignedBytes )
import Text.Printf
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8
import qualified Data.ByteString.Internal                 as B
import Prelude                                            as P

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach/machine.h>
#include <sys/mman.h>


-- Load a Mach-O object file and return pointers to the executable functions
-- defined within. This function does _not_ resolve external symbols. The
-- executable sections are aligned appropriately, as specified in the object
-- file, and are ready to be executed on the target architecture.
--
loadObject :: ByteString -> IO (FunctionTable, ObjectCode)
loadObject obj =
  case parseObject obj of
    Left err       -> error ("loadObject: " ++ err)
    Right (ld,sym) -> loadSegments obj sym ld


-- Execute the load segment commands and return executable code in the target
-- memory space.
--
loadSegments :: ByteString -> [Symbol] -> [LoadSegment] -> IO (FunctionTable, ObjectCode)
loadSegments obj sym0 ld0 = do
  r <- ld 1 sym0 ld0
  let (funs, segs)  = unzip r
      nm            = FunctionTable (concat funs)
  --
  vm <- newLifetime segs
  addFinalizer vm $ do
    Debug.traceIO Debug.dump_gc ("gc: unload module: " ++ show nm)
    forM_ segs $ \(Segment vmsize fp_vm) -> do
      withForeignPtr fp_vm $ \p_vm -> do
        -- These pages were allocated on the GC heap; unset the executable bit
        -- and mark them as read/write so that they can be reused
        mprotect p_vm vmsize ({#const PROT_READ#} .|. {#const PROT_WRITE#})
  --
  return (nm, vm)
  where
    pagesize      = 4096    -- getpagesize()
    (seg, off, _) = B.toForeignPtr obj

    -- Execute the section load commands. Sections are numbered from 1, in the
    -- order they appear in the load commands of the object file.
    --
    ld _       _       []                     = return []
    ld section symbols (LoadSegment{..}:lss)  = do
      let (this,other) = partition (\Symbol{..} -> sym_section == section) symbols

      -- Allocate a new page-aligned memory block for this section
      vm <- mallocPlainForeignPtrAlignedBytes seg_vmsize pagesize
      fs <- withForeignPtr vm    $ \p_vm  -> do
              withForeignPtr seg $ \p_seg -> do
                -- Copy the object data into the new image at the appropriate
                -- address. Leading bytes are zero filled (NOP).
                fillBytes p_vm 0 seg_vmaddr
                copyBytes (p_vm `plusPtr` seg_vmaddr) (p_seg `plusPtr` (off+seg_fileoff)) seg_filesize

                -- Mark the page as executable and read-only.
                mprotect p_vm seg_vmsize ({#const PROT_READ#} .|. {#const PROT_EXEC#})

                -- Return function pointers for symbols defined in this section
                return [ (name,fp) | Symbol{..} <- this
                                   , sym_extern
                                   , let name = B8.unpack sym_name
                                         fp   = castPtrToFunPtr (p_vm `plusPtr` (sym_offset + seg_vmaddr))
                       ]
      --
      ocs <- ld (section+1) other lss
      return $ (fs, Segment seg_vmsize vm) : ocs


-- Parse the Mach-O object file and return the set of section load commands, as
-- well as the symbols defined within the sections of this object.
--
-- Actually _executing_ the load commands, which entails copying the pointed-to
-- segments into an appropriate VM image in the target address space, happens
-- separately.
--
parseObject :: ByteString -> Either String ([LoadSegment], [Symbol])
parseObject obj = do
  ((p, ncmd, _), rest)  <- runGetState readHeader obj 0
  cmds                  <- catMaybes <$> runGet (replicateM ncmd (readLoadCommand p)) rest
  symbols               <- concat    <$> sequence [ loadSymbolTable p obj st | LC_SymbolTable st <- cmds ]
  return ( [ ld | LC_Segment ld <- cmds ], symbols )


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
    = LC_Segment      LoadSegment
    | LC_SymbolTable  LoadSymbolTable

-- Indicates that a part of this file is to be mapped into the task's
-- address space. The size of the segment in memory, vmsize, must be equal
-- to or larger than the amount to map from this file, filesize. The file is
-- mapped starting at fileoff to the beginning of the segment in memory,
-- vmaddr. If the segment has sections then the section structures directly
-- follow the segment command.
--
data LoadSegment = LoadSegment
    { seg_name      :: !ByteString
    , seg_vmaddr    :: !Int
    , seg_vmsize    :: !Int
    , seg_fileoff   :: !Int
    , seg_filesize  :: !Int
    }

-- Contains offsets and sizes of the symbol table information in the link-edit
-- 4.3BSD "stab" style symbol table information. The actual symbols are
-- extracted in a second step.
--
data LoadSymbolTable = LoadSymbolTable
    { sym_symoff    :: !Int
    , sym_nsyms     :: !Int
    , sym_stroff    :: !Int
    , sym_strsize   :: !Int
    }

-- A symbol defined in the sections of this object
--
data Symbol = Symbol
    { sym_name      :: !ByteString
    , sym_extern    :: !Bool
    , sym_section   :: !Int
    , sym_offset    :: !Int
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
-- code in order to execute it. Due to the nature of the Accelerate generated
-- code, we know we won't need to resolve external dylibs.
--
readLoadCommand :: Peek -> Get (Maybe LoadCommand)
readLoadCommand p@Peek{..} = do
  cmd     <- getWord32
  cmdsize <- fromIntegral <$> getWord32
  --
  case cmd of
    {#const LC_SEGMENT#}    -> Just . LC_Segment     <$> readLoadSegment p cmdsize
    {#const LC_SEGMENT_64#} -> Just . LC_Segment     <$> readLoadSegment p cmdsize
    {#const LC_SYMTAB#}     -> Just . LC_SymbolTable <$> readLoadSymbolTable p
    _                       -> do skip (cmdsize - 8)
                                  return Nothing

readLoadSegment :: Peek -> Int -> Get LoadSegment
readLoadSegment p@Peek{..} cmdsize =
  if is64Bit
    then readLoadSegment64 p cmdsize
    else readLoadSegment32 p cmdsize

readLoadSegment32 :: Peek -> Int -> Get LoadSegment
readLoadSegment32 Peek{..} cmdsize = do
  name      <- B.takeWhile (/= 0) <$> getBytes 16
  vmaddr    <- fromIntegral <$> getWord32
  vmsize    <- fromIntegral <$> getWord32
  fileoff   <- fromIntegral <$> getWord32
  filesize  <- fromIntegral <$> getWord32
  skip (cmdsize-4-4-16-4-4-4-4)
  return $ LoadSegment name vmaddr vmsize fileoff filesize

readLoadSegment64 :: Peek -> Int -> Get LoadSegment
readLoadSegment64 Peek{..} cmdsize = do
  name      <- B.takeWhile (/= 0) <$> getBytes 16
  vmaddr    <- fromIntegral <$> getWord64
  vmsize    <- fromIntegral <$> getWord64
  fileoff   <- fromIntegral <$> getWord64
  filesize  <- fromIntegral <$> getWord64
  skip (cmdsize-4-4-16-8-8-8-8)
  return $ LoadSegment name vmaddr vmsize fileoff filesize

readLoadSymbolTable :: Peek -> Get LoadSymbolTable
readLoadSymbolTable Peek{..} = do
  symoff  <- fromIntegral <$> getWord32
  nsyms   <- fromIntegral <$> getWord32
  stroff  <- fromIntegral <$> getWord32
  strsize <- fromIntegral <$> getWord32
  return $ LoadSymbolTable symoff nsyms stroff strsize


loadSymbolTable :: Peek -> ByteString -> LoadSymbolTable -> Either String [Symbol]
loadSymbolTable p obj LoadSymbolTable{..} =
  let symbols = B.drop sym_symoff obj
      strtab  = B.take sym_strsize (B.drop sym_stroff obj)
  in
  catMaybes <$> runGet (replicateM sym_nsyms (loadSymbol p strtab)) symbols

loadSymbol :: Peek -> ByteString -> Get (Maybe Symbol)
loadSymbol Peek{..} strtab = do
  n_strx  <- fromIntegral <$> getWord32
  n_flag  <- getWord8
  n_sect  <- fromIntegral <$> getWord8
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
    {#const N_UNDF#} -> return Nothing
    {#const N_ABS#}  -> return Nothing
    {#const N_SECT#} -> return . Just $ Symbol name (n_ext /= 0) n_sect n_value
    {#const N_PBUD#} -> fail "unhandled prebound (dylib) symbol"
    {#const N_INDR#} -> fail "unhandled indirect symbol"
    _                -> fail "unknown symbol type"


-- Control the protection of pages.
--
mprotect :: Ptr Word8 -> Int -> Int -> IO ()
mprotect addr len prot
  = throwErrnoIfMinus1_ "mprotect"
  $ c_mprotect (castPtr addr) (fromIntegral len) (fromIntegral prot)

foreign import ccall unsafe "mprotect" c_mprotect :: Ptr () -> CSize -> CInt -> IO CInt


{-# Language TransformListComp, MonadComprehensions #-}
{- |
Module           : Text.LLVM.DebugUtils
Description      : This module interprets the DWARF information associated
                   with a function's argument and return types in order to
                   interpret field name references.
License          : BSD3
Stability        : provisional
Maintainer       : emertens@galois.com
-}
module Text.LLVM.DebugUtils
  ( -- * Definition type analyzer
    Info(..), StructFieldInfo(..), BitfieldInfo(..), UnionFieldInfo(..)
  , computeFunctionTypes, valMdToInfo
  , localVariableNameDeclarations

  -- * Metadata lookup
  , mkMdMap

  -- * Type structure dereference
  , derefInfo
  , fieldIndexByPosition
  , fieldIndexByName

  -- * Info hueristics
  , guessAliasInfo
  , guessTypeInfo

  -- * Function arguments
  , debugInfoArgNames

  -- * Line numbers of definitions
  , debugInfoGlobalLines
  , debugInfoDefineLines
  ) where

import           Control.Applicative    ((<|>))
import           Control.Monad          ((<=<))
import           Data.Bits              (Bits(..))
import           Data.IntMap            (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List              (elemIndex, tails, stripPrefix)
import           Data.Map               (Map)
import qualified Data.Map    as Map
import           Data.Maybe             (fromMaybe, listToMaybe, maybeToList, mapMaybe)
import           Data.Word              (Word16, Word64)
import           Text.LLVM.AST

dbgKind :: String
dbgKind = "dbg"

llvmDbgCuKey :: String
llvmDbgCuKey = "llvm.dbg.cu"

dwarfPointer, dwarfStruct, dwarfTypedef, dwarfUnion, dwarfBasetype,
  dwarfConst, dwarfArray :: Word16
dwarfPointer  = 0x0f
dwarfStruct   = 0x13
dwarfTypedef  = 0x16
dwarfArray    = 0x01
dwarfUnion    = 0x17
dwarfBasetype = 0x24
dwarfConst    = 0x26

type MdMap = IntMap ValMd

data Info
  = Pointer Info
  | Structure (Maybe String) [StructFieldInfo]
  | Union     (Maybe String) [UnionFieldInfo]
  | Typedef String Info
  | ArrInfo Info
  | BaseType String DIBasicType
  | Unknown
  deriving Show

-- | Record debug information about a field in a struct type.
data StructFieldInfo = StructFieldInfo
  { sfiName :: String
    -- ^ The field name.
  , sfiOffset :: Word64
    -- ^ The field's offset (in bits) from the start of the struct.
  , sfiBitfield :: Maybe BitfieldInfo
    -- ^ If this field resides within a bitfield, this is
    -- @'Just' bitfieldInfo@. Otherwise, this is 'Nothing'.
  , sfiInfo :: Info
    -- ^ The debug 'Info' associated with the field's type.
  } deriving Show

-- | Record debug information about a field within a bitfield. For example,
-- the following C struct:
--
-- @
-- struct s {
--   int32_t w;
--   uint8_t x1:1;
--   uint8_t x2:2;
--   uint8_t y:1;
--   int32_t z;
-- };
-- @
--
-- Corresponds to the following 'Info':
--
-- @
-- 'Structure'
--   [ 'StructFieldInfo' { 'sfiName' = \"w\"
--                       , 'sfiOffset' = 0
--                       , 'sfiBitfield' = Nothing
--                       , 'sfiInfo' = 'BaseType' \"int32_t\"
--                       }
--   , 'StructFieldInfo' { 'sfiName' = \"x1\"
--                       , 'sfiOffset' = 32
--                       , 'sfiBitfield' = Just ('BitfieldInfo' { 'biFieldSize' = 1
--                                                              , 'biBitfieldOffset' = 32
--                                                              })
--                       , 'sfiInfo' = 'BaseType' \"uint8_t\"
--                       }
--   , 'StructFieldInfo' { 'sfiName' = \"x2\"
--                       , 'sfiOffset' = 33
--                       , 'sfiBitfield' = Just ('BitfieldInfo' { 'biFieldSize' = 2
--                                                              , 'biBitfieldOffset' = 32
--                                                              })
--                       , 'sfiInfo' = BaseType \"uint8_t\"
--                       }
--   , 'StructFieldInfo' { 'sfiName' = \"y\"
--                       , 'sfiOffset' = 35
--                       , 'sfiBitfield' = Just ('BitfieldInfo' { 'biFieldSize' = 1
--                                                              , 'biBitfieldOffset' = 32
--                                                              })
--                       , 'sfiInfo' = 'BaseType' \"uint8_t\"
--                       }
--   , 'StructFieldInfo' { 'sfiName' = \"z\"
--                       , 'sfiOffset' = 64
--                       , 'sfiBitfield' = Nothing
--                       , 'sfiInfo' = BaseType \"int32_t\"
--                       }
--   ]
-- @
--
-- Notice that only @x1@, @x2@, and @y@ have 'BitfieldInfo's, as they are the
-- only fields that were declared with bitfield syntax.
data BitfieldInfo = BitfieldInfo
  { biFieldSize :: Word64
    -- ^ The field's size (in bits) within the bitfield. This should not be
    --   confused with the size of the field's declared type. For example, the
    --   'biFieldSize' of the @x1@ field is @1@, despite the fact that its
    --   declared type, @uint8_t@, is otherwise 8 bits in size.
  , biBitfieldOffset :: Word64
    -- ^ The bitfield's offset (in bits) from the start of the struct. Note
    --   that for a given field within a bitfield, its 'sfiOffset' is equal to
    --   the 'biBitfieldOffset' plus the 'biFieldSize'.
  } deriving Show

-- | Record debug information about a field in a union type.
data UnionFieldInfo = UnionFieldInfo
  { ufiName :: String
    -- ^ The field name.
  , ufiInfo :: Info
    -- ^ The debug 'Info' associated with the field's type.
  } deriving Show

-- | Compute an 'IntMap' of the unnamed metadata in a module
mkMdMap :: Module -> IntMap ValMd
mkMdMap m = IntMap.fromList [ (umIndex md, umValues md) | md <- modUnnamedMd m ]

------------------------------------------------------------------------

getDebugInfo :: MdMap -> ValMd -> Maybe DebugInfo
getDebugInfo mdMap (ValMdRef i)    = getDebugInfo mdMap =<< IntMap.lookup i mdMap
getDebugInfo _ (ValMdDebugInfo di) = Just di
getDebugInfo _ _                   = Nothing

getInteger :: MdMap -> ValMd -> Maybe Integer
getInteger mdMap (ValMdRef i)                          = getInteger mdMap =<< IntMap.lookup i mdMap
getInteger _     (ValMdValue (Typed _ (ValInteger i))) = Just i
getInteger _     _                                     = Nothing

getList :: MdMap -> ValMd -> Maybe [Maybe ValMd]
getList mdMap (ValMdRef i) = getList mdMap =<< IntMap.lookup i mdMap
getList _ (ValMdNode di)   = Just di
getList _ _                = Nothing

------------------------------------------------------------------------

valMdToInfo :: MdMap -> ValMd -> Info
valMdToInfo mdMap val =
  maybe Unknown (debugInfoToInfo mdMap) (getDebugInfo mdMap val)


valMdToInfo' :: MdMap -> Maybe ValMd -> Info
valMdToInfo' = maybe Unknown . valMdToInfo


debugInfoToInfo :: MdMap -> DebugInfo -> Info
debugInfoToInfo mdMap (DebugInfoDerivedType dt)
  | didtTag dt == dwarfPointer  = Pointer (valMdToInfo' mdMap (didtBaseType dt))
  | didtTag dt == dwarfTypedef  = case didtName dt of
                                    Nothing -> valMdToInfo' mdMap (didtBaseType dt)
                                    Just nm -> Typedef nm (valMdToInfo' mdMap (didtBaseType dt))
  | didtTag dt == dwarfConst    = valMdToInfo' mdMap (didtBaseType dt)
debugInfoToInfo _     (DebugInfoBasicType bt)
  | dibtTag bt == dwarfBasetype = BaseType (dibtName bt) bt
debugInfoToInfo mdMap (DebugInfoCompositeType ct)
  | dictTag ct == dwarfStruct   = maybe Unknown (Structure (dictName ct)) (getStructFields mdMap ct)
  | dictTag ct == dwarfUnion    = maybe Unknown (Union     (dictName ct)) (getUnionFields mdMap ct)
  | dictTag ct == dwarfArray    = ArrInfo (valMdToInfo' mdMap (dictBaseType ct))
debugInfoToInfo _ _             = Unknown


getFieldDIs :: MdMap -> DICompositeType -> Maybe [DebugInfo]
getFieldDIs mdMap =
  traverse (getDebugInfo mdMap) <=< sequence <=< getList mdMap <=< dictElements

getStructFields :: MdMap -> DICompositeType -> Maybe [StructFieldInfo]
getStructFields mdMap = traverse (debugInfoToStructField mdMap) <=< getFieldDIs mdMap

debugInfoToStructField :: MdMap -> DebugInfo -> Maybe StructFieldInfo
debugInfoToStructField mdMap di =
  do DebugInfoDerivedType dt <- Just di
     fieldName               <- didtName dt
     -- We check if a struct field resides within a bitfield by checking its
     -- `flags` field sets `BitField`, which has a numeric value of 19.
     -- (https://github.com/llvm/llvm-project/blob/1bebc31c617d1a0773f1d561f02dd17c5e83b23b/llvm/include/llvm/IR/DebugInfoFlags.def#L51)
     --
     -- If so, the `size` field records the size in bits, and the `extraData`
     -- field records the offset of the overall bitfield from the start of the
     -- struct.
     -- (https://github.com/llvm/llvm-project/blob/ee7652569854af567ba83e5255d70e80cc8619a1/llvm/lib/CodeGen/AsmPrinter/CodeViewDebug.cpp#L2489-L2508)
     let bitfield | testBit (didtFlags dt) 19
                  , Just extraData      <- didtExtraData dt
                  , Just bitfieldOffset <- getInteger mdMap extraData
                  = do size <- getSizeOrOffset (didtSize dt)
                       Just $ BitfieldInfo { biFieldSize      = size
                                           , biBitfieldOffset = fromInteger bitfieldOffset
                                           }
                  | otherwise
                  = Nothing
     offset <- getSizeOrOffset (didtOffset dt)
     Just (StructFieldInfo { sfiName     = fieldName
                           , sfiOffset   = offset
                           , sfiBitfield = bitfield
                           , sfiInfo     = valMdToInfo' mdMap (didtBaseType dt)
                           })
  where
    -- TODO: Currently, this only recognizes bare integer (i.e., 'ValInteger')
    -- sizes and offsets. This is likely good enough for Clang-derived LLVM, but
    -- Ada-derived LLVM may contain more complex metadata values that this
    -- currently doesn't handle.
    getSizeOrOffset :: Maybe ValMd -> Maybe Word64
    getSizeOrOffset (Just (ValMdValue tv))
      | ValInteger i <- typedValue tv
      = Just (fromInteger i)
    getSizeOrOffset _ = Nothing


getUnionFields :: MdMap -> DICompositeType -> Maybe [UnionFieldInfo]
getUnionFields mdMap = traverse (debugInfoToUnionField mdMap) <=< getFieldDIs mdMap


debugInfoToUnionField :: MdMap -> DebugInfo -> Maybe UnionFieldInfo
debugInfoToUnionField mdMap di =
  do DebugInfoDerivedType dt <- Just di
     fieldName               <- didtName dt
     Just (UnionFieldInfo { ufiName = fieldName
                          , ufiInfo = valMdToInfo' mdMap (didtBaseType dt)
                          })



-- | Compute the structures of a function's return and argument types
-- using DWARF information metadata of the LLVM module. Different
-- versions of LLVM make this information available via different
-- paths. This function attempts to support the variations.
computeFunctionTypes ::
  Module       {- ^ module to search                     -} ->
  Symbol       {- ^ function symbol                      -} ->
  Maybe [Maybe Info] {- ^ return and argument type information -}
computeFunctionTypes m sym =
  [ fmap (valMdToInfo mdMap) <$> types
     | let mdMap = mkMdMap m
     , sp <- findSubprogramViaDefine mdMap m sym
         <|> findSubprogramViaCu     mdMap m sym
     , DebugInfoSubroutineType st <- getDebugInfo mdMap =<< dispType sp
     , types                      <- getList mdMap      =<< distTypeArray st
     ]


-- | This method of computing argument type information works on at least LLVM 3.8
findSubprogramViaDefine ::
  IntMap ValMd       {- ^ unnamed metadata                             -} ->
  Module             {- ^ module to search                             -} ->
  Symbol             {- ^ function symbol to find                      -} ->
  Maybe DISubprogram {- ^ debug information related to function symbol -}
findSubprogramViaDefine mdMap m sym =
  [ sp
     | def                    <- modDefines m
     , defName def == sym
     , then listToMaybe ----- commits to a choice -----
     , dbgMd                  <- Map.lookup dbgKind (defMetadata def)
     , DebugInfoSubprogram sp <- getDebugInfo mdMap dbgMd
     ]


-- | This method of computing function debugging information works on LLVM 3.7
findSubprogramViaCu ::
  MdMap              {- ^ map of unnamed metadata                -} ->
  Module             {- ^ module to search                       -} ->
  Symbol             {- ^ function symbol to search for          -} ->
  Maybe DISubprogram {- ^ debugging information for given symbol -}
findSubprogramViaCu mdMap m (Symbol sym) = listToMaybe
  [ sp
    | md                      <- modNamedMd m
    , nmName md == llvmDbgCuKey
    , ref                     <- nmValues md
    , DebugInfoCompileUnit cu <- maybeToList  $ getDebugInfo mdMap $ ValMdRef ref
    , Just entry              <- fromMaybe [] $ getList mdMap =<< dicuSubprograms cu
    , DebugInfoSubprogram sp  <- maybeToList  $ getDebugInfo mdMap entry
    , dispName sp == Just sym
    ]


------------------------------------------------------------------------

-- | If the argument describes a pointer, return the information for the
-- type that it points do. If the argument describes an array, return
-- information about the element type.
derefInfo ::
  Info {- ^ pointer type information                -} ->
  Info {- ^ type information of pointer's base type -}
derefInfo (Pointer x) = x
derefInfo (ArrInfo x) = x
derefInfo _           = Unknown

-- | If the argument describes a composite type, returns the type of the
-- field by zero-based index into the list of fields.
fieldIndexByPosition ::
  Int  {- ^ zero-based field index               -} ->
  Info {- ^ composite type information           -} ->
  Info {- ^ type information for specified field -}
fieldIndexByPosition i info =
  case info of
    Typedef _ info' -> fieldIndexByPosition i info'
    Structure _ xs  -> go [ x | StructFieldInfo{sfiInfo = x} <- xs ]
    Union     _ xs  -> go [ x | UnionFieldInfo{ufiInfo = x}  <- xs ]
    _               -> Unknown
  where
    go xs = case drop i xs of
              []  -> Unknown
              x:_ -> x

-- | If the argument describes a composite type, return the first, zero-based
-- index of the field in that type that matches the given name.
fieldIndexByName ::
  String    {- ^ field name                                  -} ->
  Info      {- ^ composite type info                         -} ->
  Maybe Int {- ^ zero-based index of field matching the name -}
fieldIndexByName n info =
  case info of
    Typedef _ info' -> fieldIndexByName n info'
    Structure _ xs  -> go [ x | StructFieldInfo{sfiName = x} <- xs ]
    Union     _ xs  -> go [ x | UnionFieldInfo{ufiName = x}  <- xs ]
    _               -> Nothing
  where
    go = elemIndex n

------------------------------------------------------------------------

localVariableNameDeclarations ::
  IntMap ValMd    {- ^ unnamed metadata      -} ->
  Define          {- ^ function definition   -} ->
  Map Ident Ident {- ^ raw name, actual name -}
localVariableNameDeclarations mdMap def =
  case defBody def of
    blk1 : _ -> foldr aux Map.empty (tails (bbStmts blk1))
    _        -> Map.empty
  where

    aux :: [Stmt] -> Map Ident Ident -> Map Ident Ident
    aux ( Effect (Store False src dst _ _) _ _
        : Effect (Call _ _ _ (ValSymbol (Symbol what)) [var,md,_]) _ _
        : _) sofar
      | what == "llvm.dbg.declare"  -- pre-LLVM19: intrinsic declaration match
      , Just dstIdent <- extractIdent dst
      , Just srcIdent <- extractIdent src
      , Just varIdent <- extractIdent var
      , dstIdent == varIdent
      , Just name <- extractLvName md
      = Map.insert name srcIdent sofar

    aux ( Effect (Call _ _ _ (ValSymbol (Symbol what)) [var,_,md,_]) _ _
        : _) sofar
      | what == "llvm.dbg.value"  -- pre-LLVM19: intrinsic declaration match
      , Just key  <- extractIdent var
      , Just name <- extractLvName md
      = Map.insert name key sofar

    aux _ sofar = sofar

    extractIdent :: Typed Value -> Maybe Ident
    extractIdent (Typed _ (ValIdent i)) = Just i
    extractIdent _                      = Nothing

    extractLvName :: Typed Value -> Maybe Ident
    extractLvName mdArg =
      do ValMd md                    <- Just (typedValue mdArg)
         DebugInfoLocalVariable dilv <- getDebugInfo mdMap md
         Ident <$> dilvName dilv

------------------------------------------------------------------------

-- | Search the metadata for debug info corresponding
-- to a given type alias. This is considered a heuristic
-- because there's no direct mapping between type aliases
-- and debug info. The debug information must be search
-- for a textual match.
--
-- Compared to @guessTypeInfo@, this function first tries
-- to strip the \"struct.\" and \"union.\" prefixes that are
-- commonly added by clang before searching for the type information.
guessAliasInfo ::
  IntMap ValMd    {- ^ unnamed metadata      -} ->
  Ident           {- ^ alias                 -} ->
  Info
guessAliasInfo mdMap (Ident name)
  | Just pfx <- stripPrefix "struct." name = guessTypeInfo mdMap pfx
  | Just pfx <- stripPrefix "union."  name = guessTypeInfo mdMap pfx
  | otherwise = guessTypeInfo mdMap name

-- | Search the metadata for debug info corresponding
-- to a given type alias. This is considered a heuristic
-- because there's no direct mapping between type aliases
-- and debug info. The debug information must be search
-- for a textual match.
guessTypeInfo ::
  IntMap ValMd    {- ^ unnamed metadata      -} ->
  String          {- ^ struct alias          -} ->
  Info
guessTypeInfo mdMap name =
  case mapMaybe (go <=< getDebugInfo mdMap) (IntMap.elems mdMap) of
    []  -> Unknown
    x:_ -> x

  where
    go di | DebugInfoDerivedType didt <- di
          , Just name == didtName didt
          = Just (debugInfoToInfo mdMap di)

    go di | DebugInfoCompositeType dict <- di
          , Just name == dictName dict
          = Just (debugInfoToInfo mdMap di)

    go _ = Nothing

------------------------------------------------------------------------

-- | Find source-level names of function arguments
debugInfoArgNames :: Module -> Define -> IntMap String
debugInfoArgNames m d =
  case Map.lookup dbgKind $ defMetadata d of
    Just (ValMdRef s) -> scopeArgs s
    _ -> IntMap.empty
  where
    scopeArgs :: Int -> IntMap String
    scopeArgs s = IntMap.fromList . mapMaybe go $ modUnnamedMd m
      where
        go :: UnnamedMd -> Maybe (Int, String)
        go
          ( UnnamedMd
              { umValues =
                  ValMdDebugInfo
                    ( DebugInfoLocalVariable
                        DILocalVariable
                          { dilvScope = Just (ValMdRef s'),
                            dilvArg = a,
                            dilvName = Just n
                          }
                      )
              }) =
            if s == s'
            then Just (fromIntegral a - 1, n)
            else Nothing
        go _ = Nothing

------------------------------------------------------------------------

-- | Map global variable names to the line on which the global is defined
debugInfoGlobalLines :: Module -> Map String Int
debugInfoGlobalLines = Map.fromList . mapMaybe go . modUnnamedMd
  where
    go :: UnnamedMd -> Maybe (String, Int)
    go (UnnamedMd
         { umValues = ValMdDebugInfo
           (DebugInfoGlobalVariable DIGlobalVariable
             { digvName = Just n
             , digvLine = l
             }
           )
         }) = Just (n, (fromIntegral l))
    go _ = Nothing

-- | Map function names to the line on which the function is defined
debugInfoDefineLines :: Module -> Map String Int
debugInfoDefineLines = Map.fromList . mapMaybe go . modUnnamedMd
  where
    go :: UnnamedMd -> Maybe (String, Int)
    go (UnnamedMd
         { umValues = ValMdDebugInfo
           (DebugInfoSubprogram DISubprogram
             { dispName = Just n
             , dispIsDefinition = True
             , dispLine = l
             }
           )
         }) = Just (n, (fromIntegral l))
    go _ = Nothing

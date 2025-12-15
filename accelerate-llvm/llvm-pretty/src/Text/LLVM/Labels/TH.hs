{-# Language TemplateHaskell #-}
module Text.LLVM.Labels.TH (generateRelabel) where

import Control.Monad (zipWithM)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

generateRelabel :: Name -> Name -> ExpQ
generateRelabel relabel dataCon =
  do di <- reifyDatatype dataCon
     generateRelabelData di (varE relabel)

generateRelabelData :: DatatypeInfo -> ExpQ -> ExpQ
generateRelabelData di relabelE =
  [| \f x -> $(caseE [| x |] (mkMatch [| f |] <$> cons)) |]
  where
    mkMatch = generateRelabelCon lastArg relabelE
    lastArg = tvName (last (datatypeVars di))
    cons    = datatypeCons di

-- | Generates the case arm for the given constructor that
-- relabels values using this constructor given a relabeling
-- function.
generateRelabelCon ::
  Name            {- ^ last type parameter            -} ->
  ExpQ            {- ^ recusive relabel expression    -} ->
  ExpQ            {- ^ function expression            -} ->
  ConstructorInfo {- ^ current constructor            -} ->
  MatchQ          {- ^ match arm for this constructor -}
generateRelabelCon lastArg relabelE fE ci =
  do names <- nameThings "x" (constructorFields ci)
     match
      (conP cn (map (varP . fst) names))
      (normalB (bodyExp cn (map gen names)))
      []
  where
    cn = constructorName ci

    -- Give a field name and type returns:
    -- Left for a pure field
    -- Right for a field using the Applicative instance
    gen :: (Name, Type) -> Either ExpQ ExpQ
    gen (n,t) =
      let nE = varE n in
      case generateRelabelField lastArg fE relabelE t of
        Just f  -> Right [| $f $nE |]
        Nothing -> Left nE

-- | Given a constructor and a list of pure and updated fields,
-- build syntax that rebuilds the expression.
bodyExp ::
  Name               {- ^ constructor                         -} ->
  [Either ExpQ ExpQ] {- ^ list of pure and applicative fields -} ->
  ExpQ               {- ^ applicative result                  -}
bodyExp conname fields = liftAE conLike updates
  where
    updates = [r | Right r <- fields]

    -- Builds a value suitable to be the argument to liftAE that can
    -- combine all of the updated field values
    conLike =
      do names <- map fst <$> nameThings "y" updates
         lamE
           (map varP names)
           (appsE (conE conname : replaceRights (map varE names) fields))

-- | Replaces all of the 'Right' values in the given list with elements
-- from the first list. The number of replacements must exactly match
-- the number of 'Right' values.
replaceRights ::
  [a]          {- ^ replacements  -} ->
  [Either a b] {- ^ source list   -} ->
  [a]          {- ^ replaced list -}
replaceRights xs     (Left y  : ys) = y : replaceRights xs ys
replaceRights (x:xs) (Right _ : ys) = x : replaceRights xs ys
replaceRights []     []             = []
replaceRights _      _              = error "Text.LLVM.Labels.TH.replaceRights: PANIC"

-- | Generate the applicative update value for a field if it
-- has an appropriate type otherwise return nothing if it
-- should be left unchagned.
generateRelabelField ::
  Name       {- ^ last type parameter         -} ->
  ExpQ       {- ^ function expression         -} ->
  ExpQ       {- ^ relabel expression          -} ->
  Type       {- ^ field type                  -} ->
  Maybe ExpQ {- ^ applicative update function -}
generateRelabelField lastArg fE relabelE t =
  case typeDepth t of
    (n, VarT tn) | tn == lastArg -> Just (exprs !! n)
    _                            -> Nothing
  where
    exprs = [| $fE Nothing |] : iterate traverseE [| $relabelE $fE |]

-- | Figure out the depth of the outer type applications and
-- return the type at the bottom of the stack
typeDepth ::
  Type        {- ^ target type                                     -} ->
  (Int, Type) {- ^ number of type applications and right-most type -}
typeDepth = go 0
  where
    go i (AppT _ x) = go (i+1) x
    go i t          = (i, t)

-- | Associate each element in a list of things with a unique name
-- derived from a given name stem.
nameThings ::
  String        {- ^ base name                       -} ->
  [a]           {- ^ things to name                  -} ->
  Q [(Name, a)] {- ^ things paired with unique names -}
nameThings base xs = zipWithM nameThing [0 :: Int ..] xs
  where
    nameThing i x = do n <- newName (base ++ show i); return (n,x)

-- | Apply 'traverse' to an expression
traverseE ::
  ExpQ {- ^ f          -} ->
  ExpQ {- ^ traverse f -}
traverseE e = [| traverse $e |]

-- Applies a pure value to zero or more applicative things to be combined
-- with (<$>) and (<*>)
liftAE :: ExpQ -> [ExpQ] -> ExpQ
liftAE c []     = [| pure $c |]
liftAE c (x:xs) = foldl (\f e -> [| $f <*> $e |]) [| $c <$> $x |] xs

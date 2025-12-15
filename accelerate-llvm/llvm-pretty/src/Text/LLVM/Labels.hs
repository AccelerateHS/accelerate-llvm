{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase, TypeOperators, FlexibleContexts #-}
module Text.LLVM.Labels where

import Text.LLVM.AST
import Text.LLVM.Labels.TH

class Functor f => HasLabel f where
  -- | Given a function for resolving labels, where the presence of a symbol
  -- denotes a label in a different function, rename all labels in a function.
  relabel :: Applicative m => (Maybe Symbol -> a -> m b) -> f a -> m (f b)

instance HasLabel Instr' where
  relabel _ RetVoid               = pure  RetVoid
  relabel _ Unreachable           = pure  Unreachable
  relabel _ Unwind                = pure  Unwind
  relabel _ (Comment str)         = pure (Comment str)
  relabel f (Ret tv)              = Ret <$> traverse (relabel f) tv
  relabel f (Arith op l r)        = Arith op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (UnaryArith op a)     = UnaryArith op
                                <$> traverse (relabel f) a
  relabel f (Bit op l r)          = Bit op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (Conv op l r)         = Conv op <$> traverse (relabel f) l <*> pure r
  relabel f (Call t fmf r n as)   = Call t fmf r
                                <$> relabel f n
                                <*> traverse (traverse (relabel f)) as
  relabel f (CallBr r n as u es)  = CallBr r
                                <$> relabel f n
                                <*> traverse (traverse (relabel f)) as
                                <*> f Nothing u
                                <*> traverse (f Nothing) es
  relabel f (Alloca t n a)        = Alloca t
                                <$> traverse (traverse (relabel f)) n
                                <*> pure a
  relabel f (Load vol t a mo ma)  = Load vol t
                                <$> traverse (relabel f) a
                                <*> pure mo
                                <*> pure ma
  relabel f (Store vol d v mo ma) = Store vol
                                <$> traverse (relabel f) d
                                <*> traverse (relabel f) v
                                <*> pure mo
                                <*> pure ma
  relabel _ (Fence s o)           = pure (Fence s o)
  relabel f (CmpXchg w v p a n s o o')
                                  = CmpXchg w v
                                <$> traverse (relabel f) p
                                <*> traverse (relabel f) a
                                <*> traverse (relabel f) n
                                <*> pure s
                                <*> pure o
                                <*> pure o'
  relabel f (AtomicRW v op p a s o)
                                  = AtomicRW v op
                                <$> traverse (relabel f) p
                                <*> traverse (relabel f) a
                                <*> pure s
                                <*> pure o
  relabel f (ICmp samesign op l r)
                                  = ICmp samesign op
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (FCmp op fmf l r)     = FCmp op fmf
                                <$> traverse (relabel f) l
                                <*> relabel f r
  relabel f (GEP ib t a is)       = GEP ib t
                                <$> traverse (relabel f) a
                                <*> traverse (traverse (relabel f)) is
  relabel f (Select fmf c l r)    = Select fmf
                                <$> traverse (relabel f) c
                                <*> traverse (relabel f) l <*> relabel f r
  relabel f (ExtractValue a is)   = ExtractValue
                                <$> traverse (relabel f) a
                                <*> pure is
  relabel f (InsertValue a i is)  = InsertValue
                                <$> traverse (relabel f) a
                                <*> traverse (relabel f) i
                                <*> pure is
  relabel f (ShuffleVector a b m) = ShuffleVector
                                <$> traverse (relabel f) a
                                <*> relabel f b
                                <*> traverse (relabel f) m
  relabel f (Jump lab)            = Jump <$> f Nothing lab
  relabel f (Br c l r)            = Br
                                <$> traverse (relabel f) c
                                <*> f Nothing l
                                <*> f Nothing r
  relabel f (Invoke r s as u e)   = Invoke r
                                <$> relabel f s
                                <*> traverse (traverse (relabel f)) as
                                <*> f Nothing u
                                <*> f Nothing e
  relabel f (VaArg al t)          = VaArg
                                <$> traverse (relabel f) al
                                <*> pure t
  relabel f (ExtractElt v i)      = ExtractElt
                                <$> traverse (relabel f) v
                                <*> relabel f i
  relabel f (InsertElt v e i)     = InsertElt
                                <$> traverse (relabel f) v
                                <*> traverse (relabel f) e
                                <*> relabel f i
  relabel f (IndirectBr d ls)     = IndirectBr
                                <$> traverse (relabel f) d
                                <*> traverse (f Nothing) ls
  relabel f (Switch c d ls)       =
    let step (n,i) = (\l -> (n,l)) <$> f Nothing i
     in Switch <$> traverse (relabel f) c <*> f Nothing d <*> traverse step ls
  relabel f (Phi fmf t ls)        =
    let step (a,l) = (,) <$> relabel f a <*> f Nothing l
     in Phi fmf t <$> traverse step ls

  relabel f (LandingPad ty fn c cs) = LandingPad ty
                                  <$> traverse (traverse (relabel f)) fn
                                  <*> pure c
                                  <*> traverse (relabel f) cs

  relabel f (Resume tv)           = Resume <$> traverse (relabel f) tv
  relabel f (Freeze tv)           = Freeze <$> traverse (relabel f) tv

instance HasLabel Stmt'                       where relabel = $(generateRelabel 'relabel ''Stmt')
instance HasLabel Clause'                     where relabel = $(generateRelabel 'relabel ''Clause')
instance HasLabel Value'                      where relabel = $(generateRelabel 'relabel ''Value')
instance HasLabel ValMd'                      where relabel = $(generateRelabel 'relabel ''ValMd')
instance HasLabel DebugRecord'                where relabel = $(generateRelabel 'relabel ''DebugRecord')
instance HasLabel DbgRecAssign'               where relabel = $(generateRelabel 'relabel ''DbgRecAssign')
instance HasLabel DbgRecDeclare'              where relabel = $(generateRelabel 'relabel ''DbgRecDeclare')
instance HasLabel DbgRecLabel'                where relabel = $(generateRelabel 'relabel ''DbgRecLabel')
instance HasLabel DbgRecValueSimple'          where relabel = $(generateRelabel 'relabel ''DbgRecValueSimple')
instance HasLabel DbgRecValue'                where relabel = $(generateRelabel 'relabel ''DbgRecValue')
instance HasLabel DILabel'                    where relabel = $(generateRelabel 'relabel ''DILabel')
instance HasLabel DebugLoc'                   where relabel = $(generateRelabel 'relabel ''DebugLoc')
instance HasLabel DebugInfo'                  where relabel = $(generateRelabel 'relabel ''DebugInfo')
instance HasLabel DIBasicType'                where relabel = $(generateRelabel 'relabel ''DIBasicType')
instance HasLabel DIDerivedType'              where relabel = $(generateRelabel 'relabel ''DIDerivedType')
instance HasLabel DISubroutineType'           where relabel = $(generateRelabel 'relabel ''DISubroutineType')
instance HasLabel DISubrange'                 where relabel = $(generateRelabel 'relabel ''DISubrange')
instance HasLabel DIGlobalVariable'           where relabel = $(generateRelabel 'relabel ''DIGlobalVariable')
instance HasLabel DIGlobalVariableExpression' where relabel = $(generateRelabel 'relabel ''DIGlobalVariableExpression')
instance HasLabel DILocalVariable'            where relabel = $(generateRelabel 'relabel ''DILocalVariable')
instance HasLabel DISubprogram'               where relabel = $(generateRelabel 'relabel ''DISubprogram')
instance HasLabel DICompositeType'            where relabel = $(generateRelabel 'relabel ''DICompositeType')
instance HasLabel DILexicalBlock'             where relabel = $(generateRelabel 'relabel ''DILexicalBlock')
instance HasLabel DICompileUnit'              where relabel = $(generateRelabel 'relabel ''DICompileUnit')
instance HasLabel DILexicalBlockFile'         where relabel = $(generateRelabel 'relabel ''DILexicalBlockFile')
instance HasLabel DINameSpace'                where relabel = $(generateRelabel 'relabel ''DINameSpace')
instance HasLabel DITemplateTypeParameter'    where relabel = $(generateRelabel 'relabel ''DITemplateTypeParameter')
instance HasLabel DITemplateValueParameter'   where relabel = $(generateRelabel 'relabel ''DITemplateValueParameter')
instance HasLabel DIImportedEntity'           where relabel = $(generateRelabel 'relabel ''DIImportedEntity')
instance HasLabel DIArgList'                  where relabel = $(generateRelabel 'relabel ''DIArgList')

-- | Clever instance that actually uses the block name
instance HasLabel ConstExpr' where
  relabel f (ConstBlockAddr t@(Typed { typedValue = ValSymbol s }) l) =
    ConstBlockAddr <$> traverse (relabel f) t <*> f (Just s) l
  relabel f x = $(generateRelabel 'relabel ''ConstExpr') f x

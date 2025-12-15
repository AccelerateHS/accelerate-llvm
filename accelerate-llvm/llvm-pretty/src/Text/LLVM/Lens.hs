{-# LANGUAGE TemplateHaskell #-}
module Text.LLVM.Lens where

import Text.LLVM
import Lens.Micro.TH
import Lens.Micro
import Language.Haskell.TH.Syntax (mkName, nameBase)

concat <$> mapM (makeLensesWith (lensRules & lensField .~ (\_ _ n -> [TopName $ mkName $ nameBase n ++ "Lens"])))
    [ ''Module
    , ''LayoutSpec
    , ''TypeDecl
    , ''GlobalAlias
    , ''ConstExpr'
    , ''Type'
    , ''Mangling
    , ''NamedMd
    , ''Value'
    , ''BlockLabel
    , ''UnnamedMd
    , ''Typed
    , ''Global
    , ''Declare
    , ''Clause'
    , ''FunAttr
    , ''GlobalAttrs
    , ''BasicBlock'
    , ''Stmt'
    , ''Linkage
    , ''DebugLoc'
    , ''DebugInfo'
    , ''DIFile
    , ''DISubrange'
    , ''DIBasicType'
    , ''DIExpression
    , ''DISubprogram'
    , ''DISubroutineType'
    , ''DILocalVariable'
    , ''DIGlobalVariableExpression'
    , ''DIGlobalVariable'
    , ''DICompileUnit'
    , ''DICompositeType'
    , ''DIDerivedType'
    , ''DILexicalBlock'
    , ''DILexicalBlockFile'
    , ''DIArgList'
    , ''Instr'
    , ''ValMd'
    , ''ConvOp
    , ''BitOp
    , ''ArithOp
    , ''FCmpOp
    , ''ICmpOp
    , ''GC
    , ''Define
    , ''PrimType
    ]

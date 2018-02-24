{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Analysis.Debug
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  prototype
-- Portability :  ghc
--
-- Pretty printing the semantic analysis representation.
-- This is currently only intended for debugging purposes.
-----------------------------------------------------------------------------
module Language.C.Analysis.Debug (
globalDeclStats,
prettyAssocs, prettyAssocsWith,
-- and many pretty instances
)
where
import Language.C.Analysis.SemRep
import Language.C.Analysis.Export
import Language.C.Analysis.DefTable
import Language.C.Analysis.NameSpaceMap

import Language.C.Data
import Language.C.Pretty
import Language.C.Syntax

import Text.PrettyPrint.HughesPJ
import Data.Map (Map) ; import qualified Data.Map as Map

prettyAssocs :: (Pretty k, Pretty v) => String -> [(k,v)] -> Doc
prettyAssocs label = prettyAssocsWith label pretty pretty
prettyAssocsWith :: String -> (k -> Doc) -> (v -> Doc) -> [(k,v)] -> Doc
prettyAssocsWith label prettyKey prettyVal theMap =
    text label $$ (nest 8) (vcat $ map prettyEntry theMap)
    where
    prettyEntry (k,v) = prettyKey k <+> text " ~> " <+> prettyVal v

instance Pretty DefTable where
    pretty dt = text "DefTable" $$ (nest 4 $ vcat defMaps)
        where
        defMaps = [ prettyNSMap "idents" identDecls
                  , prettyNSMap "tags" tagDecls
                  , prettyNSMap "labels" labelDefs
                  , prettyNSMap "members" memberDecls
                  ]
        prettyNSMap label f = prettyAssocs label . nsMapToList $ f dt

instance Pretty GlobalDecls where
    pretty gd = text "Global Declarations" $$ (nest 4 $ vcat declMaps)
        where
        declMaps = [ prettyMap "enumerators" theEnums, prettyMap "declarations" theDecls,
                     prettyMap "objects" theObjs,  prettyMap "functions" theFuns,
                     prettyMap "tags"    $ gTags gd,  prettyMap "typeDefs"  $ gTypeDefs gd ]
        prettyMap :: (Pretty t, Pretty k) => String -> Map k t -> Doc
        prettyMap label = prettyAssocs label . Map.assocs
        (theDecls, (theEnums, theObjs, theFuns)) = splitIdentDecls False (gObjs gd)

globalDeclStats :: (FilePath -> Bool) -> GlobalDecls -> [(String,Int)]
globalDeclStats file_filter gmap =
    [ ("Enumeration Constants",Map.size enumerators),
      ("Total Object/Function Declarations",Map.size all_decls),
      ("Object definitions", Map.size objDefs),
      ("Function Definitions", Map.size funDefs),
      ("Tag definitions", Map.size tagDefs),
      ("TypeDefs", Map.size typeDefs)
    ]
    where
    gmap' = filterGlobalDecls filterFile gmap
    (all_decls,(enumerators,objDefs,funDefs)) = splitIdentDecls True (gObjs gmap')
    (tagDefs,typeDefs) = (gTags gmap', gTypeDefs gmap')
    filterFile :: (CNode n) => n -> Bool
    filterFile = maybe True file_filter . fileOfNode . nodeInfo

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty = either pretty pretty
instance Pretty TagFwdDecl where
    pretty (CompDecl ct) = pretty ct
    pretty (EnumDecl et) = pretty et
instance Pretty CompTyKind where
    pretty StructTag = text "struct"
    pretty UnionTag = text "union"
instance Pretty CompTypeRef where
    pretty (CompTypeRef sue kind _) = pretty kind <+> pretty sue
instance Pretty EnumTypeRef where
    pretty (EnumTypeRef sue _ ) = text "enum" <+> pretty sue
instance Pretty Ident where
    pretty = text . identToString
instance Pretty SUERef where
    pretty (AnonymousRef name) = text $ "$" ++ show (nameId name)
    pretty (NamedRef ident) = pretty ident
instance Pretty TagDef where
    pretty (CompDef compty) = pretty compty
    pretty (EnumDef enumty) = pretty enumty
instance Pretty IdentDecl where
    pretty (Declaration decl) = pretty decl
    pretty (ObjectDef odef) = pretty odef
    pretty (FunctionDef fdef) = pretty fdef
    pretty (EnumeratorDef enumerator) = pretty enumerator
instance Pretty Decl where
    pretty (Decl vardecl _) =
        text "declaration" <+>
        pretty vardecl
instance Pretty TypeDef where
    pretty (TypeDef ident ty attrs _) =
        text "typedef" <+> pretty ident <+> text "as"  <+>
        pretty attrs <+> pretty ty
instance Pretty ObjDef where
    pretty (ObjDef vardecl init_opt _) =
        text "object" <+>
        pretty vardecl <+> maybe empty (((text "=") <+>) . pretty) init_opt
instance Pretty FunDef where
    pretty (FunDef vardecl _stmt _) =
        text "function" <+>
        pretty vardecl
instance Pretty VarDecl where
    pretty (VarDecl name attrs ty) =
        ((hsep . punctuate (text " |")) [pretty name, pretty attrs, pretty ty])
instance Pretty ParamDecl where
    pretty (ParamDecl (VarDecl name declattrs ty) _) =
        pretty declattrs <+> pretty name <+> text "::" <+> pretty ty
    pretty (AbstractParamDecl (VarDecl name declattrs ty) _) =
        text "abstract" <+> pretty declattrs <+> pretty name <+>
             text "::" <+> pretty ty
instance Pretty DeclAttrs where
    pretty (DeclAttrs inline storage attrs) =
        (if inline then (text "inline") else empty) <+>
        (hsep $ [ pretty storage, pretty attrs])
instance Pretty Type where
  pretty ty = pretty (exportTypeDecl ty)
instance Pretty TypeQuals where
    pretty tyQuals = hsep $ map showAttr [ ("const",constant),("volatile",volatile),("restrict",restrict) ]
        where showAttr (str,select) | select tyQuals = text str
                                    | otherwise      = empty

instance Pretty CompType where
    pretty (CompType sue_ref tag members attrs node) =
        (text.show) tag <+> pretty sue_ref <+>
        braces (terminateSemi members) <+>
        pretty attrs
instance Pretty MemberDecl where
    pretty (MemberDecl (VarDecl name declattrs ty) bitfield _) =
        pretty declattrs <+> pretty name <+> text "::" <+> pretty ty <+>
        (maybe empty (\bf -> text ":" <+> pretty bf) bitfield)
    pretty (AnonBitField ty bitfield_sz _) =
        pretty ty <+> text ":" <+> pretty bitfield_sz

instance Pretty EnumType where
    pretty (EnumType sue_ref enumerators attrs _) =
      text "enum" <+> pretty sue_ref <+> braces (terminateSemi_ $ map prettyEnr enumerators) <+> pretty attrs
      where
      prettyEnr (Enumerator ident expr enumty _) = pretty ident <+> text " = " <+> pretty expr

instance Pretty Enumerator where
    pretty (Enumerator ident expr enumty _) = text "<" <> text "econst" <+> pretty (sueRef enumty) <> text ">" <+>
                                              pretty ident <+> text " = " <+> pretty expr

instance Pretty Storage where
    pretty NoStorage = empty
    pretty (Auto reg) = text$ if reg then "auto/register" else "auto"
    pretty (Static linkage thread_local) =
        (hcat . punctuate (text "/") $ [ text "static",pretty linkage ])
        <+> (if thread_local then text ", __thread" else empty)
    pretty (FunLinkage linkage) = text "function/" <> pretty linkage
instance Pretty Linkage where
    pretty InternalLinkage = text "internal"
    pretty ExternalLinkage = text "external"
    pretty NoLinkage       = text "local"
instance Pretty VarName where
    pretty NoName = text "<anonymous>"
    pretty (VarName ident asmname_opt) = pretty ident <+> (maybe empty pAsmName asmname_opt)
        where pAsmName asmname = text "" <+> parens (text "asmname" <+> pretty asmname)
instance Pretty Attributes where
    pretty = joinComma
instance Pretty Attr where
    pretty (Attr ident es _) = pretty ident <+> (if null es then empty else text "(...)")

joinComma :: (Pretty a) => [a] -> Doc
joinComma = hsep . punctuate comma . map pretty
terminateSemi :: (Pretty a) => [a] -> Doc
terminateSemi = terminateSemi_ . map pretty
terminateSemi_ :: [Doc] -> Doc
terminateSemi_ = hsep . map (<> semi)


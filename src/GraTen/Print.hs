{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module GraTen.Print
  ( prettyStr
  , prettyPrint
  ) where

import           Data.List                   (intercalate)
import qualified Data.Tuple.Extra            as T
import           Prettyprinter
import           Prettyprinter.Util          (putDocW)
import           Prettyprinter.Render.String (renderString)

import qualified GraTen.Pretty               as P
import           GraTen.Syntax

prettyStr :: Pretty a => a -> String
prettyStr = renderString . layoutPretty defaultLayoutOptions . pretty

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putDocW 100 . pretty

instance {-# OVERLAPPING #-} Pretty (IProgram ID ty) where
  pretty xs = vsep (map pretty xs)

instance {-# OVERLAPPING #-} Pretty ID where
  pretty (x, -1)    = pretty x
  -- Names of temporary variables always start with '~'
  pretty ('~':x, n) = pretty ('_' : x ++ '_' : show n)
  pretty (x, _)     = pretty x

instance Pretty (IDecl ID ty) where
  pretty (ILet p e _) =
    let (args, e') = letArg e
     in indentBlock 2 (pretty "let" <+> hsep (pretty p : args) <+> pretty "=") [pretty e']
  pretty (ILetRec x _ _ e _) =
    let (args, e') = letArg e
     in indentBlock 2 (pretty "let rec" <+> hsep (pretty x : args) <+> pretty "=") [pretty e']
  pretty (IOpen x) = pretty "open" <+> pretty x
  pretty (IModule x ds) =
    pretty "module" <+> pretty x <+> pretty "= struct" <> line <>
      indent 2 (pretty ds) <> line <> pretty "end"
  pretty (ILetModule x path) =
    pretty "module" <+> pretty x <+> pretty "=" <+> pretty (intercalate "." path)
  pretty (ITypeDef (TyName _ x) (Record ys ts)) =
    case ts of
      [] -> pretty "type" <+> pretty x <> params ys
      _  -> nest 2 (pretty "type" <+> pretty x <> params ys <+> pretty "=" <> line <>
        vsep ((lbrace <+> f (head ts)) : map ((pretty ";" <+>) . f) (tail ts) ++ [rbrace]))
   where
    f (x, t) = pretty x <+> pretty ":" <+> pretty t
    params [] = emptyDoc
    params ys = space <> tupled (map pretty ys)
  pretty (ITypeDef (TyName _ x) (Variant [])) = pretty "type" <+> pretty x
  pretty (ITypeDef (TyName _ x) (Variant tags)) =
    nest 2 (pretty "type" <+> pretty x <+> pretty "=" <> line <>
      vsep ((pipe <+> f (head tags)) : map ((pipe <+>) . f) (tail tags) ++ [rbrace]))
   where
    f (x, Nothing) = pretty x
    f (x, Just ty) = pretty x <+> pretty "of" <+> pretty ty

letArg :: IExpr ID ty -> ([Doc ann], IExpr ID ty)
letArg (IFun _ Nothing p _ e) = T.first (pretty p :) (letArg e)
letArg (IFun _ (Just l) p _ e) = T.first (pretty ('~' : l ++ ":") <> pretty p :) (letArg e)
letArg (IOptFun x (Just e1) _ e2) =
  T.first (pretty '?' <> parens (pretty x <+> pretty "=" <+> pretty e1) :)
    (letArg e2)
letArg (IOptFun x Nothing _ e2) =
  T.first ((pretty '?' <> pretty x) :) (letArg e2)
letArg e = ([], e)

binop :: [String]
binop = [ "::", "+", "-", "+.", "-.", "*", "/", "*.", "/."
        , "+=", "-=", "*=", "/=", "=", ">=", "<=", ">", "<", "<>", "&&", "||"]

instance Pretty (IExpr ID ty) where
  pretty IUnit       = pretty "()"
  pretty (IConst c)  = pretty (P.pretty c)
  pretty (IVar x _) | fst x `elem` binop =
    if head (fst x) == '*' || last (fst x) == '*'
       then parens (space <> pretty x <> space) else parens (pretty x)
  pretty (IVar x _)  = pretty x
  pretty (ICtor x Nothing _)       = pretty x
  pretty (ICtor x (Just (_, e)) _) = pretty x <+> pretty' e
  pretty (ITag x  Nothing)      = pretty ('`' : x)
  pretty (ITag x  (Just e))     = pretty ('`' : x) <+> pretty' e
  pretty (ITuple es) = tupled (map pretty es)
  pretty (INil _) = pretty "[]"
  pretty (ICons _ e1 e2) = pretty' e1 <+> pretty "::" <+> pretty' e2
  pretty (IRecord _ es) = encloseSep' lbrace rbrace (pretty ";") (map (\(x, e) -> pretty x <+> pretty "=" <+> pretty e) es)
  pretty (IField _ e x) = pretty' e <> pretty ('.' : x)
  pretty (IIf _ e1 e2 e3 _) =
    indentBlock 2 (pretty "if" <+> pretty e1)
      [pretty "then" <+> pretty e2, pretty "else" <+> pretty e3]
  pretty (IFor _ i e1 e2 e3) =
    pretty "for" <+> pretty i <+> pretty "=" <+> pretty e1 <+> pretty "to" <+> pretty e2 <+>
      pretty "do" <> softline <> pretty e3 <> softline <> pretty "done"
  pretty (ILetIn (IOpen x) e) = pretty (x ++ ".") <> pretty' e
  pretty (ILetIn d e) =
    indentBlock 0 (pretty d <+> pretty "in") [pretty e]
  pretty e@IFun{} =
    let (args, e') = letArg e
     in indentBlock 2 (pretty "fun" <+> hsep args <+> pretty "->") [pretty e']
  pretty e@IOptFun{} =
    let (args, e') = letArg e
     in indentBlock 2 (pretty "fun" <+> hsep args <+> pretty "->") [pretty e']
  pretty (IApp _ _ (IVar ("__neg__", _) _) e) = pretty "-" <+> pretty e
  pretty (IApp _ _ (IVar ("__fneg__", _) _) e) = pretty "-." <+> pretty e
  pretty (IApp _ _ (IApp _ _ (IVar x _) e1) e2) | fst x `elem` binop =
    parens $ pretty e1 <+> pretty x <+> pretty e2
  pretty (IApp Nothing _ e1@IApp{} e2) = indentBlock 0 (pretty e1) [pretty' e2]
  pretty (IApp (Just l) _ e1@IApp{} e2) =
    indentBlock 0 (pretty e1) [pretty ('~' : l ++ ":") <> pretty' e2]
  pretty (IApp Nothing _ e1 e2) = indentBlock 0 (pretty' e1) [pretty' e2]
  pretty (IApp (Just l) _ e1 e2) =
    indentBlock 0 (pretty' e1) [pretty ('~' : l ++ ":") <> pretty' e2]
  pretty (IAppEnd e) = pretty e
  pretty (IMatch _ e1 [(p, e2)] _) =
    pretty "match" <+> pretty e1 <+> pretty "with" <+>
      indentBlock 2 (pretty p <+> pretty "->") [pretty e2]
  pretty (IMatch _ e ps _) = align $
    pretty "match" <+> pretty e <+> pretty "with" <> hardline <>
      vsep (map (\(p, e) ->
        indentBlock 2 (pipe <+> pretty p <+> pretty "->") [pretty e]) ps)
  pretty (ISeq _ e1 e2) = pretty' e1 <> semi <> softline <> pretty e2
  pretty (IType _ e t)  =
    indentBlock 2 (pretty "(" <> pretty e) [colon <+> pretty t <> pretty ")"]
  pretty (ICast e ctx t1 t2 _) =
    indentBlock 2 (pretty "(" <> pretty e)
      [colon <+> prettyList ctx <+> pretty "|-" <+> pretty t1 <+> pretty "=>" <+> pretty t2 <> pretty ")"]
  pretty (IAssert p) = pretty "assert " <> parens (pretty p)

instance Pretty (Pattern ID) where
  pretty PWildcard   = pretty "_"
  pretty (PConst c)  = pretty (P.pretty c)
  pretty (PVar x)    = pretty x
  pretty PUnit       = pretty "()"
  pretty (PTag x)    = pretty x
  pretty (PTuple ps) = tupled (map pretty ps)
  pretty (PRecord _    []) = undefined
  pretty (PRecord path (f:fs)) =
    encloseSep' lbrace rbrace (pretty ";") (pretty (concatMap (++ ".") path) <> field f : map field fs)
   where
    field (x, p) = pretty x <+> pretty "=" <+> pretty p
  pretty PNil        = pretty "[]"
  pretty (PCons p1 p2) = pretty p1 <+> pretty "::" <+> pretty p2
  pretty (PConstr x Nothing)  = pretty x
  pretty (PConstr x (Just p)) = pretty x <+> pretty p
  pretty (PType x _)   = pretty x

instance Pretty Type where
  pretty t = pretty (P.pretty t)

instance Pretty Predicate where
  pretty (SInt n)     = pretty n
  pretty (SBool b)    = pretty (if b then "true" else "false")
  pretty (SName x fs) = pretty x <> pretty (concatMap ('.' :) fs)
  pretty (SVar _ _)   = undefined
  pretty (SNot s)     = pretty "!(" <> pretty s <> pretty ")"
  pretty (SNeg s)     = pretty '-' <> pretty s
  pretty (SBi Add s1 (SNeg s2)) = pretty s1 <+> pretty "-"  <+> prettyAt 6 s2
  pretty (SBi Add s (SInt n)) | n < 0 = pretty s <+> pretty "-"  <+> pretty (-n)
  pretty (SBi Add s1 s2)      = prettyAt 6 s1 <+> pretty "+" <+> prettyAt 6 s2
  pretty (SBi Mul s1 s2)      = prettyAt 7 s1 <+> pretty "*" <+> prettyAt 7 s2
  pretty (SBi FloorDiv s1 s2) = prettyAt 7 s1 <+> pretty "/" <+> prettyAt 7 s2
  pretty (SFun "nth" [s1, s2]) = pretty "Base.List.nth_exn" <+> pretty' s2 <+> pretty' s1
  pretty (SFun "len" [s1]) = pretty "Base.List.length" <+> pretty' s1
  pretty (SFun "last" [s1]) = pretty "Base.List.last_exn" <+> pretty' s1
  pretty (SFun p ss) = pretty p <+> hsep (map pretty' ss)
  pretty (SList ss)  = encloseSep' lbracket rbracket (pretty ";") (map pretty ss)
  pretty (SBi Eq s1 s2) = prettyAt 4 s1 <+> pretty "=" <+> prettyAt 4 s2
  pretty (SBi Gt s1 s2) = prettyAt 4 s1 <+> pretty ">" <+> prettyAt 4 s2
  pretty (SBi Ge s1 s2) = prettyAt 4 s1 <+> pretty ">=" <+> prettyAt 4 s2
  pretty (SBi And s1 s2) = indentBlock 2 (prettyAt 3 s1 <+> pretty "&&") [prettyAt 3 s2]
  pretty (SBi Or s1 s2)  = prettyAt 2 s1 <+> pretty "||" <+> prettyAt 2 s2

prettyAt :: Int -> Predicate -> Doc ann
prettyAt n s | priority s > n = pretty s
prettyAt _ s                  = parens (pretty s)

priority :: Predicate -> Int
priority (SBi Or _ _)       = 2
priority (SBi And _ _)      = 3
priority (SBi Eq _ _)       = 4
priority (SBi Gt _ _)       = 4
priority (SBi Ge _ _)       = 4
priority (SBi Add _ _)      = 6
priority (SBi Mul _ _)      = 7
priority (SBi FloorDiv _ _) = 7
priority _                  = 100

class Complex a where
  isAtom :: a -> Bool

pretty' :: (Pretty a, Complex a) => a -> Doc ann
pretty' x | isAtom x = pretty x
pretty' x            = parens (pretty x)

instance Complex (IExpr ID ty) where
  isAtom IUnit     = True
  isAtom IConst{}  = True
  isAtom IVar{}    = True
  isAtom ICtor{}   = True
  isAtom ITag{}    = True
  isAtom ITuple{}  = True
  isAtom INil{}    = True
  isAtom IRecord{} = True
  isAtom IField{}  = True
  isAtom IType{}   = True
  isAtom ICast{}   = True
  isAtom IAssert{} = True
  isAtom (IApp _ _ (IApp _ _ (IVar x _) _) _) | fst x `elem` binop = True
  isAtom (ILetIn (IOpen _) _) = True
  isAtom _         = False

instance Complex Predicate where
  isAtom SBi{}  = False
  isAtom SFun{} = False
  isAtom _      = True

indentBlock :: Int -> Doc ann -> [Doc ann] -> Doc ann
indentBlock n header body =
  group (nest n (header <> flatAlt (hardline <> hsepHard body) (space <> hsep body)))

encloseSep' :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSep' left right _   []     = left <> right
encloseSep' left right sep (x:xs) =
  group (vsep (left <+> x : map (\x -> sep <+> x) xs ++ [right]))

hsepHard :: [Doc ann] -> Doc ann
hsepHard = concatWith (\x y -> x <> hardline <> y)

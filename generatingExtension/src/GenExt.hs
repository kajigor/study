{-# LANGUAGE OverloadedStrings #-}

module GenExt where

import L
import Control.Monad.Trans.State
import Control.Monad.Trans.Class ()
import Text.Printf (printf)
import Data.Text (Text)
import Prettyprinter
import Parser.Parser (parser)
import Prettyprinter.Render.Text
import Data.Text.IO

type HS = Doc Text

preamble :: HS
preamble = vsep
  [ "{-# LANGUAGE ScopedTypeVariables #-}"
  , "module Output where"
  , ""
  , "import Control.Monad.Trans.State"
  , "import Control.Monad.Trans.Class"
  , ""
  ]

tabsize :: Int
tabsize = 2

mainDef :: HS -> HS
mainDef compiled =
  vsep
    [ "main :: IO ()"
    , "main = do"
    , compiled
    ]

evalLGE :: L -> Input -> HS
evalLGE program input =
  vsep [preamble, (mainDef $ evalLHelper program input)]


evalOpGE :: Op -> HS
evalOpGE Plus = "(+)"
evalOpGE Mult = "(*)"

indentOnce :: [Doc ann] -> Doc ann
indentOnce = indent tabsize . vsep

hangOnce :: [Doc ann] -> Doc ann
hangOnce = hang tabsize . vsep

indentThrice :: [Doc ann] -> Doc ann
indentThrice = indent (3 * tabsize) . vsep

evalExprGE :: Expr -> HS
evalExprGE (Lit x) = "return " <> pretty x
evalExprGE (Var v) =
  hangOnce [ "do"
           , "state <- get"
           , hangOnce [ "case lookup \"" <> pretty v <> "\" state of"
                      , "Just x -> return x"
                      , "Nothing -> lift Nothing"
                      ]
           ]
evalExprGE (BinOp op l r) =
  let lComp = evalExprGE l in
  let rComp = evalExprGE r in
  let opComp = evalOpGE op in
  hangOnce [ "do"
           , "l <- (" <> lComp <>")"
           , "r <- (" <> rComp <> ")"
           , "return (" <> opComp <+> "l r)"
           ]

evalStmtGE :: Stmt -> HS
evalStmtGE (Read v) =
  hangOnce [ "do"
           , "(state, x : input, output) <- get"
           , "put ((\"" <> pretty v <> "\", x) : state, input, output)"
           , "return ()"
           ]
evalStmtGE (Write e) =
  let eComp = evalExprGE e in
  hangOnce [ "do"
           , "(state, input, output) <- get"
           , "let r = evalStateT (" <> eComp <> ") state"
           , hangOnce [ "case r of"
                      , "Just x -> do "
                      , indentOnce [ "put (state, input, x : output)"
                                   , "return ()"
                                   ]
                      , "Nothing -> lift Nothing"
                      ]
           ]
evalStmtGE (Assign v e) =
  let eComp = evalExprGE e in
  hangOnce [ "do"
           , "(state, input, output) <- get"
           , "let r = evalStateT (" <> eComp <> ") state"
           , hangOnce [ "case r of"
                      , "Just x -> do"
                      , indentOnce [ "put ((\"" <> pretty v <> "\", x) : state, input, output)"
                                    , "return ()"
                                    ]
                      , "Nothing -> lift Nothing"
                      ]
           ]

evalLHelper :: L -> Input -> HS
evalLHelper program inputs =
  let compiledStatements = indentThrice $ map (parens . evalStmtGE) program in
  indentOnce [ "let compiled = do " <> line <> compiledStatements
             , "let res = execStateT compiled ([]," <+> pretty inputs <> ", [])"
             , "print $" <+> (hangOnce [ "case res of"
                                       , "Just (_, _, output :: [Int]) -> Just output"
                                       , "Nothing -> Nothing"
                                       ])
             ]

run :: Input -> String -> Maybe FilePath -> IO ()
run inputs string path =
    case parser string of
      Left err -> fail err
      Right program ->
        let fileName = maybe "src/Output.hs" id path in
        Data.Text.IO.writeFile fileName (renderStrict . layoutPretty layoutOptions $ evalLGE program inputs)
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }


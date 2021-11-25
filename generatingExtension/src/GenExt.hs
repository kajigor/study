{-# LANGUAGE OverloadedStrings #-}

module GenExt where

import L ( Input, L, Stmt(..), Expr(..), Op(..) )
import Control.Monad.Trans.State ()
import Control.Monad.Trans.Class ()
import Text.Printf (printf)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Prettyprinter
    ( (<+>),
      hang,
      indent,
      layoutPretty,
      line,
      vsep,
      parens,
      Doc,
      LayoutOptions(LayoutOptions, layoutPageWidth),
      PageWidth(AvailablePerLine),
      Pretty(pretty) )
import Parser.Parser (parser)
import Prettyprinter.Render.Text ( renderStrict )
import Data.Text.IO ( writeFile )

type HS = Doc Text

preamble :: HS
preamble = vsep
  [ "{-# LANGUAGE ScopedTypeVariables #-}"
  , "module Output where"
  , ""
  , "import Control.Monad.Trans.State"
  , "import Control.Monad.Trans.Class"
  , ""
  , "intToBool :: Int -> Bool"
  , "intToBool x = x /= 0"
  , ""
  , "boolToInt :: Bool -> Int"
  , "boolToInt True = 1"
  , "boolToInt False = 0"
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
evalOpGE Plus  = "(+)"
evalOpGE Minus = "(-)"
evalOpGE Mult  = "(*)"
evalOpGE Div   = "div"
evalOpGE Pow   = "(^)"
evalOpGE Eq    = transformCompareGE "=="
evalOpGE Neq   = transformCompareGE "/="
evalOpGE Lt    = transformCompareGE "<"
evalOpGE Le    = transformCompareGE "<="
evalOpGE Gt    = transformCompareGE ">"
evalOpGE Ge    = transformCompareGE ">="
evalOpGE And   = transformGE "&&"
evalOpGE Or    = transformGE "||"

transformCompareGE :: HS -> HS
transformCompareGE op = "(\\x y -> boolToInt $ x " <> op <> " y)"

transformGE :: HS -> HS
transformGE op = "(\\x y -> boolToInt $ intToBool x " <> op <> " intToBool y)"

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
evalStmtGE (If c thn els) =
  let cComp = evalExprGE c in
  hangOnce [ "do"
           , "(state, _, _) <- get"
           , "let cond = evalStateT (" <> cComp <> ") state"
           , hangOnce [ "case cond of"
                      , "Just condition -> "
                      , indentOnce [ "if intToBool condition"
                                   , "then "
                                   , indentOnce
                                      [hangOnce [ "do"
                                      , compileStmts thn
                                      ]]
                                   , "else"
                                   , indentOnce
                                      [hangOnce [ "do"
                                      , compileStmts els
                                      ]]
                                   ]
                      , "Nothing -> lift Nothing"
                      ]
           ]
-- does not terminate
-- evalStmtGE w@(While c body) =
--   let cComp = evalExprGE c in
--   hangOnce [ "do"
--            , "(state, _, _) <- get"
--            , "let cond = evalStateT (" <> cComp <> ") state"
--            , hangOnce [ "case cond of"
--                       , "Just condition -> "
--                       , hangOnce [ "when"
--                                  , "(intToBool cond)"
--                                  , hangOnce [ "(do"
--                                             , "(do "
--                                             , compileStmts body <> ")"
--                                             , evalStmtGE w <> ")"
--                                             ]
--                                  ]
--                       , "Nothing -> lift Nothing"
--                       ]
--            ]

compileStmts stmts =
  (indentThrice $ map (parens . evalStmtGE) stmts)

evalLHelper :: L -> Input -> HS
evalLHelper program inputs =
  let compiledStatements = compileStmts program  in
  indentOnce [ "let compiled = do " <> line <> compiledStatements
             , "let res = execStateT compiled ([]," <+> pretty inputs <> ", [])"
             , "print $" <+> (hangOnce [ "case res of"
                                       , "Just (_, _, output :: [Int]) -> Just output"
                                       , "Nothing -> Nothing"
                                       ])
             ]

genExt :: Input -> L -> Maybe FilePath -> IO ()
genExt inputs program path =
    let fileName = fromMaybe "test/out/Output.hs" path in
    Data.Text.IO.writeFile fileName (renderStrict . layoutPretty layoutOptions $ evalLGE program inputs)
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

run :: Input -> String -> Maybe FilePath -> IO ()
run inputs string path =
    case parser string of
      Left err -> fail err
      Right program ->
        genExt inputs program path


module Main where

import Data.Maybe (fromMaybe)
import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      doesPathExist,
      removeDirectoryRecursive )
import Text.Printf (printf)
import Control.Monad (unless, when)
import Options.Applicative
import Parser.Parser (parser)
import System.FilePath ((</>), (<.>), dropExtension, takeFileName, takeDirectory)
import System.Directory (copyFile)
import Eval (evalL)
import GenExt (genExt)

data Transformation = Parser
                    | Evaluator
                    | BTA
                    | GeneratingExtension

data Action = Action { transformation :: Transformation
                     , input :: FilePath
                     , output :: FilePath
                     , inputArgs :: [Int]
                     }

data Args = Args Transformation FilePath (Maybe FilePath) (Maybe [Int])

transform :: Args -> IO Action
transform (Args transformation input output inputArgs) = do
    let i = input
    failIfNotExist i

    let out = fromMaybe defaultOutputDir output
    createDirRemoveExisting out

    let args = fromMaybe [] inputArgs

    return $ Action transformation i out args
  where
    failIfNotExist :: FilePath -> IO ()
    failIfNotExist path = do
      exist <- doesPathExist path
      unless exist (fail $ printf "%s does not exist" path)
    createDirRemoveExisting :: FilePath -> IO ()
    createDirRemoveExisting path = do
      exists <- doesDirectoryExist path
      when exists (removeDirectoryRecursive path)
      createDirectoryIfMissing True path
    defaultOutputDir = "test/out" </> dropExtension (takeFileName input)

actionParser :: Parser Args
actionParser =
  Args  <$> parseTransformation
        <*> inputParser
        <*> optional outputParser
        <*> optional inputArgsParser


parseTransformation :: Parser Transformation
parseTransformation =
      parserParser
  <|> evalParser
  <|> btaParser
  <|> genExtParser

inputParser :: Parser FilePath
inputParser = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "Input file"
  )

inputArgsParser :: Parser [Int]
inputArgsParser = argument auto (metavar "INPUT")

outputParser :: Parser FilePath
outputParser = strOption
  (  long "output"
  <> short 'o'
  <> metavar "OUTPUT"
  <> help "Directory for the output files"
  )

parserParser :: Parser Transformation
parserParser = flag' Parser
  (  long "parser"
  <> help "Run parser"
  )

evalParser :: Parser Transformation
evalParser = flag' Evaluator
  (  long "eval"
  <> help "Run evaluator"
  )

btaParser :: Parser Transformation
btaParser = flag' BTA
  (  long "bta"
  <> help "Run bta-based optimisation"
  )

genExtParser :: Parser Transformation
genExtParser = flag' GeneratingExtension
  (  long "genExt"
  <> help "Run generating extension4"
  )

main :: IO ()
main = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "A generating extension implementation for a simple imperative language"
      <> header "l-generating-extension"
      )

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  inputFile <- readFile $ input action
  let inputFileName = takeFileName (input action)
  let inputDir = takeDirectory (input action)
  case parser inputFile of
    Left err -> putStrLn err
    Right program -> do
      writeFile (output action </> "parsed.txt") (show program)
      copyFile (input action) (output action </> inputFileName)
      case transformation action of
        Parser -> return ()
        Evaluator ->
          case evalL program (inputArgs action) of
            Nothing -> putStrLn "Failed to eval"
            Just out -> writeFile (output action </> inputFileName <.> "out") (show out)
        BTA ->
          undefined
        GeneratingExtension ->
          genExt (inputArgs action) program (Just $ output action </> "Output.hs")

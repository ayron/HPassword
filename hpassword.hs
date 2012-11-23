module Main (main) where

import System.Environment (getArgs)
import System.Random
import System.Console.GetOpt

nums    = ['0'..'9']
symbols = "!@#$%^&*"
alpha   = ['a'..'z']
alphaU  = ['A'..'Z']
chars   = alpha ++ alphaU ++ nums ++ symbols

main :: IO ()
main = do
  options <- getArgs >>= parse
  getRandomInts (optLen options) >>= putStrLn . map (chars !!)

getRandomInts :: Int -> IO [Int]
getRandomInts n = getStdGen >>= 
  return . take n . randomRs (0, length chars - 1)

-- * Command line Argument Parsing

data Options = Options
  { optVersion :: Bool
  , optHelp    :: Bool
  , optUpper   :: Bool
  , optSym     :: Bool
  , optNum     :: Bool
  , optLen     :: Int
  } deriving (Show)

defaultOptions = Options
  { optVersion = False
  , optHelp    = False
  , optUpper   = True
  , optSym     = True
  , optNum     = True
  , optLen     = 8
  }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option "v?" ["version"] (NoArg  (\o   -> o {optVersion = True}   )) "Show Version Info"
  , Option "u"  ["noupper"] (NoArg  (\o   -> o {optUpper   = False}  )) "Omit Uppercase characters"
  , Option "n"  ["nonum"]   (NoArg  (\o   -> o {optSym     = False}  )) "Omit numbers"
  , Option "s"  ["nosym"]   (NoArg  (\o   -> o {optSym     = False}  )) "Omit Symbols"
  , Option "N"  []          (ReqArg (\n o -> o {optLen     = read n} ) "LEN") "Length of password"
  ]

parse :: [String] -> IO Options
parse argv =
  case getOpt Permute options argv of
    (o, [], []) -> return $ foldl (flip id) defaultOptions o
    (_,  _,  e) -> ioError $ userError $ concat e ++ usageInfo header options
  where header = "Usage: rpassword [OPTION...]"


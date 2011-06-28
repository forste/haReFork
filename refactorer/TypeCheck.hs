module TypeCheck where

import System
import Unsafe.Coerce
import Control.Exception
import System.IO.Unsafe
import System.IO
import List
--import PackageConfig    ( stringToPackageId )
import GHC hiding (SrcLoc)
import DynFlags (defaultDynFlags)
import HsSyn
import Outputable
import SrcLoc
--import AbstractIO
import Control.Monad
import Language.Haskell.Interpreter hiding (runGhc)

import LocalSettings

libdir = "/Library/Frameworks/GHC.framework/Versions/612/usr/lib/ghc-6.12.1"
targetFile = "/Users/chrisbrown/hare/tests/A1.hs"

ghcTypeCheck1 expr modName fileName =
  unsafePerformIO(
   do
      putStrLn "ghcTypeCheck1"
      putStrLn expr
      putStrLn modName
      putStrLn fileName
      r <- runInterpreter (testHint expr modName fileName)
      case r of 
         Left err -> do printInterpreterError err
                        return ""
         Right e -> return e
   )
   
-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: String -> String -> String -> InterpreterT IO String
testHint expr modName fileName =
    do
      -- say "Load SomeModule.hs"
      loadModules [fileName]
      --
      --say "Put the Prelude, Data.Map and *SomeModule in scope"
      --say "Data.Map is qualified as M!"
      setTopLevelModules [modName]
      setImportsQ [("Prelude", Nothing)]
      --
      say "Now we can query the type of an expression"
      let expr1 = modName ++ "." ++ expr
      say $ "e.g. typeOf " ++ expr1
      say =<< typeOf expr1
      return =<< typeOf expr1


say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = error $ "Ups... " ++ (show e)


ghcTypeCheckPattern closure closure_name modName fileName =
  unsafePerformIO(
   do
      r <- runInterpreter (testPattern closure closure_name modName fileName)
      case r of 
         Left err -> do printInterpreterError err
                        return ""
         Right e -> return e
   )  
  
testPattern :: String -> String -> String -> String -> InterpreterT IO String
testPattern closure closure_name modName fileName =
    do
      say fileName
      say modName
      -- say "Load SomeModule.hs"
      --
      --say "Put the Prelude, Data.Map and *SomeModule in scope"
      --say "Data.Map is qualified as M!"
      set [languageExtensions := [ExistentialQuantification, ScopedTypeVariables]]
      loadModules [fileName]
      setTopLevelModules [modName]
      setImportsQ [("Prelude", Nothing)]
      --
      say "Now we can query the type of an expression"
      let expr1 = closure
      say closure
      say closure_name
      say $ "e.g. typeOf " ++ expr1
      say =<< typeOf expr1
      -- say =<< typeOf closure_name
      return =<< typeOf expr1

debugLog :: String -> b -> b
debugLog msg b =
  unsafePerformIO (
    do
      putErrStrLn msg
      return b
    )

logAndDump :: (Outputable a) => String -> a -> b -> b
logAndDump msg a b =
  unsafePerformIO (
    do
      putErrStrLn msg
      putErrStrLn $ showSDoc (ppr a)
      return b
    )

tidyFileName :: String -> String
tidyFileName ('.':'/':str) = str
tidyFileName str           = str

data Tag = Tag TagName TagFile TagLine TagDesc
  deriving (Eq)

instance Ord Tag where
  compare (Tag t1 _ _ _) (Tag t2 _ _ _) = compare t1 t2

instance Show Tag where
  show (Tag t f l d) = makeTagsLine t f l d

type TagName = String
type TagFile = String
type TagLine = Int
type TagDesc = String

makeTagsLine :: String -> String -> Int -> String -> String
makeTagsLine tag file line desc = tag `sep` file `sep` (show line) `sep` ";\t\"" ++ desc ++ "\""
  where a `sep` b = a ++ '\t':b


putErrStrLn = hPutStrLn stderr
putErrStr = hPutStr stderr
 

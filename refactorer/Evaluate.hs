module Main where

import System
import Control.Exception
import System.IO.Unsafe
import System.IO
import List
import Unsafe.Coerce
-- Package GHC stuff
import GHC
import DynFlags
import ErrUtils
import PackageConfig
import HsSyn
import Outputable
import SrcLoc
import RdrName
import Name 
import Control.Monad
import Language.Haskell.Interpreter hiding (runGhc)
import LocalSettings

main 
 = -- defaultErrorHandler defaultDynFlags $
   do
   ar <- getArgs
   putStrLn "here"
   let args = ar !! 0
       -- newArgs = (ar !! 1) ++ "hs"
       closure_call = ar !! 1
       modName = ar !! 2
   -- error $ show (closure_call, modName, args)
   let newArgs =  args ++ ".hs"

   -- let packageConf = ghcPath 
   -- (eval_res, x) <- runEval args modName closure_call packageConf
   x <- runInterpreter (runEvalHint args modName closure_call)
   case x of
     Left err -> printInterpreterError err
     Right x -> do putStrLn $ show (x)
                   writeFile evaluate_result x 

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = error $ "Ups... " ++ (show e)

runEvalHint args modName closure_call 
 = do
      set [languageExtensions := [OverlappingInstances]]
      loadModules [args]
      setTopLevelModules [modName]
      setImportsQ [("Prelude", Nothing)]
      liftIO (putStrLn ("about to evaluate...>" ++ modName ++ "<"))
      let expr1 = (modName ++ "." ++ closure_call)
      a <- eval expr1
      return a
      

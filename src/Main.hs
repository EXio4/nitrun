{-# LANGUAGE ScopedTypeVariables, AutoDeriveTypeable, DeriveDataTypeable #-}

module Main (main) where

import           Data.Char
import           Data.Functor
import           Data.Typeable

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL

import           Crypto.Hash.SHA256 (hashlazy)

import           System.Process
import           System.Exit
import           System.Environment
import           System.FilePath.Posix

import           Control.Exception

data CompilerError = CompilerError !Int !String !String
    deriving (Show,Eq,Typeable)
instance Exception CompilerError

ghc file output = do (exc, out, err) <- readProcessWithExitCode "ghc" ["-O2", "-o", output, file] ""
                     case exc of
                          ExitSuccess{} -> return ()
                          ExitFailure n -> throwIO (CompilerError n out err)

hashFile :: FilePath -> IO BS.ByteString
hashFile = fmap hashlazy . BSL.readFile

{- does this even make sense? -}
oldName :: String -> String -> String
oldName mid file = takeDirectory file </> "." ++ mid ++ "." ++ takeFileName file

worker :: String -> [String] -> IO ()
worker file args =
        do { new_hash <- hashFile file
           ; old_hash <- try (BS.readFile (oldName "hash" file))
           ; case old_hash of
                Left (e :: IOError)                    -> update new_hash
                Right old_hash'| old_hash' /= new_hash -> update new_hash
                _ -> return ()
           ; run
           ; return ()
        } `catch` \(CompilerError n out err) -> do
            putStrLn  out
            putStrLn  err
            exitWith (ExitFailure n)
          `catch` \(e :: SomeException) -> do
              print e
    where update new_hash = do
                ghc file (oldName "bin" file)
                BS.writeFile (oldName "hash" file) new_hash
          run = waitForProcess =<< runProcess (oldName "bin" file) args Nothing Nothing Nothing Nothing Nothing

main :: IO ()
main = do { x <- getArgs
          ; case x of
                (file : args) -> worker file args
                _ -> putStrLn "Invalid parameters"
          }

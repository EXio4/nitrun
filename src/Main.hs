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

data Lang = Haskell
    deriving (Show,Eq,Ord)

data CompilerError = CompilerError !Int !String !String
    deriving (Show,Eq,Typeable)
instance Exception CompilerError

readLang x | let lower = map toLower x
           , (lower == "haskell" || lower == "hs")
           = Just Haskell
readLang _ = Nothing

ghc file output = do (exc, out, err) <- readProcessWithExitCode "ghc" ["-O2", "-o", output, file] ""
                     case exc of
                          ExitSuccess{} -> return ()
                          ExitFailure n -> throwIO (CompilerError n out err)

fromExitCode (ExitSuccess{}) = True
fromExitCode (ExitFailure{}) = False

compilers :: Map Lang (String -> String -> IO ())
compilers = M.fromList [(Haskell, ghc)]

hashFile :: FilePath -> IO BS.ByteString
hashFile = fmap hashlazy . BSL.readFile

{- does this even make sense? -}
oldName :: String -> String -> String
oldName mid file = takeDirectory file </> "." ++ mid ++ "." ++ takeFileName file

go :: (String -> String -> IO ()) -> String -> [String] -> IO ()
go compile file args =
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
            putStrLn $ "RET: " ++ show n
          `catch` \(e :: SomeException) -> do
              print e
    where update new_hash = do
                compile file (oldName "bin" file)
                BS.writeFile (oldName "hash" file) new_hash
          run = waitForProcess  =<< runProcess (oldName "bin" file) args Nothing Nothing Nothing Nothing Nothing

main :: IO ()
main = do { x <- getArgs
          ; case x of
                (lang : file : args)
                    | Just lang'   <- readLang lang
                    , Just compile <- M.lookup lang' compilers
                    -> go compile file args
                _ -> putStrLn "Invalid parameters"
          }

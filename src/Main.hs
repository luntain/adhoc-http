{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.UUID as UUID
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.GZip
import System.Environment (getArgs)
import System.Random
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do
  [dirToServe] <- getArgs
  uuid <- UUID.toASCIIBytes <$> randomIO
  print config
  putStrLn $ "serving on: " ++ "http://localhost:8787/" ++ B.unpack uuid ++ "/"
  httpServe config
      . withCompression' (Set.insert "text/csv" compressibleMimeTypes)
      . dir uuid
      $ serveDirectoryWith fancyDirectoryConfig dirToServe

  where
    config =
        setErrorLog ConfigNoLog
        . setAccessLog ConfigNoLog
        . setPort 8787
        $ defaultConfig

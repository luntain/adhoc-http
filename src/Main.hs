{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Options.Applicative
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.GZip
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = do
  (port, dirToServe) <- execParser cliParser
  uuid <- UUID.toASCIIBytes <$> randomIO
  let conf = config port
  print conf
  putStrLn $ "serving on: " ++ "http://localhost:" ++ show port ++ "/" ++ B.unpack uuid ++ "/"
  httpServe conf
    . withCompression' (Set.insert "text/csv" compressibleMimeTypes)
    . dir uuid
    $ serveDirectoryWith fancyDirectoryConfig dirToServe
  where
    config port =
      setErrorLog ConfigNoLog
        . setAccessLog ConfigNoLog
        . setPort port
        $ defaultConfig

cliParser =
  info
    (helper <*> options)
    ( fullDesc
        <> header "Ad-hoc HTTP file server" -- I have no idea where or when it displays
        <> progDesc "Serve a directory under a randomly generated GUID"
    )

options :: Parser (Port, FilePath)
options =
  (,)
    <$> option auto (long "port" <> short 'p' <> value 7878 <> showDefault <> metavar "INT")
    <*> argument str (metavar "DIR" <> help "The directory to serve")

type Port = Int

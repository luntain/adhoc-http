{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Options.Applicative as Opt
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.GZip
import System.Environment (getArgs)
import System.Random
import Data.String (IsString(fromString))
import Network.HostName

main :: IO ()
main = do
  Run{..} <- execParser cliParser
  uuid <-
    case pathPrefix of
      Nothing -> UUID.toASCIIBytes <$> randomIO
      Just prefix -> pure prefix
  let conf = config port
  print conf
  hostName <- getHostName
  putStrLn $ "Serving files on: " ++ "http://" ++ hostName ++ ":" ++ show port ++ "/" ++ B.unpack uuid ++ "/"
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
    (options <**> helper)
    ( fullDesc
        <> header "Ad-hoc HTTP file server" -- I have no idea where or when it displays
        <> progDesc "Serve a directory under a randomly generated GUID or a specified path"
    )

options :: Parser Cmd
options =
  Run
    <$> option auto (long "port" <> short 'p' <> value 7878 <> showDefault <> metavar "INT")
    <*> optional (Opt.option str (long "path-prefix" <> metavar "URL_PATH" <> help "Path prefix (random GUID by default)"))
    <*> argument str (metavar "DIR" <> help "The directories to serve")

data Cmd = Run { port :: Int, pathPrefix :: Maybe B.ByteString, dirToServe :: FilePath }

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import System.Random
import Data.String (IsString(fromString))
import Network.HostName
import Control.Monad (forM)
import Data.Foldable (msum)
import Control.Arrow (second)
import System.IO

main :: IO ()
main = do
  Run { port, dirsToServe, timeout } <- execParser cliParser
  dirsToServe' :: [(FilePath, [B.ByteString])] <- (fmap.map) (second (B.split '/')) . forM dirsToServe $ \(DirToServe path mprefix) ->
    (path,) <$> maybe randomGuid pure mprefix

  hSetBuffering stdout LineBuffering -- so that stdout makes for good logging
  putStrLn "adhoc-serve Version 0.1.1.0"

  let conf = config port timeout
  print conf
  hostName <- getHostName

  if null dirsToServe' then putStrLn "No dirs given, see --help"
    else do
      putStrLn $ "Serving following dirs:"
      forM dirsToServe' $ \(diskPath, prefix) -> do
        putStrLn $ " * " ++ diskPath ++ " at " ++ "http://" ++ hostName ++ ":" ++ show port ++ "/" ++ printPath prefix

      httpServe conf
        . withCompression' (Set.insert "text/csv" compressibleMimeTypes)
        . msum
        . flip map dirsToServe'
        $ \(diskPath, prefix) ->
           foldr dir (serveDirectoryWith fancyDirectoryConfig diskPath) prefix
  where
    config port timeout =
      setErrorLog ConfigNoLog
        . setAccessLog ConfigNoLog
        . setPort port
        . setDefaultTimeout timeout
        $ defaultConfig

    randomGuid :: IO B.ByteString
    randomGuid = UUID.toASCIIBytes <$> randomIO

    printPath :: [B.ByteString] -> String
    printPath [] = ""
    printPath (p:rest) = B.unpack p ++ "/" ++ printPath rest


cliParser :: ParserInfo Cmd
cliParser =
  info
    (options <**> helper)
    ( fullDesc
        <> header "Ad-hoc HTTP file server"
        <> progDesc "Serve a directory under a randomly generated GUID or a specified path"
    )

options :: Parser Cmd
options =
  Run
    <$> option auto (long "port" <> short 'p' <> value 7878 <> showDefault <> metavar "INT")
    <*> many dirToServe
    <*> timeoutOption
  where
    timeoutOption = option auto
       $ long "timeout"
      <> short 't'
      <> metavar "SECONDS"
      <> value 60
      <> showDefault
      <> help timeoutOptionDesc
    timeoutOptionDesc = "Incoming connection timeout [s] (adjust when serving"
      ++ " large files or using slow network)"

dirToServe :: Parser DirToServe
dirToServe =
  Opt.argument (Opt.maybeReader parse) (
            metavar "DIR[:URL_PREFIX]"
          <> help "The path to directory on disk to serve over HTTP and, optionally, the URL prefix\
                  \ under which the tree of files will be served (a random GUID by default)")
  where
    parse x =
      case break (==':') x of
        (dir, []) -> Just (DirToServe dir Nothing)
        (dir, ':' : prefix) -> Just $ DirToServe  dir (Just (fromString prefix))
        _ -> error "Impossible!"


data Cmd = Run { port :: Int, dirsToServe :: [DirToServe], timeout :: Int }
data DirToServe = DirToServe { path :: FilePath, pathPrefix :: Maybe B.ByteString}

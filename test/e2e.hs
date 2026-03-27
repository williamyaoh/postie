{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent ( forkIO, killThread, threadDelay )
import Control.Exception ( bracket )
import Control.Monad ( void, unless )
import Data.ByteString ( ByteString )
import Data.Char ( toLower )
import Data.Function ( fix )
import Data.Functor ( (<&>) )
import Data.List ( isSuffixOf )
import Data.Word ( Word8 )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Storable ( pokeElemOff, peekElemOff )
import Network.Socket ( Socket, SockAddr(..), AddrInfo(..), getAddrInfo, tupleToHostAddress, socket, connect, close, sendBuf, recvBuf )
import Pipes ( runEffect, (>->) )
import Pipes.Prelude ( drain )
import System.Directory ( listDirectory )
import System.IO ( Handle, IOMode(..), withFile, hIsEOF )
import Test.Tasty ( TestName, DependencyType(..), sequentialTestGroup, defaultMain )
import Test.Tasty.HUnit ( Assertion, testCase, assertFailure, assertBool )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Network.Mail.Postie as Postie

main :: IO ()
main = do
  tests <- fmap (filter isTestFile) $ listDirectory "test/resources/filetests"
  defaultMain $ sequentialTestGroup "postie E2E" AllFinish $ tests <&> \file ->
    let testName = "golden file: " ++ file
    in testCase testName $ mkFileTest file

postieSettings :: Postie.Settings
postieSettings = Postie.def
  { Postie.settingsPort = 50000
  }

withPostieSocket :: (Socket -> Assertion) -> Assertion
withPostieSocket body =
  bracket
    (forkIO $ Postie.runSettings postieSettings $ \(Postie.Mail _ _ _ _ body) -> do
      runEffect $ body >-> drain
      pure Postie.Accepted)
    killThread
    (\_ -> do
      threadDelay 250000
      let port = Postie.settingsPort postieSettings
      addr : _ <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show port)
      bracket
        (do sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock (SockAddrInet port (tupleToHostAddress (0x7F, 0, 0, 1)))
            pure sock)
        close
        body)

data TestLine
  = Send ByteString
  | Expect ByteString
  | Comment ByteString
  | Other ByteString
  deriving (Eq, Show)

mkFileTest :: FilePath -> Assertion
mkFileTest testFile =
  withPostieSocket $ \sock ->
    withFile ("test/resources/filetests/" ++ testFile) ReadMode $ \hdl ->
      fix $ \self -> do
        eof <- hIsEOF hdl
        unless eof $ do
          line <- convertLine <$> BSC.hGetLine hdl
          case line of
            Send line -> sendBytes sock line *> sendBytes sock "\r\n"
            Expect status -> do
              result <- recvBytes 4096 sock
              assertBool
                "status from Postie doesn't match expected!"
                (status `BS.isPrefixOf` result)
            Comment _ -> pure ()
            Other line -> assertFailure ("unexpected line in test file: " ++ show line)
          self
  where
    convertLine :: ByteString -> TestLine
    convertLine line =
      if ">" `BS.isPrefixOf` line then
        Send $ BS.dropWhile (== 0x20) $ BS.drop 1 line
      else if "<" `BS.isPrefixOf` line then
        Expect $ BS.dropWhile (== 0x20) $ BS.drop 1 line
      else if "#" `BS.isPrefixOf` line then
        Comment $ BS.dropWhile (== 0x20) $ BS.drop 1 line
      else
        Other line

    sendBytes :: Socket -> ByteString -> IO ()
    sendBytes socket bytes = do
      bracket
        (mallocArray @Word8 (BS.length bytes))
        free
        (\buf -> do
          let (loop, _) = BS.foldl'
                (\(body, n) byte -> (body *> pokeElemOff buf n byte, n + 1))
                (pure (), 0)
                bytes
          loop
          void $ sendBuf socket buf (BS.length bytes))

    recvBytes :: Int -> Socket -> IO ByteString
    recvBytes numBytes socket = do
      bracket
        (mallocArray @Word8 numBytes)
        free
        (\buf -> do
          totalBytes <- recvBuf socket buf numBytes
          let loop = fix $ \self n acc ->
                if n >= totalBytes then pure acc
                else do
                  byte <- peekElemOff buf n
                  self (n + 1) (byte : acc)
          bytes <- loop 0 []
          pure $ BS.pack $ reverse bytes)

isTestFile :: FilePath -> Bool
isTestFile file =
  ".test" `isSuffixOf` fmap toLower file

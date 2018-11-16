{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C8
import SimpleGraphics
import SimpleServer

import Control.Concurrent
import Control.Concurrent.STM

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  ch <- newTChanIO
  forkIO $ runServer "8080" (serverloop ch)
  runGraphics 512 512 "Canvas" (drawloop ch)
  return ()
  
serverloop :: TChan String -> C8.ByteString -> IO ()
serverloop ch s = do
  let s' = C8.unpack s
  atomically $ writeTChan ch s'
  putStrLn s'
    
drawloop :: TChan String -> IO (Plot ())
drawloop ch = do
  threadDelay 20000
  str <- atomically $ tryReadTChan ch
  case fmap words str of
    Just ["clear"] -> return $ do
      clear
      swapBuffers
      clear
      swapBuffers
    Just (c : arr) -> do
      let arr' = map read arr :: [Float]
      case c of
        "T" -> return $ do
          drawTriangle arr'
          swapBuffers
          drawTriangle arr'
          swapBuffers
        "L" -> return $ do
          drawLine arr'
          swapBuffers
          drawLine arr'
          swapBuffers
        "P" -> return $ do
          drawPoint arr'
          swapBuffers
          drawPoint arr'
          swapBuffers
        _ -> return $ swapBuffers
    Nothing -> return $ swapBuffers
    _ -> return $ swapBuffers

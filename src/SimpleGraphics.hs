module SimpleGraphics where

import Foreign.Ptr
import Data.Array.Storable
import Foreign.Storable
import Control.Concurrent

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

-- Helper functions ------------------------------------------------------------

offsetPtr :: Integral b => b -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

compileShader :: GL.ShaderType -> String -> IO GL.Shader
compileShader sType sSrc = do
  s <- GL.createShader sType
  GL.shaderSourceBS s $= GL.packUtf8 sSrc
  GL.compileShader s
  flag <- GL.get $ GL.compileStatus s
  return s 

linkProgram :: [GL.Shader] -> IO GL.Program
linkProgram arr = do
  prog <- GL.createProgram
  mapM_ (GL.attachShader prog) arr
  flag <- GL.linkProgram prog
  return prog

bufferData :: Storable a => GL.BufferObject -> GL.BufferTarget -> [a] -> IO ()
bufferData buff bTarget dataArr = do
  let len = length dataArr
      size = len * sizeOf (head dataArr)
  arr <- newListArray (0, len - 1) dataArr
  withStorableArray arr $ \ptr ->
    GL.bufferData bTarget $= (fromIntegral $ size, ptr, GL.StaticDraw)
  return ()

drawDirect :: GL.BufferObject -> GL.PrimitiveMode -> [Float] -> IO ()
drawDirect buff prim vert = do
  bufferData buff GL.ArrayBuffer vert
  GL.drawArrays prim 0 ((fromIntegral $ length vert) `div` 5)

-- Library functions -----------------------------------------------------------


drawLine :: [Float] -> Plot ()
drawLine arr = Plot $ \w b -> drawDirect b GL.LineStrip arr

drawPoint :: [Float] -> Plot ()
drawPoint arr = Plot $ \w b -> drawDirect b GL.Points arr

drawTriangle ::[Float] -> Plot ()
drawTriangle arr = Plot $ \w b -> drawDirect b GL.Triangles arr

swapBuffers :: Plot ()
swapBuffers = Plot $ \w b -> GLFW.swapBuffers w

clear :: Plot ()
clear = Plot $ \_ _ -> GL.clear [GL.ColorBuffer]

vs :: String
vs = "\
\attribute vec2 coord2f;\n\
\attribute vec3 col3f;\n\
\varying vec3 col3f_fs;\n\
\\n\
\void main(void) {\n\
\  gl_Position = vec4(coord2f, 0.0, 1.0);\n\
\  col3f_fs = col3f;\n\
\}"

fs :: String
fs = "\
\varying vec3 col3f_fs;\n\
\void main(void) {\n\
\  gl_FragColor = vec4(col3f_fs, 1.0);\n\
\}"

data Plot a = Plot { runPlot :: GLFW.Window -> GL.BufferObject -> IO a }

instance Functor Plot where
  fmap f (Plot x) = Plot $ \w b -> fmap f (x w b) 

instance Applicative Plot where
  pure x = Plot $ \w b -> pure x
  (Plot f) <*> (Plot x) = Plot $ \w b -> (f w b) <*> (x w b)

instance Monad Plot where
  Plot x >>= f = Plot $ \w b -> do
    x' <- x w b
    x'' <- runPlot (f x') w b
    return x''

runGraphics :: Int -> Int -> String -> IO (Plot a) -> IO ()
runGraphics width height title f = do
  flag <- GLFW.init
  if not flag then return () else do
    GLFW.windowHint $ GLFW.WindowHint'Samples $ Just 16
    GL.multisample $= GL.Enabled
    mainwin <- GLFW.createWindow width height title Nothing Nothing
    case mainwin of
      Nothing -> do
        GLFW.terminate
      Just win -> do
        GLFW.makeContextCurrent mainwin
        GLFW.swapInterval 1
        vs' <- compileShader GL.VertexShader vs
        fs' <- compileShader GL.FragmentShader fs
        prog <- linkProgram [vs', fs']
        GL.currentProgram $= Just prog
        coord2f <- GL.get $ GL.attribLocation prog "coord2f"
        col3f <- GL.get $ GL.attribLocation prog "col3f"
        GL.vertexAttribArray coord2f $= GL.Enabled
        GL.vertexAttribArray col3f $= GL.Enabled
        let size = fromIntegral $ sizeOf (0.0 :: Float)
        buff <- GL.genObjectName
        GL.bindBuffer GL.ArrayBuffer $= Just buff
        GL.vertexAttribPointer coord2f $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (5 * size) (offsetPtr 0))
        GL.vertexAttribPointer col3f $=
          (GL.ToFloat,
           GL.VertexArrayDescriptor 3 GL.Float (5 * size) (offsetPtr $ 2 * size))
        
        forkOS $ do
          GLFW.makeContextCurrent (Just win) 
          runDrawLoop win buff f
        runMainLoop win

runDrawLoop :: GLFW.Window -> GL.BufferObject -> IO (Plot a) -> IO ()
runDrawLoop win b f = do
  f' <- f
  runPlot f' win b
  runDrawLoop win b f

runMainLoop :: GLFW.Window -> IO ()
runMainLoop win = do
  close <- GLFW.windowShouldClose win
  case close of
    True -> do
      GLFW.destroyWindow win
      GLFW.terminate
    False -> do
      threadDelay 20000
      GLFW.pollEvents
      runMainLoop win
  

import Graphics.UI.GLUT
import System.IO  
import Control.Monad
import Drawbox
import ReadnSend
--import listD
main = do
    
     handle <- openFile "input.txt" ReadMode
     contents <- hGetContents handle
     let list =filter (/="  ")$filter (/=" ") $ lines contents
          
     print list    
     hClose handle
     getArgsAndInitialize
     createWindow ""
     windowSize $= Size 900 500
     clear [ColorBuffer]
     
     displayCallback $= readNsend list --[ "select","ns"]
     reshapeCallback $= Just reshape
     --actionOnWindowClose $= ContinueExectuion
     mainLoop
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

pointsl::[(GLfloat,GLfloat,GLfloat)]    
pointsl =
    [(-0.8, 0, 0.0)
    ,(-0.8, -0.2, 0.0)
    ,(-0.5, -0.2, 0.0)
    ,((-0.5), 0, 0.0)] 



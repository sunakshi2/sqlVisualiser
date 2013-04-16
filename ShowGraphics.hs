<<<<<<< HEAD

=======
{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
module ShowGraphics  where
import Graphics.UI.GLUT
import System.IO  
import Control.Monad
import Drawbox
import ReadnSend
-- | Takes input text file and creates the window for showing graphics
display = do
    
     handle <- openFile "writefile.txt" ReadMode
     contents <- hGetContents handle
     let list = filter (/="  ") $ filter (/="") $ filter (/=" ") $ lines contents
          
     print list    
     hClose handle
     getArgsAndInitialize
     createWindow ""
     windowSize $= Size 1024 700
     clear [ColorBuffer]
     
     displayCallback $= readNsend list --[ "select","ns"]
     reshapeCallback $= Just reshape
     --actionOnWindowClose $= ContinueExectuion
     mainLoop
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

{--
pointsl::[(GLfloat,GLfloat,GLfloat)]    
pointsl =
    [(-0.8, 0, 0.0)
    ,(-0.8, -0.2, 0.0)
    ,(-0.5, -0.2, 0.0)
    ,((-0.5), 0, 0.0)] 
--}


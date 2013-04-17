<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
-- | Draws the required graphics on the screen
module Drawbox where
import Graphics.UI.GLUT

-- | This function draws all the quads and the corresponding strings 
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
module Drawbox where
import Graphics.UI.GLUT
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
draw str points x y= do
    --clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 1 1)
    renderPrimitive Quads $ do
        mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    color $ (Color3 (0:: GLfloat) 0 0)
    
    currentRasterPosition $= Vertex4 (x:: GLfloat) y 0 1
    renderString Fixed8By13 $ str
    
    flush
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
-- | These are the initial points for the first quad    
points::[(GLfloat,GLfloat,GLfloat)]
points =[(0.0, 0.0, 0.0)]

{--
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
    
points::[(GLfloat,GLfloat,GLfloat)]
points =[(0.0, 0.0, 0.0)]
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
drawnothing = do 
    --clear [ColorBuffer]
    color $ (Color3 (0:: GLfloat) 1 0)
    renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
<<<<<<< HEAD
    flush--}
 
-- | This funtion prints the error in the input query on screen  
=======
<<<<<<< HEAD
    flush--}
 
-- | This funtion prints the error in the input query on screen  
=======
<<<<<<< HEAD
    flush--}
 
-- | This funtion prints the error in the input query on screen  
=======
    flush
  
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
drawError err = do
    clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0 0 1
    renderString Fixed9By15 $ err
    flush
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
    


-- | This function writes the heading of the output window
drawHeading = do
    clear [ColorBuffer]
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======

drawHeading = do
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0.9 0 1
    renderString Fixed9By15 $ "Query Flow"
    flush
<<<<<<< HEAD

-- | This function takes points as input and draws the corresponding line.            
=======
<<<<<<< HEAD

-- | This function takes points as input and draws the corresponding line.            
=======
<<<<<<< HEAD

-- | This function takes points as input and draws the corresponding line.            
=======
            
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
drawlines points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive Lines $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
    
drawNothings = do 
    --clear [ColorBuffer]
    color $ (Color3 (0:: GLfloat) 0 0)
    renderPrimitive Points $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z)  ([(0.0:: GLfloat,0.0, 0.0)]) 
    flush
    
    
drawNothing = do 
    color $ (Color3 (0:: GLfloat) 0 0)
    renderPrimitive Points $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z)  ([(0.0:: GLfloat,0.0, 0.0)]) 
    flush
    
-- | This function draws the arrows between quads
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
drawLineLoop points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineLoop $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
{--drawArrow points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush--}
-- | This function draws the arrows from nested query boxes to the outer query   
drawArrow points = do
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
drawArrow points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
drawArrowDown points = do
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
>>>>>>> 9d67e341b96fda6cb0c8d1378a0633818abd35d5
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush

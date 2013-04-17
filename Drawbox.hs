-- | Draws the required graphics on the screen
module Drawbox where
import Graphics.UI.GLUT

-- | This function draws all the quads and the corresponding strings 
draw str points x y= do
    --clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 1 1)
    renderPrimitive Quads $ do
        mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    color $ (Color3 (0:: GLfloat) 0 0)
    
    currentRasterPosition $= Vertex4 (x:: GLfloat) y 0 1
    renderString Fixed8By13 $ str
    
    flush
-- | These are the initial points for the first quad    
points::[(GLfloat,GLfloat,GLfloat)]
points =[(0.0, 0.0, 0.0)]

{--
drawnothing = do 
    --clear [ColorBuffer]
    color $ (Color3 (0:: GLfloat) 1 0)
    renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush--}
 
-- | This funtion prints the error in the input query on screen  
drawError err = do
    clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0 0 1
    renderString Fixed9By15 $ err
    flush
    


-- | This function writes the heading of the output window
drawHeading = do
    clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0.9 0 1
    renderString Fixed9By15 $ "Query Flow"
    flush

-- | This function takes points as input and draws the corresponding line.            
drawlines points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive Lines $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
    
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
drawLineLoop points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineLoop $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
{--drawArrow points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush--}
-- | This function draws the arrows from nested query boxes to the outer query   
drawArrow points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush

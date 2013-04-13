module Drawbox where
import Graphics.UI.GLUT
draw str points x y= do
    --clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 1 1)
    renderPrimitive Quads $ do
        mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    color $ (Color3 (0:: GLfloat) 0 0)
    
    currentRasterPosition $= Vertex4 (x:: GLfloat) y 0 1
    renderString Fixed8By13 $ str
    
    flush
    
points::[(GLfloat,GLfloat,GLfloat)]
points =[(0.0, 0.0, 0.0)]
drawnothing = do 
    --clear [ColorBuffer]
    color $ (Color3 (0:: GLfloat) 1 0)
    renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
  
drawError err = do
    clear [ColorBuffer]
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0 0 1
    renderString Fixed9By15 $ err
    flush

drawHeading = do
    color $ (Color3 (1:: GLfloat) 0 0)
    currentRasterPosition $= Vertex4 (-0.9:: GLfloat) 0.9 0 1
    renderString Fixed9By15 $ "Query Flow"
    flush
            
drawlines points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive Lines $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
drawLineLoop points = do 
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineLoop $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
drawArrow points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush
drawArrowDown points = do
    color $ (Color3 (0:: GLfloat) 1 1)
    renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z)->vertex$Vertex3 x y z) points
    flush

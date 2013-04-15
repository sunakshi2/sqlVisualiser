<<<<<<< HEAD
-- |Takes a list as an arg and calls draw for each token
module ReadnSend where
import Drawbox
import Graphics.UI.GLUT

-- | This function breaks the list of tokens from input file  when it finds a nested query
=======
--takes a list as an arg and calls draw for each token
module ReadnSend(readNsend) where
import Drawbox
import Graphics.UI.GLUT

-- | this is a 'breaklist' module.
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
breaklist:: [String]->([String],[String])
breaklist [] = ([],[])
breaklist (l:ls)= (list1,list2) where list1= if l==" NS " then [] else [l]++(fst'$(breaklist ls))
                                      list2= if l==" NS " then ls else snd'$(breaklist ls)
<<<<<<< HEAD
-- | This function takes a tuple of lists and returns its first list
fst' (ls,ys)=ls
-- | This function takes a tuple of lists and returns its second list
snd' (ls,ys)=ys
-- | This function checks if the input tokens are valid or not
checkForError (l:ls) = '\"' `elem` l

-- | This function calls 'drawup' , 'drawlow' , 'drawHeading'or 'drawError' depending on the tokens in the input list
=======
fst' (ls,ys)=ls
snd' (ls,ys)=ys

checkForError (l:ls) = '\"' `elem` l

>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
readNsend list   | checkForError list = do drawError $ concat list 
                            |otherwise = do 
                                                        drawHeading
                                                        let (fst,snd) = breaklist list
<<<<<<< HEAD
                                                        drawup fst  snd 0 
                                                        drawlow snd 0
                                                        
                                    

{--drawup (l:ls) 0  = do
                                --drawNothings
                                upd l pointsu 0 (x + (fromIntegral 0*0.4)) yu
                                --drawlines $ getlinepts i $ add i $ head' pointsu
                                --drawArrow $ arrowPts i  $ getlinepts i $ add i $ head' pointsu
                                drawArrow$ getlinepts 0 $ add 0 $ head' pointsu
                                drawup ls (1)    --}                                     
 
-- | This function draws the graphics for nested query                                         
drawup (l:[]) snd i = do
                                 if snd == [] then drawNothing else drawArrow ptsArrowDwn
                                 
                                 drawArrow $ getlinepts i $ add i $ head' pointsu
                                 upd l pointsu i (x + (fromIntegral i*0.4)) yu 
                                 drawup [] snd (i+1) 
                                 --drawlines $ getlinepts i $ add i $ head' pointsu
                                 --drawArrow $ arrowPts i $ getlinepts i $ add i $ head' pointsu                     
                                              
drawup [] snd i = do
                                drawLineLoop $ outerBoxPts i
                                --drawArrow ptsArrowDwn
                                drawNothing 

drawup (l:ls) snd i  = do
                                upd l pointsu i (x + (fromIntegral i*0.4)) yu
                                --drawlines $ getlinepts i $ add i $ head' pointsu
                                --drawArrow $ arrowPts i  $ getlinepts i $ add i $ head' pointsu
                                drawArrow$ getlinepts i $ add i $ head' pointsu
                                drawup ls snd (i+1) 
                                                                

=======
                                                        drawup fst 0 
                                                        drawlow snd 0
                                                        
                                    
                                        
                                        
drawup (l:ls) i  = do
                                upd l pointsu i (x + (fromIntegral i*0.4)) yu
                                --drawlines $ getlinepts i $ add i $ head' pointsu
                                --drawArrow $ arrowPts i  $ getlinepts i $ add i $ head' pointsu
                                drawArrowDown$ getlinepts i $ add i $ head' pointsu
                                drawup ls (i+1) 
                                                                
drawup (l:[]) i = do
                                 upd l pointsu i (x + (fromIntegral i*0.4)) yu 
                                 --drawlines $ getlinepts i $ add i $ head' pointsu
                                 --drawArrow $ arrowPts i $ getlinepts i $ add i $ head' pointsu                     
                                 drawArrowDown$ getlinepts i $ add i $ head' pointsu               
drawup [] i = drawLineLoop $ outerBoxPts i  --drawnothing 
>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a

outerBoxPts:: Int->[(GLfloat,GLfloat,GLfloat)]
outerBoxPts i  = [(-0.85,0.45,0::GLfloat),(-0.85,0.15,0),(-0.85 + (fromIntegral i) * 0.4 ,0.15,0),(-0.85 + (fromIntegral i) * 0.4 ,0.45,0)]

<<<<<<< HEAD
-- | This function is used for calculating points for drawing the arrow graphics
arrowPts :: Int->[(GLfloat,GLfloat,GLfloat)]->[(GLfloat,GLfloat,GLfloat)]
arrowPts  i ((x,y,z):s) | i ==0 = [(x,y,z),(x,y,z),(x,y,z)] 
                                      | otherwise = [(x-0.02::GLfloat,y+0.01,z),(x,y,z),(x-0.02,y-0.01,z)]

-- | This function draws the graphics for outer query 


drawlow [] i = do
                            drawLineLoop $ outerBoxPts i
                            drawArrow ptsArrowDwn       
                            drawNothing
                            --drawLineLoop $ outerBoxPts i

                        
drawlow (l:[]) i = do
                                
                                drawArrow$ getlinepts i $ add i $ head' pointsl
                                upd l pointsl i (x+ (fromIntegral i*0.4)) yd                                   
                                --drawlines $ getlinepts i $ add i $ head' pointsl
                                --drawArrow $ arrowPts i $ getlinepts i $ add i $ head' pointsl
                                
                                      
                                drawlow [] (i+1)

drawlow (l:ls) i  = do
                                upd l pointsl i (x + (fromIntegral i*0.4)) yd
                                --drawlines $ getlinepts i $ add i $ head' pointsl
                                drawArrow$ getlinepts i $ add i $ head' pointsl 
                                drawlow ls (i+1) 
                            
                            

-- | This function calculates new points for drawing box graphics and calls 'draw' function 
=======

arrowPts :: Int->[(GLfloat,GLfloat,GLfloat)]->[(GLfloat,GLfloat,GLfloat)]
arrowPts  i [(x,y,z),s] | i ==0 = [(x,y,z),(x,y,z),(x,y,z)] 
                                     | otherwise = [(x-0.02::GLfloat,y+0.01,z),(x,y,z),(x-0.02,y-0.01,z)]


drawlow (l:ls) i  = do
                                upd l pointsl i (x + (fromIntegral i*0.4)) yd
                               -- drawlines $ getlinepts i $ add i $ head' pointsl
                                --drawArrow$ arrowPts i $ getlinepts i $ add i $ head' pointsl
                                drawArrowDown$ getlinepts i $ add i $ head' pointsl
                                drawlow ls (i+1) 
                                
                                
drawlow (l:[]) i = do
                                upd l pointsl i (x+ (fromIntegral i*0.4)) yd                                   
                                --drawlines $ getlinepts i $ add i $ head' pointsl
                                --drawArrow $ arrowPts i $ getlinepts i $ add i $ head' pointsl
                                drawArrowDown$ getlinepts i $ add i $ head' pointsl
drawlow [] i = drawArrowDown ptsArrowDwn


>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
upd str p j x y = draw str ( map (add j) p) x y

x::GLfloat
x = -0.75
yu ::GLfloat
yu = 0.3
yd ::GLfloat
yd = -0.1
linepu::[(GLfloat,GLfloat,GLfloat)]
linepu = [(-0.8,0.3,0),(-0.8,0.3,0)]
linepd::[(GLfloat,GLfloat,GLfloat)]
linepd = [(-0.8,-0.1,0),(-0.8,-0.1,0)]

ptsArrowDwn::[(GLfloat,GLfloat,GLfloat)]
ptsArrowDwn=[(-0.25,0.15,0::GLfloat)
                   ,(-0.25,0,0)
                   ,(-0.26,0.02,0)
                   ,(-0.24,0.02,0)
                   ,(-0.25,0,0)]
pointsu::[(GLfloat,GLfloat,GLfloat)]
pointsu =
    [(-0.8, 0.4, 0.0)
    ,(-0.8, 0.2, 0.0)
    ,(-0.5, 0.2, 0.0)
    ,((-0.5), 0.4, 0.0)]

pointsl::[(GLfloat,GLfloat,GLfloat)]    
pointsl =
    [(-0.8, 0, 0.0)
    ,(-0.8, -0.2, 0.0)
    ,(-0.5, -0.2, 0.0)
    ,((-0.5), 0, 0.0)]   

--calculating new x

<<<<<<< HEAD
-- | This function updates the points for the next box whenever a new token is encountered
add j (x,y,z)  = ((x+(fromIntegral j)*(0.4)),y,z)
-- | This function calculates the points for the arrows between two boxes
=======
add j (x,y,z)  = ((x+(fromIntegral j)*(0.4)),y,z)

>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
getlinepts i (x,y,z)  |i ==0 = [(x,y,z),(x,y,z)]
                                 |otherwise = [(x -0.1, y - 0.1,z),(x, y - 0.1,z),(x-0.02::GLfloat,y-0.11,z),(x-0.02,y-0.09,z),(x,y-0.1,z)] 

--updlinepts j [(x1,y1,z1),(x2,y2,z2)]  = [((x1+(fromIntegral j)*(0.4)),y1,z1),((x2+(fromIntegral j)*(0.3)),y2,z2)]
<<<<<<< HEAD
-- | This functions returns the element at the first position of the list
=======

>>>>>>> 4be731a488ef019590e6e993e34ec07e3dc4490a
head' ::[a]->a
head' ls = (!!) ls 0

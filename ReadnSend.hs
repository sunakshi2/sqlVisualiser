--takes a list as an arg and calls draw for each token
module ReadnSend(readNsend) where
import Drawbox
import Graphics.UI.GLUT

-- | this is a 'breaklist' module.
breaklist:: [String]->([String],[String])
breaklist [] = ([],[])
breaklist (l:ls)= (list1,list2) where list1= if l==" NS " then [] else [l]++(fst'$(breaklist ls))
                                      list2= if l==" NS " then ls else snd'$(breaklist ls)
fst' (ls,ys)=ls
snd' (ls,ys)=ys

checkForError (l:ls) = '\"' `elem` l

readNsend list   | checkForError list = do drawError $ concat list 
                            |otherwise = do 
                                                        drawHeading
                                                        let (fst,snd) = breaklist list
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

outerBoxPts:: Int->[(GLfloat,GLfloat,GLfloat)]
outerBoxPts i  = [(-0.85,0.45,0::GLfloat),(-0.85,0.15,0),(-0.85 + (fromIntegral i) * 0.4 ,0.15,0),(-0.85 + (fromIntegral i) * 0.4 ,0.45,0)]


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

add j (x,y,z)  = ((x+(fromIntegral j)*(0.4)),y,z)

getlinepts i (x,y,z)  |i ==0 = [(x,y,z),(x,y,z)]
                                 |otherwise = [(x -0.1, y - 0.1,z),(x, y - 0.1,z),(x-0.02::GLfloat,y-0.11,z),(x-0.02,y-0.09,z),(x,y-0.1,z)] 

--updlinepts j [(x1,y1,z1),(x2,y2,z2)]  = [((x1+(fromIntegral j)*(0.4)),y1,z1),((x2+(fromIntegral j)*(0.3)),y2,z2)]

head' ::[a]->a
head' ls = (!!) ls 0

-- | Capture game window and detect shapes
-- This could probably be enhanced by somebody who has studied the problem, instead of just me hacking around!
module Games.OpenAlchemist.AI.Vision where

import Games.OpenAlchemist.AI.Types

import Graphics.Win32.Window
import Graphics.Win32.GDI.Bitmap
import Graphics.Win32.GDI.HDC
import Graphics.Win32.GDI.Graphics2D
import System.Win32.Types (withTString, ptrToMaybe, nullPtr)
import Control.Monad (liftM)

import Codec.BMP

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.List (foldl', isInfixOf, sortBy)
import Data.Tuple (swap)
import System.FilePath
import System.Directory (getDirectoryContents)
import Data.Either (rights)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import qualified Debug.Trace as Debug (trace)

getIconFingerPrint :: IO IconFingerPrint
getIconFingerPrint=do
        uxS<-getSmalls
        let smallMM = markerMap uxS
        -- print smallMM
        uxB<-getBigs
        let bigMM = markerMap uxB
        return $ IconFingerPrint smallMM bigMM

getGamePicture :: IO (Maybe (PMapInfo,PMapInfo,PMapInfo))
getGamePicture = do
        ms<-capture
        mpm<-getBMP ms
        case mpm of
                 Nothing -> return Nothing
                 Just pm->do
                        return $ Just $ cut pm

getBMP :: Maybe String  -> IO(Maybe PMap)
getBMP Nothing=return Nothing
getBMP (Just s)= do 
        ebmp<-readBMP s
        case ebmp of
                Left err -> do
                        print err
                        return Nothing
                Right bmp  -> do 
                       let rgba   =  unpackBMPToRGBA32 bmp
                       let (width, height) = bmpDimensions bmp
                       -- putStrLn ("width:"++(show width))
                       -- putStrLn ("height:"++(show height))
                       let m=bmp2Map (width,height) rgba
                       return $ Just m

debugBMP::PMapInfo -> String -> IO()
debugBMP (PMapInfo sz@(w,h) m) f=do
        let bs=map2Bmp (w,h) m
        let bmp=packRGBA32ToBMP w h bs
        writeBMP f bmp

bmp2PMapInfo :: BMP -> PMapInfo
bmp2PMapInfo bmp=let 
        rgba   =  unpackBMPToRGBA32 bmp
        (width, height) = bmpDimensions bmp
        pm=bmp2Map (width, height) rgba
        in PMapInfo (width, height) pm

bmp2Map :: (Int,Int) -> BS.ByteString ->PMap
bmp2Map (width,height) bs=let
       (mr,_,_)=BS.foldl' go (M.empty,(0,height-1),initPixel) bs
       in mr
       where 
                u::Int
                u=(-1)
                initPixel :: Pixel
                initPixel=Pixel u u u u
                nextCoord :: (Int,Int) -> (Int,Int)
                nextCoord (x,y) 
                        | x==width-1=(0,y-1)
                        | otherwise=(x+1,y)
                go :: (PMap,(Int,Int),Pixel) -> Word8 ->  (PMap,(Int,Int),Pixel)
                go (m,c,Pixel r g b a) w 
                        | r==u =(m,c,Pixel (fromIntegral w) u u u)
                        | g==u =(m,c,Pixel r (fromIntegral w) u u)
                        | b==u =(m,c,Pixel r g (fromIntegral w) u)
                        | a==u =(M.insert c (fuzzPixel $ Pixel r g b (fromIntegral w)) m,nextCoord c,initPixel)
   
map2Bmp :: (Int,Int) -> PMap -> BS.ByteString
map2Bmp (width,height) m= BS.pack $ foldl' conc [] [(-x,y) | y<-[0..(height-1)], x<-[-(width-1)..0]]
        where
                conc :: [Word8] -> Coord -> [Word8]
                conc bs c=
                        let Just (Pixel r g b a)=M.lookup c m
                        in (fromIntegral r:fromIntegral g:fromIntegral b:fromIntegral a:bs)

cut :: PMap -> (PMapInfo,PMapInfo,PMapInfo)
cut m= let
        preview=cutMap m (244,25) (562,119)
        toplay=cutMap m (244,126) (562,236)
        existing=cutMap m (244,244) (562,618)
       in (preview,toplay,existing)

cutMap :: PMap -> (Int,Int) -> (Int,Int) -> PMapInfo
cutMap m (startx,starty) (endx,endy)=
        let
                pm=M.mapKeys 
                        (\(x,y)->(x-startx,y-starty))
                        $ M.filterWithKey 
                                (\(x,y) _->x >= startx && x<=endx && y>=starty && y<=endy) 
                                m
        in PMapInfo (endx-startx,endy-starty) pm

capture :: IO (Maybe String)
capture = do
        mw<-withTString "OpenAlchemist" $ \ c_wname ->
                liftM ptrToMaybe $ c_FindWindow nullPtr c_wname
        case mw of
                Nothing -> do
                        putStrLn "No Open Alchemist"
                        return Nothing
                Just w-> do
                        let fn="OA.bmp"   
                        hdc       <- getWindowDC mw -- Get the dc handle of the desktop
                        (x,y,r,b) <- getWindowRect w -- Find the size of the desktop so we can know which size the destination bitmap should be
                                             -- (left, top, right, bottom)
                        newDC     <- createCompatibleDC (Just hdc) -- Create a new DC to hold the copied image. It should be compatible with the source DC
                        let width  = r - x -- Calculate the width
                        let height = b - y -- Calculate the Height
                        newBmp    <- createCompatibleBitmap hdc width height -- Create a new Bitmap which is compatible with the newly created DC
                        selBmp    <- selectBitmap newDC newBmp -- Select the Bitmap into the DC, drawing on the DC now draws on the bitmap as well
                        bitBlt newDC 0 0 width height hdc 0 0 sRCCOPY -- use SRCCOPY to copy the desktop DC into the newDC
                        createBMPFile fn newBmp newDC  -- Write out the new Bitmap file to Foo.bmp
                        deleteBitmap selBmp -- Cleanup the selected bitmap
                        deleteBitmap newBmp -- Cleanup the new bitmap
                        deleteDC newDC      -- Cleanup the DC we created.
                        return $ Just fn

getSmalls :: IO [(Tile,Marker)]
getSmalls=getIcons "-small"

getBigs :: IO [(Tile,Marker)]
getBigs=getIcons "-big"

getIcons :: String -> IO [(Tile,Marker)]
getIcons n=do
        -- files<-getDirectoryContents "data"
        mbmps <- mapM loadSmall [Green ..]
        let bmps=rights mbmps
        let ux=uniquePixels bmps
        --print ux
        return ux
       where 
                loadSmall :: Tile -> IO (Either Error (Tile,PMapInfo))
                loadSmall t=do
                        let f="data" </> ((map toLower (show t)) ++ n ++ ".bmp")
                        ebmp<-readBMP f
                        return $ fmap (\x->(t,bmp2PMapInfo x)) ebmp
       
histogram :: PMapInfo ->Histo
histogram (PMapInfo _ m)=foldr f M.empty $ M.assocs m
        where 
                f :: (Coord, Pixel) -> Histo -> Histo
                f (_,p) m=M.insertWith' (+) p 1 m 

uniquePixels :: [(Tile,PMapInfo)] -> [(Tile,Marker)]
uniquePixels bmps=
        let histos=map (\(x,y)->(x,y,histogram y)) bmps
        in map (f histos) histos
        where 
                f :: [(Tile,PMapInfo,Histo)] -> (Tile,PMapInfo,Histo) -> (Tile,Marker)
                f hs (t,PMapInfo (wi,he) pm,h)=let
                        ords=sortBy (\(_,x1) (_,x2)->compare x2 x1) $ M.assocs h
                        others=M.unions $ map (\(_,_,h)->h) $ filter (\(t1,_,_)->t1/=t) hs
                        px=fst $ head $ filter (\(p,_)->not $ M.member p others ) ords
                        c@(fstx,fsty)=fst $ head $ filter (\(_,p)->px==p) $ M.assocs pm
                        mk=Marker px c (wi-fstx,he-fsty)
                        in (t,mk)
    
markerMap :: [(Tile,Marker)] -> M.Map Pixel (Tile,Marker)
markerMap ls=foldr f M.empty ls
        where f p@(_,(Marker pix _ _)) m=M.insert (fuzzPixel pix) p m 
 
fuzzPixel :: Pixel -> Pixel
fuzzPixel (Pixel r g b a)=Pixel (fuzz r) (fuzz g) (fuzz b) (fuzz a)
        where 
                fuzz :: Int -> Int
                fuzz i=(div i 20) * 20

fuzzPixelPlus :: Pixel -> Pixel
fuzzPixelPlus (Pixel r g b a)=Pixel (fuzz r) (fuzz g) (fuzz b) (fuzz a)
        where 
                fuzz :: Int -> Int
                fuzz i=((div i 20) * 20)+20
 
findTiles :: M.Map Pixel (Tile,Marker) -> PMapInfo -> (Tile,Tile)
findTiles m pm=let
        (t1:t2:_)=map fst $ find m pm
        in (t1,t2)
 
readGame :: M.Map Pixel (Tile,Marker) -> PMapInfo -> Tiles
readGame m pm=let
        res=find m pm
        in foldr f M.empty res
        where
                f :: (Tile,Coord) -> Tiles -> Tiles
                f (t,c) ts=M.insert (convertVisionCoord c) t ts
 
convertVisionCoord :: Coord -> Coord
convertVisionCoord (x,y)=(round $ (fromIntegral x) / 53,(round $ (374-(fromIntegral y)) / 53)-1) 
        --318 374
        --53 
 
find :: M.Map Pixel (Tile,Marker) -> PMapInfo -> [(Tile,Coord)]
find m (PMapInfo (w,h) pmi)=let
        coords=[(-x,-y) | x<-[(-w)..0],y<-[(-h)..0]]
        (res,_)=foldr f ([],S.empty) coords
        in res
        where 
                f :: Coord -> ([(Tile,Coord)],S.Set Coord) -> ([(Tile,Coord)],S.Set Coord)
                f c@(cx,cy) (rs,excluded)=if S.member c excluded
                        then (rs,excluded)
                        else let
                                pix=fromJust $ M.lookup c pmi
                                fuzzp=fuzzPixel pix
                                mt=M.lookup fuzzp m
                                mt2=case mt of 
                                        Nothing-> M.lookup (fuzzPixelPlus pix) m
                                        Just mt -> Just mt
                             in case mt2 of
                                        Nothing->(rs,excluded)
                                        Just (t,Marker _ (px,py) (ex,ey))->let
                                                point=((cx) - px,(cy) - py)
                                                excl=[(x,y) | x<-[(fst point)..((cx)+ex)],y<-[(snd point)..((cy)+ey)]]
                                                excluded2= foldr (S.insert) excluded excl
                                                --excluded3=Debug.trace (show point ++":"++ show excluded2) excluded2
                                                in ((t,point):rs,excluded2) 
                        
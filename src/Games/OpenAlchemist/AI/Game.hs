
module Games.OpenAlchemist.AI.Game where

import Games.OpenAlchemist.AI.Types

import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)

import Debug.Trace

maxWidth :: Int
maxWidth = 6

maxHeight :: Int
maxHeight = 7

multiForResolution :: Int
multiForResolution = 3

gameValid :: Tiles -> Bool
gameValid =M.foldrWithKey f True
        where
                f :: Coord -> Tile -> Bool -> Bool
                f _ _ False=False
                f (x,y) _ _
                        | x<0 || x>=maxWidth=False
                        | y<0 || y>=maxHeight=False
                        | otherwise=True

dropBall :: Tiles -> Tile -> Int -> Tiles
dropBall ms m col=let
        pos=[(col,-y) | y<- [-(maxHeight+1) .. 0]] -- allow two balls over top
        in fst $ foldr f (ms,False) pos
        where 
                f :: Coord -> (Tiles,Bool) -> (Tiles,Bool)
                f _ (ms2,True)= (ms2,True)
                f c (ms2,False)=let
                        mm=M.lookup c ms2
                        in case mm of 
                                Nothing->(M.insert c m ms2,True)
                                Just _ -> (ms2,False)

dropPositions :: (Tile,Tile) -> [DropPosition]
dropPositions (m1,m2)=[((m1,x),(m2,x+1)) | x <- [0..maxWidth-2]] -- m1 and m2, horizontal
        ++ [((m2,x),(m1,x+1)) | x <- [0..maxWidth-2]] -- m2 and m1, horizontal
        ++ [((m1,x),(m2,x)) | x <- [0..maxWidth-1]] -- m1 and m2 on same column, m1 on bottom
        ++ [((m2,x),(m1,x)) | x <- [0..maxWidth-1]] -- m1 and m2 on same column, m2 on bottom
        
allDropResults :: Tiles -> (Tile,Tile) -> [(Tiles,DropPosition)]
allDropResults ts ds=let
        dps=dropPositions ds
        in map f dps     
        where
                f :: DropPosition -> (Tiles,DropPosition)
                f pos@((t1,col1),(t2,col2))=(dropBall (dropBall ts t1 col1) t2 col2,pos)
        
bestOn1 ::   Tiles -> (Tile,Tile) ->  DropPosition
bestOn1 ts ds=let
        drs=allDropResults ts ds
        res= map applyDropPosition drs
        --res2=trace (show res) res
        in fst $ head $ sortBy compDropPositions res

bestOn2 ::   Tiles -> (Tile,Tile) -> (Tile,Tile) ->  DropPosition
bestOn2 ts ds1 ds2=let
        drs1=allDropResults ts ds1
        drs2=concatMap (\(ts1,dp)->map (\x->(fst x,dp)) $
                allDropResults ts1 ds2) drs1
        res= map applyDropPosition drs2
        res2=trace (show res) res
        in fst $ head $ sortBy compDropPositions res2

applyDropPosition :: (Tiles,DropPosition) -> (DropPosition,Int)
applyDropPosition (ts1,pos)=let
                        (ts2,sc1)=resolve ts1
                        in (pos,if gameValid ts2 then sc1 else (-1))            
                
compDropPositions :: (DropPosition,Int) -> (DropPosition,Int) -> Ordering
compDropPositions (dp1,sc1) (dp2,sc2)  
        | sc1 /= sc2=compare sc2 sc1
        | (snd $ fst dp1) /= (snd $ fst dp2)=compare (snd $ fst dp1) (snd $ fst dp2)
        | (snd $ snd dp1) /= (snd $ snd dp2)=compare (snd $ snd dp1) (snd $ snd dp2)  
        | otherwise=compare (fst $ fst dp2)  (fst $ fst dp1) 
        
resolve :: Tiles -> (Tiles,Score)
resolve ts=let (ts2,sc1)=resolve1 ts 
        in if ts==ts2
                then (ts2,sc1)
                else let (ts3,sc2)=resolve ts2
                     in (ts3,sc1+sc2)


resolve1 :: Tiles -> (Tiles,Score)
resolve1 ts=let
        (grps,_)= foldr fg ([],S.empty) $ M.keys ts
        in foldr calc (ts,0) grps
        where 
                fg :: Coord -> ([Tiles],S.Set Coord) ->  ([Tiles],S.Set Coord)
                fg c (tss,cs)=if S.member c cs
                        then (tss,cs)
                        else let
                                grp=findGroup ts c
                                cs2=foldr S.insert cs $ M.keys grp
                                in if M.size grp >1
                                        then ((grp:tss),cs2)
                                        else (tss,cs2)
                calc :: Tiles -> (Tiles,Score) -> (Tiles,Score)
                calc grp (ts1,sc1)= let
                        rc=resolutionCoord grp
                        mt=M.lookup rc grp
                        in case mt of
                                Just t->
                                        let mul=if M.size grp >2 
                                                then multiForResolution else 1
                                            ts2=if M.size grp >2 
                                                then
                                                        M.insert rc (succ t) $ M.difference ts1 grp
                                                else ts1
                                        in (ts2,sc1+(mul * (tileScore t ) * (M.size grp)))
                                Nothing ->(ts1,sc1)
                                
findGroup :: Tiles -> Coord -> Tiles
findGroup ts c = 
        let 
                mt=M.lookup c ts
        in case mt of
                Nothing -> M.empty
                Just t -> let
                        grp=M.insert c t M.empty
                        ns=neighbourCoords c
                        in foldr (addGroup ts t) grp ns 

addGroup :: Tiles -> Tile -> Coord -> Tiles  -> Tiles
addGroup tsFull t1 c grp   
        | M.notMember c grp =
                let 
                        mt=M.lookup c tsFull
                in case mt of
                                Just t2 | t1==t2->
                                        let
                                                grp2=M.insert c t1 grp
                                                ns=neighbourCoords c
                                        in foldr (addGroup tsFull t1) grp2 ns 
                                _ -> grp
        | otherwise = grp
        
neighbourCoords :: Coord -> [Coord]
neighbourCoords (x,y)=[(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

resolutionCoord :: Tiles -> Coord
resolutionCoord ts = foldr f (maxWidth,maxHeight) $ M.keys ts
        where 
                f  :: Coord -> Coord -> Coord
                f (x,y) (minx,miny)=if (y<miny)
                        then (x,y)
                        else if (y==miny && x<minx)
                                then (x,y)   
                                else (minx,miny)

tileScore :: Tile -> Int
tileScore Green=1
tileScore Yellow=3
tileScore Red=9
tileScore Purple=30
tileScore Cherries=90
tileScore Penguin=30
tileScore Cheese=900
tileScore Cow=3000
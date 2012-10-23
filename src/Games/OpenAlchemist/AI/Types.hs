
module Games.OpenAlchemist.AI.Types where

import qualified Data.Map as M

data Pixel = Pixel {
        r :: Int,
        g :: Int,
        b :: Int,
        a :: Int
        }  
        deriving (Eq,Read,Show,Ord)

type Coord = (Int,Int)

type PMap=M.Map Coord Pixel

data PMapInfo=PMapInfo Coord PMap
        deriving (Eq,Read,Show,Ord)
        
type Histo=M.Map Pixel Int

type Tiles=M.Map Coord Tile

type Score=Int

type DropPosition=((Tile,Int),(Tile,Int))

data Tile= Green | Yellow | Red | Purple | Cherries | Penguin | Cheese | Cow
       deriving (Eq,Read,Show,Enum,Bounded,Ord) 

data Marker = Marker Pixel Coord Coord
         deriving (Eq,Read,Show,Ord)
         
data IconFingerPrint = IconFingerPrint {
        smalls ::  M.Map Pixel (Tile,Marker),
        bigs :: M.Map Pixel (Tile,Marker)
        }
        deriving (Eq,Read,Show,Ord)
        
data GameState = GameState {
        preview :: (Tile,Tile),
        next :: (Tile,Tile),
        existing :: Tiles
        }        
        deriving (Eq,Read,Show,Ord)
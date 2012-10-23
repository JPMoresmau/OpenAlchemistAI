-- | Types
module Games.OpenAlchemist.AI.Types where

import qualified Data.Map as M

-- | Represents a Pixel with red green blue and alpha
data Pixel = Pixel {
        r :: Int,
        g :: Int,
        b :: Int,
        a :: Int
        }  
        deriving (Eq,Read,Show,Ord)

-- | Coordinates
type Coord = (Int,Int)

-- | simple representation of an image: a map of coordinates to Pixel
type PMap=M.Map Coord Pixel

-- | image: size + pixel map
data PMapInfo=PMapInfo Coord PMap
        deriving (Eq,Read,Show,Ord)
        
-- | each pixel associated with the number of pixel in that image        
type Histo=M.Map Pixel Int

-- | simple representation of a "undersood" game: a map of coordinates to Tiles
type Tiles=M.Map Coord Tile

-- | Score for moves
type Score=Int

-- | where to drop the two tiles: tile and column
type DropPosition=((Tile,Int),(Tile,Int))

-- | a tile represents a distinct shape
data Tile= Green | Yellow | Red | Purple | Cherries | Penguin | Cheese | Cow
       deriving (Eq,Read,Show,Enum,Bounded,Ord) 

-- | a Marker represents detection information for a Tile: a Pixel value, and how much before and after the pixel should be ignored to avoid detecting the same shape twice
data Marker = Marker Pixel Coord Coord
         deriving (Eq,Read,Show,Ord)
         
-- | finger prints for icons: holds the pixels that identify a tile, for both small icons (preview) and big         
data IconFingerPrint = IconFingerPrint {
        smalls ::  M.Map Pixel (Tile,Marker),
        bigs :: M.Map Pixel (Tile,Marker)
        }
        deriving (Eq,Read,Show,Ord)
        
-- | the full game state        
data GameState = GameState {
        preview :: (Tile,Tile), -- preview tiles
        next :: (Tile,Tile), -- current tiles to play
        existing :: Tiles -- existing tiles
        }        
        deriving (Eq,Read,Show,Ord)
 
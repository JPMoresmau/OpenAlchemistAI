-- | The executable entry point
module Main where

import Games.OpenAlchemist.AI.Game
import Games.OpenAlchemist.AI.Vision
import Games.OpenAlchemist.AI.Types

-- | entry point
main::IO()
main = do
        IconFingerPrint smallMM bigMM<-getIconFingerPrint -- get icons fingerprint
        mpcis<-getGamePicture -- capture picture of game
        case mpcis of
                 Nothing -> return ()
                 Just (preview,toplay,existing)->do
                        -- debugging
                        let smallFound=find smallMM preview -- see what shapes are in the preview
                        print smallFound
                        let bigFound1=find bigMM toplay -- see what shapes are there to play
                        print bigFound1
                        let bigFound2=find bigMM existing -- see the existing state of the game
                        print bigFound2
                        let previewTiles=findTiles smallMM preview -- get the preview tiles
                        print previewTiles
                        let toPlayTiles=findTiles bigMM toplay -- get the current tiles
                        print toPlayTiles
                        let game=readGame bigMM existing -- get the game
                        print game
                        let advice=bestOn2 game toPlayTiles previewTiles -- calculate best move
                        print advice
                        return()

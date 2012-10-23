module Main where

import Games.OpenAlchemist.AI.Game
import Games.OpenAlchemist.AI.Vision
import Games.OpenAlchemist.AI.Types

main::IO()
main = do
        IconFingerPrint smallMM bigMM<-getIconFingerPrint
        mpcis<-getGamePicture
        case mpcis of
                 Nothing -> return ()
                 Just (preview,toplay,existing)->do
                        let smallFound=find smallMM preview
                        print smallFound
                        let bigFound1=find bigMM toplay
                        print bigFound1
                        let bigFound2=find bigMM existing
                        print bigFound2
                        let previewTiles=findTiles smallMM preview
                        print previewTiles
                        let toPlayTiles=findTiles bigMM toplay
                        print toPlayTiles
                        let game=readGame bigMM existing
                        print game
                        let advice=bestOn2 game toPlayTiles previewTiles
                        print advice
                        return()

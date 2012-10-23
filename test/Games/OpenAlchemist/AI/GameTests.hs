
module Games.OpenAlchemist.AI.GameTests where

import Games.OpenAlchemist.AI.Game
import Games.OpenAlchemist.AI.Types

import qualified Data.Map as M
import Test.HUnit


gameTests :: [Test]
gameTests=[testValid,testDropPositions,testDrop,testFindGroups,testResolutionCoord,testResolve1,testResolve2]

testValid :: Test
testValid = TestLabel "testValid" (TestCase (
        do
        let empty=M.empty
        assertBool "empty is not valid" (gameValid empty)
        let zero=M.insert (0,0) Green empty
        assertBool "zero is not valid" (gameValid zero)
        let mx=M.insert (5,6) Green empty
        assertBool "max is not valid" (gameValid mx)
        let minusX=M.insert (-1,0) Green empty
        assertBool "minuxX is valid" (not $ gameValid minusX)
        let minusY=M.insert (0,-1) Green empty
        assertBool "minuxY is valid" (not $ gameValid minusY)
        let bigX=M.insert (6,0) Green empty
        assertBool "bigX is valid" (not $ gameValid bigX)
        let bigY=M.insert (0,7) Green empty
        assertBool "bigY is valid" (not $ gameValid bigY)
        ))
        
testDropPositions :: Test
testDropPositions = TestLabel "testDropPositions" (TestCase (
        do
        let dp=dropPositions(Green,Yellow)
        assertEqual "not 22 positions" 22 (length dp)        
        ))
        
testDrop :: Test
testDrop = TestLabel "testDrop" (TestCase (
        do
        let empty=M.empty
        let zero=M.insert (0,0) Green empty
        assertEqual "empty + Green !=zero" zero (dropBall empty Green 0)
        let full0=M.fromList $ map (\y-> ((0,y),Green)) [0 .. maxHeight-1]
        let invalid0=M.fromList $ map (\y-> ((0,y),Green)) [0 .. maxHeight]
        assertEqual "full0 + Green !=invalid0" invalid0 (dropBall full0 Green 0)
        ))    
        
testFindGroups     :: Test
testFindGroups = TestLabel "testFindGroups" (TestCase (do       
        let empty=M.empty
        let r1=findGroup empty (0,0)
        assertEqual "empty->empty" empty r1
        let one=M.insert (0,0) Green empty
        let r2=findGroup one (0,0)
        assertEqual "one->one" one r2
        let two=M.insert (0,1) Green one
        let r3=findGroup two (0,0)
        assertEqual "two->two" two r3
        let three=M.insert (0,2) Red two
        let r4=findGroup three (0,0)
        assertEqual "three->two" two r4
        ))
        
testResolutionCoord     :: Test
testResolutionCoord = TestLabel "testResolutionCoord" (TestCase (do    
        let empty=M.empty
        assertEqual "empty" (6,7) $ resolutionCoord empty
        let one=M.insert (0,0) Green empty
        assertEqual "one" (0,0) $ resolutionCoord one
        ))       
        
testResolve1     :: Test
testResolve1 = TestLabel "testResolve1" (TestCase (do    
        let empty=M.empty
        let one=M.insert (0,2) Green $ M.insert (0,1) Green $ M.insert (0,0) Green empty
        let (r1,sc1)=resolve1 one
        let y= M.insert (0,0) Yellow empty
        assertEqual "yellow" y r1
        assertEqual "sc1" 3 sc1
        ))              
        
testResolve2     :: Test
testResolve2 = TestLabel "testResolve2" (TestCase (do    
        let empty=M.empty
        let one=M.insert (1,1) Yellow $ M.insert (1,0) Yellow $ M.insert (0,2) Green $ M.insert (0,1) Green $ M.insert (0,0) Green empty
        let (r1,sc1)=resolve one
        let re= M.insert (0,0) Red empty
        assertEqual "red" re r1
        assertEqual "sc1" 12 sc1
        ))         
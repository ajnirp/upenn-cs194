{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- second parameter is True for attackers, False for defenders
unitsSent :: Army -> Bool -> Int
unitsSent 0 _ = error "no units left"
unitsSent army True = if army > 3 then 3 else army - 1
unitsSent army False = if army >= 2 then 2 else 1

roll :: Int -> Rand StdGen [DieValue]
roll times = replicateM times die

-- args: attacker rolls zipped with defender rolls,
--       attacking units,
--       defending units
-- output: (#attacker casualties, #defender casualties)
skirmish :: [(Int, Int)] -> (Int, Int)
skirmish xs = (numDefenderWins, numAttackerWins)
    where (aRolls, dRolls) = unzip xs
          aRollsSorted = reverse $ sort aRolls
          dRollsSorted = reverse $ sort dRolls
          didAttackerWin = zipWith (>) aRollsSorted dRollsSorted
          numFights = length didAttackerWin
          numAttackerWins = length $ filter (==True) didAttackerWin
          numDefenderWins = numFights - numAttackerWins

updateArmies :: Battlefield -> (Int, Int) -> Battlefield
updateArmies b@(Battlefield { attackers = a, defenders = d }) (dWins, aWins) =
    Battlefield { attackers = (a - dWins), defenders = (d - aWins) }

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield { attackers = a, defenders = d }) =
    updateArmies b <$> skirmish <$> zipped
    where aSent = unitsSent a True
          dSent = unitsSent d False
          getInts :: Rand StdGen [DieValue] -> Rand StdGen [Int]
          getInts r = map unDV <$> r
          aRolls = getInts $ roll aSent
          dRolls = getInts $ roll dSent
          zipped = liftM2 zip aRolls dRolls

data BattleWinner = InProgress | Attacker | Defender deriving (Eq, Show)

battleResult :: Battlefield -> BattleWinner
battleResult b@(Battlefield { attackers = a, defenders = d })
    | a < 2 = Defender
    | d <= 0 = Attacker
    | otherwise = InProgress

successProb :: Battlefield -> Rand StdGen Double
successProb b = attackerWinRatio <$> invadeMultiple 1000 b
    where invadeMultiple :: Int -> Battlefield -> Rand StdGen [BattleWinner]
          invadeMultiple times b = mapM invasionResult (replicate times b)
          invasionResult :: Battlefield -> Rand StdGen BattleWinner
          invasionResult b = battleResult <$> invade b
          attackerWinRatio :: [BattleWinner] -> Double
          attackerWinRatio bws = (fromIntegral numAttackerWins) / (fromIntegral numFights)
              where numFights = length bws
                    numAttackerWins = length $ filter (==Attacker) bws

invade :: Battlefield -> Rand StdGen Battlefield
invade b | battleResult b == InProgress = battle b >>= invade
         | otherwise = pure b
module TypeOperations where

import           GameTypes
import           Data.Map                      as Map
                                                ( Map
                                                , fromListWith
                                                )
import           Data.Maybe                     ( isJust )
import           Data.List                      ( (\\) )


cardSellGold :: Card -> Player -> Gold
cardSellGold c p =
  sellCardBaseCost + length [ c | c <- playerDeck p, cardClass c == YELLOW ]

buildableBuildCost :: Buildable -> Player -> Player -> Gold
buildableBuildCost c p1 p2 = buildableCostGold + baseCostGold + tradeTaxGold
 where
  buildableCostGold = case buildableCostUnit c of
    Free                   -> 0
    CostGold       g       -> g
    CostProduction _       -> 0
    CostGoldProduction g _ -> g
  buildableCostResources = case buildableCostUnit c of
    Free                    -> []
    CostGold       _        -> []
    CostProduction rs       -> rs
    CostGoldProduction _ rs -> rs
  baseCostResources = foldr1
    (\a b ->
      if purchaseResourceBaseCost
         * length a
         < purchaseResourceBaseCost
         * length b
      then
        a
      else
        b
    )
    (map (buildableCostResources \\) currentPlayerResources)
  tradeTaxGold =
    maximum $ map (computeTradeTax baseCostResources) otherPlayerResources
  baseCostGold           = purchaseResourceBaseCost * length baseCostResources
  currentPlayerResources = playerProductionCapability p1
  otherPlayerResources   = playerProductionCapability p2

cardBuildCost :: Card -> Player -> Player -> Gold
cardBuildCost c p1 p2 | isUpgrade = 0
                      | otherwise = buildableBuildCost (cardBuildable c) p1 p2
  where isUpgrade = cardUpgradesTo c `elem` playerUpgrades p1

wonderBuildCost :: Wonder -> Player -> Player -> Gold
wonderBuildCost w = buildableBuildCost (wonderBuildable w)

playerUpgrades :: Player -> [Maybe Name]
playerUpgrades p =
  [ cardUpgradesTo c | c <- playerDeck p, isJust (cardUpgradesTo c) ]

playerProductionCapability :: Player -> [[Resource]]
playerProductionCapability p = [ m ++ i | i <- e ]
 where
  e  = exclusiveResourceCombinations p
  m  = mc ++ mw
  mc = concat $ filter
    (not . null)
    [ extractMultiples (buildableProductionUnit $ cardBuildable c)
    | c <- playerDeck p
    ]
  mw = concat $ filter
    (not . null)
    [ extractMultiples (buildableProductionUnit $ wonderBuildable w)
    | w <- playerWonders p
    ]

computeBasePurchaseCost :: PurchaserResources -> CardCostResources -> Gold
computeBasePurchaseCost pr ccr = length $ ccr \\ pr

-- given the resources required for card purchase and opponent resources
-- this function computes the amount of gold paid as trade tax
computeTradeTax :: [Resource] -> [Resource] -> Gold
computeTradeTax cr or = sum $ map (`numTimesFound` or) cr

numTimesFound x = length . filter (== x)

exclusiveResourceCombinations :: Player -> [[Resource]]
exclusiveResourceCombinations p =
  sequence
    $  filter
    (not . null)
         [ extractExclusives $ buildableProductionUnit $ cardBuildable c
         | c <- playerDeck p
         ]
    ++ [ extractExclusives $ buildableProductionUnit $ wonderBuildable w
       | w <- playerWonders p
       ]

extractExclusives :: ProductionUnit -> [Resource]
extractExclusives pu = case pu of
  NonProducing                -> []
  Multiple  _                 -> []
  Exclusive rs                -> rs
  MixedMultipleExclusive _ rs -> rs

extractMultiples :: ProductionUnit -> [Resource]
extractMultiples pu = case pu of
  NonProducing               -> []
  Multiple  m                -> m
  Exclusive _                -> []
  MixedMultipleExclusive m _ -> m

frequency :: (Ord k, Num a) => [k] -> Map k a
frequency xs = Map.fromListWith (+) [ (x, 1) | x <- xs ]


pu1 = MixedMultipleExclusive [WOOD] [STONE, BRICK]
pu2 = Exclusive [PAPYRUS, GLASS]
pu3 = Multiple [BRICK, WOOD]
pu4 = Multiple [PAPYRUS, GLASS]
cu1 = CostProduction [WOOD, PAPYRUS]
cu2 = CostGoldProduction 7 [GLASS, PAPYRUS]
b1 = Buildable cu1 pu1 Nothing 0 0 [] []
b2 = Buildable cu2 pu4 Nothing 0 0 [] []
c1 = Card "Pallisade" BROWN AGE_I Nothing Nothing b1
c2 = Card "Moat" BROWN AGE_I Nothing Nothing b2


p1 = Player "bob" PLAYER_I 7 [c1] [] [] [] [] []
p2 = Player "dob" PLAYER_II 7 [c2] [] [] [] [] []

module GameTypes where


type Gold = Int

type Shields = Int

type VictoryPoints = Int

type InvocationToken = Int

type Name = String

type Location = Int

type MarkerSize = Int

type NumberOfDiscountedResources = Int

type HasMinervaToken = Bool

type WonderIsBuilt = Bool

type CardIsVisible = Bool

type PurchaserResources = [Resource]

type OpponentResources = [Resource]

type CardCostResources = [Resource]

sellCardBaseCost :: Gold
sellCardBaseCost = 2

purchaseResourceBaseCost :: Gold
purchaseResourceBaseCost = 2

tradeTax :: Gold
tradeTax = 1

data Age = AGE_I
         | AGE_II
         | AGE_III
         deriving (Show, Eq)

data Pantheon = GREEK
              | ROMAN
              | EGYPTIAN
              | PHOENICIAN
              | MESOPOTAMIAN
              deriving (Show, Eq)

data ScienceSymbol = ARMILLARY
                   | WHEEL
                   | SUNDIAL
                   | MORTAL_AND_PESTLE
                   | PLUMB_BOB
                   | INK_AND_QUILL
                   | LAW
                   deriving (Show, Eq)

newtype SummoningToken = Summoning Pantheon deriving Show

data Resource = WOOD
              | STONE
              | BRICK
              | GLASS
              | PAPYRUS
              deriving (Show, Eq)

data CardClass = RED
               | BLUE
               | GREY
               | GREEN
               | BROWN
               | YELLOW
               | GUILD
               | TEMPLE
               | ANY
               deriving (Show, Eq)

data ProductionUnit = NonProducing
                    | Multiple [Resource]
                    | Exclusive [Resource]
                    | MixedMultipleExclusive [Resource] [Resource]
                    deriving Show

data CostUnit = Free
              | CostGold Gold
              | CostProduction [Resource]
              | CostGoldProduction Gold [Resource]
              deriving Show

data Action = SELL_CARD [Card]
            | BUILD_CARD [Card]
            | BUILD_WONDER [Wonder]
            | PLACE_GOD [GodCard]
            | INVOKE_GOD [GodCard]
            deriving Show


data Buildable = Buildable { buildableCostUnit :: CostUnit
                           , buildableProductionUnit :: ProductionUnit
                           , builableScienceSymbol :: Maybe ScienceSymbol
                           , buildableShields :: Shields
                           , buildableVictoryPoints :: VictoryPoints
                           , buildableInstantEffects :: [InstantEffect]
                           , buildableExtraRules :: [ExtraRule]} deriving Show

data Wonder = Wonder { wonderName :: Name
                     , wonderBuildable :: Buildable
                     , wonderIsBuilt :: WonderIsBuilt
                     } deriving Show

data Card = Card { cardName :: Name
                 , cardClass :: CardClass
                 , cardAge :: Age
                 , cardUpgradesTo :: Maybe Name
                 , cardUpgradesFrom :: Maybe Name
                 , cardBuildable :: Buildable
                 } deriving Show

data InstantEffect = EXTRA_TURN
                   | BUILD_CARD_FROM_DISCARD CardClass
                   | DISCARD_OPPONENT_CARD CardClass
                   | STEAL_CARD CardClass
                   | GET_GOLD PlayerReference Gold
                   | LOSE_GOLD PlayerReference Gold
                   deriving Show

data ExtraRule = RESOURCE_FIXED_PRICE PlayerSign Resource Gold
               | GET_GOLD_ON_UPGRADE PlayerSign Gold
               | CARD_COSTS_N_RESOURCES_LESS PlayerSign CardClass NumberOfDiscountedResources
               | RED_CARDS_PROVIDE_EXTRA_SHIELD
               | GET_OPPONENT_GOLD_ON_TRADE_TAX
               deriving Show

data ProgressToken = ProgressToken { progressTokenName :: Name
                                   , progressTokenInstantEffects :: [InstantEffect]
                                   , progressTokenExtraRules :: [ExtraRule]
                                   } deriving Show

data PlayerSign = PLAYER_I
                | PLAYER_II
                deriving Show

data PlayerReference = CURRENT_PLAYER
                     | OPPONENT
                     deriving Show

data GodCard = GodCard { godName :: Name
                       , pantheon :: Pantheon
                       , isGate :: Bool
                       } deriving Show

data AgeDeck = AgeDeck { deckAge :: Age
                       , deckCards :: [Card]
                       } deriving Show

data PantheonDeck = PantheonDeck { greekDeck :: [GodCard]
                                 , romanDeck :: [GodCard]
                                 , egyptianDeck :: [GodCard]
                                 , phoenicianDeck :: [GodCard]
                                 , mesopotamianDeck :: [GodCard]
                                 } deriving Show

data PantheonLocation = PI_I
                      | PI_II
                      | PI_III
                      | PII_I
                      | PII_II
                      | PII_III
                      deriving Show

data PantheonSlot = PantheonSlot { pantheonGod :: Maybe GodCard
                                 , pantheonCardVisible :: CardIsVisible
                                 , pantheonLocation :: PantheonLocation
                                 , playerOneInvokeCost :: Gold
                                 , playerTwoInvokeCost :: Gold
                                 } deriving Show

data MarkerData = MarkerData { warMarkerLocation ::  Maybe Location
                             , warMakerTerritory :: Maybe PlayerSign
                             , minervaTokenLocation :: Maybe Location
                             , markerGoldPenalty :: Maybe Gold
                             , markerPentalyConsumed :: Maybe Bool
                             , markerVictoryPoints :: VictoryPoints
                             , markerSize :: MarkerSize
                             } deriving Show

data WarProgressData = NEUTRAL HasMinervaToken
                     | PI_FIRST MarkerData
                     | PI_SECOND MarkerData
                     | PI_THIRD MarkerData
                     | PI_HOME HasMinervaToken
                     | PII_FIRST MarkerData
                     | PII_SECOND MarkerData
                     | PII_THIRD MarkerData
                     | PII_HOME HasMinervaToken
                     deriving Show

data Player = Player { playerName :: Name
                     , playerSign :: PlayerSign
                     , playerGold :: Gold
                     , playerDeck :: [Card]
                     , playerWonders :: [Wonder]
                     , playerGods :: [GodCard]
                     , playerProgressTokens :: [ProgressToken]
                     , playerSummoningTokens :: [SummoningToken]
                     , playerInvocationTokens :: [InvocationToken]
                     } deriving Show

data CardTree a = Nil
                | Leaf a
                | Node (CardTree a) (CardTree a)
                deriving Show

data Board = Board { boardName :: Name
                   , boardDeckAgeI :: [Card]
                   , boardDeckAgeII :: [Card]
                   , boardDeckAgeIII :: [Card]
                   , boardDiscards :: [Card]
                   , boardProgressTokens :: [ProgressToken]
                   , boardPantheonDeck :: PantheonDeck
                   , boardSummoningTokens :: [SummoningToken]
                   , boardInvocationTokens :: [InvocationToken]
                   , boardPantheon :: [PantheonSlot]
                   } deriving Show

data GameState = GameState { player1 :: Player
                           , player2 :: Player
                           , board :: Board
                           , gameAge :: Age
                           } deriving Show


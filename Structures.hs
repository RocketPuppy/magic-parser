-----------------------------------------------------------------------------
--
-- Module      :  Structures_new
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  wilsonhardrock@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Structures_new (

) where

-- | A Quantified Object is an object with a list of quantifiers
-- | attached to it.
type QuantifiedObject        = ([Quantifier],    Object)
    deriving (Show)

-- | These are the quantifiers that can be attached to an object
data Quantifier     = Reference         Reference
                    | Count             Count
                    | Characteristic    Characteristic
                    | Status            Status
                    | Timing            Timing
    deriving (Show)

-- | These are the objects that can have quantifiers
data Object         = Player            Player
                    | Ability           Ability
                    | Phase             Phase
                    | Turn              Turn
                    | Step              Step
                    | Effect            Effect
    deriving (Show)

-- | These are the various characteristics
data Characteristic = Name              String
                    | Mana              Cost [Mana]
                    | Color             [Color]
                    | Typestring        [Type]
                    | Abilities         [Ability]
                    | Power             Count
                    | Toughness         Count
                    | Loyalty           Count
                    | Hand_Mod          Count
                    | Life_Mod          Count
    deriving (Show)

-- | These are the various references
data Reference      = You
                    | Your
                    | This
                    | Target
                    | ThisCard
                    | Other
    deriving (Show)

-- | This represents the 'count' of an object, with Digit being the natural
-- | representation as a number
data Count          = Digit             Integer
                    | All
                    | Only
                    | Single
                    | None
    deriving (Show)

-- | These are the various statuses an object can have
data Status         = Tapped
                    | Untapped
                    | Flipped
                    | Unflipped
                    | FaceUp
                    | FaceDown
                    | PhasedIn
                    | PhasedOut
    deriving (Show)

-- | These are the representations of different players
data Player         = Opponent
                    | Teammates
                    | Self
    deriving (Show)

-- | A quantified time is a way of representing when something should happen.
-- | The Maybe monads take care of the case when any of those isn't specified.
-- | The list can have nothing in it, in which case there isn't a Timing.
data QuantifiedTime =
     QuantifiedTime { times :: [Timing]
                    , step  :: Maybe Step
                    , phase :: Maybe Phase
                    , turn  :: Maybe Turn
                    }
    deriving (Show)

--type QuantifiedTime = ([Timing], Maybe Step, Maybe Phase, Maybe Turn)

-- | These are all of the phases of a turn
data Phase          = Beginning
                    | Main
                    | Combat
                    | Ending
    deriving (Show)

-- | These are all the turns
data Turn           = Next_Turn
                    | Opponent_Turn
                    | Your_Turn
                    | This_Turn
    deriving (Show)

-- | These are all the steps in a turn
data Step           = Untap
                    | Upkeep
                    | Draw
                    | Beginning_Combat
                    | Declare_Attack
                    | Declare_Block
                    | Combat_Damage
                    | End_Combat
                    | End
                    | Cleanup
    deriving (Show)

-- | These are the timing quantifiers
data Timing         = Next
                    | Precombat
                    | Postcombat
    deriving (Show)

-- | This is the representation of abilities
data Ability        = Spell                 Spell
                    | Activated             Activated
                    | Triggered             Triggered
                    | Static                Static
                    | Linked                Linked
    deriving (Show)

-- | A spell ability is a list of instructions separated by periods.
type Spell          = Instructions

-- | An activated ability has a (list of) costs, an effect, and optional
-- | instructions.
type Activated      = (Costs, Effect, Instructions)

-- | A triggered ability has a trigger condition, an optional condition, an
-- | effect, and optional instructions.
type Triggered      = (Trigger, Condition, Effect, Instructions)

-- | A loyalty ability is an activated ability on a planeswalker where the
-- | cost is a number of loyalty counters
type Loyalty        = Activated

-- | A static ability is just an effect.
type Static         = Effect

-- | A linked ability is one which references something that happens in a
-- | previous ability
type Linked         = (Ability, [Ability])

-- | These are all the colors
data Color          = White
                    | Blue
                    | Black
                    | Red
                    | Green
                    | Colorless
    deriving (Show)

-- | A shorthand for multiple effects
type Effects        = [Effect]

-- | Effects are like the game's version of functions. They take parameters
-- | and do something with them.
-- | There are many effects in the game
data Effect         = Pay                   QObject
                    | ZoneChange            QObject Zone Zone
                    | Destroy               QObject
                    | Sacrifice             QObject
                    | Dies                  QObject
    deriving (Show)

-- | This is the representation of conditional statements
data Condition      = If                    Condition
                    | Or                    Condition Condition
                    | And                   Condition Condition
                    | CEffect               Effect
                    | NoCondition
    deriving (Show)
-- | Shorthand for multiple costs
type Costs          = [Cost]

-- | A Cost can be modeled as an effect that has to happen in order for another
-- | effect to happen.
type Cost           = Effect

-- | Shorthand for multiple instructions
type Instructions   = [Instruction]

-- | Not implemented yet
--data Instruction    =

-- | All the possible types of mana, including a arbitrary numbers of generic mana.
data Mana           = W  | U  | B  | R  | G  | X
                    | Generic               Count
                    | WU | WB | UB | UR | BR | BG | RG | RW | GW | GU
                    | W2 | U2 | B2 | R2 | G2
                    | WP | UP | BP | RP | GP
                    | S
    deriving (Show)

-- | Like a cost, a trigger is an effect that must happen for something else to
-- | happen. The difference lies in that costs are optional, and triggers always
-- | happen.
type Trigger        = Effect

-- | This is a representation of a single value of a typestring.
-- | A full typestring is a list of types.
data Type           = SuperType             String
                    | CardType              String
                    | SubType               String
    deriving (Show)

-- | The possible zones
data Zone           = Library
                    | Hand
                    | Graveyard
                    | Battlefield
                    | Command
                    | Stack
                    | Exile
                    | Ante
                    | Anywhere
    deriving (Show)

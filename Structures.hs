-----------------------------------------------------------------------------
--
-- Module      :  Structures
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

module Structures where

data Card = Card
    { abilities :: [Ability]
    }

data Ability    = Spell     { effects :: [Effect]}
                | Triggered { trigger :: Effect
                            , condition :: Condition
                            , mainEffects :: [Effect]
                            , constraintEffects :: [Effect]
                            }
                | Activated { costs :: [Effect]
                            , mainEffects :: [Effect]
                            , constraintEffects :: [Effect]
                            }
-- | Effects are like the game's version of functions. They take parameters
-- | and do something with them.
-- | There are many effects in the game
data Effect
    = Pay                   PaymentType
    | ZoneChange            Target Zone Zone
    | Destroy               Target
    | Sacrifice             Target
    | Dies                  Target
    | Counter               Target
    | ETB                   Target
    | LTB                   Target

-- | This is the representation of conditional statements
data Condition
    = If                    Condition
    | Or                    Condition Condition
    | And                   Condition Condition
    | CEffect               Effect
    | NoCondition

data PaymentType = ManaPay [Mana] | LifePay Count

data QuantifiedObject = Foo

data Target =
    Target  { targetCharacteristics :: [Characteristic]
            , targetStatuses :: [Status]
            , targetReferences :: [Reference]
            , targetEffects :: [Effect]
            , targetAbilities :: [Ability]
            }

-- | These are the various characteristics
data Characteristic = Name              String
                    | CMC               Count
                    | Color             [Color]
                    | Typestring        ([SuperType], [CardType], [SubType])
                    | Abilities         [Ability]
                    | Power             Count
                    | Toughness         Count
                    | Loyalty           Count
                    | Hand_Mod          Count
                    | Life_Mod          Count
instance Eq Characteristic where
    (==) (Typestring xs) (Typestring ys) = xs == ys
instance PrettyPrint Characteristic where
    pp (Typestring (supers, cards, []))   = take ((length str) -1 ) (str)
        where str = concatMap (\x -> (pp x) ++ " ") supers ++ concatMap (\x -> (pp x) ++ " ") cards
    pp (Typestring (supers, cards, subs)) = concatMap (\x -> (pp x) ++ " ") supers ++ concatMap (\x -> (pp x) ++ " ") cards ++ "-" ++ concatMap (\x-> " " ++ (pp x)) subs

-- | These are the various statuses an object can have
data Status         = Tapped
                    | Untapped
                    | Flipped
                    | Unflipped
                    | FaceUp
                    | FaceDown
                    | PhasedIn
                    | PhasedOut

-- | These are the various references
data Reference      = You
                    | Your
                    | This
                    | Other

-- | These are all the colors
data Color          = White
                    | Blue
                    | Black
                    | Red
                    | Green
                    | Colorless

-- | This represents the 'count' of an object, with Digit being the natural
-- | representation as a number
data Count          = Digit Integer
                    | All
                    | Only
                    | Single
                    | None
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

-- | All the possible types of mana, including arbitrary numbers of generic mana.
data Mana           = W  | U  | B  | R  | G  | X
                    | Generic Count
                    | WU | WB | UB | UR | BR | BG | RG | RW | GW | GU
                    | W2 | U2 | B2 | R2 | G2
                    | WP | UP | BP | RP | GP
                    | S
    deriving (Show)
instance PrettyPrint Mana where
    pp (Generic (Digit i)) = "{" ++ (show i) ++ "}"
    pp x = "{" ++ show x ++ "}"

data SuperType  = Basic | Legendary | Ongoing | Snow | World
    deriving (Show, Enum, Read, Eq)
instance PrettyPrint SuperType where
    pp x = show x

data CardType   = Artifact
                | Creature
                | Enchantment
                | Instant
                | Land
                | Planeswalker
                | Sorcery
                | Tribal
    deriving (Show, Enum, Read, Eq)
instance PrettyPrint CardType where
    pp ct = show ct

class SubTypeClass a where
    toSubType :: a -> SubType

data SubType    = ArtifactType      ArtifactType
                | EnchantmentType   EnchantmentType
                | InstantType       InstantType
                | LandType          LandType
                | PlaneswalkerType  PlaneswalkerType
                | CreatureType      CreatureType
    deriving (Show, Eq)
instance PrettyPrint SubType where
    pp (ArtifactType x)     = pp x
    pp (EnchantmentType x)  = pp x
    pp (InstantType x)      = pp x
    pp (LandType x)         = pp x
    pp (PlaneswalkerType x) = pp x
    pp (CreatureType x)     = pp x

data ArtifactType    = Contraption | Equipment | Fortification
    deriving (Show, Enum, Eq)
instance PrettyPrint ArtifactType where
    pp x = show x
instance SubTypeClass ArtifactType where
    toSubType x = ArtifactType x

data EnchantmentType =  Aura | Curse | Shrine
    deriving (Show, Enum, Eq)
instance PrettyPrint EnchantmentType where
    pp x = show x
instance SubTypeClass EnchantmentType where
    toSubType x = EnchantmentType x

data InstantType = Arcane | Trap
    deriving (Show, Enum, Eq)
instance PrettyPrint InstantType where
    pp x = show x
instance SubTypeClass InstantType where
    toSubType x = InstantType x

data LandType = Desert | Forest | Island | Lair | Locus | Mine | Mountain
              | Plains | PowerPlant | Swamp | Tower | Urzas
    deriving (Show, Enum, Eq)
instance PrettyPrint LandType where
    pp Urzas = "Urza's"
    pp x = show x
instance SubTypeClass LandType where
    toSubType x = LandType x

data PlaneswalkerType   = Ajani | Bolas | Chandra | Elspeth | Garruk | Gideon | Jace
                        | Karn | Koth | Liliana | Nissa | Sarkhan | Sorin | Tezzeret
                        | Venser
    deriving (Show, Enum, Eq)
instance PrettyPrint PlaneswalkerType where
    pp x = show x
instance SubTypeClass PlaneswalkerType where
    toSubType x = PlaneswalkerType x

data CreatureType   = Advisor | Ally | Angel | Anteater | Antelope | Ape
                    | Archer | Archon | Artificer | Assassin | AssemblyWorker
                    | Atog | Aurochs | Avatar | Badger | Barbarian | Basilisk | Bat
                    | Bear | Beast | Beeble | Berserker | Bird | Blinkmoth | Boar
                    | Bringer | Brushwagg | Camarid | Camel | Caribou | Carrier
                    | Cat | Centaur | Cephalid | Chimera | Citizen | Cleric
                    | Cockatrice | Construct | Coward | Crab | Crocodile | Cyclops
                    | Dauthi | Demon | Deserter | Devil | Djinn | Dragon | Drake
                    | Dreadnought | Drone | Druid | Dryad | Dwarf | Efreet | Elder
                    | Eldrazi | Elemental | Elephant | Elf | Elk | Eye | Faerie
                    | Ferret | Fish | Flagbearer | Fox | Frog | Fungus | Gargoyle
                    | Germ | Giant | Gnome | Goat | Goblin | Golem | Gorgon
                    | Graveborn | Gremlin | Griffin | Hag | Harpy | Hellion | Hippo
                    | Hippogriff | Homarid | Homunculus | Horror | Horse | Hound
                    | Human | Hydra | Hyena | Illusion | Imp | Incarnation | Insect
                    | Jellyfish | Juggernaut | Kavu | Kirin | Kithkin | Knight
                    | Kobold | Kor | Kraken | Lammasu | Leech | Leviathan | Lhurgoyf
                    | Licid | Lizard | Manticore | Masticore | Mercenary | Merfolk
                    | Metathran | Minion | Minotaur | Monger | Mongoose | Monk
                    | Moonfolk | Mutant | Myr | Mystic | Nautilus | Nephilim
                    | Nightmare | Nightstalker | Ninja | Noggle | Nomad | Octopus
                    | Ogre | Ooze | Orb | Orc | Orgg | Ouphe | Ox | Oyster | Pegasus
                    | Pentavite | Pest | Phelddagrif | Phoenix | Pincher | Pirate
                    | Plant | Praetor | Prism | Rabbit | Rat | Rebel | Reflection
                    | Rhino | Rigger | Rogue | Salamander | Samurai | Sand | Saproling
                    | Satyr | Scarecrow | Scorpion | Scout | Serf | Serpent | Shade
                    | Shaman | Shapeshifter | Sheep | Siren | Skeleton | Slith
                    | Sliver | Slug | Snake | Soldier | Soltari | Spawn | Specter
                    | Spellshaper | Sphinx | Spider | Spike | Spirit | Splinter
                    | Sponge | Squid | Squirrel | Starfish | Surrakar | Survivor
                    | Tetravite | Thalakos | Thopter | Thrull | Treefolk
                    | Triskelavite | Troll | Turtle | Unicorn | Vampire | Vedalken
                    | Viashino | Volver | Wall | Warrior | Weird | Werewolf | Whale
                    | Wizard | Wolf | Wolverine | Wombat | Worm | Wraith | Wurm
                    | Yeti | Zombie | Zubera
    deriving (Show, Enum, Eq)
instance PrettyPrint CreatureType where
    pp AssemblyWorker = "Assembly-Worker"
    pp x = show x
instance SubTypeClass CreatureType where
    toSubType x = CreatureType x

class PrettyPrint a where
    pp :: a -> String

-----------------------------------------------------------------------------
--
-- Module      :  Parsers
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

module Parsers where

import Text.Parsec
import Text.Parsec.String
import Structures
import Normalizer
import Data.Char

parse_Typestring :: String -> Characteristic
parse_Typestring input = undefined

parse_SubType :: String -> SubType
parse_SubType input = undefined

parseArtifactType :: String -> Either ParseError ArtifactType
parseArtifactType input = case (parse artifactType "" input') of
                            Left e -> Left e
                            Right a -> Right a
    where input' = map toLower input

parseEnchantmentType :: String -> Either ParseError EnchantmentType
parseEnchantmentType input = case (parse enchantmentType "" input') of
                                Left e -> Left e
                                Right a -> Right a
    where input' = map toLower input

parseInstantType :: String -> Either ParseError InstantType
parseInstantType input = case (parse instantType "" input') of
                            Left e -> Left e
                            Right a -> Right a
    where input' = map toLower input

parseLandType :: String -> Either ParseError LandType
parseLandType input = case (parse landType "" input') of
                        Left e -> Left e
                        Right a -> Right a
    where input' = map toLower input

parsePlaneswalkerType :: String -> Either ParseError PlaneswalkerType
parsePlaneswalkerType input = case (parse planeswalkerType "" input') of
                        Left e -> Left e
                        Right a -> Right a
    where input' = map toLower input

parseCreatureType :: String -> Either ParseError CreatureType
parseCreatureType input = case (parse creatureType "" input') of
                        Left e -> Left e
                        Right a -> Right a
    where input' = map toLower input

artifactType :: Parser ArtifactType
artifactType =
    do  spaces
        do  try (pluralize "contraption") <|> string "contraption"
            return Contraption
        <|>
        do  try (pluralize "equipment") <|> string "equipment"
            return Equipment
        <|>
        do  try (pluralize "fortification") <|> string "fortification"
            return Fortification

enchantmentType :: Parser EnchantmentType
enchantmentType =
    do  spaces
        do  try (pluralize "aura") <|> string "aura"
            return Aura
        <|>
        do  try (pluralize "curse") <|> string "curse"
            return Curse
        <|>
        do  try (pluralize "shrine") <|> string "shrine"
            return Shrine

instantType :: Parser InstantType
instantType =
    do  spaces
        do  try (pluralize "arcane") <|> string "arcane"
            return Arcane
        <|>
        do  try (pluralize "trap") <|> string "trap"
            return Trap

landType :: Parser LandType
landType =
    do  spaces
        do  try (pluralize "desert") <|> string "desert"
            return Desert
        <|>
        do  try (pluralize "forest") <|> string "forest"
            return Forest
        <|>
        do  try (pluralize "island") <|> string "island"
            return Island
        <|>
        do  try (char 'l')
            try (
                do  try (pluralize "air") <|> string "air"
                    return Lair
                <|>
                do  try (pluralize "ocus") <|> string "ocus"
                    return Locus
                )
        <|>
        do  try (char 'm')
            try (
                do  try (pluralize "ine") <|> string "ine"
                    return Mine
                <|>
                do  try (pluralize "ountain") <|> string "ountain"
                    return Mountain
                )
        <|>
        do  try (char 'p')
            try (
                do  try (pluralize "lains") <|> string "lains"
                    return Plains
                <|>
                do  try (pluralize "owerplant") <|> string "owerplant"
                    return PowerPlant
                )
        <|>
        do  try (pluralize "swamp") <|> string "swamp"
            return Swamp
        <|>
        do  try (pluralize "tower") <|> string "tower"
            return Tower
        <|>
        do  try (pluralize "urza's") <|> string "urza's"
            return Urzas

planeswalkerType :: Parser PlaneswalkerType
planeswalkerType =
        do  try (pluralize "ajani") <|> string "ajani"
            return Ajani
        <|>
        do  try (pluralize "bolas") <|> string "bolas"
            return Bolas
        <|>
        do  try (pluralize "chandra") <|> string "chandra"
            return Chandra
        <|>
        do  try (pluralize "elspeth") <|> string "elspeth"
            return Elspeth
        <|>
        do  try (char 'g')
            try (
                do  try (pluralize "arruk") <|> string "arruk"
                    return Garruk
                <|>
                do  try (pluralize "ideon") <|> string "ideon"
                    return Gideon
                )
        <|>
        do  try (pluralize "jace") <|> string "jace"
            return Jace
        <|>
        do  try (char 'k')
            try (
                do  try (pluralize "arn") <|> string "arn"
                    return Karn
                <|>
                do  try (pluralize "oth") <|> string "oth"
                    return Koth
                )
        <|>
        do  try (pluralize "liliana") <|> string "liliana"
            return Liliana
        <|>
        do  try (pluralize "nissa") <|> string "nissa"
            return Nissa
        <|>
        do  try (char 's')
            try (
                do  try (pluralize "arkhan") <|> string "arkhan"
                    return Sarkhan
                <|>
                do  try (pluralize "orin") <|> string "orin"
                    return Sorin
                )
        <|>
        do  try (pluralize "tezzeret") <|> string "tezzeret"
            return Tezzeret
        <|>
        do  try (pluralize "venser") <|> string "venser"
            return Venser


creatureType :: Parser CreatureType
creatureType =
        do  try (char 'a')
            try (
                do  try (pluralize "dvisor") <|> string "dvisor"
                    return Advisor
                <|>
                do  try (pluralize "lly") <|> string "lly"
                    return Ally
                <|>
                do  try (char 'n')
                    try (
                        do  try (pluralize "gel") <|> string "gel"
                            return Angel
                        <|>
                        do  try (string "te")
                            try (
                                do  try (pluralize "ater") <|> string "ater"
                                    return Anteater
                                <|>
                                do  try (pluralize "lope") <|> string "lope"
                                    return Antelope
                                )
                        )
                <|>
                do  try (pluralize "pe") <|> string "pe"
                    return Ape
                <|>
                do  try (char 'r')
                    try (
                        do  try (string "ch")
                            try (
                                do  try (pluralize "er") <|> string "er"
                                    return Archer
                                <|>
                                do  try (pluralize "on") <|> string "on"
                                    return Archon
                                )
                        <|>
                        do  try (pluralize "tificer") <|> string "tificer"
                            return Artificer
                        )
                <|>
                do  try (string "ss")
                    try (
                        do  try (pluralize "assin") <|> string "assin"
                            return Assassin
                        <|>
                        do  try (pluralize "embly-worker") <|> string "embly-worker"
                            return AssemblyWorker
                        )
                <|>
                do  try (pluralize "tog") <|> string "tog"
                    return Atog
                <|>
                do  try (pluralize "urochs") <|> string "urochs"
                    return Aurochs
                <|>
                do  try (pluralize "vatar") <|> string "vatar"
                    return Avatar
                )
        <|>
        do  try (pluralize "badger") <|> string "badger"
            return Badger
        <|>
        do  try (pluralize "barbarian") <|> string "barbarian"
            return Barbarian
        <|>
        do  try (pluralize "basilisk") <|> string "basilisk"
            return Basilisk
        <|>
        do  try (pluralize "bat") <|> string "bat"
            return Bat
        <|>
        do  try (pluralize "bear") <|> string "bear"
            return Bear
        <|>
        do  try (pluralize "beast") <|> string "beast"
            return Beast
        <|>
        do  try (pluralize "beeble") <|> string "beeble"
            return Beeble
        <|>
        do  try (pluralize "berserker") <|> string "berserker"
            return Berserker
        <|>
        do  try (pluralize "bird") <|> string "bird"
            return Bird
        <|>
        do  try (pluralize "blinkmoth") <|> string "blinkmoth"
            return Blinkmoth
        <|>
        do  try (pluralize "boar") <|> string "boar"
            return Boar
        <|>
        do  try (pluralize "bringer") <|> string "bringer"
            return Bringer
        <|>
        do  try (pluralize "brushwagg") <|> string "brushwagg"
            return Brushwagg
        <|>
        do  try (pluralize "camarid") <|> string "camarid"
            return Camarid
        <|>
        do  try (pluralize "camel") <|> string "camel"
            return Camel
        <|>
        do  try (pluralize "caribou") <|> string "caribou"
            return Caribou
        <|>
        do  try (pluralize "carrier") <|> string "carrier"
            return Carrier
        <|>
        do  try (pluralize "cat") <|> string "cat"
            return Cat
        <|>
        do  try (pluralize "centaur") <|> string "centaur"
            return Centaur
        <|>
        do  try (pluralize "cephalid") <|> string "cephalid"
            return Cephalid
        <|>
        do  try (pluralize "chimera") <|> string "chimera"
            return Chimera
        <|>
        do  try (pluralize "citizen") <|> string "citizen"
            return Citizen
        <|>
        do  try (pluralize "cleric") <|> string "cleric"
            return Cleric
        <|>
        do  try (pluralize "cockatrice") <|> string "cockatrice"
            return Cockatrice
        <|>
        do  try (pluralize "construct") <|> string "construct"
            return Construct
        <|>
        do  try (pluralize "coward") <|> string "coward"
            return Coward
        <|>
        do  try (pluralize "crab") <|> string "crab"
            return Crab
        <|>
        do  try (pluralize "crocodile") <|> string "crocodile"
            return Crocodile
        <|>
        do  try (pluralize "cyclops") <|> string "cyclops"
            return Cyclops
        <|>
        do  try (pluralize "dauthi") <|> string "dauthi"
            return Dauthi
        <|>
        do  try (pluralize "demon") <|> string "demon"
            return Demon
        <|>
        do  try (pluralize "deserter") <|> string "deserter"
            return Deserter
        <|>
        do  try (pluralize "devil") <|> string "devil"
            return Devil
        <|>
        do  try (pluralize "djinn") <|> string "djinn"
            return Djinn
        <|>
        do  try (pluralize "dragon") <|> string "dragon"
            return Dragon
        <|>
        do  try (pluralize "drake") <|> string "drake"
            return Drake
        <|>
        do  try (pluralize "dreadnought") <|> string "dreadnought"
            return Dreadnought
        <|>
        do  try (pluralize "drone") <|> string "drone"
            return Drone
        <|>
        do  try (pluralize "druid") <|> string "druid"
            return Druid
        <|>
        do  try (pluralize "dryad") <|> string "dryad"
            return Dryad
        <|>
        do  try (pluralize "dwarf") <|> string "dwarf"
            return Dwarf
        <|>
        do  try (pluralize "efreet") <|> string "efreet"
            return Efreet
        <|>
        do  try (pluralize "elder") <|> string "elder"
            return Elder
        <|>
        do  try (pluralize "eldrazi") <|> string "eldrazi"
            return Eldrazi
        <|>
        do  try (pluralize "elemental") <|> string "elemental"
            return Elemental
        <|>
        do  try (pluralize "elephant") <|> string "elephant"
            return Elephant
        <|>
        do  try (pluralize "elf") <|> string "elf"
            return Elf
        <|>
        do  try (pluralize "elk") <|> string "elk"
            return Elk
        <|>
        do  try (pluralize "eye") <|> string "eye"
            return Eye
        <|>
        do  try (pluralize "faerie") <|> string "faerie"
            return Faerie
        <|>
        do  try (pluralize "ferret") <|> string "ferret"
            return Ferret
        <|>
        do  try (pluralize "fish") <|> string "fish"
            return Fish
        <|>
        do  try (pluralize "flagbearer") <|> string "flagbearer"
            return Flagbearer
        <|>
        do  try (pluralize "fox") <|> string "fox"
            return Fox
        <|>
        do  try (pluralize "frog") <|> string "frog"
            return Frog
        <|>
        do  try (pluralize "fungus") <|> string "fungus"
            return Fungus
        <|>
        do  try (pluralize "gargoyle") <|> string "gargoyle"
            return Gargoyle
        <|>
        do  try (pluralize "germ") <|> string "germ"
            return Germ
        <|>
        do  try (pluralize "giant") <|> string "giant"
            return Giant
        <|>
        do  try (pluralize "gnome") <|> string "gnome"
            return Gnome
        <|>
        do  try (pluralize "goat") <|> string "goat"
            return Goat
        <|>
        do  try (pluralize "goblin") <|> string "goblin"
            return Goblin
        <|>
        do  try (pluralize "golem") <|> string "golem"
            return Golem
        <|>
        do  try (pluralize "gorgon") <|> string "gorgon"
            return Gorgon
        <|>
        do  try (pluralize "graveborn") <|> string "graveborn"
            return Graveborn
        <|>
        do  try (pluralize "gremlin") <|> string "gremlin"
            return Gremlin
        <|>
        do  try (pluralize "griffin") <|> string "griffin"
            return Griffin
        <|>
        do  try (pluralize "hag") <|> string "hag"
            return Hag
        <|>
        do  try (pluralize "harpy") <|> string "harpy"
            return Harpy
        <|>
        do  try (pluralize "hellion") <|> string "hellion"
            return Hellion
        <|>
        do  try (pluralize "hippo") <|> string "hippo"
            return Hippo
        <|>
        do  try (pluralize "hippogriff") <|> string "hippogriff"
            return Hippogriff
        <|>
        do  try (pluralize "homarid") <|> string "homarid"
            return Homarid
        <|>
        do  try (pluralize "homunculus") <|> string "homunculus"
            return Homunculus
        <|>
        do  try (pluralize "horror") <|> string "horror"
            return Horror
        <|>
        do  try (pluralize "horse") <|> string "horse"
            return Horse
        <|>
        do  try (pluralize "hound") <|> string "hound"
            return Hound
        <|>
        do  try (pluralize "human") <|> string "human"
            return Human
        <|>
        do  try (pluralize "hydra") <|> string "hydra"
            return Hydra
        <|>
        do  try (pluralize "hyena") <|> string "hyena"
            return Hyena
        <|>
        do  try (pluralize "illusion") <|> string "illusion"
            return Illusion
        <|>
        do  try (pluralize "imp") <|> string "imp"
            return Imp
        <|>
        do  try (pluralize "incarnation") <|> string "incarnation"
            return Incarnation
        <|>
        do  try (pluralize "insect") <|> string "insect"
            return Insect
        <|>
        do  try (pluralize "jellyfish") <|> string "jellyfish"
            return Jellyfish
        <|>
        do  try (pluralize "juggernaut") <|> string "juggernaut"
            return Juggernaut
        <|>
        do  try (pluralize "kavu") <|> string "kavu"
            return Kavu
        <|>
        do  try (pluralize "kirin") <|> string "kirin"
            return Kirin
        <|>
        do  try (pluralize "kithkin") <|> string "kithkin"
            return Kithkin
        <|>
        do  try (pluralize "knight") <|> string "knight"
            return Knight
        <|>
        do  try (pluralize "kobold") <|> string "kobold"
            return Kobold
        <|>
        do  try (pluralize "kor") <|> string "kor"
            return Kor
        <|>
        do  try (pluralize "kraken") <|> string "kraken"
            return Kraken
        <|>
        do  try (pluralize "lammasu") <|> string "lammasu"
            return Lammasu
        <|>
        do  try (pluralize "leech") <|> string "leech"
            return Leech
        <|>
        do  try (pluralize "leviathan") <|> string "leviathan"
            return Leviathan
        <|>
        do  try (pluralize "lhurgoyf") <|> string "lhurgoyf"
            return Lhurgoyf
        <|>
        do  try (pluralize "licid") <|> string "licid"
            return Licid
        <|>
        do  try (pluralize "lizard") <|> string "lizard"
            return Lizard
        <|>
        do  try (pluralize "manticore") <|> string "manticore"
            return Manticore
        <|>
        do  try (pluralize "masticore") <|> string "masticore"
            return Masticore
        <|>
        do  try (pluralize "mercenary") <|> string "mercenary"
            return Mercenary
        <|>
        do  try (pluralize "merfolk") <|> string "merfolk"
            return Merfolk
        <|>
        do  try (pluralize "metathran") <|> string "metathran"
            return Metathran
        <|>
        do  try (pluralize "minion") <|> string "minion"
            return Minion
        <|>
        do  try (pluralize "minotaur") <|> string "minotaur"
            return Minotaur
        <|>
        do  try (pluralize "monger") <|> string "monger"
            return Monger
        <|>
        do  try (pluralize "mongoose") <|> string "mongoose"
            return Mongoose
        <|>
        do  try (pluralize "monk") <|> string "monk"
            return Monk
        <|>
        do  try (pluralize "moonfolk") <|> string "moonfolk"
            return Moonfolk
        <|>
        do  try (pluralize "mutant") <|> string "mutant"
            return Mutant
        <|>
        do  try (pluralize "myr") <|> string "myr"
            return Myr
        <|>
        do  try (pluralize "mystic") <|> string "mystic"
            return Mystic
        <|>
        do  try (pluralize "nautilus") <|> string "nautilus"
            return Nautilus
        <|>
        do  try (pluralize "nephilim") <|> string "nephilim"
            return Nephilim
        <|>
        do  try (pluralize "nightmare") <|> string "nightmare"
            return Nightmare
        <|>
        do  try (pluralize "nightstalker") <|> string "nightstalker"
            return Nightstalker
        <|>
        do  try (pluralize "ninja") <|> string "ninja"
            return Ninja
        <|>
        do  try (pluralize "noggle") <|> string "noggle"
            return Noggle
        <|>
        do  try (pluralize "nomad") <|> string "nomad"
            return Nomad
        <|>
        do  try (pluralize "octopus") <|> string "octopus"
            return Octopus
        <|>
        do  try (pluralize "ogre") <|> string "ogre"
            return Ogre
        <|>
        do  try (pluralize "ooze") <|> string "ooze"
            return Ooze
        <|>
        do  try (pluralize "orb") <|> string "orb"
            return Orb
        <|>
        do  try (pluralize "orc") <|> string "orc"
            return Orc
        <|>
        do  try (pluralize "orgg") <|> string "orgg"
            return Orgg
        <|>
        do  try (pluralize "ouphe") <|> string "ouphe"
            return Ouphe
        <|>
        do  try (pluralize "ox") <|> string "ox"
            return Ox
        <|>
        do  try (pluralize "oyster") <|> string "oyster"
            return Oyster
        <|>
        do  try (pluralize "pegasus") <|> string "pegasus"
            return Pegasus
        <|>
        do  try (pluralize "pentavite") <|> string "pentavite"
            return Pentavite
        <|>
        do  try (pluralize "pest") <|> string "pest"
            return Pest
        <|>
        do  try (pluralize "phelddagrif") <|> string "phelddagrif"
            return Phelddagrif
        <|>
        do  try (pluralize "phoenix") <|> string "phoenix"
            return Phoenix
        <|>
        do  try (pluralize "pincher") <|> string "pincher"
            return Pincher
        <|>
        do  try (pluralize "pirate") <|> string "pirate"
            return Pirate
        <|>
        do  try (pluralize "plant") <|> string "plant"
            return Plant
        <|>
        do  try (pluralize "praetor") <|> string "praetor"
            return Praetor
        <|>
        do  try (pluralize "prism") <|> string "prism"
            return Prism
        <|>
        do  try (pluralize "rabbit") <|> string "rabbit"
            return Rabbit
        <|>
        do  try (pluralize "rat") <|> string "rat"
            return Rat
        <|>
        do  try (pluralize "rebel") <|> string "rebel"
            return Rebel
        <|>
        do  try (pluralize "reflection") <|> string "reflection"
            return Reflection
        <|>
        do  try (pluralize "rhino") <|> string "rhino"
            return Rhino
        <|>
        do  try (pluralize "rigger") <|> string "rigger"
            return Rigger
        <|>
        do  try (pluralize "rogue") <|> string "rogue"
            return Rogue
        <|>
        do  try (pluralize "salamander") <|> string "salamander"
            return Salamander
        <|>
        do  try (pluralize "samurai") <|> string "samurai"
            return Samurai
        <|>
        do  try (pluralize "sand") <|> string "sand"
            return Sand
        <|>
        do  try (pluralize "saproling") <|> string "saproling"
            return Saproling
        <|>
        do  try (pluralize "satyr") <|> string "satyr"
            return Satyr
        <|>
        do  try (pluralize "scarecrow") <|> string "scarecrow"
            return Scarecrow
        <|>
        do  try (pluralize "scorpion") <|> string "scorpion"
            return Scorpion
        <|>
        do  try (pluralize "scout") <|> string "scout"
            return Scout
        <|>
        do  try (pluralize "serf") <|> string "serf"
            return Serf
        <|>
        do  try (pluralize "serpent") <|> string "serpent"
            return Serpent
        <|>
        do  try (pluralize "shade") <|> string "shade"
            return Shade
        <|>
        do  try (pluralize "shaman") <|> string "shaman"
            return Shaman
        <|>
        do  try (pluralize "shapeshifter") <|> string "shapeshifter"
            return Shapeshifter
        <|>
        do  try (pluralize "sheep") <|> string "sheep"
            return Sheep
        <|>
        do  try (pluralize "siren") <|> string "siren"
            return Siren
        <|>
        do  try (pluralize "skeleton") <|> string "skeleton"
            return Skeleton
        <|>
        do  try (pluralize "slith") <|> string "slith"
            return Slith
        <|>
        do  try (pluralize "sliver") <|> string "sliver"
            return Sliver
        <|>
        do  try (pluralize "slug") <|> string "slug"
            return Slug
        <|>
        do  try (pluralize "snake") <|> string "snake"
            return Snake
        <|>
        do  try (pluralize "soldier") <|> string "soldier"
            return Soldier
        <|>
        do  try (pluralize "soltari") <|> string "soltari"
            return Soltari
        <|>
        do  try (pluralize "spawn") <|> string "spawn"
            return Spawn
        <|>
        do  try (pluralize "specter") <|> string "specter"
            return Specter
        <|>
        do  try (pluralize "spellshaper") <|> string "spellshaper"
            return Spellshaper
        <|>
        do  try (pluralize "sphinx") <|> string "sphinx"
            return Sphinx
        <|>
        do  try (pluralize "spider") <|> string "spider"
            return Spider
        <|>
        do  try (pluralize "spike") <|> string "spike"
            return Spike
        <|>
        do  try (pluralize "spirit") <|> string "spirit"
            return Spirit
        <|>
        do  try (pluralize "splinter") <|> string "splinter"
            return Splinter
        <|>
        do  try (pluralize "sponge") <|> string "sponge"
            return Sponge
        <|>
        do  try (pluralize "squid") <|> string "squid"
            return Squid
        <|>
        do  try (pluralize "squirrel") <|> string "squirrel"
            return Squirrel
        <|>
        do  try (pluralize "starfish") <|> string "starfish"
            return Starfish
        <|>
        do  try (pluralize "surrakar") <|> string "surrakar"
            return Surrakar
        <|>
        do  try (pluralize "survivor") <|> string "survivor"
            return Survivor
        <|>
        do  try (pluralize "tetravite") <|> string "tetravite"
            return Tetravite
        <|>
        do  try (pluralize "thalakos") <|> string "thalakos"
            return Thalakos
        <|>
        do  try (pluralize "thopter") <|> string "thopter"
            return Thopter
        <|>
        do  try (pluralize "thrull") <|> string "thrull"
            return Thrull
        <|>
        do  try (pluralize "treefolk") <|> string "treefolk"
            return Treefolk
        <|>
        do  try (pluralize "triskelavite") <|> string "triskelavite"
            return Triskelavite
        <|>
        do  try (pluralize "troll") <|> string "troll"
            return Troll
        <|>
        do  try (pluralize "turtle") <|> string "turtle"
            return Turtle
        <|>
        do  try (pluralize "unicorn") <|> string "unicorn"
            return Unicorn
        <|>
        do  try (pluralize "vampire") <|> string "vampire"
            return Vampire
        <|>
        do  try (pluralize "vedalken") <|> string "vedalken"
            return Vedalken
        <|>
        do  try (pluralize "viashino") <|> string "viashino"
            return Viashino
        <|>
        do  try (pluralize "volver") <|> string "volver"
            return Volver
        <|>
        do  try (pluralize "wall") <|> string "wall"
            return Wall
        <|>
        do  try (pluralize "warrior") <|> string "warrior"
            return Warrior
        <|>
        do  try (pluralize "weird") <|> string "weird"
            return Weird
        <|>
        do  try (pluralize "werewolf") <|> string "werewolf"
            return Werewolf
        <|>
        do  try (pluralize "whale") <|> string "whale"
            return Whale
        <|>
        do  try (pluralize "wizard") <|> string "wizard"
            return Wizard
        <|>
        do  try (pluralize "wolf") <|> string "wolf"
            return Wolf
        <|>
        do  try (pluralize "wolverine") <|> string "wolverine"
            return Wolverine
        <|>
        do  try (pluralize "wombat") <|> string "wombat"
            return Wombat
        <|>
        do  try (pluralize "worm") <|> string "worm"
            return Worm
        <|>
        do  try (pluralize "wraith") <|> string "wraith"
            return Wraith
        <|>
        do  try (pluralize "wurm") <|> string "wurm"
            return Wurm
        <|>
        do  try (pluralize "yeti") <|> string "yeti"
            return Yeti
        <|>
        do  try (pluralize "zombie") <|> string "zombie"
            return Zombie
        <|>
        do  try (pluralize "zubera") <|> string "zubera"
            return Zubera

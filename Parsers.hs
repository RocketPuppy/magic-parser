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
        do  try (char 'b')
            try (
                do  try (char 'a')
                    try (
                        do  try (pluralize "dger") <|> string "dger"
                            return Badger
                        <|>
                        do  try (pluralize "rbarian") <|> string "rbarian"
                            return Barbarian
                        <|>
                        do  try (pluralize "silisk") <|> string "silisk"
                            return Basilisk
                        <|>
                        do  try (pluralize "t") <|> string "t"
                            return Bat
                        )
                <|>
                do  try (char 'e')
                    try (
                        do  try (char 'a')
                            try (
                                do  try (pluralize "r") <|> string "r"
                                    return Bear
                                <|>
                                do  try (pluralize "st") <|> string "st"
                                    return Beast
                                )
                        <|>
                        do  try (pluralize "eble") <|> string "eble"
                            return Beeble
                        <|>
                        do  try (pluralize "rserker") <|> string "rserker"
                            return Berserker
                        )
                <|>
                do  try (pluralize "ird") <|> string "ird"
                    return Bird
                <|>
                do  try (pluralize "linkmoth") <|> string "linkmoth"
                    return Blinkmoth
                <|>
                do  try (pluralize "oar") <|> string "oar"
                    return Boar
                <|>
                do  try (char 'r')
                    try (
                        do  try (pluralize "inger") <|> string "inger"
                            return Bringer
                        <|>
                        do  try (pluralize "ushwagg") <|> string "ushwagg"
                            return Brushwagg
                        )
                )
        <|>
        do  try (char 'c')
            try (
                do  try (char 'a')
                    try (
                        do  try (char 'm')
                            try (
                                do  try (pluralize "arid") <|> string "arid"
                                    return Camarid
                                <|>
                                do  try (pluralize "el") <|> string "el"
                                    return Camel
                                )
                        <|>
                        do  try (char 'r')
                            try (
                                do  try (pluralize "ibou") <|> string "ibou"
                                    return Caribou
                                <|>
                                do  try (pluralize "rier") <|> string "rier"
                                    return Carrier
                                )
                        <|>
                        do  try (pluralize "t") <|> string "t"
                            return Cat
                        )
                <|>
                do  try (char 'e')
                    try (
                        do  try (pluralize "ntaur") <|> string "ntaur"
                            return Centaur
                        <|>
                        do  try (pluralize "phalid") <|> string "phalid"
                            return Cephalid
                        )
                <|>
                do  try (pluralize "himera") <|> string "himera"
                    return Chimera
                <|>
                do  try (pluralize "itizen") <|> string "itizen"
                    return Citizen
                <|>
                do  try (pluralize "leric") <|> string "leric"
                    return Cleric
                <|>
                do  try (char 'o')
                    try (
                        do  try (pluralize "ckatrice") <|> string "ckatrice"
                            return Cockatrice
                        <|>
                        do  try (pluralize "nstruct") <|> string "nstruct"
                            return Construct
                        <|>
                        do  try (pluralize "ward") <|> string "ward"
                            return Coward
                        )
                <|>
                do  try (char 'r')
                    try (
                        do  try (pluralize "ab") <|> string "ab"
                            return Crab
                        <|>
                        do  try (pluralize "ocodile") <|> string "ocodile"
                            return Crocodile
                        )
                <|>
                do  try (pluralize "yclops") <|> string "yclops"
                    return Cyclops
                )
        <|>
        do  try (char 'd')
            try (
                do  try (pluralize "authi") <|> string "authi"
                    return Dauthi
                <|>
                do  try (char 'e')
                    try (
                        do  try (pluralize "mon") <|> string "mon"
                            return Demon
                        <|>
                        do  try (pluralize "serter") <|> string "serter"
                            return Deserter
                        <|>
                        do  try (pluralize "vil") <|> string "vil"
                            return Devil
                        )
                <|>
                do  try (pluralize "jinn") <|> string "jinn"
                    return Djinn
                <|>
                do  try (char 'r')
                    try (
                        do  try (char 'a')
                            try (
                                do  try (pluralize "gon") <|> string "gon"
                                    return Dragon
                                <|>
                                do  try (pluralize "ke") <|> string "ke"
                                    return Drake
                                )
                        <|>
                        do  try (pluralize "eadnought") <|> string "eadnought"
                            return Dreadnought
                        <|>
                        do  try (pluralize "one") <|> string "one"
                            return Drone
                        <|>
                        do  try (pluralize "uid") <|> string "uid"
                            return Druid
                        <|>
                        do  try (pluralize "yad") <|> string "yad"
                            return Dryad
                        )
                <|>
                do  try (pluralize "warf") <|> string "warf"
                    return Dwarf
                )
        <|>
        do  try (char 'e')
            try (
                do  try (pluralize "freet") <|> string "freet"
                    return Efreet
                <|>
                do  try (char 'l')
                    try (
                        do  try (char 'd')
                            try (
                                do  try (pluralize "er") <|> string "er"
                                    return Elder
                                <|>
                                do  try (pluralize "razi") <|> string "razi"
                                    return Eldrazi
                                )
                        <|>
                        do  try (char 'e')
                            try (
                                do  try (pluralize "mental") <|> string "mental"
                                    return Elemental
                                <|>
                                do  try (pluralize "phant") <|> string "phant"
                                    return Elephant
                                )
                        <|>
                        do  try (pluralize "f") <|> string "f"
                            return Elf
                        <|>
                        do  try (pluralize "k") <|> string "k"
                            return Elk
                        )
                <|>
                do  try (pluralize "ye") <|> string "ye"
                    return Eye
                )
        <|>
        do  try (char 'f')
            try (
                do  try (pluralize "aerie") <|> string "aerie"
                    return Faerie
                <|>
                do  try (pluralize "erret") <|> string "erret"
                    return Ferret
                <|>
                do  try (pluralize "ish") <|> string "ish"
                    return Fish
                <|>
                do  try (pluralize "lagbearer") <|> string "lagbearer"
                    return Flagbearer
                <|>
                do  try (pluralize "ox") <|> string "ox"
                    return Fox
                <|>
                do  try (pluralize "rog") <|> string "rog"
                    return Frog
                <|>
                do  try (pluralize "ungus") <|> string "ungus"
                    return Fungus
                )
        <|>
        do  try (char 'g')
            try (
                do  try (pluralize "argoyle") <|> string "argoyle"
                    return Gargoyle
                <|>
                do  try (pluralize "erm") <|> string "erm"
                    return Germ
                <|>
                do  try (pluralize "iant") <|> string "iant"
                    return Giant
                <|>
                do  try (pluralize "nome") <|> string "nome"
                    return Gnome
                <|>
                do  try (char 'o')
                    try (
                        do  try (pluralize "at") <|> string "at"
                            return Goat
                        <|>
                        do  try (pluralize "blin") <|> string "blin"
                            return Goblin
                        <|>
                        do  try (pluralize "lem") <|> string "lem"
                            return Golem
                        <|>
                        do  try (pluralize "rgon") <|> string "rgon"
                            return Gorgon
                        )
                <|>
                do  try (char 'r')
                    try (
                        do  try (pluralize "aveborn") <|> string "veborn"
                            return Graveborn
                        <|>
                        do  try (pluralize "emlin") <|> string "emlin"
                            return Gremlin
                        <|>
                        do  try (pluralize "iffin") <|> string "iffin"
                            return Griffin
                        )
                )
        <|>
        do  try (char 'h')
            try (
                do  try (char 'a')
                    try (
                        do  try (pluralize "g") <|> string "g"
                            return Hag
                        <|>
                        do  try (pluralize "rpy") <|> string "rpy"
                            return Harpy
                        )
                <|>
                do  try (pluralize "ellion") <|> string "ellion"
                    return Hellion
                <|>
                do  try (string "ippo")
                    try (
                        do  try (pluralize "griff") <|> string "griff"
                            return Hippogriff
                        <|>
                        do  return Hippo
                        )
                <|>
                do  try (char 'o')
                    try (
                        do  try (char 'm')
                            try (
                                do  try (pluralize "arid") <|> string "arid"
                                    return Homarid
                                <|>
                                do  try (pluralize "unculus") <|> string "unculus"
                                    return Homunculus
                                )
                        <|>
                        do  try (char 'r')
                            try (
                                do  try (pluralize "ror") <|> string "ror"
                                    return Horror
                                <|>
                                do  try (pluralize "se") <|> string "se"
                                    return Horse
                                )
                        <|>
                        do  try (pluralize "und") <|> string "und"
                            return Hound
                        )
                <|>
                do  try (pluralize "uman") <|> string "uman"
                    return Human
                <|>
                do  try (char 'y')
                    try (
                        do  try (pluralize "dra") <|> string "dra"
                            return Hydra
                        <|>
                        do  try (pluralize "ena") <|> string "ena"
                            return Hyena
                        )
                )
        <|>
        do  try (char 'i')
            try (
                do  try (pluralize "llusion") <|> string "llusion"
                    return Illusion
                <|>
                do  try (pluralize "mp") <|> string "mp"
                    return Imp
                <|>
                do  try (char 'n')
                    try (
                        do  try (pluralize "carnation") <|> string "carnation"
                            return Incarnation
                        <|>
                        do  try (pluralize "sect") <|> string "sect"
                            return Insect
                        )
                )
        <|>
        do  try (char 'j')
            try (
                do  try (pluralize "ellyfish") <|> string "ellyfish"
                    return Jellyfish
                <|>
                do  try (pluralize "uggernaut") <|> string "uggernaut"
                    return Juggernaut
                )
        <|>
        do  try (char 'k')
            try (
                do  try (pluralize "avu") <|> string "avu"
                    return Kavu
                <|>
                do  try (char 'i')
                    try (
                        do  try (pluralize "rin") <|> string "rin"
                            return Kirin
                        <|>
                        do  try (pluralize "thkin") <|> string "thkin"
                            return Kithkin
                        )
                <|>
                do  try (pluralize "night") <|> string "night"
                    return Knight
                <|>
                do  try (char 'o')
                    try (
                        do  try (pluralize "bold") <|> string "bold"
                            return Kobold
                        <|>
                        do  try (pluralize "r") <|> string "r"
                            return Kor
                        )
                <|>
                do  try (pluralize "raken") <|> string "raken"
                    return Kraken
                )
        <|>
        do  try (char 'l')
            try (
                do  try (pluralize "ammasu") <|> string "ammasu"
                    return Lammasu
                <|>
                do  try (char 'e')
                    try (
                        do  try (pluralize "ech") <|> string "ech"
                            return Leech
                        <|>
                        do  try (pluralize "viathan") <|> string "viathan"
                            return Leviathan
                        )
                <|>
                do  try (pluralize "hurgoyf") <|> string "hurgoyf"
                    return Lhurgoyf
                <|>
                do  try (char 'i')
                    try (
                        do  try (pluralize "cid") <|> string "cid"
                            return Licid
                        <|>
                        do  try (pluralize "zard") <|> string "zard"
                            return Lizard
                        )
                )
        <|>
        do  try (char 'm')
            try (
                do  try (char 'a')
                    try (
                        do  try (pluralize "nticore") <|> string "nticore"
                            return Manticore
                        <|>
                        do  try (pluralize "sticore") <|> string "sticore"
                            return Masticore
                        )
                <|>
                do  try (char 'e')
                    try (
                        do  try (char 'r')
                            try (
                                do  try (pluralize "cenary") <|> string "cenary"
                                    return Mercenary
                                <|>
                                do  try (pluralize "folk") <|> string "folk"
                                    return Merfolk
                                )
                        <|>
                        do  try (pluralize "tathran") <|> string "tathran"
                            return Metathran
                        )
                <|>
                do  try (string "in")
                    try (
                        do  try (pluralize "ion") <|> string "ion"
                            return Minion
                        <|>
                        do  try (pluralize "otaur") <|> string "otaur"
                            return Minotaur
                        )
                <|>
                do  try (char 'o')
                    try (
                        do  try (char 'n')
                            try (
                                do  try (char 'g')
                                    try (
                                        do  try (pluralize "er") <|> string "er"
                                            return Monger
                                        <|>
                                        do  try (pluralize "oose") <|> string "oose"
                                            return Mongoose
                                        )
                                <|>
                                do  try (pluralize "k") <|> string "k"
                                    return Monk
                                )
                        <|>
                        do  try (pluralize "onfolk") <|> string "onfolk"
                            return Moonfolk
                        )
                <|>
                do  try (pluralize "utant") <|> string "utant"
                    return Mutant
                <|>
                do  try (char 'y')
                    try (
                        do  try (pluralize "r") <|> string "r"
                            return Myr
                        <|>
                        do  try (pluralize "stic") <|> string "stic"
                            return Mystic
                        )
                )
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

-----------------------------------------------------------------------------
--
-- Module      :  Tests
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

module Tests (

) where

import Structures
import Parsers
import Test.QuickCheck
import Normalizer

prop_ArtifactType_ident :: ArtifactType -> Property
prop_ArtifactType_ident ct = case (parseArtifactType (pp ct)) of
                                Left _ -> property False
                                Right pt -> property (pt == ct)

prop_ArtifactType_ident_plural :: ArtifactType -> Property
prop_ArtifactType_ident_plural ct = case (parseArtifactType (pluralize_string (pp ct))) of
                                        Left _ -> property False
                                        Right pt -> property (pt == ct)

prop_EnchantmentType_ident :: EnchantmentType -> Property
prop_EnchantmentType_ident ct = case (parseEnchantmentType (pp ct)) of
                                    Left _ -> property False
                                    Right pt -> property (pt == ct)

prop_EnchantmentType_ident_plural :: EnchantmentType -> Property
prop_EnchantmentType_ident_plural ct = case (parseEnchantmentType (pluralize_string (pp ct))) of
                                            Left _ -> property False
                                            Right pt -> property (pt == ct)

prop_InstantType_ident :: InstantType -> Property
prop_InstantType_ident ct = case (parseInstantType (pp ct)) of
                                Left _ -> property False
                                Right pt -> property (pt == ct)

prop_InstantType_ident_plural :: InstantType -> Property
prop_InstantType_ident_plural ct = case (parseInstantType (pluralize_string (pp ct))) of
                                    Left _ -> property False
                                    Right pt -> property (pt == ct)

prop_LandType_ident :: LandType -> Property
prop_LandType_ident ct = case (parseLandType (pp ct)) of
                                Left _ -> property False
                                Right pt -> property (pt == ct)

prop_LandType_ident_plural :: LandType -> Property
prop_LandType_ident_plural ct = case (parseLandType (pluralize_string (pp ct))) of
                                    Left _ -> property False
                                    Right pt -> property (pt == ct)

prop_PlaneswalkerType_ident :: PlaneswalkerType -> Property
prop_PlaneswalkerType_ident ct = case (parsePlaneswalkerType (pp ct)) of
                                    Left _ -> property False
                                    Right pt -> property (pt == ct)

prop_PlaneswalkerType_ident_plural :: PlaneswalkerType -> Property
prop_PlaneswalkerType_ident_plural ct = case (parsePlaneswalkerType (pluralize_string (pp ct))) of
                                            Left _ -> property False
                                            Right pt -> property (pt == ct)

prop_CreatureType_ident :: CreatureType -> Property
prop_CreatureType_ident ct = case (parseCreatureType (pp ct)) of
                                Left _ -> property False
                                Right pt -> property (pt == ct)

prop_CreatureType_ident_plural :: CreatureType -> Property
prop_CreatureType_ident_plural ct = case (parseCreatureType (pluralize_string (pp ct))) of
                                        Left _ -> property False
                                        Right pt -> property (pt == ct)

testSubTypes =
    do  quickCheck prop_ArtifactType_ident
        quickCheck prop_EnchantmentType_ident
        quickCheck prop_InstantType_ident
        quickCheck prop_LandType_ident
        quickCheck prop_PlaneswalkerType_ident
        quickCheck prop_CreatureType_ident
        quickCheck prop_ArtifactType_ident_plural
        quickCheck prop_EnchantmentType_ident_plural
        quickCheck prop_InstantType_ident_plural
        quickCheck prop_LandType_ident_plural
        quickCheck prop_PlaneswalkerType_ident_plural
        quickCheck prop_CreatureType_ident_plural

prop_Typestring_ident :: Characteristic -> Property
prop_Typestring_ident ts = undefined --property (parse_Typestring (pp ts) == ts)

instance Arbitrary ArtifactType where
    arbitrary = elements [Contraption .. Fortification]
instance Arbitrary EnchantmentType where
    arbitrary = elements [Aura .. Shrine]
instance Arbitrary InstantType where
    arbitrary = elements [Arcane .. Trap]
instance Arbitrary LandType where
    arbitrary = elements [Desert .. Urzas]
instance Arbitrary PlaneswalkerType where
    arbitrary = elements [Ajani .. Venser]
instance Arbitrary CreatureType where
    arbitrary = elements [Advisor .. Zubera]

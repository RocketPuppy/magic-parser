module Normalizer where
{-
This module contains normalizing function to normalize irregularities in card text due to pluralization or tenses.
-}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
import qualified Text.Regex.PCRE.Light as R
import qualified Data.ByteString.Char8 as BS

normalize_destroy :: Parser String
normalize_destroy =
    do {
    try (string "destroyed") <|> try (string "destroys") <|> string "destroy" <?> "form of destroy";
    return "destroy"
}

normalize_dies :: Parser String
normalize_dies =
    do {
    try (string "died") <|> try (string "dies") <|> string "die" <?> "form of die";
    return "dies"
}

normalize_pay :: Parser String
normalize_pay =
    do {
    try (string "pays") <|> try (string "paid") <|> string "pay" <?> "form of pay";
    return "pay"
}

pluralize :: String -> Parser String
pluralize input
    | doMatch sibilants input' = if (doMatch e_eol input') then do { string (input ++ "s"); return input} else do { string (input ++ "es"); return input}
    | doMatch voiceless input'  = do {string (input ++ "s"); return input}
    | doMatch consonant_o input' = do {string (input ++ "es") <|> string (input ++ "s"); return input}
    | doMatch consonant_y input' = do {string ((de_y input) ++ "ies"); return input}
    | doMatch vowel_y input'   = do {string (input ++ "s"); return input}
    | otherwise = do {string (input ++ "s"); return input}
    where   sibilants = R.compile (BS.pack "s$|se$|sh$|ge$|tch$|dge$") []
            voiceless = R.compile (BS.pack "p$|t$|k$|f$|th$") []
            consonant = R.compile (BS.pack "[^aeiou]") []
            vowel     = R.compile (BS.pack "[aeiou]") []
            e_eol     = R.compile (BS.pack "e$") []
            consonant_o = R.compile (BS.pack "[^aeiou]o$") []
            consonant_y = R.compile (BS.pack "[^aeiou]y$") []
            vowel_y   = R.compile (BS.pack "[aeiou]y$") []
            de_y yed  = take ((length yed) - 1) yed
            input' = BS.pack input
            doMatch re inp = case (R.match re inp [R.exec_notempty]) of
                                Nothing -> False
                                Just m -> True

pluralize_string :: String -> String
pluralize_string input
    | doMatch sibilants input' = if (doMatch e_eol input') then input ++ "s" else input ++ "es"
    | doMatch voiceless input'  = input ++ "s"
    | doMatch consonant_o input' = input ++ "s" --This is wrong, sometimes it can be input + 'es'
    | doMatch consonant_y input' = (de_y input) ++ "ies"
    | doMatch vowel_y input'   = input ++ "s"
    | otherwise = input ++ "s"
    where   sibilants = R.compile (BS.pack "s$|se$|sh$|ge$|tch$|dge$") []
            voiceless = R.compile (BS.pack "p$|t$|k$|f$|th$") []
            consonant = R.compile (BS.pack "[^aeiou]") []
            vowel     = R.compile (BS.pack "[aeiou]") []
            e_eol     = R.compile (BS.pack "e$") []
            consonant_o = R.compile (BS.pack "[^aeiou]o$") []
            consonant_y = R.compile (BS.pack "[^aeiou]y$") []
            vowel_y   = R.compile (BS.pack "[aeiou]y$") []
            de_y yed  = take ((length yed) - 1) yed
            input' = BS.pack input
            doMatch re inp = case (R.match re inp [R.exec_notempty]) of
                                Nothing -> False
                                Just m -> True

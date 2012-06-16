module Lexer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Language as Lan
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Structures
{-
-}

-- Lexer --
langDef =   Lan.emptyDef
            { Tok.reservedNames = [
                                  -- reference quantifiers
                                  "you", "your", "this", "target", "$this$", "other",
                                  -- count quantifiers
                                  "only", "all", "a", "an", "no",
                                  -- object quantifiers
                                  "copy", "card", "token", "spell", "permanent", "emblem", "ability", "life", "mana", "mana pool",
                                  -- status quantifiers
                                  "tapped", "untapped",
                                  -- mana symbols
                                  "{W}", "{U}", "{B}", "{R}", "{G}", "{X}", "{W/U}", "{W/B}",
                                  "{U/B}", "{U/R}", "{B/R}", "{B/G}", "{R/G}", "{R/W}", "{G/W}",
                                  "{G/U}", "{2/W}", "{2/U}", "{2/B}", "{2/R}", "{2/G}", "{W/P}",
                                  "{U/P}", "{B/P}", "{R/P}", "{G/P}", "{S}",
                                  -- turn, phase, step --
                                  "beginning", "main", "combat", "ending",
                                  "untap", "upkeep", "draw", "beginning of combat",
                                  "declare attackers", "declare blockers", "combat damage",
                                  "end of combat", "end", "cleanup", "this turn",
                                  "next turn", "your turn", "opponent's turn",
                                  -- timing --
                                  "next", "precombat", "postcombat"
                                  ]
            , Tok.reservedOpNames = [
                                    -- punctuation ops
                                    ":", ".", ",",
                                    -- effects
                                    "pay", "destroy", "dies",
                                    -- Trigger ops --
                                    "Whenever", "When", "At", "the beginning of", "becomes",
                                    "enters the battlefield", "leaves the battlefield", "is put into", "from",
                                    -- Boolean ops --
                                    "if", "or", "and"
                                    ]
            , Tok.caseSensitive = False
            , Tok.commentStart = "("
            , Tok.commentEnd = ")"
            , Tok.identStart = char '{'
            , Tok.identLetter = alphaNum <|> char '}'
            --, Tok.opStart = oneOf ".:,"
            , Tok.opLetter = alphaNum
            }

lexer = Tok.makeTokenParser langDef

-- Parsers --
-- ripped from Parsec
--brackets = Tok.brackets lexer
--comma = Tok.comma lexer
--colon = Tok.colon lexer
whitespace = Tok.whiteSpace lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
natural = Tok.natural lexer
identifier = Tok.identifier lexer

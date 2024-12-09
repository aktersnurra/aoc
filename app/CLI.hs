{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLI  where

import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts {optCommand :: !Command}

type Year = String
type Day = String
type Part = String
type Answer = String

data Command
    = Input Year Day
    | Submit Year Day Part Answer

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> versionOption <*> programOptions)
        ( fullDesc
            <> progDesc "Advent of Code API CLI"
            <> header
                "aoc - get puzzle input and submit answers via the command line"
        )

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1.0.0" (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions =
    Opts
        <$> hsubparser (inputCommand <> submitCommand)

inputCommand :: Mod CommandFields Command
inputCommand =
    command
        "input"
        (info inputOptions (progDesc "Get puzzle input for year and day"))

inputOptions :: Parser Command
inputOptions =
    Input
        <$> strArgument (metavar "YEAR" <> help "The year")
        <*> strArgument (metavar "DAY" <> help "The day")

submitCommand :: Mod CommandFields Command
submitCommand =
    command
        "submit"
        (info submitOptions (progDesc "Submit solution to a puzzle part"))

submitOptions :: Parser Command
submitOptions =
    Submit
        <$> strArgument (metavar "YEAR" <> help "The year")
        <*> strArgument (metavar "DAY" <> help "The day")
        <*> strArgument (metavar "PART" <> help "Puzzle part")
        <*> strArgument (metavar "ANSWER" <> help "The answer")

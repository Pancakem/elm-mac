module Reader exposing (parseInstructions)

import Parser exposing ((|.), (|=), Parser, Trailing(..), int, run, spaces, succeed)
import Parser.Extras as ParserExtra


getInstructions : String -> List String
getInstructions str =
    String.split "\n" str


parseInstructions : String -> List Int
parseInstructions str =
    run instrParser str
        |> Result.toMaybe
        |> Maybe.withDefault []


instrParser : Parser (List Int)
instrParser =
    ParserExtra.many intParser


intParser : Parser Int
intParser =
    succeed identity
        |= int
        |. spaces

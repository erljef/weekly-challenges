module LSystem exposing (..)

import List exposing (..)
import String exposing (..)


type alias Token =
    Char


type alias Rule =
    { token : Token
    , replacement : List Token
    }


type alias Iterations =
    { current : Int
    , target : Int
    }


generate : List Token -> List Rule -> Iterations -> List Token
generate tokens rules iterations =
    if iterations.current == iterations.target then
        tokens
    else
        generate (replace tokens rules) rules { iterations | current = iterations.current + 1 }


replace : List Token -> List Rule -> List Token
replace tokens rules =
    List.concat (List.map (\t -> replaceToken t rules) tokens)


replaceToken : Token -> List Rule -> List Token
replaceToken token rules =
    let
        list =
            List.filter (\rule -> rule.token == token) rules
                |> List.map (\rule -> rule.replacement)
                |> List.concat
    in
        if List.isEmpty list then
            token :: []
        else
            list


tokenList : String -> List Token
tokenList s =
    toList s


fromString : String -> Maybe Rule
fromString s =
    let
        parts =
            String.split "->" s

        token =
            toChar (head parts)

        r =
            tokenList (String.concat (Maybe.withDefault [] (tail parts)))
    in
        case token of
            Just t ->
                Just
                    { token = t
                    , replacement = r
                    }

            Nothing ->
                Nothing


toChar : Maybe String -> Maybe Char
toChar s =
    Maybe.map (\a -> head (String.toList a)) s |> Maybe.withDefault Nothing


ruleString : Rule -> String
ruleString rule =
    fromChar rule.token ++ "->" ++ String.concat (List.map fromChar rule.replacement)

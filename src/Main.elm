module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Bytes
import Bytes.Decode as BD
import CPU exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Instruction exposing (..)
import Json.Decode as D
import Memory exposing (..)
import Reader
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { frame : Frame
    , program : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { frame = initFrame
       , program = """1 5 1 1
                    3 11 1 9
                    1 0
                    """
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = LoadProgram
    | Run


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadProgram ->
            let
                list =
                    Reader.parseInstructions model.program           
            in
            ( { model | frame = loadInstructions (instructionsFromList list) model.frame }, Cmd.none )

        Run ->
            let
                cycle fr =
                    if fr.instructionPointer < fr.instructionCount && fr.running then
                        let
                            isJumpUpdate cpuState =
                                if cpuState.isJump then
                                    cpuState

                                else
                                    { cpuState | instructionPointer = cpuState.instructionPointer + 1 }
                        in
                        cycle <|
                            isJumpUpdate <|
                                eval fr

                    else
                        fr

                newFrame =
                    cycle (readyToRun model.frame)
            in
            ( { model | frame = newFrame }, Cmd.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        asm =
            String.split "," <|
                instructionToAssembly 0 model.frame.instructions ""

        breakFunc str =
            div []
                [ text str
                , br [] []
                ]
    in
    div []
        [ div
            []
            [ text "A virtual machine: "
            ]
         , div
             []
                 [ button [onClick LoadProgram ] [ text "Load Program" ]
                 ]
        , div
            []
            [ text "Assembly code: "
            , div []
                (List.map breakFunc asm)
            ]
        , div []
            [ button [ onClick Run ] [ text "Run Program" ] ]
        , div []
            [ if model.frame.running then
                div []
                    [ p [] [ text "Program is running" ]
                    , p [] [ text <| Debug.toString model.frame ]
                    ]

              else
                text <| Debug.toString model.frame
            ]
        ]

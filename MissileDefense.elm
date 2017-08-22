{-
    Missile Defense
    By Euirim Choi

    Inspired by Atari's Missile Command.
-}

port module MissileDefense exposing (main)

-- external libraries
import Html exposing (Html)
import Html.Attributes as Attr
import Element as El
import Color exposing (black)
import Keyboard
import Mouse
import Time exposing (every, millisecond, second)
import Random exposing (initialSeed)

-- local libraries
import Models exposing 
    (Model, Position, Silo, City, Missile, genInitialModel, newM)
import Variables exposing (..)
import Graphics exposing (..)
import Utilities exposing 
    (moveMs, jsPosConvert, handleTick, growBlasts, 
    findNearestSilo, nearestASilo, replaceSilo, incomingM,
    handleBlasts, blastCollateral, gameIsOver, roundIsOver, 
    endRound, handleSplits)


-- MODEL

--initialModel : Model
initialModel = 
    genInitialModel 
    innerW -- need inner margin
    (baseY + (toFloat groundH)) maxSiloMs -- consider ground position


-- UPDATE

type Msg = 
    Noop | 
    Tick | 
    AbsClick Mouse.Position | 
    RelClick (Maybe Position) |
    RandomNumber Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Noop -> (model, Cmd.none)
        Tick -> 
            if model.seed == Nothing then
                (model, Cmd.none)
            else if model.instructions then
                (model, Cmd.none)
            else if gameIsOver model then
                ({model | gOver = True}, Cmd.none) 
            else if roundIsOver model then
                ({model | rOver = True}, Cmd.none) 
            else
                let
                    -- add offensive missiles
                    mod = incomingM model
                in
                    case handleTick mod.mSpeed 8 mod of
                        (ms, bs) -> 
                            ({
                                mod | 
                                missiles = ms, 
                                blasts = bs ++ (growBlasts 0.4 bRad mod.blasts)
                            } 
                            |> handleBlasts 
                            |> blastCollateral 
                            |> handleSplits, 
                            Cmd.none)
        AbsClick p -> (model, reqRelClickPos p)
        RelClick mp -> 
            -- do nothing if person clicked outside of screen
            case mp of 
                Nothing -> (model, Cmd.none)
                Just rawP -> 
                    let
                        p = jsPosConvert rawP
                        silo = nearestASilo model.silos (jsPosConvert rawP)
                    in
                    if model.instructions then
                        ({model | instructions = False}, Cmd.none)
                    else if model.gOver then
                        init
                    else if model.rOver then
                        (endRound model, Cmd.none)
                    else
                        case silo of
                            Nothing -> (model, Cmd.none)
                            Just s ->
                                (
                                    {model | 
                                    missiles=
                                        (newM s.pos s.pos p)::model.missiles,
                                    silos = 
                                        replaceSilo 
                                        model.silos
                                        {s | numMissiles = s.numMissiles - 1} 
                                    },
                                    Cmd.none
                                )
        RandomNumber i -> ({model | seed = Just (initialSeed i)}, Cmd.none)

-- VIEW

-- centering: goo.gl/1VsI0U
view : Model -> Html Msg
view model = 
    let
        style = 
            Attr.style <|
                [ ("position", "fixed")
                , ("top", "50%")
                , ("left", "50%")
                , ("transform", "translate(-50%, -50%)")
                ]
    in
        Html.div [style, Attr.id "mdScreen"] [El.toHtml (renderWindow model)]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [
            every (millisecond * 15) (\_ -> Tick),
            Mouse.clicks (\pos -> AbsClick pos),
            -- Mouse.clicks (\pos -> Click {x = toFloat pos.x, y = toFloat pos.y})
            recRelClickPos RelClick
        ]

-- MAIN

init : (Model, Cmd Msg)
init = (initialModel, Random.generate RandomNumber (Random.int 1 1000))

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- PORTS

port reqRelClickPos : Mouse.Position -> Cmd msg
port recRelClickPos : (Maybe Position -> msg) -> Sub msg

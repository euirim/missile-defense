module Models exposing (..)

import Random exposing (Seed, initialSeed)
import Variables exposing (baseMSpeed)

type alias Position = {x : Float, y : Float}
type alias Silo = {pos : Position, numMissiles : Int, alive : Bool}
type alias City = {pos : Position, alive : Bool}
type alias Missile = {
    src : Position, 
    pos : Position, 
    dest : Position, 
    alive : Bool
}
type alias Blast = {pos : Position, rad : Float, alive : Bool}

type alias Model = {
    silos : List Silo, 
    cities : List City, 
    missiles : List Missile,
    blasts : List Blast,
    remMissiles : Int, 
    mSpeed : Float,
    round : Int,
    score : Int,
    seed : Maybe Seed,
    gOver : Bool,
    rOver : Bool,
    instructions : Bool
}

-- Model utilities

newPos : Float -> Float -> Position
newPos x y =
    {x = x, y = y}

newSilo : Position -> Int -> Bool -> Silo
newSilo pos nm alive = 
    {pos = pos, numMissiles = nm, alive = alive}

newCity : Position -> Bool -> City
newCity pos alive = 
    {pos = pos, alive = alive}

newM : Position -> Position -> Position -> Missile
newM src pos dest = 
    {src = src, pos = pos, dest = dest, alive = True}

newB : Position -> Blast
newB p =
    {pos = p, alive = True, rad = 0}

-- Initial model

genSilos : Float -> Float -> Int -> List Silo
genSilos w y nm = 
    let 
        base = -1 * (w / 2)
        partW = w / 8
    in
        List.foldl 
        (\x acc -> (newSilo (newPos (base + (partW * x)) y) nm True)::acc) 
        []
        [0,4,8]

genCities : Float -> Float -> List City
genCities w y =
    let
        base = -1 * (w / 2)
        partW = w / 8
    in
        List.foldl 
        (\x acc -> (newCity (newPos (base + (partW * x)) y) True)::acc) 
        [] 
        [1,2,3,5,6,7]

-- Given width of space to take, the y position of the
-- cities and silos, and the number of missiles that each 
-- silo should have, generate an initial model.
genInitialModel : Float -> Float -> Int -> Model
genInitialModel w y nm =
    {
        silos = genSilos w y nm,
        cities = genCities w y,
        missiles = [],
        blasts = [],
        remMissiles = 10,
        mSpeed = baseMSpeed,
        round = 1,
        score = 0,
        seed = Nothing,
        -- seed = initialSeed 1776,
        gOver = False,
        rOver = False,
        instructions = True
    }

module Utilities exposing (..)

import Mouse
import Random

import Models exposing 
    (Model, Missile, Position, Silo, Blast, 
    City, newB, newM, newSilo, newCity, genSilos)
import Variables exposing (..)


-- GENERAL

-- distance between two positions
distanceBTW : Position -> Position -> Float
distanceBTW p1 p2 =
    sqrt(((p2.y - p1.y)^2) + ((p2.x - p1.x)^2))

-- convert JS given relative coordinates to coordinates compatible
-- with Collage given winHeight and winWidth constants of game screen. 
jsPosConvert : Position -> Position
jsPosConvert {x,y} =
    {x = x - ((toFloat winW) / 2), y = y - ((toFloat winH) / 2)}

-- replace object in list (no type signature to allow for replacing both
-- cities and silos)
replaceLoc ls o =
    List.foldl (\x acc -> if o.pos == x.pos then o::acc else x::acc) [] ls

-- kill object in list (no type signature to allow for killing
-- both cities and silos) if object is within the given blast's radius
killLoc ls b =
    case ls of
        [] -> []
        x::rest -> 
            if (distanceBTW b.pos x.pos) < b.rad then
                {x | alive = False}::rest
            else
                x::(killLoc rest b)

-- MISSILES

-- isDead : Missile -> Bool
isDead m = 
    m.dest == m.pos

-- check if given missile is a defensive missile
isDefM : Missile -> Bool
isDefM {src,pos,dest} =
    src.y < pos.y

moveM : Float -> Missile -> Missile
moveM d m = 
    let
        vec = {x = m.dest.x - m.pos.x, y = m.dest.y - m.pos.y}
        len = distanceBTW m.pos m.dest
    in
        -- prevent missile from flying past dest
        if (len > d) then
            let
                -- weighted (based on distance) unit vector
                wUnitV = {x = (vec.x / len) * d, y = (vec.y / len) * d}
            in
                {m | pos = {x = m.pos.x + wUnitV.x, y = m.pos.y + wUnitV.y}} 
        else
            {m | pos = m.dest}

moveMs : Float -> Float -> List Missile -> List Missile
moveMs aD dD ms = 
    let
        func m acc =
            if m.pos == m.dest then 
                m::acc
            else
                case (isDefM m) of 
                    True -> (moveM dD m)::acc
                    _ -> (moveM aD m)::acc
    in
        List.foldl func [] ms

handleTick : Float -> Float -> Model -> (List Missile, List Blast)
handleTick aD dD mod =
    let 
        func m (l1,l2) =
            if m.pos == m.dest then
                if m.alive then
                    ({m | alive=False}::l1, (newB m.pos)::l2)
                else
                    (m::l1, l2)
            else
                case (isDefM m) of
                    True -> ((moveM dD m)::l1, l2)
                    _ -> ((moveM aD m)::l1, l2)
    in
        List.foldl func ([],[]) mod.missiles

-- recursively add missiles to model state through random generation of numbers
incomingM : Model -> Model
incomingM m =
    if m.remMissiles == 0 then m
    else
        case m.seed of
            Nothing -> Debug.crash "incomingM: Impossible"
            Just seed ->
                let
                    (rVal, aSeed) = Random.step (Random.int 0 maxRand) seed
                    (srcX, bSeed) = Random.step (Random.float baseX maxX) seed
                    -- random value to determine dest
                    (locVal, cSeed) = Random.step (Random.int 0 8) seed
                    wSpace = innerW / 8 -- space between each city and silo
                    base = -1 * (innerW / 2)
                in
                    if rVal > (100 - m.remMissiles) then {m | seed = Just bSeed}
                    else 
                        let
                            missile = (newM 
                                {x=srcX, y=maxY} 
                                {x=srcX, y=maxY} 
                                {x=base + (wSpace * (toFloat locVal)), 
                                y=(toFloat groundH) + baseY}
                            )
                        in
                            incomingM {
                                m | 
                                remMissiles = m.remMissiles - 1, 
                                missiles = missile::m.missiles,
                                seed = Just bSeed
                            }

-- remove missile in given list that matches given missile
removeM : Missile -> List Missile -> List Missile
removeM m ms =
    case ms of
        [] -> []
        x::rest -> if x == m then rest else x::(removeM m rest)

-- handle missile splitting (also cleans up dead missiles)
handleSplits : Model -> Model
handleSplits mod =
    let
        func m md = 
            if not m.alive then
                md
            else if ((md.remMissiles <= 0) || isDefM m) then
                {md | missiles = m::md.missiles}
            else
                if m.pos.y < 0 then
                    {md | missiles = m::md.missiles}
                else
                    case md.seed of 
                        Nothing -> Debug.crash "handleSplits: Impossible"
                        Just seed ->
                            let
                                (rVal, aSeed) = 
                                    Random.step 
                                    (Random.int 0 maxSplitRand) seed
                            in
                                let
                                    -- number of missiles generated
                                    num_gen = 
                                        (clamp 
                                        1
                                        5
                                        (rVal % (clamp 1 md.remMissiles 5))
                                        )
                                in
                                    if (rVal < (5 + (clamp 0 50 md.round))) then
                                        genMs 
                                        {
                                            md | 
                                            seed = Just seed,
                                            missiles = m::md.missiles
                                        } 
                                        num_gen m
                                    else
                                        {
                                            md | 
                                            missiles = m::md.missiles,
                                            seed = Just seed
                                        } 
    in
        List.foldl func {mod | missiles = []} mod.missiles

genMs : Model -> Int -> Missile -> Model
genMs mod n m =
    if n <= 0 then
        mod
    else
        case mod.seed of
            Nothing -> Debug.crash "genMs: Impossible"
            Just seed ->
                let
                    (locVal, cSeed) = Random.step (Random.int 0 8) seed
                    wSpace = innerW / 8 -- space between each city and silo
                    base = -1 * (innerW / 2)
                in
                    let
                        missile = (newM 
                            m.pos
                            m.pos
                            {x=base + (wSpace * (toFloat locVal)), 
                            y=(toFloat groundH) + baseY}
                        )
                    in
                        if (distanceBTW missile.dest m.dest) < 1 then 
                            genMs {mod | seed = Just cSeed} n m
                        else
                            genMs {
                                mod | 
                                missiles = missile::mod.missiles,
                                remMissiles = mod.remMissiles - 1,
                                seed = Just cSeed
                            }
                            (n - 1)
                            m
                    
-- BLASTS

growBlasts : Float -> Float -> List Blast -> List Blast
growBlasts rate maxR bs =
    let 
        func b acc =
            if b.alive then
                let
                    fR = b.rad + rate
                in
                    if fR >= maxR then
                        {b | rad = maxR, alive=False}::acc
                    else
                        {b | rad = fR}::acc
            else
                if b.rad == 0 then
                    b::acc
                else
                    let 
                        fR = b.rad - rate
                    in
                        if fR >= 0 then
                            {b | rad = fR}::acc
                        else
                            {b | rad = 0}::acc
    in
        List.foldl func [] bs
        

-- SILOS

findNearestSilo : List Silo -> Position -> Silo
findNearestSilo ls pos =
    let
        func s acc = 
            if (distanceBTW s.pos pos) < (distanceBTW acc.pos pos) then
                s
            else
                acc
    in
        case ls of 
            [] -> Debug.crash "findNearestSilo: Impossible"
            s::rest -> List.foldl func s rest

-- nearest available silo (with available missiles)
nearestASilo : List Silo -> Position -> Maybe Silo
nearestASilo ls pos =
    let
        func s acc = 
            if (distanceBTW s.pos pos) < (distanceBTW acc.pos pos) then
                s
            else
                acc
        create s acc =
            if s.alive && (s.numMissiles > 0) then
                s::acc
            else
                acc
    in
        case List.foldl create [] ls of 
            [] -> Nothing
            s::rest -> 
                Just (List.foldl func s rest)

-- replace given silo in list of silos with same position
replaceSilo : List Silo -> Silo -> List Silo
replaceSilo ls s =
    replaceLoc ls s
--    List.foldl (\x acc -> if s.pos == x.pos then s::acc else x::acc) [] ls


-- CITIES

replaceCity : List City -> City -> List City
replaceCity ls c =
    replaceLoc ls c


-- BLASTS

-- handles blasts and adds score
handleBlasts : Model -> Model
handleBlasts mod = 
    List.foldl handleBlast mod mod.blasts

handleBlast : Blast -> Model -> Model
handleBlast b mod =
    let
        handleBlast_ m acc = 
            -- don't do anything if missile is dead or defensive
            if (isDead m) || (isDefM m) then acc
            else if distanceBTW m.pos b.pos <= b.rad then
                {
                    acc | 
                    missiles = removeM m acc.missiles,
                    blasts = (newB m.pos)::acc.blasts,
                    score = acc.score + ppM
                }
            else
                acc
    in
        if b.alive then
            List.foldl handleBlast_ mod mod.missiles
        else
            mod

-- given model, determine if any alive silos or cities are 
-- destroyed by blasts.
blastCollateral : Model -> Model
blastCollateral mod = 
    let
        fSilo b acc = if b.alive then killLoc mod.silos b else acc
        fCity b acc = if b.alive then killLoc mod.cities b else acc
    in
        {
            mod | 
            silos = List.foldl fSilo mod.silos mod.blasts,
            cities = List.foldl fCity mod.cities mod.blasts
        }


-- STATE UTILS

-- returns True if the game is over based on given model
gameIsOver : Model -> Bool
gameIsOver mod =
    let
        func x acc = x.alive || acc
    in
        List.foldl func False mod.cities |> not

-- returns True if the round is over based on given model
roundIsOver : Model -> Bool
roundIsOver mod =
    let
        func o acc = if o.alive then o.alive || acc else acc  
    in
    if mod.remMissiles == 0 then
        (not (List.foldl func False mod.missiles)) 
        &&
        (not (List.foldl func False mod.blasts))
    else
        False

-- updates given model to account for new round
endRound : Model -> Model
endRound mod =
    {
        mod |
        missiles = [],
        remMissiles = clamp 10 20 (10 + (mod.round // 2)),
        mSpeed = baseMSpeed + (clamp 0.1 1 ((toFloat mod.round) * 0.1)),
        silos = genSilos innerW (baseY + (toFloat groundH)) maxSiloMs,
        blasts = [],
        round = mod.round + 1,
        rOver = False
    }

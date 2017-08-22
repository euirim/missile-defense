module Graphics exposing (..)

import Element as El exposing (Element, bottomLeft)
import Collage as Clg exposing (Form)
import Text
import Color exposing (Color, black, yellow, blue, red, green, white)

import Models exposing (Model, City, Silo, Missile, Blast)
import Utilities exposing (isDefM, isDead, gameIsOver, roundIsOver)
import Variables exposing (..)

-- render screen of given width, height, and color
renderScreen : Int -> Int -> Color -> Element
renderScreen w h c = 
    El.color c (El.size w h El.empty)

-- render background of game
renderBG : Int -> Int -> Element
renderBG w h =
    El.image w h "assets/bg.png"

-- render ground of given width, height, and color
renderGround : Int -> Int -> Color -> Element
renderGround w h c =
    El.color c (El.size w h El.empty)

renderCities : Int -> Int -> Color -> Color -> List City -> Element
renderCities w h aCol dCol cities = 
    let
        cAForm = Clg.toForm <| El.image cSize cSize "assets/city.png"
        cDForm = Clg.toForm <| El.image cSize cSize "assets/citydead.png"
        cToForm {pos,alive} acc =
                if alive then
                    (Clg.move (pos.x, pos.y) cAForm)::acc
                else
                    (Clg.move (pos.x, pos.y) cDForm)::acc
    in
        Clg.collage w h 
        (List.foldl 
            cToForm
            [] cities)

renderSilos : Int -> Int -> Color -> Color -> List Silo -> Element
renderSilos w h aCol dCol silos =
    let
        sAForm = Clg.toForm <| El.image sSize sSize "assets/silo.png"
        sDForm = Clg.toForm <| El.image sSize sSize "assets/silodead.png"
        sToForm {pos, numMissiles, alive} acc =
            let
                score = Clg.move 
                    (pos.x, (pos.y - 20)) 
                    (
                        (Text.fromString (toString numMissiles))
                        |> Text.color white |> Clg.text
                    )

            in
                if alive then
                    (Clg.move (pos.x, pos.y) sAForm)::score::acc
                else
                    (Clg.move (pos.x, pos.y) sDForm)::acc
    in
        Clg.collage w h 
        (List.foldl 
            sToForm
            [] silos)

renderScore : Int -> Int -> Color -> Int -> Element
renderScore w h c score =
    let 
        txtForm =   
            (Text.color c (Text.fromString ("Score: " ++ (toString score)))) 
            |> Text.height scoreH |> Clg.text
    in
        Clg.collage w h [Clg.move (0, scoreY) txtForm]

renderRoundScore : Int -> Int -> Color -> Int -> Int -> Element
renderRoundScore w h c round score =
    let 
        scoForm =   
            (Text.color c (Text.fromString ("SCORE: " ++ (toString score))))
            |> Text.height scoreH 
            |> Text.typeface ["Courier New", "Courier", "sans-serif"]
            |> Text.bold
            |> Clg.text
        rndForm =   
            (Text.color c (Text.fromString ("ROUND: " ++ (toString round))))
            |> Text.height scoreH 
            |> Text.typeface ["Courier New", "Courier", "sans-serif"]
            |> Text.bold
            |> Clg.text
    in
        [Clg.move (scoreY * 0.4, scoreY) scoForm, 
        Clg.move (scoreY * -0.4, scoreY) rndForm] |>
        Clg.collage w h 

formMissile : Color -> Color -> Missile -> Form
formMissile sColor mColor {src,pos,dest} =
    -- sColor is the color of the missile stream
    let
        lStyle = 
            {
                color = sColor,     
                width = sWidth,
                cap = Clg.Flat,
                join = Clg.Smooth,
                dashing = [],
                dashOffset = 0
            }
    in
        Clg.group
        [
            Clg.segment (src.x, src.y) (pos.x, pos.y) |> Clg.traced lStyle,
            Clg.circle mRad |> 
            Clg.filled mColor |>
            Clg.move (pos.x, pos.y)
        ]

renderMissiles : Int -> Int -> List Missile -> Element
renderMissiles w h ms =
    let
        func m acc =
            if isDead m then
                acc
            else
                if isDefM m then
                    (formMissile green white m)::acc
                else
                    (formMissile red white m)::acc
    in
        List.foldl func [] ms |>
        Clg.collage w h

renderBlasts : Int -> Int -> List Blast -> Element
renderBlasts w h bs = 
    let
        func b acc =
            if b.rad == 0 then
                acc
            else
                (Clg.circle b.rad |> 
                Clg.filled white |>
                Clg.move (b.pos.x, b.pos.y))::acc
    in
        List.foldl func [] bs |>
        Clg.collage w h

renderGameOver : Int -> Int -> Element
renderGameOver w h =
    let
        -- game over text
        goText = 
            Text.fromString "GAME OVER" |> Text.height goH |> Text.color green
            |> Text.typeface ["Courier New", "Courier", "sans-serif"]
            |> Text.bold
        -- click instruction text
        iText =
            Text.fromString "CLICK SCREEN TO RESTART" |> 
            Text.height gocH |> 
            Text.color green |>
            Text.typeface ["Courier New", "Courier", "sans-serif"] |>
            Text.bold
    in
        let 
            goForm = Clg.text goText |> Clg.move (0, goH * 0.5)
            iForm = Clg.text iText |> Clg.move (0, goH * -0.5)
        in
            Clg.collage w h [goForm, iForm]

renderRoundOver : Int -> Int -> Element
renderRoundOver w h =
    let
        rcText = 
            Text.fromString "ROUND COMPLETE" 
            |> Text.height goH 
            |> Text.color green
            |> Text.typeface ["Courier New", "Courier", "sans-serif"]
            |> Text.bold
        -- click instruction text
        iText =
            Text.fromString "CLICK SCREEN TO CONTINUE" |> 
            Text.height gocH |> 
            Text.color green |>
            Text.typeface ["Courier New", "Courier", "sans-serif"] |>
            Text.bold
    in
        let 
            rcForm = Clg.text rcText |> Clg.move (0, goH * 0.5)
            iForm = Clg.text iText |> Clg.move (0, goH * -0.5)
        in
            Clg.collage w h [rcForm, iForm]

renderInstructions w h =
    let
        title = 
            Text.fromString "MISSILE DEFENSE"
            |> Text.height goH
            |> Text.color green
            |> Text.typeface ["Courier New", "Courier", "sans-serif"]
            |> Text.bold
        instructions = 
            Text.fromString "CLICK TO START / SHOOT"
{-                [
                    Text.fromString "Shoot down the falling red missiles ",
                    Text.fromString "before they destroy all your cities. ",
                    Text.fromString "You shoot missiles from your silos. ",
                    Text.fromString "Silos can be destroyed, but, unlike ",
                    Text.fromString "cities, they come back alive after ",
                    Text.fromString "every round. Rounds end when all ",
                    Text.fromString "the missiles blow up or are destroyed. ",
                    Text.fromString "The game is over when all your cities ",
                    Text.fromString "are leveled."
                ]-}
            |> Text.height gocH
            |> Text.color green
            |> Text.typeface ["Courier New", "Courier", "sans-serif"] 
            |> Text.bold
    in
        let
            titleForm = Clg.text title |> Clg.move (0, goH * 0.5)
            instForm = Clg.text instructions |> Clg.move (0, goH * -0.5)
        in
            Clg.collage w h [titleForm, instForm]

renderWindow : Model -> Element
renderWindow model = 
    if model.instructions then
        El.layers [
            renderScreen winW winH black,
            renderInstructions winW winH
        ]
    else if model.gOver then
        El.layers [
            renderScreen winW winH black,
            renderRoundScore winW winH green model.round model.score,
            renderGameOver winW winH
        ]
    else if model.rOver then
        El.layers [
            renderScreen winW winH black,
            renderRoundScore winW winH green model.round model.score,
            renderRoundOver winW winH
            -- renderScore winW winH green model.score
        ]
    else
        El.layers [
            -- renderScreen winW winH black
            renderBG winW winH
            |> El.container winW winH bottomLeft,
            renderCities winW winH blue red model.cities,
            renderSilos winW winH yellow red model.silos,
            renderMissiles winW winH model.missiles,
            renderBlasts winW winH model.blasts,
            renderRoundScore winW winH green model.round model.score
        ]

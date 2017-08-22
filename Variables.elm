module Variables exposing (..)

import Color exposing (Color, black)

maxSiloMs : Int
maxSiloMs = 10


-- DIMENSIONS VARS

-- blast radius
bRad : Float
bRad = 35

mRad : Float
mRad = 2

-- stream width
sWidth : Float
sWidth = mRad * 2

winW : Int
winW = 550

winH : Int
winH = 550

-- margin within which cities and silos are rendered
innerMargin : Float
innerMargin = toFloat (sSize * 2)

innerW : Float
innerW = (toFloat winW) - innerMargin

groundH : Int
groundH = winH // 16

cSize : Int
cSize = winW // 15

sSize : Int
sSize = winW // 15

-- x,y can be negative and positive (collage aligns in dead center)
-- w,h are positive ints
baseY : Float
baseY = -1 * ((toFloat winH) / 2)

maxY : Float
maxY = -1 * baseY

baseX : Float
baseX = -1 * ((toFloat winW) / 2)

maxX : Float
maxX = -1 * baseX

scoreY : Float
scoreY = baseY + (toFloat winH) - (toFloat sSize)


-- COLORS

screenColor : Color
screenColor = black


-- TYPEFACES

scoreH : Float
scoreH = 21

goH : Float
goH = scoreH * 2

-- game over click instruction h
gocH : Float
gocH = scoreH * 0.75

-- height of missile number text
numMH : Float
numMH = 16


-- PROBABILITIES
-- maximum random number value
maxRand : Int
maxRand = 6000

-- maximum random number split value
maxSplitRand : Int
maxSplitRand = 3000


-- SCORE
-- points per missile shot down
ppM : Int
ppM = 40


-- SPEED
-- missile base speed
baseMSpeed : Float
baseMSpeed = 0.5

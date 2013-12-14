
import open Graphics.WebGL
import open MJS
import open Touch
import open Window

import open Joystick

relPos : (Int, Int) -> { a | x : Int, y : Int } -> (Float, Float)
relPos (w, h) {x, y} = (toFloat x - (toFloat w / 2), (toFloat h / 2) - toFloat y)

safeHead : [a] -> Maybe a
safeHead l = case l of
    [] -> Nothing
    (x :: xs) -> Just x

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f m = case m of
    Nothing -> Nothing
    Just x -> Just (f x)

appMaybe : Maybe (a -> b) -> Maybe a -> Maybe b
appMaybe mf mx = case (mf, mx) of
    (Just f, Just x) -> Just (f x)
    _ -> Nothing

lift2Maybe : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2Maybe f x y = (f `mapMaybe` x) `appMaybe` y

joyCenter : (Int, Int) -> (Float, Float)
joyCenter (w,h) = (0, -(toFloat h / 4))

joystick : Signal (Maybe (Float, Float))
joystick = (lift2Maybe relPos <~ (Just <~ dimensions) ~ (safeHead <~ touches))

realJoy : Signal (Float,Float)
realJoy = (\mjs jc -> case mjs of
                Nothing -> jc
                Just js -> js) <~ joystick ~ (joyCenter <~ dimensions)

mapPair : (a -> b) -> (a, a) -> (b, b)
mapPair f (x,y) = (f x, f y)

zipWithPair : (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipWithPair f (x,y) (z,w) = (f x z, f y w)

fDimensions : Signal (Float,Float)
fDimensions = mapPair toFloat <~ dimensions

controls = (zipWithPair (/)) <~ realJoy ~ fDimensions

aspect : Signal Float
aspect = (/) <~ (fst <~ fDimensions) ~ (snd <~ fDimensions)

scene : Signal [Model]
scene = joyModels <~ controls ~ aspect

overlay : Signal [Form]
overlay = combine <|
    [ move <~ (joyCenter <~ dimensions) ~ constant (filled green <| circle 7)
    ] 

main = flow outward <~ combine 
    [ webgl <~ dimensions ~ scene
    , collage <~ (fst <~ dimensions) ~ (snd <~ dimensions) ~ overlay
    ]


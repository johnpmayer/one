
import open Graphics.WebGL
import open MJS
import open Touch
import open Window

import Joystick (joyModels)
import Skybox (skyModel)

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

sAspect : Signal Float
sAspect = (/) <~ (fst <~ fDimensions) ~ (snd <~ fDimensions)

eye : V3
eye = v3 -1 1 0
center : V3
center = v3 0 0.4 0
up : V3
up = v3 0 1 0

view : V3 -> V3 -> V3 -> Float -> M4x4
view eye center up aspect = 
    let proj = m4x4makePerspective 45 aspect 0.01 1000
        look = m4x4makeLookAt eye center up
    in m4x4mul proj look

sView = view eye center up <~ sAspect

scene : Signal [Model]
scene = (::) <~ (skyModel <~ sView) ~ (joyModels <~ controls ~ sView)

overlay : Signal [Form]
overlay = combine <|
    [ move <~ (joyCenter <~ dimensions) ~ constant (filled green <| circle 7)
    ] 

main = flow outward <~ combine 
    [ webgl <~ dimensions ~ scene
    , collage <~ (fst <~ dimensions) ~ (snd <~ dimensions) ~ overlay
    ]


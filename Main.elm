
import open Graphics.WebGL
--import Maybe
import open MJS
import open Touch
import open Window

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
joyCenter (w,h) = (0, -(toFloat h / 3))

joystick : Signal (Maybe (Float, Float))
joystick = (lift2Maybe relPos <~ (Just <~ dimensions) ~ (safeHead <~ touches))

realJoy : Signal (Float,Float)
realJoy = (\mjs jc -> case mjs of
                Nothing -> jc
                Just js -> js) <~ joystick ~ (joyCenter <~ dimensions)

overlay : Signal [Form]
overlay = combine <|
    [ constant <| filled red <| circle 5
    , move <~ realJoy ~ constant (filled green <| circle 20)
    ] 

id_vert = [glShader|
attribute vec3 pos;
void main() {
    gl_Position = vec4(pos,1.0);
}
|]

black_frag = [glShader|
void main() {
    gl_FragColor = vec4(0.0,0.0,0.0,1.0);
}
|]

type Pos = { pos : V3 }

box : Buffer Pos
box = bind <| map (mapTriangle Pos) [(v3 1 -1 0, v3 1 1 0, v3 -1 -1 0)]

black_prog = link id_vert black_frag

scene : Signal [Model]
scene = constant <| [encapsulate black_prog box {}]

main = flow inward <~ combine 
    [ asText <~ (lift2Maybe relPos <~ (Just <~ dimensions) ~ (safeHead <~ touches))
    , collage <~ (fst <~ dimensions) ~ (snd <~ dimensions) ~ overlay
    , webgl <~ dimensions ~ scene
    ]


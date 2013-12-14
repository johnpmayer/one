
module Joystick where

import open MJS
import open Graphics.WebGL

import open Scene
import Simulation (Plane, planeView, joyBase)
import open Utils

joySection : Float -> Float -> Float -> Buffer { pos : V3, norm : V3 }
joySection l r t = 
    let a = v3 0 0 0
        b = v3 r 0 0
        c = v3 (r * cos t) 0 (r * sin t)
        d = v3 0 l 0
        e = v3 r l 0
        f = v3 (r * cos t) l (r * sin t)
        up = v3 0 1 0
        down = v3 0 -1 0
        out = v3 (cos <| t / 2) 0 (sin <| t / 2)
        bottom = mapTriangle (\p -> { pos = p, norm = down }) (a,b,c)
        top = mapTriangle (\p -> { pos = p, norm = up }) (d,e,f)
        sides = map (mapTriangle (\p -> { pos = p, norm = out })) [(b,c,e),(c,e,f)]
    in bind <| bottom :: top :: sides

joyVert : Shader { pos : V3, norm : V3 } { model : M4x4, view : M4x4 } { vNorm : V3 }
joyVert = [glShader|

attribute vec3 pos;
attribute vec3 norm;

uniform mat4 model;
uniform mat4 view;

varying vec3 vNorm;

void main() {
    gl_Position = view * model * vec4(pos, 1.0);
    vNorm = (model * vec4(norm, 0.0)).xyz;
}

|]

joyFrag : Shader {} {} { vNorm : V3 }
joyFrag = [glShader|

precision mediump float;

varying vec3 vNorm;

void main() {
    float diffuse = dot(vec3(0.0,1.0,0.0), vNorm.xyz) * 0.6 + 0.4;
    gl_FragColor = vec4(vec3(0.5,0,0) * diffuse, 1.0);
}

|]

joyProg = link joyVert joyFrag

n =16 
l = 0.5
r = 0.02
dt = 2 * pi / n

joyBuf : Buffer { pos : V3, norm : V3 }
joyBuf = joySection l r dt

up : V3
up = v3 0 1 0

joyScene : { view : M4x4 } -> [Scene { pos : V3, norm : V3 } { view : M4x4 }]
joyScene view =
    let ts = map (\i -> dt * i) [0..(n-1)]
        toRot t = m4x4makeRotate t up
    in map (\t -> SceneNode (toRot t) [SceneLeaf joyBuf view]) ts

joyModels : (Float,Float) -> M4x4 -> Plane -> [Model]
joyModels (roll, pitch) view plane = 
    let rollJoy = m4x4makeRotate roll <| v3 1 0 0
        pitchJoy = m4x4makeRotate pitch <| v3 0 0 (-1)
        rotate = m4x4mul rollJoy pitchJoy
        localScene = SceneNode rotate <| joyScene {view=view}
        absScene = joyBase plane localScene
    in makeModels absScene joyProg 


module Skybox where

import open MJS
import open Graphics.WebGL

worldSize = 500

p0 = scale (v3  1  1  1) worldSize
p1 = scale (v3 -1  1  1) worldSize
p2 = scale (v3 -1 -1  1) worldSize
p3 = scale (v3  1 -1  1) worldSize
p4 = scale (v3  1 -1 -1) worldSize
p5 = scale (v3  1  1 -1) worldSize
p6 = scale (v3 -1  1 -1) worldSize
p7 = scale (v3 -1 -1 -1) worldSize

front  = [(p0,p1,p2),(p2,p3,p0)]
back   = [(p5,p6,p7),(p7,p4,p5)]
right  = [(p0,p3,p4),(p4,p5,p0)]
left   = [(p1,p2,p7),(p7,p6,p1)]
top    = [(p0,p5,p6),(p6,p1,p0)]
bottom = [(p3,p4,p7),(p7,p2,p3)]

positions = bottom ++ front ++ back ++ right ++ left ++ top

green   = v3 0 1 0
blue    = v3 0 0 1

repeat n elem = map (\_ -> elem) [0..n-1]

colors = concat . map (\c -> repeat 2 (c,c,c)) <| green :: repeat 5 blue

mesh : [Triangle {pos : V3, color : V3}]
mesh = zipWith (zipTriangle (\pos color -> { pos = pos, color = color })) positions colors

skyBuf = bind mesh

skyVert = [glShader|

attribute vec3 pos;
attribute vec3 color;

uniform mat4 view;

varying vec3 vcolor;

void main () {
    gl_Position = view * vec4(pos, 1.0);
    vcolor = color;
}

|]

skyFrag = [glShader|

precision mediump float;

varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]

skyProg = link skyVert skyFrag

skyModel : M4x4 -> Model
skyModel view = encapsulate skyProg skyBuf {view = view}

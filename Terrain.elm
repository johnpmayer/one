
module Terrain where

import open Graphics.WebGL
import open MJS

import DiamondSquare (seed,terrainTriangles)

world = seed (4,4,4,4) 4

terrTris : [Triangle {pos:V3}]
terrTris = terrainTriangles world

terrBuf = bind terrTris

terrVert = [glShader|

attribute vec3 pos;

uniform mat4 view;

void main () {
    gl_Position = view * vec4(pos, 1.0);
}

|]

terrFrag = [glShader|

precision mediump float;

void main () {
    gl_FragColor = vec4(0.8,0.8,0.0,1.0);
}

|]

terrProg = link terrVert terrFrag

terrModel : M4x4 -> Model
terrModel view = encapsulate terrProg terrBuf {view = view}


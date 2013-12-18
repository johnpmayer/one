
module Terrain where

import open Graphics.WebGL
import open MJS

import DiamondSquare (seed,terrainTriangles,TerrainRow,Terrain)

world = seed (4,4,4,4) 4

rowPairTriangles : (Int,Int) -> TerrainRow -> TerrainRow -> [Triangle {pos:V3,norm:V3}]
rowPairTriangles (n',m') r1 r2 = case (r1,r2) of
    ((a::b::r1s),(c::d::r2s)) ->
        let n = toFloat n'
            m = toFloat m'
            t1 = (v3 n a m, v3 n b (m+1), v3 (n+1) c m)
            t2 = (v3 n b (m+1), v3 (n+1) d (m+1), v3 (n+1) c m)
            p t = 
                let (u,v,w) = t
                    norm = v3normalize <| v3cross (v3sub v u) (v3sub w u)
                in mapTriangle (\v -> { pos = v, norm = norm }) t
        in p t1 :: p t2 :: rowPairTriangles (n', m' + 1) (b :: r1s) (d :: r2s)
    _ -> []

terrainTriangles : Terrain -> [Triangle {pos:V3,norm:V3}]
terrainTriangles t = 
    let helper t' n = case t' of
        (a::b::rs) -> rowPairTriangles (n,0) a b :: helper (b :: rs) (n+1)
        _ -> []
    in concat <| helper t 0

terrTris : [Triangle {pos:V3,norm:V3}]
terrTris = terrainTriangles world

terrBuf = bind terrTris

terrVert = [glShader|

attribute vec3 pos;
attribute vec3 norm;
uniform mat4 view;
varying vec3 vNorm;

void main () {
    gl_Position = view * vec4(10.0 * pos.x, pos.y, 10.0 * pos.z, 1.0);
    vNorm = norm;
}

|]

terrFrag = [glShader|

precision mediump float;
varying vec3 vNorm;

void main () {
    float diffuse = dot(vec3(0.0,1.0,0.0), vNorm) * 0.8 + 0.2;
    gl_FragColor = vec4(vec3(0.8,0.8,0.0) * vNorm.y, 1.0);
}

|]

terrProg = link terrVert terrFrag

terrModel : M4x4 -> Model
terrModel view = encapsulate terrProg terrBuf {view = view}


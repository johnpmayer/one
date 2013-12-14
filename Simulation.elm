
module Simulation where

import open MJS
import Scene (Scene, SceneNode)

type Plane = { pos : V3, ahead : V3, up : V3, wing : V3 }

initialPlane : Plane
initialPlane = 
    { pos = v3 0 -300 0
    , ahead = v3 1 0 0
    , up = v3 0 1 0
    , wing = v3 0 0 1
    }

speed : Float
speed = 0.5
factor : Float
factor = 0.2

simulate : (Float,Float) -> Plane -> Plane
simulate (roll, pitch) {pos, ahead, up, wing} = 
    let pos' = v3add (v3scale ahead speed) pos
        rotRoll = m4x4makeRotate (roll * factor) ahead
        up' = v3normalize <| v3mul4x4 rotRoll up
        wing' = v3normalize <| v3mul4x4 rotRoll wing
        rotPitch = m4x4makeRotate (-pitch * factor) wing
        up'' = v3normalize <| v3mul4x4 rotPitch up'
        ahead' = v3normalize <| v3mul4x4 rotPitch ahead
    in { pos = pos'
        , ahead = ahead'
        , up = up''
        , wing = wing' }

joyBase : Plane -> Scene a u -> Scene a u
joyBase {pos, ahead, up, wing} scene = 
    let translateJoy = m4x4makeTranslate (v3add pos ahead) 
        rotJoy = m4x4makeCoords ahead up wing
        transformJoy = m4x4mul translateJoy rotJoy
    in SceneNode transformJoy [scene]

planeView : Plane -> { eye : V3, center : V3, up : V3 }
planeView {pos, ahead, up} = 
    let eye = v3 0 0 0
        center = v3 0 0 0
    in { eye = v3add pos up
        , center = v3add pos (v3add ahead (v3scale up 0.4))
        , up = up }


module Simulation where

import open MJS
import Scene (Scene, SceneNode)

type Plane = { pos : V3, ahead : V3, up : V3, wing : V3 }

initialPlane : Plane
initialPlane = 
    { pos = v3 0 10 0
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
    let pos' = add (scale ahead speed) pos
        rotRoll = makeRotate (roll * factor) ahead
        up' = normalize <| mul4x4 rotRoll up
        wing' = normalize <| mul4x4 rotRoll wing
        rotPitch = makeRotate (-pitch * factor) wing
        up'' = normalize <| mul4x4 rotPitch up'
        ahead' = normalize <| mul4x4 rotPitch ahead
    in { pos = pos'
        , ahead = ahead'
        , up = up''
        , wing = wing' }

joyBase : Plane -> Scene a u -> Scene a u
joyBase {pos, ahead, up, wing} scene = 
    let translateJoy = makeTranslate (add pos ahead) 
        rotJoy = makeBasis ahead up wing
        transformJoy = mul translateJoy rotJoy
    in SceneNode transformJoy [scene]

planeView : Plane -> { eye : V3, center : V3, up : V3 }
planeView {pos, ahead, up} = 
    let eye = v3 0 0 0
        center = v3 0 0 0
    in { eye = add pos up
        , center = add pos (add ahead (scale up 0.4))
        , up = up }

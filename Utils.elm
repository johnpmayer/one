
module Utils where

import open MJS

makeView : { eye : V3, center : V3, up : V3 } -> (Float,Float) -> M4x4
makeView {eye, center, up} (w,h) = 
    let aspect = w / h
        proj = makePerspective 45 aspect 0.01 1000
        look = makeLookAt eye center up
    in mul proj look


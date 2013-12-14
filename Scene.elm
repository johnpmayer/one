
module Scene where

import open MJS
import open Graphics.WebGL

data Scene a b
    = SceneNode M4x4 [Scene a b]
    | SceneLeaf (Buffer a) b

makeModels : Scene a b -> Program a { b | model : M4x4 } -> [Model]
makeModels root prog = 
    let walkScene model scene = case scene of
        SceneLeaf buf u ->
            [encapsulate prog buf {u | model = model}]
        SceneNode trans childs -> 
            let subModel = m4x4mul model trans
            in concat <| map (walkScene subModel) childs
    in walkScene m4x4identity root

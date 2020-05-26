module Main exposing (main)

import Actions exposing (Action(..))
import Browser
import Model
import Update
import View


main : Program () Model.Model Action
main =
    Browser.sandbox
        { init = Model.initial
        , update = Update.update
        , view = View.view
        }

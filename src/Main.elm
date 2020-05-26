import Model
import Update
import View
import Html exposing (beginnerProgram)
import Actions exposing (Action(..))

main : Program Never Model.Model Action
main =
  beginnerProgram
    { model = Model.initial
    , update = Update.update
    , view = View.view
    }

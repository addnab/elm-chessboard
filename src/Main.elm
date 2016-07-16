import Model
import Update
import View
import Html.App as App
import Actions exposing (Action(..))

main : Program Never
main =
  App.beginnerProgram
    { model = Model.initial
    , update = Update.update
    , view = View.view
    }

import Model
import Update
import View
import Html.App as App
import Actions exposing (Action(..))

main : Program Never
main =
  App.program
    { init = (Model.initial, Cmd.none)
    , update = Update.update
    , view = View.view
    , subscriptions = always Sub.none
    }

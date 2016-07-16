module Update exposing (update)

import Model exposing (Model)
import Actions exposing (Action(..))
import Debug

update : Action -> Model -> Model
update action model =
  case action of
    Select square ->
      { model | selectedSquare = Just square }
    Move square position ->
      model
    Reset ->
      model

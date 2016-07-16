module Update exposing (update)

import Chess.Board exposing (getBoardViewForNextMoves)
import Model exposing (Model)
import Actions exposing (Action(..))
import Debug

update : Action -> Model -> Model
update action model =
  case action of
    Select square ->
      { model
      | boardView =
          getBoardViewForNextMoves
            model.player
            square
            model.board
      , selectedSquare = Just square
      }
    Move square position ->
      model
    Reset ->
      model

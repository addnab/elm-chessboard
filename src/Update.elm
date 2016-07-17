module Update exposing (update)

import Chess.Board exposing (getBoardViewForNextMoves, movePiece)
import Model exposing (Model)
import Actions exposing (Action(..))
import Chess.Players exposing (opponent)
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
    Deselect ->
      { model | selectedSquare = Nothing }
    Move square position ->
      let
        { board, capturedPiece } = movePiece square position model.board
      in
        { model
        | board = board
        , boardView = board
        , player = opponent model.player
        }
    Reset ->
      model

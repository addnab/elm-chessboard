module Update exposing (update)

import Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe)
import Chess.Board exposing (movePiece)
import Model exposing (Model)
import Actions exposing (Action(..))
import Chess.Players exposing (opponent, getPlayerInfo)
import Debug

update : Action -> Model -> Model
update action model =
  case action of
    Select square ->
      { model
      | boardView =
          getBoardViewForNextMoves
            model.playerInTurn
            (getPlayerInfo model.playerInTurn model.playersInfo)
            square
            model.board
      , selectedSquare = Just square
      }
    Deselect ->
      { model | selectedSquare = Nothing }
    Move square position ->
      let
        { board, capturedPiece } = movePiece square.position position model.board
      in
        { model
        | board = board
        , boardView = board
        , playerInTurn = opponent model.playerInTurn
        }
    Reset ->
      model

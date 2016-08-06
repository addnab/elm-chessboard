module Update exposing (update)

import Chess.Types exposing (Move)
import Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)
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
    MovePiece fromPosition move ->
      let
        { board, capturedPiece } = applyMove fromPosition move model.board
      in
        { model
        | board = board
        , boardView = board
        , playerInTurn = opponent model.playerInTurn
        }
    Reset ->
      model

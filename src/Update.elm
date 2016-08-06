module Update exposing (update)

import Chess.Types exposing (Move)
import Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)
import Model exposing (Model)
import Actions exposing (Action(..))
import Chess.Players exposing (opponent, getPlayerInfo, setPlayerInfo)

import Debug

update : Action -> Model -> Model
update action model =
  case action of
    Select square ->
      { model
      | boardView =
          getBoardViewForNextMoves
            model.playerInTurn
            (getPlayerInfo model.playersInfo model.playerInTurn)
            square
            model.board
      , selectedSquare = Just square
      }
    Deselect ->
      { model | selectedSquare = Nothing }
    MovePiece fromPosition move ->
      let
        { board, capturedPiece, playerInfo } =
          applyMove
            model.playerInTurn
            fromPosition
            move
            (getPlayerInfo model.playersInfo model.playerInTurn)
            model.board
      in
        { model
        | board = board
        , boardView = board
        , playerInTurn = opponent model.playerInTurn
        , playersInfo = setPlayerInfo model.playersInfo model.playerInTurn playerInfo
        }
    Reset ->
      model

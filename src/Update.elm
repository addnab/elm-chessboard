module Update exposing (update)

import Chess.Types exposing (Move)
import Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)
import Chess.Board exposing (setPiece)
import Model exposing (Model)
import Actions exposing (Action(..))
import Chess.Players exposing (opponent, getPlayerInfo, setPlayerInfo)

import Debug

update : Action -> Model -> Model
update action model =
  case model.promote of
    Nothing ->
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
            ({ board, capturedPiece, playerInfo, promote }) =
              applyMove
                model.playerInTurn
                fromPosition
                move
                (getPlayerInfo model.playersInfo model.playerInTurn)
                model.board
            playerInTurn =
              case promote of
                Nothing ->
                  opponent model.playerInTurn
                _ ->
                  model.playerInTurn
          in
            { model
            | board = board
            , boardView = board
            , playerInTurn = playerInTurn
            , playersInfo = setPlayerInfo model.playersInfo model.playerInTurn playerInfo
            , promote = promote
            }
        _ ->
          model
    Just _ ->
      case action of
        PromotePawn playerPiece position ->
          let
            board = setPiece position (model.board, Just playerPiece)
          in
            { model
            | board = board
            , boardView = board
            , playerInTurn = opponent model.playerInTurn
            , promote = Nothing
            }
        _ ->
          model

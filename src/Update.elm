module Update exposing (update)

import Chess.Types exposing (Move)
import Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)
import Chess.Board exposing (setPiece)
import Model exposing (Model)
import Actions exposing (Action(..))
import Chess.Players exposing (opponent, getPlayerInfo, setPlayerInfo)

import Debug

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case model.promote of
    Nothing ->
      case action of
        Select square ->
          ( { model
            | boardView =
                getBoardViewForNextMoves
                  model.playerInTurn
                  (getPlayerInfo model.playersInfo model.playerInTurn)
                  square
                  model.board
            , selectedSquare = Just square
            }
          , Cmd.none
          )
        Deselect ->
          ( { model | selectedSquare = Nothing }
          , Cmd.none
          )
        MovePiece fromPosition move ->
          let
            ({ board, capturedPiece, playerInfo }, command) =
              applyMove
                model.playerInTurn
                fromPosition
                move
                (getPlayerInfo model.playersInfo model.playerInTurn)
                model.board
          in
            ( { model
              | board = board
              , boardView = board
              , playerInTurn = opponent model.playerInTurn
              , playersInfo = setPlayerInfo model.playersInfo model.playerInTurn playerInfo
              }
            , command
            )
        PickPromotionPiece position ->
          ( { model
            | promote = Just position
            , playerInTurn = opponent model.playerInTurn
            }
          , Cmd.none
          )
        _ ->
          ( model
          , Cmd.none
          )
    Just _ ->
      case action of
        PromoteToPiece playerPiece position ->
          let
            board = setPiece position (model.board, Just playerPiece)
          in
            ( { model
              | board = board
              , boardView = board
              , playerInTurn = opponent model.playerInTurn
              , promote = Nothing
              }
            , Cmd.none
            )
        _ ->
          ( model
          , Cmd.none
          )

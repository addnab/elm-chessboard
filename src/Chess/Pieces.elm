module Chess.Pieces exposing (getPieceDisplayInfo, toPlayerPiece)

import Chess.Types exposing (Player(..), Position, PlayerPiece, Piece(..))
import Color exposing (Color)

toPlayerPiece : Player -> Piece -> PlayerPiece
toPlayerPiece player piece =
  { player = player
  , piece = piece
  , moved = False
  }

getImageName : PlayerPiece -> String
getImageName playerPiece =
  let
    color = case playerPiece.player of
        White -> "W"
        Black -> "B"
    piece  =
      case playerPiece.piece of
        K -> "K"
        Q -> "Q"
        B -> "B"
        N -> "N"
        R -> "R"
        P -> "P"
  in
    color ++ "_" ++ piece ++ ".png"

getPieceDisplayInfo : Maybe PlayerPiece -> Maybe String
getPieceDisplayInfo playerPieceMaybe =
    Maybe.map getImageName playerPieceMaybe

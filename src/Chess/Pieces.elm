module Chess.Pieces exposing (PlayerPiece, Piece(..), getPieceDisplayInfo, toPlayerPiece)

import Chess.Players exposing (Player(..))
import Chess.Position exposing (Position)
import Color exposing (Color)

type Piece = K | Q | R | N | B | P

type alias PlayerPiece =
  { player : Player
  , moved : Bool
  , piece : Piece
  }

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

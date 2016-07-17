module Chess.Pieces exposing (PlayerPiece, Piece(..), getPieceDisplayInfo)

import Chess.Players exposing (Player(..))
import Chess.Position exposing (Position)
import Color exposing (Color)

type Piece = K | Q | R | N | B | P

type alias PlayerPiece =
  { player : Player
  , moved : Bool
  , piece : Piece
  }

getPieceDisplayInfo playerPieceMaybe =
  let
    (text, color) = case playerPieceMaybe of
      Just playerPiece ->
        let
          text' = case playerPiece.piece of
            K -> "K"
            Q -> "Q"
            R -> "R"
            N -> "N"
            B -> "B"
            P -> "P"
          color' = case playerPiece.player of
            White -> "white"
            Black -> "black"
        in
          (text', color')
      Nothing ->
        ("", "white")
  in
    { text = text, color = color }

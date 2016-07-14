module Chess.Pieces exposing (PlayerPiece, Piece(..), getPieceDisplayInfo)

import Chess.Players exposing (Player(..))

import Color exposing (Color)

type Piece = K | Q | R | N | B | P

type alias PlayerPiece =
  { player : Player
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
            White -> Color.blue
            Black -> Color.grey
        in
          (text', color')
      Nothing ->
        ("", Color.white)
  in
    { text = text, color = color }

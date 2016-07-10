module Chess.Square exposing (Square)

import Chess.Piece exposing (Piece, Position)

type alias Square =
  { piece : Piece
  , position : Position
  }

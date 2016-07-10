module Chess.Square exposing (Square)

import Chess.Position exposing (Position)
import Chess.Piece exposing (PlayerPiece)

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  }

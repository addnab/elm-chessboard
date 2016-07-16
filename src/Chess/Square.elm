module Chess.Square exposing (Square)

import Chess.Position exposing (Position)
import Chess.Pieces exposing (PlayerPiece)

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  , highlightColor: Maybe String
  }

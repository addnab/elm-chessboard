module Chess.Square exposing (Square, MoveState(..))

import Chess.Position exposing (Position)
import Chess.Pieces exposing (PlayerPiece)

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  , moveState: MoveState
  }

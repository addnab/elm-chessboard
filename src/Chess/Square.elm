module Chess.Square exposing (Square, MoveState(..))

import Chess.Position exposing (Position)
import Chess.Pieces exposing (PlayerPiece)

type MoveState = Movable | Capturable | None

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  , moveState: MoveState
  }

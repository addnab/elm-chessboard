module Actions exposing (Action(..))

import Chess.Square exposing (Square)
import Chess.Position exposting (Position)

type Action
  = Reset
  | Move Square Position
  | Select Square

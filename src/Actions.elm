module Actions exposing (Action(..))

import Chess.Types exposing (Move, PlayerPiece, Position, Square)


type Action
    = Reset
    | MovePiece Position Move
    | PickPromotionPiece Position
    | PromotePawn PlayerPiece Position
    | Select Square
    | Deselect

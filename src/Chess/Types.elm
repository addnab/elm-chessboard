module Chess.Types exposing (..)

import Dict


type Player
    = White
    | Black


type Piece
    = K
    | Q
    | R
    | N
    | B
    | P { enPassant : Maybe Position }


type alias PlayerPiece =
    { player : Player
    , moved : Bool
    , piece : Piece
    }


type alias PlayerInfo =
    { canCastle : Bool
    , inCheck : Bool
    , kingPosition : Position
    }


type alias Position =
    { file : Int
    , rank : Int
    }


type MoveType
    = Capture
    | Goto
    | PawnJump
    | CastleKingSide
    | CastleQueenSide
    | Enpassant
    | Promotion


type alias Move =
    { moveType : MoveType
    , position : Position
    }


type alias Square =
    { piece : Maybe PlayerPiece
    , position : Position
    , moveToPlay : Maybe Move
    }


type alias Rank =
    Dict.Dict Int Square


type alias Board =
    Dict.Dict Int Rank

module Chess.Board exposing (Board)

import Chess.Square exposing (Square)

type alias BoardRow = List Square

type alias Board = List BoardRow

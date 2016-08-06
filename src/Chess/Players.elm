module Chess.Players exposing (opponent, playerInit, getPlayerInfo)

import Chess.Types exposing (Player(..), PlayerInfo)

getPlayerInfo : Player -> { white : PlayerInfo, black : PlayerInfo } -> PlayerInfo
getPlayerInfo player players =
  case player of
    White -> players.white
    Black -> players.black

playerInit =
  PlayerInfo False False

opponent : Player -> Player
opponent player =
  case player of
    White -> Black
    Black -> White

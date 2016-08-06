module Chess.Players exposing (opponent, playerInit, getPlayerInfo, setPlayerInfo)

import Chess.Types exposing (Player(..), PlayerInfo)

getPlayerInfo : { white : PlayerInfo, black : PlayerInfo } -> Player -> PlayerInfo
getPlayerInfo playersInfo player =
  case player of
    White -> playersInfo.white
    Black -> playersInfo.black

setPlayerInfo playersInfo player playerInfo =
  case player of
    White -> { playersInfo | white = playerInfo }
    Black -> { playersInfo | black = playerInfo }

playerInit =
  PlayerInfo False False

opponent : Player -> Player
opponent player =
  case player of
    White -> Black
    Black -> White

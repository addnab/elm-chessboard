module Chess.Position exposing
  ( Position
  , Column, Row
  )

type Column = Int

type Row = Int

type alias Position =
  { column : Column
  , row : Row
  }

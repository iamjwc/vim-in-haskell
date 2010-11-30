
module Document where

import Position

type Line     = String
type Lines    = [Line]

data Document = Document Lines

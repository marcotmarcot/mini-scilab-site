module Exercises where

-- base
import Prelude

-- scilab
import Scilab.Interpreter

type ExerciseIO = ([Value], [Value])

data EName = E311 | E314 | E318 deriving Read

io :: EName -> ExerciseIO
io E311
  = (map scalarD [11, 29, 46, 47, 57, 24, 50, 92, 10, 84],
    [vecL [5.5, 14.5, 23, 23.5, 28.5, 12, 25, 46, 5, 42]])
io E314
  = (map
      scalarD
      [11, 29, 46, 47, 57, 24, 50, 92, 10, 84, -18, -29, -25, -40, -35],
    [vecL
      [3.3166247903554,
        5.385164807134504,
        6.782329983125268,
        6.855654600401044,
        7.54983443527075,
        4.898979485566356,
        7.0710678118654755,
        9.591663046625438,
        3.1622776601683795,
        9.16515138991168,
        -1,
        -1,
        -1,
        -1,
        -1]])
io E318 = ([scalarD $ pi / 6], [scalarD 0.49999999999999994])
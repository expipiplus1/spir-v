module SpirV.LoopControl (LoopControl(..)) where
data LoopControl
  = NoControl 
  | Unroll 
  | DontUnroll 
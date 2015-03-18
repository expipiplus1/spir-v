module SpirV.SelectionControl (SelectionControl(..)) where
data SelectionControl
  = NoControl 
  | Flatten 
  | DontFlatten 
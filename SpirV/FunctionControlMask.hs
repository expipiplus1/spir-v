module SpirV.FunctionControlMask (FunctionControlMask(..)) where
data FunctionControlMask
  = InLine 
  | DontInline 
  | Pure 
  | Const 
module SpirV.FPFastMathMode (FPFastMathMode(..)) where
data FPFastMathMode
  = NotNaN 
  | NotInf 
  | NSZ 
  | AllowRecip 
  | Fast 
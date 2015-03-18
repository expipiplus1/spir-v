module SpirV.SamplerAddressingMode (SamplerAddressingMode(..))
       where
data SamplerAddressingMode
  = None 
  | ClampEdge 
  | Clamp 
  | Repeat 
  | RepeatMirrored 
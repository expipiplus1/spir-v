module SpirV.MemoryModel (MemoryModel(..)) where
data MemoryModel
  = Simple 
  | GLSL450 
  | OpenCL12 
  | OpenCL20 
  | OpenCL21 
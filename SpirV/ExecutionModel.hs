module SpirV.ExecutionModel (ExecutionModel(..)) where
data ExecutionModel
  = Vertex 
  | TessellationControl 
  | TessellationEvaluation 
  | Geometry 
  | Fragment 
  | GLCompute 
  | Kernel 
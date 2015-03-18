module SpirV.ExecutionScope (ExecutionScope(..)) where
data ExecutionScope
  = CrossDevice 
  | Device 
  | Workgroup 
  | Subgroup 
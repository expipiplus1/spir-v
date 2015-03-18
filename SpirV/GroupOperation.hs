module SpirV.GroupOperation (GroupOperation(..)) where
data GroupOperation
  = Reduce 
  | InclusiveScan 
  | ExclusiveScan 
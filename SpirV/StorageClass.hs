module SpirV.StorageClass (StorageClass(..)) where
data StorageClass
  = UniformConstant 
  | Input 
  | Uniform 
  | Output 
  | WorkgroupLocal 
  | WorkgroupGlobal 
  | PrivateGlobal 
  | Function 
  | Generic 
  | Private 
  | AtomicCounter 
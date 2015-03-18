module SpirV.MemorySemantics (MemorySemantics(..)) where
data MemorySemantics
  = Relaxed 
  | SequentiallyConsistent 
  | Acquire 
  | Release 
  | UniformMemory 
  | SubgroupMemory 
  | WorkgroupLocalMemory 
  | WorkgroupGlobalMemory 
  | AtomicCounterMemory 
  | ImageMemory 
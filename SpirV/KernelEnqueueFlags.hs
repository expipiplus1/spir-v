module SpirV.KernelEnqueueFlags (KernelEnqueueFlags(..)) where
data KernelEnqueueFlags
  = NoWait 
  | WaitKernel 
  | WaitWorkGroup 
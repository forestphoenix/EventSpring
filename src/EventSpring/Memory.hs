module EventSpring.Memory (

) where
{-
data MemoryState = MemoryState {
    msProjections :: MVar (M.Map AnyProjectionId (MVar AnyProjection)),
    msEvents      :: MVar (Seq AnyEvent)
}

runMemoryTransaction :: MemoryState -> p -> md -> TransactionT p md IO a -> IO a
runMemoryTransaction = undefined
-}

module Debug(debug, debug2) where
import Debug.Trace
import Config
debug res = if activateDebug then traceShow res res else res
debug2 res2 res = if activateDebug then traceShow res2 res else res

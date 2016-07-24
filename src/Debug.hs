module Debug(d1, d2, ppShow) where
import Debug.Trace
import Config
import Text.Show.Pretty
d1 res = if activateDebug then trace (ppShow res) res else res
d2 res2 res = if activateDebug then trace (ppShow res2) res else res

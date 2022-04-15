# https://github.com/returntocorp/semgrep/issues/2859
import pandas as pd
def test():
    df = pd.DataFrame(x)
    ix = df.index
    # using symbolic propagation we know that `ix` is `pandas.DataFrame(x).index`
    # ruleid: test
    ix.set_value(a, b, c)

import numpy as np
import pandas as pd
import pandas_profiling 

df = pd.DataFrame(
	np.random.rand(100, 5),
	columns=['a', 'b', 'c', 'd', 'e']
	)

profile = pandas_profiling.ProfileReport(df)
profile.to_file(outputfile="output.html")

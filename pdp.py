import pandas as pd
import pandas_profiling

df = pd.read_csv("dat_master.csv")
profile = pandas_profiling.ProfileReport(df)
profile.to_file(outputfile="output.html")

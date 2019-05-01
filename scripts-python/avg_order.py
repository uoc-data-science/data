import pandas as pd
import numpy as np
import plotnine
from tabulate import tabulate 
from read_data import load_dataframe

df = load_dataframe("order.pkl")

f = open("../tmp_outputs/console.txt","w")

#for column in df:
#	f.write(column+" ")
#	f.write(str(df[column].isna().sum()))
#	f.write("\r\n")

f.write(tabulate(df.describe(include='all'), headers=[""]+df.T.index, tablefmt='psql'))
f.write("\r\n")
f.write(str(df.dtypes))
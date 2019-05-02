import pandas as pd
import numpy as np
from tabulate import tabulate 
from read_data import load_dataframe
from read_data import load_dataframe_without_ignore

df = load_dataframe("order.pkl")
df_mod = load_dataframe_without_ignore("order.pkl","order_columns.txt")

f = open("../tmp_outputs/console.txt","w")

#for column in df:
#	f.write(column+" ")
#	f.write(str(df[column].isna().sum()))
#	f.write("\r\n")

f.write(tabulate(df.describe(include='all'), headers=[""]+df.T.index, tablefmt='psql'))
f.write("\r\n")
f.write(tabulate(df_mod.describe(include='all'), headers=[""]+df_mod.T.index, tablefmt='psql'))
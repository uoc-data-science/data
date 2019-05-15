import pandas as pd
import numpy as np
from tabulate import tabulate 
from read_data import load_dataframe
from read_data import load_dataframe_without_ignore
from find_identical_columns import find_identical

df = load_dataframe("order.pkl")
df2 = load_dataframe("click.pkl")

# df_mod["order_datetime"] = pd.to_datetime(df['Order Line Date'] + ' ' + df['Order Line Date_Time'], format="%Y-%m-%d %H\\:%M\\:%S")

# df_joined = pd.merge(df, df2, how='outer', left_on="Order Session ID", right_on="Session ID",
#         left_index=False, right_index=False, sort=True,
#         suffixes=('_x', '_y'), copy=True, indicator=False,
#         validate=None)

f = open("../tmp_outputs/console.txt","w")

# f.write(tabulate(df_joined.describe(include='all'), headers=[""]+df_joined.T.index, tablefmt='psql'))
f.write("order\r\n")
for item in find_identical(df):
    f.write("\""+item+"\" ")
    if item == "":
        f.write("\r\n")
f.write("click\r\n")
for item in find_identical(df2):
    f.write("\""+item+"\" ")
    if item == "":
        f.write("\r\n")
        
# f.write(tabulate(df_mod.describe(include='all'), headers=[""]+df_mod.T.index, tablefmt='psql'))
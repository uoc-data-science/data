import pandas as pd
import numpy as np
from read_data import load_dataframe

def find_identical(df):
    list = []
    for column in df:
        for column2 in df:
            if(column != column2):
                if(df[column].dtypes.name == df[column2].dtypes.name):
                    if(df[column].dtypes.name == "category"):
                        if(df[column].dtypes == df[column2].dtypes):
                            df['additional'] = df[column].eq(df[column2]).astype(int)
                            if(df['additional'].sum() == len(df['additional'])):
                                list.append(column)
                                list.append(column2)
                                list.append("")
                    else:
                        df['additional'] = df[column].eq(df[column2]).astype(int)
                        if(df['additional'].sum() == len(df['additional'])):
                            list.append(column)
                            list.append(column2)
                            list.append("")
    return list
#f.write(tabulate(df_mod.describe(include='all'), headers=[""]+df_mod.T.index, tablefmt='psql'))
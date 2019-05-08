import pandas as pd

# CSV to DataFrame
def csvToDF(file):
    path = '../0 Data/' + file
    df = pd.read_csv(path, sep=",", dtype=str, encoding="ISO-8859-1")
    return df

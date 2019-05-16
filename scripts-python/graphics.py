import pandas as pd
import numpy as np
from read_data import load_dataframe

df = load_dataframe("order.pkl")
df2 = load_dataframe("click.pkl")

f = open("../tmp_outputs/console.txt", "w")


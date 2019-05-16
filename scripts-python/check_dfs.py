from read_data import load_dataframe
from find_identical_columns import find_identical

df = load_dataframe("order.pkl")
df2 = load_dataframe("click.pkl")

print(find_identical(df))

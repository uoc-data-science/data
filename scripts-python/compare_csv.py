from read_data import create_dataframe_from_processed_csv

pyorder_df = create_dataframe_from_processed_csv("order_py.csv")

print(pyorder_df.head(10))

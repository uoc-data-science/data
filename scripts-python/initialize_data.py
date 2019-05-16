from copy_open_zip import copy_into_raw_data_and_data_understanding
from read_data import read_raw_data
from read_data import save_dataframe
from clean_dataframe import clean_df_generic

# get raw_datasets

copy_into_raw_data_and_data_understanding()


# Order
####
# create new csv with head and data
# And create sample Excelsheet with 100 lines
df_order = read_raw_data("order_data.csv", "order_columns.txt", "order.xlsx", "order.pkl")
# clean
df_order = clean_df_generic(df_order,0)

print(df_order.head(10))

# click stream
####
# create new csv with head and data
# And create sample Excel sheet with 100 lines
df_clickstream = read_raw_data("clickstream_data.csv", "clickstream_columns.txt",
                               "clickstream.xlsx", "click.pkl")
df_clickstream_part_2 = read_raw_data("clickstream_data_part_2.csv", "clickstream_columns.txt",
                                      "clickstream2.xlsx", "click2.pkl")
df_clickstream = df_clickstream.append(df_clickstream_part_2)
save_dataframe(df_clickstream, "click_all.pkl")

# clean
df_clickstream = clean_df_generic(df_clickstream, 0)


print(df_clickstream.head(10))


from read_data import read_raw_data

# Order
####
# create new csv with head and data
# And create sample Excelsheet with 100 lines
read_raw_data("order_data.csv", "order_columns.txt", "order.xlsx", "order.pkl")

# click stream
####
# create new csv with head and data
# And create sample Excel sheet with 100 lines
read_raw_data("clickstream_data.csv", "clickstream_columns.txt", "clickstream.xlsx", "click.pkl")

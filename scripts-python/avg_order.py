from tabulate import tabulate
from read_data import load_dataframe
import clean_dataframe
import numpy

df = load_dataframe("order.pkl")

clean_dataframe.clean_df_generic_alternative(df)

clean_dataframe.drop_identical(df)


# df_mod["order_datetime"] = pd.to_datetime(df['Order Line Date'] + ' ' + df['Order Line Date_Time'], format="%Y-%m-%d %H\\:%M\\:%S")

f = open("../tmp_outputs/avgOrder.txt", "w")

sum1 = df.describe()
sum2 = df.describe(include=[numpy.object, numpy.bool])
summary = sum1.iloc[1].append(sum2.iloc[2])
summary.to_csv("AverageOrder.txt")

# f.write("order\r\n")
# for item in find_identical(df):
#     f.write("\""+item+"\" ")
#     if item == "":
#         f.write("\r\n")
# f.write("click\r\n")
# for item in find_identical(df2):
#     f.write("\""+item+"\" ")
#     if item == "":
#         f.write("\r\n")
        
# f.write(tabulate(df_mod.describe(include='all'), headers=[""]+df_mod.T.index, tablefmt='psql'))

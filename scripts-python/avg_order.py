from tabulate import tabulate
from read_data import load_dataframe
import clean_dataframe

df = load_dataframe("order.pkl")
df2 = load_dataframe("click.pkl")

clean_dataframe.clean_df_generic_alternative(df)
clean_dataframe.clean_df_generic_alternative(df2)

clean_dataframe.drop_identical(df)
clean_dataframe.drop_identical(df2)


# df_mod["order_datetime"] = pd.to_datetime(df['Order Line Date'] + ' ' + df['Order Line Date_Time'], format="%Y-%m-%d %H\\:%M\\:%S")

f = open("../tmp_outputs/consoleWODupl.txt", "w")

f.write(str(df.shape)+" Shape of Order Data\r\n")
f.write(tabulate(df.describe(include='all'), headers=[""]+df.T.index, tablefmt='psql'))
f.write("\r\n")
f.write(tabulate(df.head(5), headers=[""]+df.T.index, tablefmt='psql'))
f.write("\r\n")
f.write(str(df2.shape)+" Shape of Clickstream Data\r\n")
f.write(tabulate(df2.describe(include='all'), headers=[""]+df2.T.index, tablefmt='psql'))
f.write("\r\n")
f.write(tabulate(df2.head(5), headers=[""]+df2.T.index, tablefmt='psql'))
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
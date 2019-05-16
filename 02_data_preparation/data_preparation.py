import pandas as pd
import numpy as np 
import os.path
import csv
import pickle
import datetime


def read_raw_data(filename,column_file,samplename,dataset_name):
    filename = "./00_raw_data/"+filename
    column_list = read_raw_columns(column_file)
    df = pd.read_csv(filename, names=column_list,low_memory=False, encoding="ansi")
    create_sampel_excel(df,samplename)
    save_dataframe(df,dataset_name)
    print(df)




def read_raw_columns(filename):
    filename = "./01_data_understanding/"+filename
    textfull= open(filename,"r")
    line_list = textfull.readlines()
    column_list = []
    for line in line_list:
        string = line
        string_list = string.split(":")
        column_list.append(string_list[0])
    return column_list




def create_sampel_excel(dataframe,samplename):
    from pandas import ExcelWriter
    #create a Excelsheet with the heads and the first 100 lines for data unterstanding
    df = dataframe.iloc[:1000]
    samplename = "./01_data_understanding/"+samplename
    xlsx_writer = ExcelWriter(samplename)
    df.to_excel(xlsx_writer,"Sheet1",index=False)
    xlsx_writer.save()




def create_csv_from_dataframe(dataframe,filename):
    filename = "./00_raw_data/"+filename
    df = dataframe
    df.to_csv(path_or_buf=filename,index=False)




def save_dataframe(dataframe,filename):
    filename = "./00_raw_data/data_sets/"+filename
    dataframe.to_pickle(filename)




def load_dataframe(filename):
    filename = "./00_raw_data/data_sets/"+filename
    df = pd.DataFrame()
    df= pd.read_pickle(filename)
    return df


#Order
####
##create new csv with head and data
##And create sample Excelsheet with 100 lines 
read_raw_data("order_data.csv","order_columns.txt","order.xlsx","order.pkl")



#clickstream
####
##create new csv with head and data
##And create sample Excelsheet with 100 lines
read_raw_data("clickstream_data.csv","clickstream_columns.txt","clickstream.xlsx","click.pkl") 











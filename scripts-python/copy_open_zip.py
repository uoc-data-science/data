import os.path
import zipfile
import os
import csv
import shutil
from zipfile import ZipFile
from absolute_data_paths import relative_to_absolute

def extra_zip_to(filename,destination):
    filename = relative_to_absolute(filename)
    with ZipFile(filename,"r") as zip:
        zfile = zip.namelist()[0]
        zip.extract(zfile,destination)


def identify_and_tret(source_folder,filename):
        path_file = source_folder+"\\"+filename
        ending = filename.split(".")[1]
        if ending =="txt":
                shutil.copy(path_file,relative_to_absolute("../01_data_understanding"))
        elif ending =="csv":
                shutil.copy(path_file,relative_to_absolute("../00_raw_data"))
        elif ending =="zip":
                extra_zip_to(path_file,relative_to_absolute("../00_raw_data"))




def get_files_from_folder(source_folder):
    folderpath = relative_to_absolute(source_folder)
    for path,list_folder,list_filename in os.walk(folderpath):
        if path == folderpath and list_folder == []:
            for row in list_filename:
                    identify_and_tret(folderpath,row)               


def copy_into_raw_data_and_data_understanding():
        #clickstream
        clickstream_path = "../clickstream"
        get_files_from_folder(clickstream_path)
        #order
        order_path = "../orders"
        get_files_from_folder(order_path)



copy_into_raw_data_and_data_understanding()



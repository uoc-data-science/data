# Here I would like to build up Methodes that Clean the Data:
# 1. delete leading and trailing whitespaces:
# 2. handle empty fields
# 3. handle false or wrong formatted data in fields
# x....

# Knowlegde:
# https://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string

# 1. delete Whitespaces
# strtrim()
# it seems that whitespaces at the end of a field are not displayed in the in-memory Data

# itterate through Data:
# Coloumn-wise itteration:
for(i in mydata)
  {
    #Row-wise itteration:
    for(j in i)
    {
      print(j)
    }
  }
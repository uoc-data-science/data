import sys
import os


# Syntax - for each ../ the path goes one stage up
# then the absolute path is calculated
def relative_to_absolute(path):
    copy = path
    absFilePath = os.path.abspath(os.path.dirname(__file__))
    while copy.startswith('../'):
        absFilePath = os.path.dirname(absFilePath)
        copy = copy[3:]
    
    return (os.path.join(absFilePath,copy))
import numpy as np
import pandas as pd

import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.model_selection import train_test_split

from sklearn.metrics import accuracy_score
from sklearn.metrics import recall_score
from sklearn.metrics import roc_auc_score
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import GridSearchCV

from sklearn.ensemble import RandomForestClassifier

import math

pathInput = '../prediction_input_data.csv'
pathConfusionMatrix = "../cm_forest.png"

# --------------------------Utility functions--------------------------------
#Methode zum Plotten einer Confusion Matrix
#Quelle: https://gist.github.com/shaypal5/94c53d765083101efc0240d776a23823

def print_confusion_matrix(confusion_matrix, class_names, figsize=(10, 7), fontsize=14):
    """Prints a confusion matrix, as returned by sklearn.metrics.confusion_matrix, as a heatmap.

    Arguments
    ---------
    confusion_matrix: numpy.ndarray
        The numpy.ndarray object returned from a call to sklearn.metrics.confusion_matrix.
        Similarly constructed ndarrays can also be used.
    class_names: list
        An ordered list of class names, in the order they index the given confusion matrix.
    figsize: tuple
        A 2-long tuple, the first value determining the horizontal size of the ouputted figure,
        the second determining the vertical size. Defaults to (10,7).
    fontsize: int
        Font size for axes labels. Defaults to 14.

    Returns
    -------
    matplotlib.figure.Figure
        The resulting confusion matrix figure
    """
    df_cm = pd.DataFrame(
        confusion_matrix, index=class_names, columns=class_names,
    )
    fig = plt.figure(figsize=figsize)
    try:
        heatmap = sns.heatmap(df_cm, annot=True, fmt="d")
    except ValueError:
        raise ValueError("Confusion matrix values must be integers.")
    heatmap.yaxis.set_ticklabels(heatmap.yaxis.get_ticklabels(), rotation=0, ha='right', fontsize=fontsize)
    heatmap.xaxis.set_ticklabels(heatmap.xaxis.get_ticklabels(), rotation=45, ha='right', fontsize=fontsize)
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    return fig

# --------------------------Utility functions--------------------------------


df = pd.read_csv(pathInput,sep=',',encoding='utf-8')
df.Ordered = df.Ordered.astype('str', copy=False)

subset0 = df.loc[df['Ordered'] == 'Yes']
subset0 = subset0.sample(n=math.ceil(len(subset0.index)/2))
subset1 = df.loc[df['Ordered'] == 'No']
subset1 = subset1.sample(n=math.ceil(len(subset0.index)))

train = subset1.append(subset0)
test = df[~df.isin(train)].dropna()

X_train_val = train.drop('Ordered', axis=1)
y_train_val = train.loc[:,'Ordered']
X_test = test.drop('Ordered', axis=1)
y_test = test.loc[:,'Ordered']

classes = ['No','Yes']

parameters = {'n_estimators':[10,25,50],
              'max_depth':[2,3,4],
              'min_samples_leaf':[1,0.0025,0.01],
              'min_impurity_decrease':[0, 0.05,0.1]}

clf = GridSearchCV(estimator=RandomForestClassifier(), param_grid=parameters, scoring ='roc_auc', n_jobs=-1, cv=10)
clf.fit(X_train_val, y_train_val)

print('Best score for data1:', clf.best_score_)
print('Best number of estimators:', clf.best_estimator_.n_estimators)
print('Best Depth:',clf.best_estimator_.max_depth)
print('Best Min Samples Leaf:',clf.best_estimator_.min_samples_leaf)
print('Best Min Impurity Decrease:',clf.best_estimator_.min_impurity_decrease)

clf = RandomForestClassifier(
    n_estimators = clf.best_estimator_.n_estimators,
    max_depth = clf.best_estimator_.max_depth,
    min_samples_leaf = clf.best_estimator_.min_samples_leaf,
    min_impurity_decrease=clf.best_estimator_.min_impurity_decrease).fit(X_train_val,y_train_val)
y_predictions = clf.predict(X_test)

cm = confusion_matrix(y_test, y_predictions,classes)
print_confusion_matrix(cm,classes)
plt.savefig(pathConfusionMatrix)
plt.show()


accuracy = round(accuracy_score(y_test, y_predictions),2)
f1 = round(recall_score(y_test, y_predictions, average="binary", pos_label="Yes"),2)
print("Accuracy: "+str(accuracy))
print("F1: "+str(f1))
# Convert y to int for roc scoring
y_test = y_test.replace("No",0)
y_test = y_test.replace("Yes",1)
y_predictions[y_predictions == "No"] = 0
y_predictions[y_predictions == "Yes"] = 1
roc = round(roc_auc_score(y_test, y_predictions),2)
print("ROC AUC score: "+str(roc))


##Projekt z uczenia maszynowego
#Adam Gontarek 106619

Zależności


```python
import pandas as pd
from sklearn.cross_validation import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.grid_search import GridSearchCV, RandomizedSearchCV
from sklearn.metrics import classification_report
from sklearn.svm import SVC
from sklearn.metrics import recall_score
from sklearn.externals import joblib
from sklearn.cross_validation import cross_val_predict
from scipy.stats import randint as sp_randint
from time import time
from operator import itemgetter
import numpy as np
from pandas import DataFrame
```

Zmienne


```python
workspace_path = "C:/development/repos/git/github/ZED/projekt2/"

```

Zaladowanie zbiorów: treningowy, testowy


```python
train_data_df = pd.read_csv(workspace_path + "data/training_data.txt", sep=";", na_values=["nan"], keep_default_na=False, low_memory=False)
X_test = pd.read_csv(workspace_path + "data/test_data.txt", sep=",", na_values=["nan"], keep_default_na=False)

```

Filtracja danych [dostosowanie do danych z pogrupowanymi etykietami] 


```python
df_filtered = train_data_df.query('res_name not in ("DA","DC","DT","DU","DG","DI","UNK","UNX","UNL","PR","PD","Y1","EU","N","15P","UQ","PX4","NAN")')
df_grouped = df_filtered.drop_duplicates(['pdb_code', 'res_name'])
df_without_classes = df_grouped.groupby("res_name").filter(lambda x: len(x) >= 5)
print 'Filtered training data dim:', df_without_classes.shape
```

    Filtered training data dim: (11005, 795)
    

Zaladowanie danych z pogrupowanymi etykietami


```python
group_label_df = pd.read_csv(workspace_path + "data/grouped_res_name.txt", sep=",", na_values=["nan"], keep_default_na=False, low_memory=False)
print 'Group label dim:', group_label_df.shape
```

    Group label dim: (11005, 2)
    

Wybranie istotnych atrybutow do predykcji klas


```python
part_00_cols = df_without_classes.filter(regex='part_00_.*');
rest_cols = df_without_classes.filter(items=[
  'local_volume', 'local_electrons', 'local_mean', 'local_std',
  'local_max', 'local_skewness', 'local_parts',
  'solvent_mask_count', 'void_mask_count', 'modeled_mask_count', 'solvent_ratio'])
```

Przygotowanie zbioru testowego


```python
X_test = X_test.fillna(0)
X_test_part_00_cols = X_test.filter(regex='part_00_.*');
X_test_rest_cols = X_test.filter(items=[
  'local_volume', 'local_electrons', 'local_mean', 'local_std',
  'local_max', 'local_skewness', 'local_parts',
  'solvent_mask_count', 'void_mask_count', 'modeled_mask_count', 'solvent_ratio'])
X_test =  pd.concat([X_test_part_00_cols,  X_test_rest_cols], axis=1);
```

Przygotowanie zbioru pod klasyfikator 1


```python
filtered_df = pd.concat([rest_cols,  part_00_cols], axis=1);
filtered_df = filtered_df.fillna(0);
X_train = filtered_df
y_train = df_without_classes['res_name']
```

Rozmiar zbioru dla klasyfikatora 1


```python
print "X_train", X_train.shape
print "y_train", y_train.shape
```

    X_train (11005, 80)
    y_train (11005L,)
    

Klasyfikator 1


```python
rfc = RandomForestClassifier(n_jobs=8)

param_grid = {
              "n_estimators": [100],
              # "max_features": [3],
              # "min_samples_split": [5],
              # "min_samples_leaf": [2],
              "bootstrap": [False],
              "criterion": ["gini"]
}

clf = GridSearchCV(estimator=rfc, param_grid=param_grid, cv=5, scoring='recall_macro')
```

Obliczenia klasyfikator 1


```python
clf.fit(X_train, y_train)
print("Best parameters set found on development set:")
print()
print(clf.best_estimator_)
print()
print "Best score:", clf.best_score_

```

    Best parameters set found on development set:
    ()
    RandomForestClassifier(bootstrap=False, class_weight=None, criterion='gini',
                max_depth=None, max_features='auto', max_leaf_nodes=None,
                min_samples_leaf=1, min_samples_split=2,
                min_weight_fraction_leaf=0.0, n_estimators=100, n_jobs=8,
                oob_score=False, random_state=None, verbose=0,
                warm_start=False)
    ()
    Best score: 0.173626498947
    

Predykcja klasyfikatora 1 dla dostarczonych przez prowadzącego danych testowych


```python
y_pred =clf.best_estimator_.predict_proba(X_test)

y_pred_df = DataFrame(data=y_pred, columns=clf.best_estimator_.classes_)
y_pred_df.to_csv(workspace_path + "predictions/classifier1_predicts_probability.txt", sep=",")


y_pred =clf.best_estimator_.predict(X_test)

y_pred_df = DataFrame(data=y_pred)
y_pred_df.to_csv(workspace_path + "predictions/classifier1_predicts.txt", sep=",")


y_pred = cross_val_predict(clf.best_estimator_, X_train, y_train, cv=5)
print "Recall score[based on cv]: ", recall_score(y_train, y_pred, average='macro')
```

    Recall score[based on cv]:  0.174720539636
    

Przygotowanie zbioru pod klasyfikator 2


```python
X_train = filtered_df
y_train = group_label_df['res_name_group']


```

Rozmiar zbioru dla klasyfikatora 2


```python
print "X_train", X_train.shape
print "y_train", y_train.shape
```

    X_train (11005, 80)
    y_train (11005L,)
    

Klasyfikator 2


```python
rfc = RandomForestClassifier(n_jobs=8)

param_grid = {
              "n_estimators": [100],
              # "max_features": [3],
              # "min_samples_split": [5],
              # "min_samples_leaf": [2],
              "bootstrap": [False],
              "criterion": ["gini"]
}

clf2 = GridSearchCV(estimator=rfc, param_grid=param_grid, cv=5, scoring='recall_macro')
```

Obliczenia klasyfikator 2


```python
clf2.fit(X_train, y_train)
print("Best parameters set found on development set:")
print()
print(clf2.best_estimator_)
print()
print "Best score:", clf2.best_score_
```

    Best parameters set found on development set:
    ()
    RandomForestClassifier(bootstrap=False, class_weight=None, criterion='gini',
                max_depth=None, max_features='auto', max_leaf_nodes=None,
                min_samples_leaf=1, min_samples_split=2,
                min_weight_fraction_leaf=0.0, n_estimators=100, n_jobs=8,
                oob_score=False, random_state=None, verbose=0,
                warm_start=False)
    ()
    Best score: 0.211201429703
    

Predykcja klasyfikatora 2 dla dostarczonych przez prowadzącego danych testowych


```python
y_pred =clf2.best_estimator_.predict_proba(X_test)

y_pred_df = DataFrame(data=y_pred, columns=clf2.best_estimator_.classes_)
y_pred_df.to_csv(workspace_path + "predictions/classifier2_predicts_probability.txt", sep=",")


y_pred =clf2.best_estimator_.predict(X_test)

y_pred_df = DataFrame(data=y_pred)
y_pred_df.to_csv(workspace_path + "predictions/classifier2_predicts.txt", sep=",")


y_pred = cross_val_predict(clf2.best_estimator_, X_train, y_train, cv=5)
print "Recall score[based on cv]: ", recall_score(y_train, y_pred, average='macro')
```

    Recall score[based on cv]:  0.206720470832
    

Serializacja klasyfikatorów


```python
joblib.dump(clf.best_estimator_, workspace_path + 'classifiers/classifier1.pkl')
joblib.dump(clf2.best_estimator_, workspace_path + 'classifiers/classifier2.pkl')

```




    ['C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_01.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_02.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_03.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_04.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_05.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_06.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_07.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_08.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_09.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_10.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_11.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_12.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_13.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_14.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_15.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_16.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_17.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_18.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_19.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_20.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_21.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_22.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_23.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_24.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_25.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_26.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_27.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_28.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_29.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_30.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_31.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_32.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_33.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_34.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_35.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_36.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_37.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_38.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_39.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_40.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_41.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_42.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_43.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_44.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_45.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_46.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_47.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_48.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_49.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_50.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_51.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_52.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_53.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_54.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_55.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_56.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_57.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_58.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_59.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_60.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_61.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_62.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_63.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_64.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_65.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_66.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_67.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_68.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_69.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_70.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_71.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_72.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_73.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_74.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_75.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_76.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_77.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_78.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_79.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_80.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_81.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_82.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_83.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_84.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_85.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_86.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_87.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_88.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_89.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_90.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_91.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_92.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_93.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_94.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_95.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_96.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_97.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_98.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_99.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_100.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_101.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_102.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_103.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_104.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_105.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_106.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_107.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_108.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_109.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_110.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_111.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_112.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_113.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_114.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_115.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_116.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_117.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_118.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_119.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_120.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_121.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_122.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_123.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_124.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_125.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_126.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_127.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_128.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_129.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_130.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_131.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_132.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_133.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_134.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_135.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_136.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_137.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_138.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_139.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_140.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_141.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_142.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_143.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_144.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_145.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_146.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_147.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_148.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_149.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_150.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_151.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_152.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_153.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_154.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_155.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_156.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_157.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_158.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_159.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_160.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_161.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_162.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_163.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_164.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_165.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_166.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_167.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_168.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_169.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_170.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_171.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_172.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_173.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_174.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_175.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_176.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_177.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_178.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_179.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_180.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_181.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_182.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_183.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_184.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_185.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_186.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_187.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_188.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_189.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_190.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_191.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_192.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_193.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_194.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_195.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_196.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_197.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_198.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_199.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_200.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_201.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_202.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_203.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_204.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_205.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_206.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_207.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_208.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_209.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_210.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_211.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_212.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_213.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_214.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_215.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_216.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_217.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_218.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_219.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_220.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_221.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_222.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_223.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_224.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_225.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_226.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_227.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_228.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_229.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_230.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_231.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_232.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_233.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_234.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_235.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_236.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_237.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_238.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_239.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_240.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_241.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_242.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_243.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_244.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_245.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_246.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_247.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_248.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_249.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_250.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_251.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_252.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_253.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_254.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_255.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_256.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_257.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_258.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_259.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_260.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_261.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_262.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_263.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_264.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_265.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_266.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_267.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_268.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_269.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_270.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_271.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_272.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_273.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_274.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_275.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_276.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_277.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_278.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_279.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_280.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_281.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_282.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_283.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_284.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_285.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_286.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_287.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_288.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_289.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_290.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_291.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_292.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_293.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_294.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_295.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_296.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_297.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_298.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_299.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_300.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_301.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_302.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_303.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_304.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_305.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_306.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_307.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_308.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_309.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_310.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_311.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_312.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_313.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_314.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_315.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_316.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_317.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_318.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_319.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_320.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_321.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_322.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_323.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_324.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_325.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_326.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_327.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_328.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_329.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_330.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_331.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_332.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_333.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_334.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_335.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_336.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_337.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_338.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_339.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_340.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_341.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_342.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_343.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_344.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_345.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_346.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_347.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_348.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_349.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_350.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_351.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_352.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_353.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_354.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_355.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_356.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_357.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_358.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_359.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_360.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_361.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_362.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_363.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_364.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_365.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_366.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_367.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_368.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_369.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_370.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_371.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_372.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_373.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_374.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_375.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_376.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_377.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_378.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_379.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_380.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_381.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_382.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_383.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_384.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_385.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_386.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_387.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_388.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_389.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_390.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_391.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_392.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_393.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_394.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_395.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_396.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_397.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_398.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_399.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_400.npy',
     'C:/development/repos/git/github/ZED/projekt2/classifiers/classifier2.pkl_401.npy']



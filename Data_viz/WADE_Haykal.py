#!/usr/bin/env python
# coding: utf-8

# # Master Data Science Data_VIZ
# ### WADE El Hadji Malick
# ### HAYKAL Fayad

# ## 1. Nettoyage des données 

# In[1]:


import pandas as pd
import numpy as np

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)

path_School = "/users/mmath/wade/Bureau/Master_DS/Projets/Data_viz/Synthese_donnees_des_semis.csv"
path_Malick = "/home/malick/Bureau/Projets/Projets_Malick_fayad/Data_viz/Synthese_donnees_des_semis.csv"

df = pd.read_csv(path_Malick,  sep='\t',  decimal=",")

df.head(10)


# In[2]:


print ("Lignes: " ,df.shape[0])
print ("Colonnes: " ,df.shape[1])
print ("\nVariables:\n",df.dtypes)
print ("\nValeurs uniques :  \n",df.nunique())


# In[3]:


print ("\nTotal des valeurs manquantes :  ", df.isnull().sum().values.sum())

print("\n",df.isnull().sum())


# On voit que notre jeux de données posséde des lignes avec avec au moins un e valeur manquante.  
# On choisit de les supprimer, ce qui représente une perte de 2 lignes d'observations de données

# In[4]:


Perte_de_donnees = df.shape[0]
df = df.dropna()

Perte_de_donnees = (Perte_de_donnees - df.shape[0])/100

print("En supprimant les lignes avec des valeurs manquantes, on a une perte de données de ", Perte_de_donnees,"%")


# In[5]:


df.isnull().sum()


# In[6]:


import seaborn as sns

Col_pairplot = ["5_degres_C_TMG_h", "5_degres_C_TMG_j", "Aire_sous_la_courbe", "15_j", "16_j", "17_j", "18_j", "19_j", "20_j", "21_j"]

# sns.set(style="ticks", color_codes=True)
# g = sns.pairplot(df[Col_pairplot])


# ## 2. Réduction de dimensions

# In[7]:


import matplotlib.pyplot as plt

fig = plt.figure(1, figsize=(12, 12))

sns.heatmap(round(df[Col_pairplot].corr(),2), annot=True, fmt=".2f")
plt.show()

On supprime la variable df["5_degres_C_TMG_h"]
# In[8]:


df.drop(["5_degres_C_TMG_h"], axis = 1, inplace = True) 


# ### a. ACP compléte      

# In[9]:


#classe pour standardisation(On centre et réduis nos valeurs)
from sklearn.preprocessing import StandardScaler

Col_ACP = ["5_degres_C_TMG_j", "Aire_sous_la_courbe", "15_j", "16_j", "17_j", "18_j", "19_j", "20_j", "21_j"]

#instanciation
sc = StandardScaler()

#transformation – centrage-réduction
df_ACP = sc.fit_transform(df[Col_ACP])


# In[10]:


#vérification

#moyenne
print(np.around(np.mean(df_ACP,axis=0),3))


# In[11]:


#écart-type
print(np.std(df_ACP,axis=0,ddof=0))

Nous sommes maintenant parés pour lancer l’ACP.
# In[12]:


#classe pour l'ACP
from sklearn.decomposition import PCA

#instanciation
acp = PCA(n_components=2)
acp.fit(df_ACP)
#calculs
ACP_complete = acp.transform(df_ACP)

#nombre de composantes calculées
print(acp.n_components_) 


# In[13]:


#proportions de variance associées aux axes
acp.explained_variance_ratio_


# On voit que Les deux premières composantes accapare 95.83% de l’information disponible.

# In[14]:


ACP_complete


# In[15]:


dataframe = pd.DataFrame(ACP_complete, columns=['X1','X2']) 


# In[16]:


Col = ["Bancs", "camera", "zone", "Pop"]
df_ACP_complete = pd.concat([df[Col], dataframe], axis=1)


# In[17]:


df_ACP_complete.head()


# ### a. ACP partielle 

# In[18]:


Col_ACP = ["15_j", "16_j", "17_j", "18_j", "19_j", "20_j", "21_j"]

#instanciation
acp = PCA(n_components=1)

#calculs
acp.fit(df[Col_ACP])
ACP_parielle = acp.transform(df[Col_ACP])

#proportions de variance associées aux axes
print("Proportion de variance associée à la premiére axe:",round(sum(acp.explained_variance_ratio_),4))


# In[19]:


df_partielle = pd.DataFrame(ACP_parielle, columns=['ACP_j']) 
Col = ["Bancs", "camera", "zone", "Pop", "5_degres_C_TMG_j", "Aire_sous_la_courbe"]
df_ACP_parielle = pd.concat([df[Col], df_partielle], axis=1)


# In[20]:


df_ACP_parielle.head()


# In[21]:


fig = plt.figure(1, figsize=(5, 5))

sns.heatmap(round(df_ACP_parielle[["5_degres_C_TMG_j", "Aire_sous_la_courbe", "ACP_j"]].corr(),2), annot=True, fmt=".2f")
plt.show()


# ## 3. Graphes

# ### a. graphe avec une ACP compléte

# In[22]:


Y = ["Bancs", "camera", "zone", "Pop"]
i = 3

sns.set(style="darkgrid")
filled_markers = ('o', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', 'D', 'd', 'P', 'X')
plt.figure(figsize=(12,12))
sns.scatterplot(x="X1", y="X2", hue=Y[i], style = Y[i], palette="Set2", markers=filled_markers, data=df_ACP_complete, s =50)


# ### b. graphe avec une ACP partielle

# In[23]:


df_ACP_parielle.head()


# In[24]:


X = ["5_degres_C_TMG_j", "Aire_sous_la_courbe", "ACP_j"]
Y = ["Bancs", "camera", "zone", "Pop"]
i = 0

sns.set(style="darkgrid")
filled_markers = ('o', '^', '<', '>', '8', 's', 'p', '*', 'h', 'H', 'D', 'd', 'P', 'X')
plt.figure(figsize=(12,12))
sns.scatterplot(x=X[2], y=X[0], hue=Y[i], style = Y[i], palette="Set2", markers=filled_markers, data=df_ACP_parielle, s =50)


# ## 4. Prédictions avec X(G)Boost
Une tranformation des variables est necéssaire. Icic toutes les variables de types 'Object' seront indexées.
# In[25]:


from sklearn.preprocessing import LabelEncoder

def object_to_int(dataframe_series):
    """une simple fonction d'indexation"""
    
    if dataframe_series.dtype=='object':
        dataframe_series = LabelEncoder().fit_transform(dataframe_series)
    return dataframe_series


# In[26]:


Col_X = ["zone", "5_degres_C_TMG_j", "Aire_sous_la_courbe", "15_j", "16_j", "17_j", "18_j", "19_j", "20_j", "21_j"]
Col_Y = ["Bancs", "camera", "Zone", "Pop"]

df = df.apply(lambda x: object_to_int(x))
df[["Bancs", "camera", "zone"]] = df[["Bancs", "camera", "zone"]].astype(int)

df.head()


# In[27]:


from sklearn.model_selection import train_test_split

X = df[Col_X]
y = df[Col_Y[0]]

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.30, random_state = 40)


# In[28]:


df.dtypes


# In[29]:


from sklearn.model_selection import GridSearchCV
from xgboost import XGBClassifier
from time import time

start = time()

parameters = {
    "learning_rate": np.linspace(0, 0.6, 4),
    "gamma": np.linspace(0, 0.6, 4),
    "min_samples_leaf": [0.005, 0.05,0.01],
    "max_depth": [3, 5, 8],
    "max_features": np.linspace(0, 1, 4),
    "n_estimators":[10, 50, 100, 200],
    }


clf_XGB = GridSearchCV(XGBClassifier(), parameters, cv=5, n_jobs=-1,verbose=10)

clf_XGB.fit(X_train, y_train)

end=time()
train_time_xgb=end-start

print("Train: ",clf_XGB.score(X_train, y_train))
print("Test: ",clf_XGB.score(X_test, y_test))
print("\n")
print(clf_XGB.best_params_)


# In[30]:


from sklearn.model_selection import train_test_split

X = df[Col_X]
y = df[Col_Y[1]]

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.30, random_state = 40)

from sklearn.model_selection import GridSearchCV
from xgboost import XGBClassifier
from time import time

start = time()

parameters = {
    "learning_rate": np.linspace(0, 0.6, 4),
    "gamma": np.linspace(0, 0.6, 4),
    "min_samples_leaf": [0.005, 0.05,0.01],
    "max_depth": [3, 5, 8],
    "max_features": np.linspace(0, 1, 4),
    "n_estimators":[10, 50, 100, 200],
    }


clf_XGB = GridSearchCV(XGBClassifier(), parameters, cv=5, n_jobs=-1,verbose=10)

clf_XGB.fit(X_train, y_train)

end=time()
train_time_xgb=end-start

print("Train: ",clf_XGB.score(X_train, y_train))
print("Test: ",clf_XGB.score(X_test, y_test))
print("\n")
print(clf_XGB.best_params_)


# In[31]:


# from sklearn.model_selection import train_test_split

# X = df[Col_X]
# y = df[Col_Y[2]]

# X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.30, random_state = 40)

# from sklearn.model_selection import GridSearchCV
# from xgboost import XGBClassifier
# from time import time

# start = time()

# parameters = {
#     "learning_rate": np.linspace(0, 0.6, 4),
#     "gamma": np.linspace(0, 0.6, 4),
#     "min_samples_leaf": [0.005, 0.05,0.01],
#     "max_depth": [3, 5, 8],
#     "max_features": np.linspace(0, 1, 4),
#     "n_estimators":[10, 50, 100, 200],
#     }


# clf_XGB = GridSearchCV(XGBClassifier(), parameters, cv=5, n_jobs=-1,verbose=10)

# clf_XGB.fit(X_train, y_train)

# end=time()
# train_time_xgb=end-start

# print("Train: ",clf_XGB.score(X_train, y_train))
# print("Test: ",clf_XGB.score(X_test, y_test))
# print("\n")
# print(clf_XGB.best_params_)


# In[32]:


from sklearn.model_selection import train_test_split

X = df[Col_X]
y = df[Col_Y[3]]

X_train, X_test, y_train, y_test = train_test_split(X,y,test_size = 0.30, random_state = 40)

from sklearn.model_selection import GridSearchCV
from xgboost import XGBClassifier
from time import time

start = time()

parameters = {
    "learning_rate": np.linspace(0, 0.6, 4),
    "gamma": np.linspace(0, 0.6, 4),
    "min_samples_leaf": [0.005, 0.05,0.01],
    "max_depth": [3, 5, 8],
    "max_features": np.linspace(0, 1, 4),
    "n_estimators":[10, 50, 100, 200],
    }


clf_XGB = GridSearchCV(XGBClassifier(), parameters, cv=5, n_jobs=-1,verbose=10)

clf_XGB.fit(X_train, y_train)

end=time()
train_time_xgb=end-start

print("Train: ",clf_XGB.score(X_train, y_train))
print("Test: ",clf_XGB.score(X_test, y_test))
print("\n")
print(clf_XGB.best_params_)


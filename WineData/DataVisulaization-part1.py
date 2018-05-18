
# coding: utf-8

# In[1]:


import pandas as pd


# In[2]:


df_original = pd.read_csv('winemag-data-130k-v.csv')


# In[3]:


df_original.head()


# In[7]:


#Bar plot
df_original['country'].value_counts().head(6).plot.bar()


# In[9]:


df_original['designation'].value_counts().head().plot.bar()


# In[12]:


df_original['points'].value_counts().sort_index().plot.bar()


# In[ ]:


# Line chart


# In[14]:


df_original['price'].value_counts().sort_index().plot.line()


# In[15]:


df_original['points'].value_counts().sort_index().plot.line()


# In[16]:


#Area plot


# In[19]:


df_original['points'].value_counts().sort_index().plot.area()


# In[20]:


#Histogram


# In[21]:


df_original['price'].plot.hist()


# In[23]:


df_original[df_original['price'] > 200].plot.hist()


# In[24]:


df_original['points'].plot.hist()


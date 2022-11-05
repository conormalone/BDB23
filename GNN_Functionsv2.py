# -*- coding: utf-8 -*-
import numpy as np
import math
import tensorflow
import spektral
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Model
from spektral.data import Dataset, BatchLoader, Graph, DisjointLoader
from spektral.transforms.normalize_adj import NormalizeAdj
from tensorflow.keras.layers import Dropout
from tensorflow.keras.regularizers import l1
from spektral.layers import GCNConv, GCSConv, GlobalSumPool, GlobalAvgPool, ECCConv, GraphMasking

################################################################################
# Config
################################################################################
learning_rate = 1e-3  # Learning rate
epochs = 100  # Number of training epochs
es_patience = 10  # Patience for early stopping
batch_size = 21  # Batch size
l1_reg = 5e-3
n_out = 1
#local_all = r.all_data
local_test = r.testing_data
local_train = r.training_data
local_val = r.validation_data
local_all = r.all_data
################################################################################
# Load data
################################################################################

class GraphDataset(Dataset):
    def __init__(self, n_samples, df, n_colors=1, **kwargs):
        self.n_samples = n_samples
        self.df = df  
        self.n_colors = n_colors  
        super().__init__(**kwargs)

    def read(self):
        output = []
        for i in range(self.n_samples):
            # Node features
            iter_x = self.df["train_x"][i].copy()
            #print(iter_x)
            x = np.array(iter_x)#.reshape(iter_x.shape)

            # Edges
            iter_a =  self.df["train_a"][i].copy()
            the_length = len(iter_a)
            a = np.array(iter_a).reshape(the_length,the_length)
            #
            y = int(self.df["y"][i])
            
           
            output.append(Graph(x=x, a=a, y=y))
        return(output)

        # We must return a list of Graph objects

 #
# Train/valid/test split
len_train = len(local_train["train_x"])
len_val = len(local_val["train_x"])
len_test = len(local_test["train_x"])
len_all = len(local_all["train_x"])
data_tr = GraphDataset(n_samples = len_train, df = local_train, transforms=NormalizeAdj())
data_va = GraphDataset(n_samples = len_val, df = local_val, transforms=NormalizeAdj())
data_te = GraphDataset(n_samples = len_test, df=local_test, transforms=NormalizeAdj())
data_all = GraphDataset(n_samples = len_all, df=local_all, transforms=NormalizeAdj())

# Data loaders
loader_tr = BatchLoader(data_tr, batch_size=batch_size, epochs=epochs, mask=True)
loader_va = BatchLoader(data_va, batch_size=batch_size, mask=True)
loader_te = BatchLoader(data_te, batch_size=batch_size, mask=True)
loader_all = BatchLoader(data_all, batch_size=batch_size, mask=True)


################################################################################
# Build model
################################################################################
class BDB22GNN(Model):

     def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.conv1 = GCNConv(128, activation="relu", kernel_regularizer=l1(l1_reg))
        self.conv2 = GCSConv(64, activation="relu", kernel_regularizer=l1(l1_reg))
        self.flatten = GlobalSumPool()
        self.fc1 = Dense(32, activation="relu")
        self.fc2 = Dense(1, activation="sigmoid")  

     def call(self, inputs):
        x, a = inputs
        x = self.conv1([x, a])
        x = self.conv2([x, a])
        output = self.flatten(x)
        output = self.fc1(output)
        output = self.fc2(output)
        return output

    


model = BDB22GNN()
model.compile('adam', "binary_crossentropy","binary_accuracy")

model.fit(loader_tr.load(), validation_data= loader_va.load(), steps_per_epoch=loader_tr.steps_per_epoch,
    validation_steps=loader_va.steps_per_epoch, epochs=100)

test_loss = model.evaluate(loader_te.load(), steps=loader_te.steps_per_epoch)

print('Test loss: {}'.format(test_loss))

predictions = model.predict(loader_all.load(), steps =loader_all.steps_per_epoch)

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
from spektral.layers import GCNConv, GlobalSumPool

################################################################################
# Config
################################################################################
learning_rate = 1e-3  # Learning rate
epochs = 100  # Number of training epochs
es_patience = 10  # Patience for early stopping
batch_size = 21  # Batch size
#local_all = r.all_data
#local_test = r.testing_data
local_train = r.training_data
local_val = r.validation_data
local_all = r.all_data
################################################################################
# Load data
################################################################################

class GraphDataset(Dataset):
    def __init__(self, n_samples, df, n_colors=21, **kwargs):
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
            y = np.zeros((21,))
            y_index = int(1+self.df["y"][i])
            y[:y_index] = 1
            
           
            output.append(Graph(x=x, a=a, y=y))
        return(output)

        # We must return a list of Graph objects

 #
# Train/valid/test split
len_train = len(local_train["train_x"])
len_val = len(local_val["train_x"])
#len_test = len(local_test["train_x"])
len_all = len(local_all["train_x"])
data_tr = GraphDataset(n_samples = len_train, df = local_train, transforms=NormalizeAdj())
data_va = GraphDataset(n_samples = len_val, df = local_val, transforms=NormalizeAdj())
#data_te = GraphDataset(n_samples = len_test, df=local_test, transforms=NormalizeAdj())
data_all = GraphDataset(n_samples = len_all, df=local_all, transforms=NormalizeAdj())

# Data loaders
loader_tr = BatchLoader(data_tr, batch_size=batch_size, epochs=epochs)
loader_va = BatchLoader(data_va, batch_size=batch_size)
#loader_te = BatchLoader(data_te, batch_size=batch_size)
loader_all = BatchLoader(data_all, batch_size=batch_size)


################################################################################
# Build model
################################################################################
class BDB22GNN(Model):

    def __init__(self, n_hidden, n_labels):
        super().__init__()
        self.graph_conv = GCNConv(n_hidden)
        self.pool = GlobalSumPool()
        self.dropout = Dropout(0.5)
        self.dense = Dense(n_labels, 'sigmoid')

    def call(self, inputs):
        out = self.graph_conv(inputs)
        out = self.dropout(out)
        out = self.pool(out)
        out = self.dense(out)

        return out


model = BDB22GNN(200, 21)
model.compile('adam', "mean_absolute_error")

model.fit(loader_tr.load(), validation_data= loader_va.load(), steps_per_epoch=loader_tr.steps_per_epoch,
    validation_steps=loader_va.steps_per_epoch, epochs=100)

#test_loss = model.evaluate(loader_te.load(), steps=loader_te.steps_per_epoch)

#print('Test loss: {}'.format(test_loss))

predictions = model.predict(loader_all.load(), steps =loader_all.steps_per_epoch)

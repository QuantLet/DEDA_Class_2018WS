#!/usr/bin/env python
# coding: utf-8

# In[19]:


import random
import os
import sys
import tensorflow as tf
os.environ['KERAS_BACKEND'] = 'tensorflow' 
import numpy as np
from keras.models import load_model
from keras.models import Sequential
from keras.layers import Embedding, LSTM, Dense, Dropout, CuDNNLSTM
from keras.callbacks import LambdaCallback
from keras.optimizers import RMSprop
from collections import defaultdict, Counter


# In[13]:


print('This script will run first the Markov Chain and then the LSTM model to generate Donald Trump tweets.\nRun the Jupyter notebook if you want to use just one or the other \n- or if you wannt to play around with state length/seed text/temperature etc.\nThe training part is commented out to prevent you from accidentally running it. It will take hours per epoch on a CPU, so make sure you run it on a powerful GPU. If your GPU is tensorflow compatible, use CuDNNLSTM instead of LSTM.')


# In[17]:


## import and prepare text
# Import Text > can be replaced with your own text, but then you need to train the model from scratch.
file= open('TRUMPTWEETS.txt',  encoding="utf8") #taken from http://www.trumptwitterarchive.com/
text = file.read()
file.close()
print(len(text))
rawtxt=text.encode("utf8").decode("ascii",'ignore')
text=text[:2145192] #drop half the text to speed up everything.


# adapted from https://github.com/keras-team/keras/blob/master/examples/lstm_text_generation.py
# Create a list of Chars
chars = sorted(list(set(text[:2145192])))
print('total unique chars:', len(chars),'\ncharacters:\n', chars)
char_indices = dict((c, i) for i, c in enumerate(chars))
indices_char = dict((i, c) for i, c in enumerate(chars))

# cut the text in semi-redundant sequences of maxlen characters
maxlen = 50
step = 4
sentences = []
next_chars = []
for i in range(0, len(text) - maxlen, step):
    sentences.append(text[i: i + maxlen])
    next_chars.append(text[i + maxlen])
print('sequences:', len(sentences))

#vectorise
print('Vectorization... \nThis can take a moment.')
x = np.zeros((len(sentences), maxlen, len(chars)), dtype=np.bool)
y = np.zeros((len(sentences), len(chars)), dtype=np.bool)
for i, sentence in enumerate(sentences):
    for t, char in enumerate(sentence):
        x[i, t, char_indices[char]] = 1
    y[i, char_indices[next_chars[i]]] = 1
print('Done.\nReady to generate text and further train the model.')


# In[21]:


# Markov chain
#'Training'
#taken from:
#https://eli.thegreenplace.net/2018/elegant-python-code-for-a-markov-chain-text-generator/
STATE_LEN = 5
print('\nMARKOV CHAIN\n\nCreating the model with state length = {}.\nYou can edit STATE_LEN in the script.\n lower numbers yield more creative but messier results, higher numbers yield cleaner results but mostly regurgitate the training data.'.format(STATE_LEN))

data = rawtxt
model = defaultdict(Counter)

print('Learning model...')
for i in range(len(data) - STATE_LEN):
    state = data[i:i + STATE_LEN]
    next = data[i + STATE_LEN]
    model[state][next] += 1
print('Done.')

#  generation
statelist = ['Crooked Hillary', 'Fake News', 'China ', 'Robert Muller', 'We'] # you can add your own see text here.
for state in statelist:
    if len(state)>=STATE_LEN:
        state=state[:STATE_LEN]
        print('Starting state: "{}"'.format(state))
        out = list(state)
        for i in range(250):
            out.extend(random.choices(list(model[state]), model[state].values()))
            state = state[1:] + out[-1]
        print(''.join(out),'\n')


# In[4]:


# build the model: a two-layer LSTM
print('\n\nLSTM\nBuilding LSTM model...')
model = Sequential()
model.add(LSTM(250,return_sequences=True, input_shape=(maxlen, len(chars))))
model.add(Dropout(rate=0.1))
model.add(LSTM(250))
model.add(Dropout(rate=0.1))
model.add(Dense(len(chars), activation='softmax'))
optimizer = RMSprop(lr=0.005)
print('Compiling ...')
model.compile(loss='categorical_crossentropy', optimizer=optimizer, metrics=['accuracy'])
print('Done!')
#load weights
model.load_weights('TRUMPWEIGHTS.h5')
print('Weights loaded.')


# In[9]:


def sample(preds, temperature=1.0):
    # helper function to sample an index from a probability array
    preds = np.asarray(preds).astype('float64')
    preds = np.log(preds+0.000000000000001) / temperature
    exp_preds = np.exp(preds)
    preds = exp_preds / np.sum(exp_preds)
    probas = np.random.multinomial(1, preds, 1)
    return np.argmax(probas)
def gen_text():
    #adapted from https://github.com/keras-team/keras/blob/master/examples/lstm_text_generation.py
    start_index = random.randint(0, len(text) - maxlen - 1)
    for diversity in [0.008,0.2, 0.5, 1.0, 1.2]:
        print('\n----- diversity:', diversity)

        generated = ''
        sentence = text[start_index: start_index + maxlen]
        generated += sentence
        print('----- Generating with seed: "' + sentence + '"')
        sys.stdout.write(generated)

        for i in range(300):
            x_pred = np.zeros((1, maxlen, len(chars)))
            for t, char in enumerate(sentence):
                x_pred[0, t, char_indices[char]] = 1.

            preds = model.predict(x_pred, verbose=0)[0]
            next_index = sample(preds, diversity)
            next_char = indices_char[next_index]

            generated += next_char
            sentence = sentence[1:] + next_char

            sys.stdout.write(next_char)
            sys.stdout.flush()
    print()


# In[10]:


print('Text generation with a random starter seed:\n')
gen_text()
print('\nText generation with another random starter:')
gen_text()
print('DONE.')


# In[ ]:


# define print out call back
def on_epoch_end(epoch, _):
    if epoch == 1 or epoch%2==0:
            # Function invoked at end of each epoch. Prints generated text.
            print()
            print('----- Generating text after Epoch: %d' % epoch)

            start_index = random.randint(0, len(text) - maxlen - 1)
            for diversity in [0.1, 0.5, 1.0, 1.4]:
                print('----- diversity:', diversity)
                generated = ''
                sentence = text[start_index: start_index + maxlen]
                #sentence = starter_seq[:]
                generated += sentence
                print('----- Generating with seed: "' + sentence + '"-----')
                sys.stdout.write(generated)

                for i in range(1400):
                    x_pred = np.zeros((1, maxlen, len(chars)))
                    for t, char in enumerate(sentence):
                        x_pred[0, t, char_indices[char]] = 1.

                    preds = model.predict(x_pred, verbose=0)[0]
                    next_index = sample(preds, diversity)
                    next_char = indices_char[next_index]

                    generated += next_char
                    sentence = sentence[1:] + next_char

                    sys.stdout.write(next_char)
                    sys.stdout.flush()
                print()

print_callback = LambdaCallback(on_epoch_end=on_epoch_end)
print('\nDone.\nModel training would start here. Uncomment the code to train it further.')
# train model some more
## if you're not a GPU, this will take a very, very long time
# model.fit(x, y,
#           batch_size=1024,
#           epochs=10,
#           verbose=1,
#           callbacks=[print_callback,tf.keras.callbacks.ModelCheckpoint('{epoch:02d}-{loss:.2f}.hdf5', monitor='loss', period=2)])


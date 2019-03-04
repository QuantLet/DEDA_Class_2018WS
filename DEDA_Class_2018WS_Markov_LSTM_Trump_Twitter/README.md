[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DEDA_WS201819_MARKOV_Chains_LSTM_Trump_Tweets** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet:  DEDA_WS201819_MARKOV_Chains_LSTM_Trump_Tweets
 


Published in:      "Digital Economy and Decision Analytics (WS 18/19)"

  

Description:       "Uses Markov Chains and LSTMs to generate new text in the style of some input corpus. Includes the weights for a pretrained LSTM model for Donald Trump tweets. Both techniques can in principle be used for any kind of natural language. The LSTM model should also be able to deal with highly structured text such as computer code - but that might require more nodes per layer and more training time. If you get an error, make sure you have both tensorflow installed and numpy upgraded to the latest version."

 

Keywords:          "LSTM, Markov Chains, text generation, Twitter, NLP"


Author:            Justin Engelmann

  

Submitted:         Wed, Feb 20 2019 by Justin Engelmann

  

Datafile:          Jupyter notebook, py script, model weights, trump tweets (txt)

```

### IPYNB Code
```ipynb

{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 19,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Requirement already satisfied: h5py in c:\\users\\justi\\anaconda3\\lib\\site-packages (2.8.0)\n",
      "Requirement already satisfied: pyyaml in c:\\users\\justi\\anaconda3\\lib\\site-packages (3.13)\n",
      "Requirement already satisfied: numpy>=1.7 in c:\\users\\justi\\anaconda3\\lib\\site-packages (from h5py) (1.16.1)\n",
      "Requirement already satisfied: six in c:\\users\\justi\\anaconda3\\lib\\site-packages (from h5py) (1.11.0)\n"
     ]
    },
    {
     "name": "stderr",
     "output_type": "stream",
     "text": [
      "You are using pip version 18.1, however version 19.0.3 is available.\n",
      "You should consider upgrading via the 'python -m pip install --upgrade pip' command.\n"
     ]
    }
   ],
   "source": [
    "import random\n",
    "import os\n",
    "import sys\n",
    "import tensorflow as tf\n",
    "os.environ['KERAS_BACKEND'] = 'tensorflow' \n",
    "!pip install h5py pyyaml \n",
    "import numpy as np\n",
    "from keras.models import load_model\n",
    "from keras.models import Sequential\n",
    "from keras.layers import Embedding, LSTM, Dense, Dropout, CuDNNLSTM\n",
    "from keras.callbacks import LambdaCallback\n",
    "from keras.optimizers import RMSprop\n",
    "from collections import defaultdict, Counter"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "This script will run first the Markov Chain and then the LSTM model to generate Donald Trump tweets.\n",
      "Run the Jupyter notebook if you want to use just one or the other \n",
      "- or if you wannt to play around with state length/seed text/temperature etc.\n",
      "The training part is commented out to prevent you from accidentally running it. It will take hours per epoch on a CPU, so make sure you run it on a powerful GPU. If your GPU is tensorflow compatible, use CuDNNLSTM instead of LSTM.\n"
     ]
    }
   ],
   "source": [
    "print('This script will run first the Markov Chain and then the LSTM model to generate Donald Trump tweets.\\nRun the Jupyter notebook if you want to use just one or the other \\n- or if you wannt to play around with state length/seed text/temperature etc.\\nThe training part is commented out to prevent you from accidentally running it. It will take hours per epoch on a CPU, so make sure you run it on a powerful GPU. If your GPU is tensorflow compatible, use CuDNNLSTM instead of LSTM.')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 17,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "4290385\n",
      "total unique chars: 92 \n",
      "characters:\n",
      " ['\\n', ' ', '!', '\"', '#', '$', '%', '&', \"'\", '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '=', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', ']', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~']\n",
      "sequences: 536286\n",
      "Vectorization... \n",
      "This can take a moment.\n",
      "Done.\n",
      "Ready to generate text and further train the model.\n"
     ]
    }
   ],
   "source": [
    "## import and prepare text\n",
    "# Import Text > can be replaced with your own text, but then you need to train the model from scratch.\n",
    "file= open('TRUMPTWEETS.txt',  encoding=\"utf8\") #taken from http://www.trumptwitterarchive.com/\n",
    "text = file.read()\n",
    "file.close()\n",
    "print(len(text))\n",
    "rawtxt=text.encode(\"utf8\").decode(\"ascii\",'ignore')\n",
    "text=text[:2145192] #drop half the text to speed up everything.\n",
    "\n",
    "\n",
    "# adapted from https://github.com/keras-team/keras/blob/master/examples/lstm_text_generation.py\n",
    "# Create a list of Chars\n",
    "chars = sorted(list(set(text[:2145192])))\n",
    "print('total unique chars:', len(chars),'\\ncharacters:\\n', chars)\n",
    "char_indices = dict((c, i) for i, c in enumerate(chars))\n",
    "indices_char = dict((i, c) for i, c in enumerate(chars))\n",
    "\n",
    "# cut the text in semi-redundant sequences of maxlen characters\n",
    "maxlen = 50\n",
    "step = 4\n",
    "sentences = []\n",
    "next_chars = []\n",
    "for i in range(0, len(text) - maxlen, step):\n",
    "    sentences.append(text[i: i + maxlen])\n",
    "    next_chars.append(text[i + maxlen])\n",
    "print('sequences:', len(sentences))\n",
    "\n",
    "#vectorise\n",
    "print('Vectorization... \\nThis can take a moment.')\n",
    "x = np.zeros((len(sentences), maxlen, len(chars)), dtype=np.bool)\n",
    "y = np.zeros((len(sentences), len(chars)), dtype=np.bool)\n",
    "for i, sentence in enumerate(sentences):\n",
    "    for t, char in enumerate(sentence):\n",
    "        x[i, t, char_indices[char]] = 1\n",
    "    y[i, char_indices[next_chars[i]]] = 1\n",
    "print('Done.\\nReady to generate text and further train the model.')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 21,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "\n",
      "MARKOV CHAIN\n",
      "\n",
      "Creating the model with state length = {}.\n",
      "You can edit STATE_LEN in the script.\n",
      " lower numbers yield more creative but messy results, higher numbers yield cleaner results but mostly reguritate the training data.\n",
      "Learning model...\n",
      "Done.\n",
      "Starting state: \"Crook\"\n",
      "Crooked Hillary Vets; fact than I remark tank. She chosen little serve. I have anothers To Rave Run for all abuse probe is nothing to believing news has ZERO marginal and is going OUR FIRED\"\"  Thanks!|@MarkEastonishington D.C. is of Florida http://t.co/Xz \n",
      "\n",
      "Starting state: \"Fake \"\n",
      "Fake Newsmax_Media not be dislike to be allowers LIKE IT AND DESPERATELY escort to say Obama setbacks to JerryJrFalwell the opposed on The judges. WRONG law so that you have been 3.9% in nerve\" deserve our naval Achieving to go Eric Schneiderman @KarlRove \n",
      "\n",
      "Starting state: \"China\"\n",
      "China has president  MAKE DEAL|China would be they wasn't seems on @BarackObama abandon that them for you an also promised on CSN:@CHICKatCSN + @Bmitched ISIS should you and boring) but now. NeverHillarys husband want to start toughes @realDonaldTrump Int \n",
      "\n",
      "Starting state: \"Rober\"\n",
      "RobertUndefine more not the sneak politicizing during careful!|Thank you can House S https://t.co/nsPDXFTs|My visit Trump @Trump @Foxnews foreign land! http://t.co/3qSUVRaf6f via @kmbznews|I had at 3% .You'll not purchased fantastic Sec Kerry disgustin Tr \n",
      "\n"
     ]
    }
   ],
   "source": [
    "# Markov chain\n",
    "#'Training'\n",
    "#taken from:\n",
    "#https://eli.thegreenplace.net/2018/elegant-python-code-for-a-markov-chain-text-generator/\n",
    "STATE_LEN = 5\n",
    "print('\\nMARKOV CHAIN\\n\\nCreating the model with state length = {}.\\nYou can edit STATE_LEN in the script.\\n lower numbers yield more creative but messy results, higher numbers yield cleaner results but mostly reguritate the training data.')\n",
    "\n",
    "data = rawtxt\n",
    "model = defaultdict(Counter)\n",
    "\n",
    "print('Learning model...')\n",
    "for i in range(len(data) - STATE_LEN):\n",
    "    state = data[i:i + STATE_LEN]\n",
    "    next = data[i + STATE_LEN]\n",
    "    model[state][next] += 1\n",
    "print('Done.')\n",
    "\n",
    "#  generation\n",
    "statelist = ['Crooked Hillary', 'Fake News', 'China ', 'Robert Muller', 'We'] # you can add your own see text here.\n",
    "for state in statelist:\n",
    "    if len(state)>=STATE_LEN:\n",
    "        state=state[:STATE_LEN]\n",
    "        print('Starting state: \"{}\"'.format(state))\n",
    "        out = list(state)\n",
    "        for i in range(250):\n",
    "            out.extend(random.choices(list(model[state]), model[state].values()))\n",
    "            state = state[1:] + out[-1]\n",
    "        print(''.join(out),'\\n')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Build model...\n",
      "WARNING:tensorflow:From C:\\Users\\justi\\Anaconda3\\lib\\site-packages\\tensorflow\\python\\framework\\op_def_library.py:263: colocate_with (from tensorflow.python.framework.ops) is deprecated and will be removed in a future version.\n",
      "Instructions for updating:\n",
      "Colocations handled automatically by placer.\n",
      "WARNING:tensorflow:From C:\\Users\\justi\\Anaconda3\\lib\\site-packages\\keras\\backend\\tensorflow_backend.py:3445: calling dropout (from tensorflow.python.ops.nn_ops) with keep_prob is deprecated and will be removed in a future version.\n",
      "Instructions for updating:\n",
      "Please use `rate` instead of `keep_prob`. Rate should be set to `rate = 1 - keep_prob`.\n",
      "Compiling ...\n",
      "Done!\n"
     ]
    }
   ],
   "source": [
    "# build the model: a two-layer LSTM\n",
    "print('\\n\\nLSTM\\nBuilding LSTM model...')\n",
    "model = Sequential()\n",
    "model.add(LSTM(250,return_sequences=True, input_shape=(maxlen, len(chars))))\n",
    "model.add(Dropout(rate=0.1))\n",
    "model.add(LSTM(250))\n",
    "model.add(Dropout(rate=0.1))\n",
    "model.add(Dense(len(chars), activation='softmax'))\n",
    "optimizer = RMSprop(lr=0.005)\n",
    "print('Compiling ...')\n",
    "model.compile(loss='categorical_crossentropy', optimizer=optimizer, metrics=['accuracy'])\n",
    "print('Done!')\n",
    "#load weights\n",
    "model.load_weights('TRUMPWEIGHTS.h5')\n",
    "print('Weights loaded.')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "metadata": {},
   "outputs": [],
   "source": [
    "def sample(preds, temperature=1.0):\n",
    "    # helper function to sample an index from a probability array\n",
    "    preds = np.asarray(preds).astype('float64')\n",
    "    preds = np.log(preds+0.000000000000001) / temperature\n",
    "    exp_preds = np.exp(preds)\n",
    "    preds = exp_preds / np.sum(exp_preds)\n",
    "    probas = np.random.multinomial(1, preds, 1)\n",
    "    return np.argmax(probas)\n",
    "def gen_text():\n",
    "    #adapted from https://github.com/keras-team/keras/blob/master/examples/lstm_text_generation.py\n",
    "    start_index = random.randint(0, len(text) - maxlen - 1)\n",
    "    for diversity in [0.008,0.2, 0.5, 1.0, 1.2]:\n",
    "        print('\\n----- diversity:', diversity)\n",
    "\n",
    "        generated = ''\n",
    "        sentence = text[start_index: start_index + maxlen]\n",
    "        generated += sentence\n",
    "        print('----- Generating with seed: \"' + sentence + '\"')\n",
    "        sys.stdout.write(generated)\n",
    "\n",
    "        for i in range(300):\n",
    "            x_pred = np.zeros((1, maxlen, len(chars)))\n",
    "            for t, char in enumerate(sentence):\n",
    "                x_pred[0, t, char_indices[char]] = 1.\n",
    "\n",
    "            preds = model.predict(x_pred, verbose=0)[0]\n",
    "            next_index = sample(preds, diversity)\n",
    "            next_char = indices_char[next_index]\n",
    "\n",
    "            generated += next_char\n",
    "            sentence = sentence[1:] + next_char\n",
    "\n",
    "            sys.stdout.write(next_char)\n",
    "            sys.stdout.flush()\n",
    "    print()"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "\n",
      "----- diversity: 0.008\n",
      "----- Generating with seed: \"ftyWurker: Some  Democrats think Rand Paul is a th\"\n",
      "ftyWurker: Some  Democrats think Rand Paul is a thoughts to the most and the United States with the Fake News News New York and the United States with the FBI and the United States will be interviewed by the United States will be interviewed by the United States will be interviewed by the Fake News Media will be a great and the polls are the best w\n",
      "----- diversity: 0.2\n",
      "----- Generating with seed: \"ftyWurker: Some  Democrats think Rand Paul is a th\"\n",
      "ftyWurker: Some  Democrats think Rand Paul is a thought in the FBI and the statement of the U.S. and the Fake News News is a long to the @FoxNews is a great deals and the United States and the Democrats are what the world in the Democrats are with the U.S. and the Fake News News will be the record to the U.S. and the story of the Fake News Media wi\n",
      "----- diversity: 0.5\n",
      "----- Generating with seed: \"ftyWurker: Some  Democrats think Rand Paul is a th\"\n",
      "ftyWurker: Some  Democrats think Rand Paul is a though that have been for Trump Country and I will get a great confirment. The results on Federal man. Border on the Great Again NOT to support of the people of the success of the Florida. The problem! Brodict of the Trump and all of the @NBCNews in the media must be protection of the investigation of\n",
      "----- diversity: 1.0\n",
      "----- Generating with seed: \"ftyWurker: Some  Democrats think Rand Paul is a th\"\n",
      "ftyWurker: Some  Democrats think Rand Paul is a though forward to help a talk your undersonciled - waste!|And Ted Cruz is including to stop it the will allow future show. The Wall never strelege massive pricined from fight he look liesh track! YOU!|Look negotiation leaders are more at the INs fow #bencarance?|My a center for never been top on Evect\n",
      "----- diversity: 1.2\n",
      "----- Generating with seed: \"ftyWurker: Some  Democrats think Rand Paul is a th\"\n",
      "ftyWurker: Some  Democrats think Rand Paul is a thouse giving show! dishonest. He are is form for else(spiomco what the haver stion from Nevada OSCO on Trump!|I will be my ban of PLP guy! https://t.co/CYFQCexCyI|Joinel Security sures that you one tomorrow clown's Jiif Path to Shy to yet for our Billions! Lungire much barriagate almost overace,03-03\n",
      "\n",
      "----- diversity: 0.008\n",
      "----- Generating with seed: \". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantas\"\n",
      ". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantastic that the United States will be interviewed by the United States will be interviewed by the United States will be interviewed by the Fake News New York and the United States will be interviewed by the Fake News New York and the United States will be interviewed by the United States will be interv\n",
      "----- diversity: 0.2\n",
      "----- Generating with seed: \". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantas\"\n",
      ". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantastic job of the Fake News a country will be interviewed by the United States will be interviewed by the world in the world in the United States and the most be a great and the world will be interviewed by the Fake News News Carolina and the United States will be interviewed by the U.S. and he is the \n",
      "----- diversity: 0.5\n",
      "----- Generating with seed: \". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantas\"\n",
      ". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantastic and vote for White House. If the border and strong report &amp; worse the media did the United States to the lies of the Dems will be being all of the polls between any the First Schooked and the than a very good of America will be totally doing supporters! #Trump2016|@Grannelles @realDonaldTrum\n",
      "----- diversity: 1.0\n",
      "----- Generating with seed: \". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantas\"\n",
      ". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantasting you for me. Thank you Well 19 skillers. No bosines....|If a dishonest. Going Ression. Shown deperty to angry.|@adcredwsonmers:  @jedjonxBruduarader-&amp; @BretherledFAle belled words is so is beautiful has biggest of pushing hard!|... people a really all of the See President Trump's in more!|Bi\n",
      "----- diversity: 1.2\n",
      "----- Generating with seed: \". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantas\"\n",
      ". Vote @HawleyMO! https://t.co/tn2zsEWQJ5|A fantastic on going at the who just leld respected how became.|For 40sO-Dorul over $8005000: #MakeAmericaGreatAgain #SupariNew since WELLUDO!|RepuClITR &amp; join. Is to con are notch today to rebuild Mr. Trump cilled with Bruerle FBINFigutelin time!! It as BobzY  Thin was more!!|@mekierBabisDos: M.!! Watc\n"
     ]
    }
   ],
   "source": [
    "print('Text generation with a random starter seed:\\n')\n",
    "gen_text()\n",
    "print('\\nText generation with another random starter:')\n",
    "gen_text()\n",
    "print('DONE.')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "WARNING:tensorflow:From C:\\Users\\justi\\Anaconda3\\lib\\site-packages\\tensorflow\\python\\ops\\math_ops.py:3066: to_int32 (from tensorflow.python.ops.math_ops) is deprecated and will be removed in a future version.\n",
      "Instructions for updating:\n",
      "Use tf.cast instead.\n",
      "Epoch 1/1\n",
      "  3072/536286 [..............................] - ETA: 1:00:19 - loss: 1.3991 - acc: 0.5980"
     ]
    }
   ],
   "source": [
    "# define print out call back\n",
    "def on_epoch_end(epoch, _):\n",
    "    if epoch == 1 or epoch%2==0:\n",
    "            # Function invoked at end of each epoch. Prints generated text.\n",
    "            print()\n",
    "            print('----- Generating text after Epoch: %d' % epoch)\n",
    "\n",
    "            start_index = random.randint(0, len(text) - maxlen - 1)\n",
    "            for diversity in [0.1, 0.5, 1.0, 1.4]:\n",
    "                print('----- diversity:', diversity)\n",
    "                generated = ''\n",
    "                sentence = text[start_index: start_index + maxlen]\n",
    "                #sentence = starter_seq[:]\n",
    "                generated += sentence\n",
    "                print('----- Generating with seed: \"' + sentence + '\"-----')\n",
    "                sys.stdout.write(generated)\n",
    "\n",
    "                for i in range(1400):\n",
    "                    x_pred = np.zeros((1, maxlen, len(chars)))\n",
    "                    for t, char in enumerate(sentence):\n",
    "                        x_pred[0, t, char_indices[char]] = 1.\n",
    "\n",
    "                    preds = model.predict(x_pred, verbose=0)[0]\n",
    "                    next_index = sample(preds, diversity)\n",
    "                    next_char = indices_char[next_index]\n",
    "\n",
    "                    generated += next_char\n",
    "                    sentence = sentence[1:] + next_char\n",
    "\n",
    "                    sys.stdout.write(next_char)\n",
    "                    sys.stdout.flush()\n",
    "                print()\n",
    "\n",
    "print_callback = LambdaCallback(on_epoch_end=on_epoch_end)\n",
    "print('Model training would start here. Uncomment the code to train it further.')\n",
    "# train model some more\n",
    "## if you're not a GPU, this will take a very, very long time\n",
    "# model.fit(x, y,\n",
    "#           batch_size=1024,\n",
    "#           epochs=10,\n",
    "#           verbose=1,\n",
    "#           callbacks=[print_callback,tf.keras.callbacks.ModelCheckpoint('{epoch:02d}-{loss:.2f}.hdf5', monitor='loss', period=2)])"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.7.0"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 2
}

```

automatically created on 2019-02-22

### PYTHON Code
```python

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


```

automatically created on 2019-02-22
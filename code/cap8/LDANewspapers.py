# -*- coding: utf-8 -*-
"""
Created on Sat May  2 20:06:18 2015
What are newspapers talking about
@author: JJimenez
"""

import urllib2
import numpy as np
from sklearn import feature_extraction
import lda
from bs4 import BeautifulSoup
import re

newspapers = ['http://www.bbc.com/news', 'http://www.ft.com/home/uk',
              'http://www.nytimes.com/', 'http://www.chicagotribune.com/',
              'http://www.washingtonpost.com/']

texts = []
              
for web in newspapers:
    parse = urllib2.urlopen(web)
    soup = BeautifulSoup(parse)
    texto = soup.get_text()
    texto = texto.replace("\t", "").replace("\r", "").replace("\n", "")
    texto = re.sub('[^a-zA-Z]'," ", texto)
    texts.append(texto)
    


paravocab = feature_extraction.text.CountVectorizer(stop_words = 'english',
                                              max_features = 1000).fit(texts)

vocab = paravocab.vocabulary_
vocab = vocab.keys()
vocab = list(vocab)
vocab.sort()

matriz = feature_extraction.text.CountVectorizer(stop_words = 'english',
                                                 vocabulary = vocab).fit_transform(texts)

import lda

model = lda.LDA(n_topics=10, n_iter=1500, random_state=1)
model.fit(matriz)

topic_word = model.topic_word_
n_top_words = 10

for i, topic_dist in enumerate(topic_word):
    topic_words = np.array(vocab)[np.argsort(topic_dist)][:-n_top_words:-1]
    print('Topic {}: {}'.format(i, ' '.join(topic_words)))

#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Sun May  3 01:43:34 2015
Book recurrent themes
@author: JJimenez
"""
import urllib2
import numpy as np
from sklearn import feature_extraction
import lda
from bs4 import BeautifulSoup
import re

titles = ['Pride and Prejudice', 'Sense and Sensibility', 'Oliver Twist',
          'Christmas Carol', 'Moby Dick']
books = ['https://www.gutenberg.org/files/42671/42671-h/42671-h.htm',
         'https://www.gutenberg.org/files/21839/21839-h/21839-h.htm',
         'https://www.gutenberg.org/files/730/730-h/730-h.htm',
         'https://www.gutenberg.org/files/46/46-h/46-h.htm',
         'https://www.gutenberg.org/files/2701/2701-h/2701-h.htm']
         
texts = []
              
for link in books:
    parse = urllib2.urlopen(link)
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

model = lda.LDA(n_topics=10, n_iter=1500, random_state=1)
model.fit(matriz)

topic_word = model.topic_word_
n_top_words = 10

for i, topic_dist in enumerate(topic_word):
    topic_words = np.array(vocab)[np.argsort(topic_dist)][:-n_top_words:-1]
    print('Topic {}: {}'.format(i, ' '.join(topic_words)))
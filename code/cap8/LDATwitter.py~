#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Latent Dirichlet Allocation
Twitter Crawler

@author: hawk31
"""
import numpy as np

access_token = "104187533-wpwRqx0eJdMPe9XEFH9QNhakWZTqcmRrkYfAgO35"
access_token_secret = "uadtTH6biyw0MYrvSyT4FQpFC06Zjd4KBpvp9bHmSeIPx"
consumer_key = "RIGYultpNAe932VEikT4zHSfe"
consumer_secret = "ZmvXSYGbiGxpHHHe5QDeqdWIOMTvShbphXlq6DkjmDZnQ9ZTwv"

import tweepy

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

public_tweets = api.user_timeline('elonmusk', count = 1000)

texts = []
for tweet in public_tweets:
    texts.append(tweet.text)

from sklearn import feature_extraction

                                              
paravocab = feature_extraction.text.CountVectorizer(stop_words = 'english',
                                              max_features = 50).fit(texts)

vocab = paravocab.vocabulary_
vocab = vocab.keys()
vocab = list(vocab)
vocab.sort()

matriz = feature_extraction.text.CountVectorizer(stop_words = 'english',
                                                 vocabulary = vocab).fit_transform(texts)

import lda

model = lda.LDA(n_topics=5, n_iter=1500, random_state=1)
model.fit(matriz)

topic_word = model.topic_word_
n_top_words = 5

for i, topic_dist in enumerate(topic_word):
    topic_words = np.array(vocab)[np.argsort(topic_dist)][:-n_top_words:-1]
    print('Topic {}: {}'.format(i, ' '.join(topic_words)))

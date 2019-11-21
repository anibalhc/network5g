import pandas as pd 
import csv

df = pd.read_csv(r"df_text2.csv", encoding="cp1252")

df.head()

import os
os.environ["GOOGLE_APPLICATION_CREDENTIALS"] = "file.json"

#######################################################
# Imports the Google Cloud client library
from google.cloud import language
from google.cloud.language import enums
from google.cloud.language import types

# Instantiates a client
client = language.LanguageServiceClient()

# The text to analyze
v_score = []
v_magnitude= []
i = 0
while True:
    text = str(df.iloc[i][0])
    document = types.Document(
        content=text,
        type=enums.Document.Type.PLAIN_TEXT)

    # Detects the sentiment of the text
    sentiment = client.analyze_sentiment(document=document).document_sentiment
    v_score.append(sentiment.score)
    v_magnitude.append(sentiment.magnitude)
    #print('Text: {}'.format(text))
    #print('Sentiment: {}, {}'.format(sentiment.score, sentiment.magnitude))
    i = i + 1
    if(i >= len(df)):
        break

#######################################################
with open('data_score.csv','w') as csvFile:
    writer = csv.writer(csvFile)
    writer.writerows(map(lambda x: [x], v_score))
csvFile.close()

with open('data_magnitude.csv','w') as csvFile:
    writer = csv.writer(csvFile)
    writer.writerows(map(lambda x: [x], v_magnitude))
csvFile.close()
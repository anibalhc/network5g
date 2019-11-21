#libraries
import pandas as pd
from nltk.corpus import stopwords

#load file
url = 'df_text.csv'

df = pd.read_csv(url,sep=",")

#add character strings (tweets) in a vector separated by commas
corpus=[]
a=[]
for i in range(len(df['text'])):
        a=df['text'][i]
        corpus.append(a)
        
corpus[0:10]

#declare stopword in english
stopwords = stopwords.words('english')

#calculate the matrices

#No. topics
n_components = 30

#model lda
lda = LatentDirichletAllocation(n_components=n_components, max_iter=5,
                                learning_method='online',
                                learning_offset=50.,
                                random_state=0)
lda.fit(X_train_tfidf)

W=lda.fit_transform(X_train_tfidf)
H=lda.components_
print("W:",W.shape)
print("H:",H.shape)

#number of words by topic
n_top_words = 30

def print_top_words(components,feature_names):
    for topic_idx, topic in enumerate(components):
        message = "Topic #%d: " % topic_idx
        message += " ".join([feature_names[i]
                             for i in topic.argsort()[:-30 - 1:-1]])
        print(message)
    print()
    
print_top_words(H,tfidf_vect.get_feature_names())
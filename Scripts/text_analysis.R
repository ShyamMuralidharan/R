############# Text Mining ####################
## Libraries needed
dyn.load("/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib")
#install.packages("tm")
library(tm)
library(qdap)
library(data.table)
library(dplyr)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(SnowballC)
library(RWeka)

##### Reading the comments data ######
tox <- fread("~/Documents/Kaggle - Toxic Comments/train.csv")
head(tox)
tex <- tox$comment_text
### Find frequent words ####
wordFreq <- freq_terms(tex,10)
plot(wordFreq)
### Build the corpus and conver the text into a corpus format #####
tox_vect <- VectorSource(tex)

#### Conver the vector source into corpus ####
# 2 types of corpus
# 1. VCorpus <- volative corpus, loaded to the memory
# 2. PCorpus <- Permanent loaded on to the HDD
tex_corp <- VCorpus(tox_vect)
tex_corp[[1]]$content
SnowballC::wordStem(tex_copr[[1]])

###### Creating the final cleaned corpus data ######
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus,replace_number)
  corpus <- tm_map(corpus,replace_abbreviation)
  corpus <- tm_map(corpus,replace_ordinal)
  return(corpus)
}

clean_tex_corp <- clean_corpus(tex_corp)

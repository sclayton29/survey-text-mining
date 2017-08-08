#Trying out R for text analysis on survey data
#loading neccessary packages -- these should be installed before loading
library(tm)
library(ggplot2)
library(wordcloud)
library (plyr)
library(tidytext)
library(cluster)
library(dplyr)
library(tidyr)
library(topicmodels)

#Creating the corpus
survey <- read.csv("survey.csv", header=TRUE)
corpus <- Corpus(VectorSource(survey$COLLI.1))
corpus

#Adding metadata (ids) to corpus -- need to come back to this. 
ids <- as.list(levels(survey$DB_ID))
meta(corpus[[1]])


#Cleaning up data
#Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
#making all lower case
corpus <- tm_map(corpus, content_transformer(tolower))
#Removing English stopwords. Might prefer to custom list for project, removes all version of not -- could be problematic
corpus_nostopwords <-corpus #saving a version of the corpus without stopwords for later use
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#striping extra whitespace
corpus <- tm_map(corpus, stripWhitespace)
#stemming words -- may not want to do - I left it out because it didn't seem helpful. Uncomment the next line to try it out.
#corpus <- tm_map(corpus, stemDocument)
#checking cleaned data
inspect(corpus[30])


#Creating a Term Document Matrix
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[])
dtm

#Finding frequent terms
findFreqTerms(dtm, lowfreq=50) #only returns words that appear 50 times or more

#finding Associations - words that often appear near other words
findAssocs(dtm, "job", 0.2)
findAssocs(dtm, "good", 0.2)
findAssocs(dtm, "wanted", 0.2)

#Trying to plot
#putting data in a plottable format
freqr <- colSums(as.matrix(dtm))
freqr
class(freqr)

#Create bar chart of most frequent terms
wf=data.frame(term=names(freqr), occurrences=freqr)
p <- ggplot(subset(wf, freqr>25), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Creating a wordcloud
#limit words by specifying minimum frequency
wordcloud(names(freqr), freqr, min.freq = 2, colors=brewer.pal(6, "Dark2"))

#Trying out clustering
#Removing sparse terms
dtm_rm_sparse <- removeSparseTerms(dtm, 0.98) #removes terms that only appear in 2% of the responses -- have to do this or cluster will be unreadable
dtm_rm_sparse

#Hierarchal Clustering
dtm_rm_sparse
d <- dist(t(dtm_rm_sparse), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

plot(fit, hang=-1)

#identifing clusters
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5) #k is the number of cluster
rect.hclust(fit, k=5, border="red")

# Doing some sentiment analysis
#turning it into tidy data (one document per row data frame)
corpus_td <- tidy(dtm)

#Pulling in the sentiment vocabs
corpus_sentiments <- corpus_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) #There are two other sentiment vocabs you can use

corpus_sentiments

#Finding the most negative documents
corpus_sentiments %>%
  count(document, sentiment, wt=count) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

#Visualing word contributed to postivie and negative sentiment
corpus_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 20) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#Trying out Topic Models
#getting rid of empty entries in dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

#set a seed so that the output of model is predictable
corpus_lda <-LDA(dtm.new, k=2, control = list(seed =1399)) #Need to figure out exactly what seed parameter does

corpus_topics <-tidy(corpus_lda, matrix="beta")
corpus_topics

#pulling out top 10 terms most common within each topic
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Plotting those terms
corpus_top_terms %>%
  mutate(term= reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Looking at terms with the greatest difference in beta between topics 1 and 2
beta_spread <- corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic2))

beta_spread

#Examining per document per topic, called gamma
corpus_documents <- tidy(corpus_lda, matrix = "gamma")
corpus_documents

tidy(dtm.new) %>%
  filter(document == 5) %>%
  arrange(desc(count))

#Using some of the terms to pull assign cateogories to entries. Using regex patterns with keywords that might apply
family = "parents|family|mother|father"
career = "career|job"
knowledge = "knowledge|learning"
money = "money|salary|pay|financ|income"

#Writing a loop to crawl through the assigned terms
inspect(corpus_nostopwords)
inspect(corpus_nostopwords[[5]])

corpus_nostopwords[[5]]

response <- corpus_nostopwords[[5]]
string <- "education "
bool <- grepl(family, response)
if (bool == TRUE) {
  print("ok")
}




#Checking family words
total_family = 0
for (doc in 1:corpus_length){
  keywords = family
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_family = total_family + 1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_family)

#Checking for career words
total_career = 0
for (doc in 1:corpus_length){
  keywords = career
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_career = total_career + 1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_career)

#Checking for knowledge/learning words
total_knowledge = 0
for (doc in 1:corpus_length){
  keywords = knowledge
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_knowledge = total_knowledge + 1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_knowledge)

#Checking for money
total_money = 0
for (doc in 1:corpus_length){
  keywords = money
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_money = total_money +1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_money)

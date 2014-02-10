library(tm)
mydata = read.csv("C:/Users/SRINATH/Desktop/New folder/thor_new.csv")
mydata.corpus <- Corpus(VectorSource(mydata$tweets))
mydata.corpus <- tm_map(mydata.corpus, tolower)
mydata.corpus <- tm_map(mydata.corpus, removePunctuation)
my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)
mydata.dtm <- TermDocumentMatrix(mydata.corpus)


mydata.dtm2 <- removeSparseTerms(mydata.dtm, sparse=0.95)
mydata.df <- as.data.frame(inspect(mydata.dtm2))
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit)
groups <- cutree(fit, k=6)
rect.hclust(fit, k=5, border="red")

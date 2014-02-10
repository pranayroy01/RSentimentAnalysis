#To run the file
#thor.csv,thecounselor.csv,freebirds.csv must be present in wd with heading sno ,tweets in each


setwd("A:/ca/sentiment")



#for twitter calls
#cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
#registerTwitterOAuth(cred)
#load("A:/ca/textmining/twitter authentication.Rdata")



#Directories
projectDir = getwd()
codeDir = file.path(projectDir, 'R')
dataDir = file.path(projectDir, 'data')
outputDir = file.path(projectDir, 'output')


print("Loading libraries and functions for project")
require('twitteR')
require('RJSONIO')
require('RCurl')
require('stringr')
require('plyr')
require('Unicode')
#require('sentiment')
require('ggplot2')
require('doBy')
require('ROAuth')
require('tm')
require('wordcloud')

# load our score.sentiment() function:
source( file.path(codeDir, 'sentiment.R') )
source( file.path(codeDir, 'classify_polarity.R') )
source( file.path(codeDir, 'create_matrix.R') )
source( file.path(codeDir, 'classify_emotion.R') )

#add positive and negative words
hu.liu.pos = scan(file.path(dataDir, 'opinion-lexicon-English', 'positive-words.txt'), what='character', comment.char=';')
hu.liu.neg = scan(file.path(dataDir, 'opinion-lexicon-English', 'negative-words.txt'), what='character', comment.char=';')
# add a few twitter and industry favorites
pos.words = c(hu.liu.pos, 'upgrade', 'wait', 'waiting')
neg.words = c(hu.liu.neg, 'wtf', 'epicfail')


#load twitter data from csv
thor<-read.delim("thor.csv",header=TRUE,sep=',')
counselor<-read.delim("thecounselor.csv",header=TRUE,sep=',')
freebirds<-read.delim("freebirds.csv",header=TRUE,sep=',')

#Read as vector
thor.tweets<-as.vector(thor[,"tweets"])
counselor.tweets<-as.vector(counselor[,"tweets"])
freebirds.tweets<-as.vector(freebirds[,"tweets"])

#calculating the scores
thor.scores = score.sentiment(thor.tweets, pos.words, neg.words, .progress='text')
counselor.scores = score.sentiment(counselor.tweets, pos.words, neg.words, .progress='text')
freebirds.scores = score.sentiment(freebirds.tweets, pos.words, neg.words, .progress='text')

#assigning for categorical  data
thor.scores $movie = 'Thor'
thor.scores $code = 'TH'
counselor.scores$movie= 'Counselor'
counselor.scores$code = 'CO'
freebirds.scores$movie= 'Freebirds'
freebirds.scores$code = 'FB'

#binding all scores
all.scores = rbind( thor.scores, counselor.scores, freebirds.scores)


#colours to columns
cols = c("#7CAE00", "#0022A4", "#F8766D")
names(cols) = c("Thor", "Counselor", "Freebirds")

# ggplot works on data.frames, always 
g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=movie) )
# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_bar( binwidth=1 )
# make a separate plot for each airline
g.hist = g.hist + facet_grid(movie~.)
# plain display, nice colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() +scale_fill_manual(values=cols)
#to get the histogram
print(g.hist)
ggsave(file.path(outputDir, 'all_score_histograms.jpeg'), g.hist, width=6, height=5.5)



meanscore = tapply(all.scores$score, all.scores$movie, mean)
df = data.frame(movie=names(meanscore), meanscore=meanscore)
df$movies <- reorder(df$movie, df$meanscore)

##to get the ggplot barplot
ggpl2<-ggplot(data=df, aes(x=movie, y=meanscore, fill=movie)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE)
print(ggpl2)
ggsave(file.path(outputDir, 'mean_all_score_histograms.jpeg'), ggpl2, width=6, height=5.5)



	
	
#Bayes classifiaction
##BAYES
# classify emotion
class_emo = classify_emotion(thor.tweets, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"


# classify polarity
class_pol = classify_polarity(thor.tweets, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

## data frame with results
sent_df = data.frame(text=thor.tweets, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

#plot emotions
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets") +
opts(title = "Sentiment Analysis of Tweets about  FreeBirds\n(classification by emotion)",
     plot.title = theme_text(size=12))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
labs(x="polarity categories", y="number of tweets") +
opts(title = "Sentiment Analysis of Tweets about Freebirds\n(classification by polarity)",
     plot.title = theme_text(size=12))


# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = freebirds.tweets[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)
















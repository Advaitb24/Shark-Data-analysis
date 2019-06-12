# set current working directory
setwd("D:\\Projects\\greatlakes\\Shark Data Deal classifcation")

#Read Files
#shark = read.csv("Shark Tank Companies.csv", stringsAsFactors=FALSE)
shark = read.csv(file.choose(), stringsAsFactors=FALSE)
table(shark$deal)

library(tm)
library(SnowballC)
library(wordcloud)

corpusShark = Corpus(VectorSource(shark$description))
wordcloud(corpusShark,colors=rainbow(7),max.words=50)

# Convert to lower-case
corpusShark = tm_map(corpusShark, tolower)
corpusShark = tm_map(corpusShark, removePunctuation)
corpusShark

# Remove stopwords and apple
corpusShark = tm_map(corpusShark, removeWords, c(stopwords("english")))
wordcloud(corpusShark,colors=rainbow(7),max.words=50)


# Stem document 
corpusShark = tm_map(corpusShark, stemDocument)
frequenciesShark = DocumentTermMatrix(corpusShark)
frequenciesShark
sparseShark = removeSparseTerms(frequenciesShark, 0.995)
wordcloud(corpusShark,colors=rainbow(7),max.words=50)

SharkSparse = as.data.frame(as.matrix(sparseShark))

# Make all variable names R-friendly
colnames(SharkSparse) = make.names(colnames(SharkSparse))
colnames(SharkSparse)

# Add dependent variable

SharkSparse$DV = shark$deal
SharkSparse$DV=as.factor(SharkSparse$DV)

# Build a CART model

library(rpart)
library(rpart.plot)

SharkCART = rpart(DV ~ ., data=SharkSparse, method="class")
rpart.plot(SharkCART)
prp(SharkCART,extra=3)


library(randomForest)

SharkRF=randomForest(DV~.,data=SharkSparse)
importance(SharkRF)
varImpPlot(SharkRF)

#Logistic regression
SharkLogit=glm(DV~.,data=SharkSparse,family="binomial")
SharkPred=predict(SharkLogit,data=SharkSparse,type="response")
table(SharkSparse$DV,SharkPred>0.5)
(136+115)/495

SharkSparse$category=NULL

SharkLogit2=glm(DV~ratio,data=SharkSparse,family="binomial")
SharkLogit2Pred=predict(SharkLogit2,data=SharkSparse,type="response")
table(SharkSparse$DV,SharkLogit2Pred>0.5)

SharkSparse$ratio=(shark$askedFor/shark$valuation)
SharkSparse$ratio

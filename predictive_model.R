#Capstone data mining
#18 de Agosto de 2019

install.packages("tidyr")
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("base64enc", repos = "http://cran.us.r-project.org")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("plyr")
install.packages("htmlwidgets")
install.packages("arules")
install.packages("topicmodels")
install.packages('wordcloud', repos = "http://cran.us.r-project.org")
install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
install.packages('slam', repos = "http://cran.us.r-project.org")
install.packages('tm', repos = "http://cran.us.r-project.org")
install.packages("factoextra")
install.packages("text2vec")
install.packages("GGally")
install.packages("ggnet")
install.packages("network")
install.packages("sna")
install.packages("intergraph")
install.packages("igraph")
install.packages("ggraph")
install.packages("rlang")
install.packages("devtools")
install.packages("devtools")
install.packages("backports")
install.packages("digest")
install.packages("tsne")
install.packages("sentimentr")
install.packages("mlr")
install.packages("FSelector")
install.packages("kernlab")
install.packages("gbm")
install.packages("xgboost")
install.packages("RecordLinkage")
install.packages("ggthemes")
install.packages("randomForestSRC")
#install.packages("rjson")
install.packages("RWeka")

library(tidyr)
library(base64enc)
library(tidyverse)
library(dplyr)
library(tidytext)
library(ggplot2)
library(jsonlite)
library(plyr)
library(stringr)
library(htmlwidgets)
library(arules)
library(topicmodels)
library(RColorBrewer)
library(wordcloud)
library(text2vec)
library(factoextra)
library(tm)
library(GGally)
library(network)
library(sna)
library(igraph)
library(ggraph)
library(devtools)
library(httr)
library(wordVectors)
library(rword2vec)
ls("package:rword2vec")
library(sentimentr)
library(mlr)
library(FSelector)
library(kernlab)
library(gbm)
library(xgboost)
library(data.table) 
library(purrr)
library(RecordLinkage) 
library(ggthemes)
library(randomForestSRC)
library(RWeka)

#Prueba
#load data
#para cargar datos de texto de un fichero se recomienda encarecidamente utilizar fread con el parámetro quote=""
corpusDat<-fread("hygiene.dat", header=FALSE, quote="", sep="¿", stringsAsFactors = FALSE)
additionalDat<-read.csv("hygiene.dat.additional", header=FALSE, sep=",")
labelsDat<-read.csv("hygiene.dat.labels", header=FALSE)

#Construyo los datos
tDat<-cbind(additionalDat, labelsDat)
colnames(tDat)<-c("type.cousine", "zipcode", "numreviews", "rating", "label")

sapply(tDat, class)
summarizeColumns(tDat)
summary(tDat)

#Convierto zipcode y rating a factor
tDat$zipcode<-as.numeric(tDat$zipcode)

#Identificamos outliers en numreviews
boxplot(tDat$numreviews)

#Hay un único valor de más de 500 cuando el resto no pasa de 300. Lo ajustamos para que no nos desvirtue los datos
tDat <- capLargeValues(tDat, target = "label",cols = c("numreviews"),threshold = 300)

tDat<-mutate(tDat,idDat=1:13299)
tDat$type.cousine<-as.vector(tDat$type.cousine)
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\'", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\]", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\[", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\,", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "	American (New)", "	American(New)")
tDat$type.cousine<-str_replace(tDat$type.cousine, "American (Traditional)", "American(Traditional)")
tDat$type.cousine<-str_replace(tDat$type.cousine, "Breakfast & Brunch", "Breakfast&Brunch")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\(", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\)", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\-", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\&", "")
tDat$type.cousine<-str_replace(tDat$type.cousine, "\\/", "")

tDat<-tDat[1:546,]

View(tDat$type.cousine)

text_corpus_type <- Corpus(VectorSource(tDat$type.cousine))

View(text_corpus)
#convert to document term matrix
docterm_corpus_type <- DocumentTermMatrix(text_corpus_type)

#docterm_corpus_type <- removeSparseTerms(docterm_corpus_type,sparse = 0.3)

#find frequent terms
colS_type <- colSums(as.matrix(docterm_corpus_type))
length(colS_type)
doc_features_type <- data.table(name = attributes(colS_type)$names, count = colS_type)

#most frequent and least frequent words
doc_features_type[order(-count)][1:10] #top 10 most frequent words
doc_features_type[order(count)][1:10] #least 10 frequent words

ggplot(doc_features_type[count>600],aes(name, count)) +
  geom_bar(stat = "identity",fill='lightblue',color='black')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_economist()+
  scale_color_economist()

fdata<-as.data.table(as.matrix(docterm_corpus_type))
#Eliminamos restaurant que no aporta nada al modelo
fdata<-fdata[,2:77]

View(fdata)

#----------------------------------------------------------
#fdata <- data.table(idDat = rep(unlist(tDat$idDat), lapply(tDat$type.cousine, length)), type.cousine = unlist(tDat$type.cousine))
#fdata[,type.cousine := unlist(lapply(type.cousine, tolower))]
#Cuento para cada fila cuántas veces se repite ese type
#fdata[,count := .N, type.cousine]

#Me quedo sólo con los que se repiten más de 100 veces
#fdata <- fdata[count >= 100]
#Transformo las variables contenidas en types en columnas
#fdata <- dcast(data = fdata, formula =idDat ~ type.cousine, fun.aggregate = length, value.var = "type.cousine")

#Ahora preparamos el corpus
#create a corpus of descriptions
corpusDatos<-corpusDat$V1[1:546]
text_corpus <- Corpus(VectorSource(corpusDatos))

View(text_corpus)

#tolower
text_corpus <- tm_map(text_corpus, tolower)
print(as.character(text_corpus[[1]]))

#remove punctuation
text_corpus <- tm_map(text_corpus, removePunctuation)
print(as.character(text_corpus[[1]]))

#remove number
text_corpus <- tm_map(text_corpus, removeNumbers)
print(as.character(text_corpus[[1]]))

#remove whitespaces
text_corpus <- tm_map(text_corpus, stripWhitespace)
print(as.character(text_corpus[[1]]))

#remove stopwords
text_corpus <- tm_map(text_corpus, removeWords, c(stopwords('english')))
print(as.character(text_corpus[[1]]))

#convert to text document
#text_corpus <- tm_map(text_corpus, PlainTextDocument)

#perform stemming - this should always be performed after text doc conversion
text_corpus <- tm_map(text_corpus, stemDocument,language = "english")
text_corpus[[1]]$content

#Eliminamos caracteres que nos sean ASCII
text_corpus <- tm_map(text_corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))

#convert to document term matrix
docterm_corpus <- DocumentTermMatrix(text_corpus)
dim(docterm_corpus)
View(docterm_corpus$dimnames$Terms)

#eliminamos  which are 95% or more sparse.
new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.6)
dim(new_docterm_corpus)

#find frequent terms
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

#most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words

ggplot(doc_features[count>40000],aes(name, count)) +
  geom_bar(stat = "identity",fill='lightblue',color='black')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_economist()+
  scale_color_economist()

#selecting top 6 important features
#top_task <- filterFeatures(trainTask, method = "randomForestSRC_importance", abs = 6)

#check association of terms of top features
#findAssocs(new_docterm_corpus,"toilet",corlimit = 0.5)
#findAssocs(new_docterm_corpus,"dirty",corlimit = 0.5)

#--------------------------------------------------
#Segunda estrategia de caracterización
#TF-IDF
#TF IDF Data set
data_mining_tf <- as.data.table(as.matrix(weightTfIdf(new_docterm_corpus)))

new_data_mining_tf <- removeSparseTerms(weightTfIdf(new_docterm_corpus),sparse = 0.4)


#--------------------------------------------------
#Tercera estrategia de caracterización
#bigram function
corpusDatos<-corpusDat$V1[1:546]
vtext_corpus <- VCorpus(VectorSource(corpusDatos))

View(vtext_corpus)

#tolower
vtext_corpus <- tm_map(vtext_corpus, tolower)
print(as.character(text_corpus[[1]]))

#remove punctuation
vtext_corpus <- tm_map(vtext_corpus, removePunctuation)
print(as.character(text_corpus[[1]]))

#remove number
vtext_corpus <- tm_map(vtext_corpus, removeNumbers)
print(as.character(text_corpus[[1]]))

#remove whitespaces
vtext_corpus <- tm_map(vtext_corpus, stripWhitespace)
print(as.character(text_corpus[[1]]))

#remove stopwords
vtext_corpus <- tm_map(vtext_corpus, removeWords, c(stopwords('english')))
print(as.character(text_corpus[[1]]))

#Eliminamos caracteres que nos sean ASCII
vtext_corpus <- tm_map(vtext_corpus, function(x) iconv(x, "latin1", "ASCII", sub=""))

#create a matrix

df_text_corpus<-as.data.frame(as.matrix(vtext_corpus$content))
df_text_corpus<-mutate(df_text_corpus, id=1:546)

bi_docterm_matrix<-df_text_corpus %>%
  unnest_tokens(bigram, V1, token = "ngrams", n=2, collapse = FALSE)

bi_docterm_matrix<-mutate(bi_docterm_matrix, count=1)
bi_docterm_matrix<-as_tibble(bi_docterm_matrix)

#bi_fdata<-bi_docterm_matrix %>%
#  spread(bigram, count)
View(bi_docterm_matrix)

colnames(bi_docterm_matrix)<-c("idrest", "bifeature", "ncount")
bi_docterm_matrix$bifeature<-str_replace(bi_docterm_matrix$bifeature, " ", "_")

new_bi_docterm_matrix<-bi_docterm_matrix %>%
  cast_dtm(idrest, bifeature, ncount)

#eliminamos  which are 95% or more sparse.
new_bi_docterm_corpus_II <- removeSparseTerms(new_bi_docterm_matrix,sparse = 0.80)
dim(new_bi_docterm_corpus_II)

#find frequent terms
colS <- colSums(as.matrix(new_bi_docterm_corpus_II))
length(colS)
bi_doc_features <- data.table(name = attributes(colS)$names, count = colS)

#most frequent and least frequent words
bi_doc_features[order(-count)][1:10] #top 10 most frequent words
bi_doc_features[order(count)][1:10] #least 10 frequent words

#--------------------------------------------------------------------------------------------
#Estrategia 3 caracterización del modelo
#Mutual information para acotar las palabras y bigrams a las más cercanas a temas de limpieza


write.csv(file="italian/corpus_healthy.txt", text_corpus$content)
prep_word2vec(origin="corpus_healthy.txt",destination="corpus.txt",lowercase=T,bundle_ngrams=2)
if (!file.exists("corpus_healthy_vectors.bin")) {model = train_word2vec("corpus_healthy.txt","corpus_healthy.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0, force=TRUE)} else model = read.vectors("corpus_healthy_vectors.bin")

a<-c("clean", "dirty", "hygienic", "toilet", "quality")
class(a)
a

wordsclean<-model %>% closest_to(a,50)

View(wordsclean)

write.csv(file="wordsclean.txt", wordsclean$word)
wordsclean_II<-read.csv(file="wordsclean.txt", stringsAsFactors = FALSE)

View(wordsclean_II)

#Filtramos las palabras por estas 50 relativas a temas de limpieza
words<-as.data.frame(as.matrix(docterm_corpus))
View(words)
wordsc<-select(words, wordsclean_II$x)

View(wordsc)


#--------------------------------------------------------------------------------------------

#Procedemos ahora a unir el corpus con el resto de atributos para crear el modelo predictivo
#create data set for training
processed_data <- as.data.table(as.matrix(new_data_mining_tf))
View(processed_data)

#2 estrategi
processed_data <- as.data.table(as.matrix(new_bi_docterm_corpus_II))
View(processed_data)
#fin 2 estrategia

#3 estrategi
processed_data <- as.data.table(as.matrix(wordsc))
View(processed_data)
#fin 3 estrategia

#combing the data
#data_one <- cbind(data.table(listing_id = tdata$listing_id, interest_level = tdata$interest_level),processed_data)
#merging the features
#data_one <- fdata[data_one, on="listing_id"]

View(tDat)
View(fdata)
View(processed_data)

sapply(tDat, class)

#Unimos tDat, fdata y processed_data

data_one<-cbind(tDat,fdata)
data_one<-cbind(data_one,processed_data)


View(data_one)

#Quitamos la columna type.cuisine que ya no aporta
data_one<-data_one[,2:116]

#sapply(data_one, class)

#split the data set into train and test. De los que tienen label, 80% training 20%test
#train_one <- filter(data_one, label != "[None]")
#test_one <- filter(data_one, label == "[None]")
train_one<-data_one[1:436,]
test_one<-data_one[437:546,]

test_one$label<-""
View(test_one)
View(train_one)
sapply(train_one, class)



#Prueba--------------------------------------------------------
#create a task
trainTask <- makeClassifTask(data = train_one,target = "label")
testTask <- makeClassifTask(data = test_one, target = "label")

View(trainTask$env$data$label)
View(testTask$env$data$label)

#Ponemos positive class a Y, porque esta a N

trainTask <- makeClassifTask(data = train_one,target = "label", positive = "1")

str(getTaskData(trainTask))

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

#eliminamos las variables que queremos para el modelo
#trainTask <- dropFeatures(task = trainTask,features = c("Loan_ID","Married.dummy"))

#Feature importance. Con esta función vemos qué variables son más importantes o relevantes para el modelo
im_feat <- generateFilterValuesData(trainTask, method = c("FSelector_information.gain","FSelector_chi.squared"))
#lotFilterValues(im_feat,n.show = 5)

top_im_feat<-im_feat$data %>%
  top_n(20,value) %>%
  arrange(desc(value))

View(top_im_feat)

ggplot(top_im_feat, aes(x=name, y=value)) +
  geom_col()

View(im_feat)

#Aplicamos mlr, probamos distintos algoritmos predictivos

#2. Logistic Regression
#logistic regression
logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

#cross validation (cv) accuracy
cv.logistic <- crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)

summarizeColumns(data_one)

View(cv.logistic)

#cross validation accuracy
cv.logistic$aggr

cv.logistic$measures.test

#train model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)

#predict on test data
fpmodel <- predict(fmodel, testTask)

resp<-fpmodel$data$response
labeltest<-data_one$label[437:546]

View(resp)
View(labeltest)

FP<-0
TP<-0
FN<-0
TN<-0

for (i in 1:110){
  if (resp[i] == 1 & labeltest[i] == 1) TP<-TP+1
  if (resp[i] == 1 & labeltest[i] == 0) FP<-FP+1
  if (resp[i] == 0 & labeltest[i] == 1) FN<-FN+1
  if (resp[i] == 0 & labeltest[i] == 0) TN<-TN+1
}
  
F1<-(TP/(TP+((FP+FN)/2)))
View(F1)

#create submission file
#submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = fpmodel$data$response)
#write.csv(submit, "submit2.csv",row.names = F)


#3. Decision Tree
getParamSet("classif.rpart")

makeatree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
gs <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#As you can see, I've set 3 parameters. minsplit represents the minimum number of observation in a node for a split to take place. minbucket says the minimum number of observation I should keep in terminal nodes. cp is the complexity parameter. The lesser it is, the tree will learn more specific relations in the data which might result in overfitting.

#do a grid search
gscontrol <- makeTuneControlGrid()

#hypertune the parameters
stune <- tuneParams(learner = makeatree, resampling = set_cv, task = trainTask, par.set = gs, control = gscontrol, measures = acc)

#You may go and take a walk until the parameter tuning completes. May be, go catch some pokemons! It took 15 minutes to run at my machine. I've 8GB intel i5 processor windows machine.

#check best parameter
stune$x

#cross validation result
stune$y

#using hyperparameters for modeling
t.tree <- setHyperPars(makeatree, par.vals = stune$x)

#train the model
t.rpart <- train(t.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
tpmodel <- predict(t.rpart, testTask)

tresp<-tpmodel$data$response

View(tresp)
View(labeltest)

FP<-0
TP<-0
FN<-0
TN<-0

for (i in 1:110){
  if (tresp[i] == 1 & labeltest[i] == 1) TP<-TP+1
  if (tresp[i] == 1 & labeltest[i] == 0) FP<-FP+1
  if (tresp[i] == 0 & labeltest[i] == 1) FN<-FN+1
  if (tresp[i] == 0 & labeltest[i] == 0) TN<-TN+1
}

F1T<-(TP/(TP+((FP+FN)/2)))
View(F1T)


#create a submission file
#submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = tpmodel$data$response)
#write.csv(submit, "submit3.csv",row.names = F)


#4. Random Forest
getParamSet("classif.randomForest")

#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(
  importance = TRUE
)

#set tunable parameters
#grid search to find hyperparameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#let's do random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)

#Though, random search is faster than grid search, but sometimes it turns out to be less efficient. In grid search, the algorithm tunes over every possible combination of parameters provided. In a random search, we specify the number of iterations and it randomly passes over the parameter combinations. In this process, it might miss out some important combination of parameters which could have returned maximum accuracy, who knows.

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)

#cv accuracy
rf_tune$y

#best parameters
rf_tune$x

#Let's build the random forest model now and check its accuracy.

#using hyperparameters for modeling
rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(rf.tree, trainTask)
getLearnerModel(t.rpart)

#make predictions
rfmodel <- predict(rforest, testTask)

rfresp<-rfmodel$data$response

View(rfresp)
View(labeltest)

FP<-0
TP<-0
FN<-0
TN<-0

for (i in 1:110){
  if (rfresp[i] == 1 & labeltest[i] == 1) TP<-TP+1
  if (rfresp[i] == 1 & labeltest[i] == 0) FP<-FP+1
  if (rfresp[i] == 0 & labeltest[i] == 1) FN<-FN+1
  if (rfresp[i] == 0 & labeltest[i] == 0) TN<-TN+1
}

F1RF<-(TP/(TP+((FP+FN)/2)))
View(F1RF)


#submission file
#submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = rfmodel$data$response)
#write.csv(submit, "submit4.csv",row.names = F)


#5. SVM

#load svm
getParamSet("classif.ksvm") #do install kernlab package 
ksvm <- makeLearner("classif.ksvm", predict.type = "response")

#Set parameters
pssvm <- makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
ctrl <- makeTuneControlGrid()

#tune model
res <- tuneParams(ksvm, task = trainTask, resampling = set_cv, par.set = pssvm, control = ctrl,measures = acc)

#CV accuracy
res$y

#set the model with best params
t.svm <- setHyperPars(ksvm, par.vals = res$x)

#train
par.svm <- train(ksvm, trainTask)

#test
predict.svm <- predict(par.svm, testTask)

rfresp<-predict.svm$data$response

View(rfresp)
View(labeltest)

FP<-0
TP<-0
FN<-0
TN<-0

for (i in 1:110){
  if (rfresp[i] == 1 & labeltest[i] == 1) TP<-TP+1
  if (rfresp[i] == 1 & labeltest[i] == 0) FP<-FP+1
  if (rfresp[i] == 0 & labeltest[i] == 1) FN<-FN+1
  if (rfresp[i] == 0 & labeltest[i] == 0) TN<-TN+1
}

F1RF<-(TP/(TP+((FP+FN)/2)))
View(F1RF)


#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predict.svm$data$response)
write.csv(submit, "submit5.csv",row.names = F)

#6. GBM
#load GBM
getParamSet("classif.gbm")
g.gbm <- makeLearner("classif.gbm", predict.type = "response")

#specify tuning method
rancontrol <- makeTuneControlRandom(maxit = 50L)

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#parameters
gbm_par<- makeParamSet(
  makeDiscreteParam("distribution", values = "bernoulli"),
  makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
  makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
  makeIntegerParam("n.minobsinnode", lower = 10, upper = 80),
  makeNumericParam("shrinkage",lower = 0.01, upper = 1)
)

#n.minobsinnode refers to the minimum number of observations in a tree node. shrinkage is the regulation parameter which dictates how fast / slow the algorithm should move.

#tune parameters
tune_gbm <- tuneParams(learner = g.gbm, task = trainTask,resampling = set_cv,measures = acc,par.set = gbm_par,control = rancontrol)

#check CV accuracy
tune_gbm$y

#set parameters
final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)

#train
to.gbm <- train(final_gbm, trainTask)

#test
pr.gbm <- predict(to.gbm, testTask)

#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = pr.gbm$data$response)
write.csv(submit, "submit6.csv",row.names = F)

#7. Xgboost
#load xgboost
set.seed(1001)
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)

#define parameters for tuning
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 100L) #do 100 iterations

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune <- tuneParams(learner = xg_set, task = trainTask, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)

#train model
xgmodel <- train(xg_new, trainTask)

#test model
predict.xg <- predict(xgmodel, testTask)

xgresp<-predict.xg$data$response

View(xgresp)
View(labeltest)

FP<-0
TP<-0
FN<-0
TN<-0

for (i in 1:110){
  if (xgresp[i] == 1 & labeltest[i] == 1) TP<-TP+1
  if (xgresp[i] == 1 & labeltest[i] == 0) FP<-FP+1
  if (xgresp[i] == 0 & labeltest[i] == 1) FN<-FN+1
  if (xgresp[i] == 0 & labeltest[i] == 0) TN<-TN+1
}

F1XG<-(TP/(TP+((FP+FN)/2)))
View(F1XG)


#submission file
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = predict.xg$data$response)
write.csv(submit, "submit7.csv",row.names = F)




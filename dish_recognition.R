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
devtools::install_github("bmschmidt/wordVectors")
devtools::install_github("mukul13/rword2vec")
install.packages("tsne")

#install.packages("rjson")
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
library(ggnet)
library(network)
library(sna)
library(igraph)
library(ggraph)
library(devtools)
library(httr)
library(wordVectors)
library(rword2vec)
ls("package:rword2vec")

#Importamos los datos de YELP
dReviews<-jsonlite::fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_review.json"), collapse=",")))
dUsers<-jsonlite::fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_user.json"), collapse=",")))
dBusiness<-jsonlite::fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_business.json"), collapse=",")))
dTips<-jsonlite::fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_tip.json"), collapse=",")))
dCheckin<-jsonlite::fromJSON(sprintf("[%s]", paste(readLines("yelp_academic_dataset_checkin.json"), collapse=",")))

Rev_Buss <- dReviews %>%
  left_join(dBusiness, by="business_id")

dCategoriesSel11<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Mexican", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis") %>%
  str_c(collapse="|")

#Seleccionamos las reviews de negocios de las categorías identificadas
Rev_Buss_11<-filter(Rev_Buss, str_detect(Rev_Buss$categories, dCategoriesSel11))

#Añado una columna para indicar qué tipo de cocina es. Utilizo todas las categorías excepto la de Restaurant, que es genérica
dCategoriesSel111<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Mexican", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis") %>%
  str_c(collapse="|")
Rev_Buss_111<-Rev_Buss_11 %>%
  mutate(type=str_extract(Rev_Buss_11$categories, dCategoriesSel111))

Rev_Buss_1111<-Rev_Buss_111

#Agrupo los textos por tipo de cocina (documentos)

Rev_Buss_111$textclean<-Rev_Buss_111$text

by_type<-select(Rev_Buss_111, type, textclean)

a<-by_type %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(count=dplyr::n())

View(a)

  
text_by_type<-stats::aggregate(formula=textclean ~ type, data=by_type, FUN=paste, collapse=" ")

text_by_type_italian<-text_by_type %>%
  filter(text_by_type$type=="Italian")

write.csv(file="italian/italian_dishes.txt", text_by_type_italian$textclean)
prep_word2vec(origin="italian",destination="italian.txt",lowercase=T,bundle_ngrams=2)
if (!file.exists("italian_vectors.bin")) {model = train_word2vec("italian.txt","italian_vectors.bin",vectors=200,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("italian_vectors.bin")

a<-c("guanciale", "pasta", "spaghetti")
class(a)
a

model %>% closest_to("main_course",50)

label<-read.csv(file="Week3/italian1.label", header=FALSE)
label<-label %>%
  mutate(dish=str_replace(label$V1, " ", "_"))
label$dish<-str_replace(label$dish, "1$", "")
label$dish<-str_replace(label$dish, "0$", "")
label$dish<-str_replace(label$dish, "\t", "")

label_c<-as.character(label$dish)

model %>% closest_to(label_c,50)

label<-read.csv(file="Week3/italian2.label", header=FALSE)
label<-label %>%
  mutate(dish=str_replace(label$V1, " ", "_"))
label$dish<-str_replace(label$dish, "1$", "")
label$dish<-str_replace(label$dish, "0$", "")
label$dish<-str_replace(label$dish, "\t", "")

label_c<-as.character(label$dish)

model %>% closest_to(label_c,50)

label<-read.csv(file="Week3/italian3.label", header=FALSE)
label<-label %>%
  mutate(dish=str_replace(label$V1, " ", "_"))
label$dish<-str_replace(label$dish, "1$", "")
label$dish<-str_replace(label$dish, "0$", "")
label$dish<-str_replace(label$dish, "\t", "")

label_c<-as.character(label$dish)

model %>% closest_to(label_c,50)

#Para finalizar, genero un fichero que me sirva para la herramienta ToPMine

by_type_italian<-by_type %>%
  filter(type=="Italian")

by_type_italian_trigrams<-by_type_italian %>%
  unnest_tokens(bigram, textclean, token = "ngrams", n = 3, collapse = FALSE)

bigram_counts_italian<-by_type_italian_trigrams %>%
  dplyr::count(type, bigram, sort=TRUE) %>%
  ungroup()

#Selecciono aquellas frases que tienen al menos una ocurrencia de 10
topmineitalian<-bigram_counts_italian %>%
  filter(n>9)

write.csv(topmineitalian$bigram, file="TopMineItalian.txt", quote=FALSE)



#Fin Week3

#Otras aplicaciones
some_dish = closest_to(model,model[[c("guanciale","pasta","bolognesa")]],50)
dish = model[[some_dish$word,average=F]]
plot(dish,method="pca")

#Aplicamos clustering
set.seed(10)
centers = 150
clustering = kmeans(model,centers=centers,iter.max = 40)

sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})

ingredients = c("guanciale","peper","oil","tomato")
term_set = lapply(ingredients, 
                  function(ingredient) {
                    nearest_words = model %>% closest_to(model[[ingredient]],20)
                    nearest_words$word
                  }) %>% unlist
subset = model[[term_set,average=F]]
subset %>%
  cosineDist(subset) %>% 
  as.dist %>%
  hclust %>%
  plot

#Visualizations
tastes = model[[c("dish","dessert"),average=F]]
# model[1:3000,] here restricts to the 3000 most common words in the set.
sweet_and_saltiness = model[1:3000,] %>% cosineSimilarity(tastes)
# Filter to the top 20 sweet or salty.
sweet_and_saltiness = sweet_and_saltiness[
  rank(-sweet_and_saltiness[,1])<50 |
    rank(-sweet_and_saltiness[,2])<50,
  ]
plot(sweet_and_saltiness,type='n')
text(sweet_and_saltiness,labels=rownames(sweet_and_saltiness))

tastes = model[[c("sweet","salty","savory","bitter","sour"),average=F]]
# model[1:3000,] here restricts to the 3000 most common words in the set.
common_similarities_tastes = model[1:3000,] %>% cosineSimilarity(tastes)
common_similarities_tastes[20:30,]

high_similarities_to_tastes = common_similarities_tastes[rank(-apply(common_similarities_tastes,1,max)) < 75,]
high_similarities_to_tastes %>% 
  prcomp %>% 
  biplot(main="Fifty words in a\nprojection of flavor space")

plot(model,perplexity=10)

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
install.packages("sentimentr")

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
library(sentimentr)

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

a<-Rev_Buss_111 %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(count=dplyr::n())

View(a)

#Me quedo con las reviews del tipo de cocina italiana

Rev_Buss_Italian<-Rev_Buss_111 %>%
  filter(type=="Italian")

View(Rev_Buss_Italian)

#Selecciono las reviews que hablan de un plato

#dish<-"pasta"

#rev_dish<-Rev_Buss_Italian%>%
#  filter (str_extract(Rev_Buss_Italian$text, dish)==dish)

rev_dish<-Rev_Buss_Italian

View(rev_dish)

#Tokenizo el texto en frases, y extraigo únicamente las que tienen el plato que estoy analizando

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
        # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

rev_dish$textclean = prep_fun(rev_dish$text)

my_text<-get_sentences(rev_dish$textclean)

sentiment<-sentiment(my_text)

View(my_text)
View(sentiment)
View(rev_dish)

#Uno el análisis de sentimiento al data frame original, junto con el split de las frases

dim<-dim(rev_dish)[1]

rev_dish<-rev_dish %>%
  mutate(element_id=1:dim) %>%
  merge(sentiment, by="element_id")

sentences<-as_tibble(my_text[[1]]) %>%
  mutate(element_id=1) %>%
  mutate(sentence_id=1:(dim(as.tibble(my_text[[1]]))[1]))

for (i in 2:dim) {
  sentences_var<-as_tibble(my_text[[i]]) %>%
    mutate(element_id=i) %>%
    mutate(sentence_id=1:(dim(as.tibble(my_text[[i]]))[1]))
  sentences<-rbind(sentences, sentences_var)
}

rev_dish<-merge(rev_dish, sentences)

#Añado una columna en la que calculo el sentimiento amplificado por el número de votes useful
rev_dish<-rev_dish %>%
  mutate(sentiment_amp=sentiment*(1+log10(1+rev_dish$votes$useful)))

View(rev_dish)

dish_filter<-rev_dish %>%
  filter(str_detect(value, "gorgonzola")==TRUE) %>%
  select(sentiment, value, word_count) %>%
  filter(word_count<30) %>%
  arrange(desc(sentiment))

View(dish_filter)

#Calculo el número total de restaurantes distintos italianos

num_total_rest<-nrow(distinct(rev_dish, business_id))

#Selecciono las frases que hablan de un plato específico y hago los cálculos de mi ranking

#Obtengo los platos típicos de la lista de la week3

label<-read.csv(file="Week3/italian3.label", header=FALSE)
label<-label %>%
  mutate(dish=str_replace(label$V1, "_", " "))
label$dish<-str_replace(label$dish, "1$", "")
label$dish<-str_replace(label$dish, "0$", "")
label$dish<-str_replace(label$dish, "\t", "")

dish_name<-label$dish[1]

dish_filter<-rev_dish %>%
  filter(str_detect(value, dish_name)==TRUE)

ranking<-dish_filter %>%
  summarise(sentiment_amp=mean(sentiment_amp), sentiment_mean=mean(sentiment), sentiment_variance=var(sentiment))

ranking<-ranking %>%
  mutate(dish=dish_name) %>%
  mutate(occurrence=nrow(dish_filter)) %>%
  mutate(exclusive=log10((occurrence+1)/(nrow(distinct(dish_filter, business_id))))) %>%
  mutate(occurrence_norm=(occurrence/max(num_total_rest,occurrence))) %>%
  mutate(exclusive_norm=(1-(exclusive/min(num_total_rest, occurrence))))

dim<-nrow(label)

for (i in 2:dim) {
  dish_filter<-rev_dish %>%
    filter(str_detect(value, label$dish[i])==TRUE)

  ranking_aux<-dish_filter %>%
    summarise(sentiment_amp=mean(sentiment_amp), sentiment_mean=mean(sentiment), sentiment_variance=var(sentiment))
  
  ranking_aux<-ranking_aux %>%
    mutate(dish=label$dish[i]) %>%
    mutate(occurrence=nrow(dish_filter)) %>%
    mutate(exclusive=log10((occurrence+1)/(nrow(distinct(dish_filter, business_id))))) %>%
    mutate(occurrence_norm=(occurrence/max(num_total_rest, occurrence))) %>%
    mutate(exclusive_norm=(1-(exclusive/min(num_total_rest,occurrence))))
  
  ranking<-rbind(ranking, ranking_aux)
  
}

top_ranking<-ranking %>%
  mutate(rank=(0.6*occurrence_norm + 0.35*sentiment_amp + 0.05*exclusive_norm)) %>%
  mutate(dish=paste(dish, " - ", occurrence)) %>%
  top_n(100, rank) %>%
  arrange(-rank)

#Los campos que utilizaré para el ranking son: sentiment_amp, occurrence_norm y exclusive_norm
#Hago un gráfico de araña para entender la distribución y el comportamiento de estos campos

top_ranking %>%
  mutate(dish=reorder(dish, rank)) %>%
  ggplot(aes(x=dish, y=rank, fill=sentiment_amp)) +
  geom_col() +
  scale_fill_gradient(low="Red", high="Green") +
  theme(text=element_text(size=8),
  axis.text.x=element_text(angle=90, hjust=1))

#Superpongo un gráfico de linea con la exclusividad

top_ranking %>%
  mutate(dish=reorder(dish, rank)) %>%
  ggplot(aes(x=dish, y=rank, fill=sentiment_amp)) +
  geom_col() +
  scale_fill_gradient(low="Red", high="Green") +
  theme(text=element_text(size=8),
  axis.text.x=element_text(angle=90, hjust=1)) +
  geom_point(aes(x=dish, y=exclusive), shape=23)


#Task 5. Ranking de restaurantes para un plato dado

dish_filter<-rev_dish %>%
  filter(str_detect(value, "carbonara")==TRUE)

num_total_occ<-nrow(dish_filter)

View(dish_filter)

dish_filter<-dish_filter %>%
  left_join(dish_filter, dBusiness, by="business_id")

restaurantes<-distinct(dish_filter, name.y)

r<-restaurantes[1,1]


dish_filter_r<-dish_filter %>%
  filter(name.y==r)


ranking_aux_r<-dish_filter_r %>%
  summarise(sentiment_amp=mean(sentiment_amp.y), sentiment_mean=mean(sentiment.y), sentiment_variance=var(sentiment.y))

ranking_r<-ranking_aux_r %>%
  mutate(name_r=r) %>%
  mutate(occurrence=nrow(dish_filter_r)) %>%
  mutate(occurrence_norm=(occurrence/num_total_occ))

dim<-nrow(restaurantes)

for (i in 2:dim) {
  
  r<-restaurantes[i,1]

  dish_filter_r<-dish_filter %>%
    filter(name.y==r)

  ranking_aux_r<-dish_filter_r %>%
    summarise(sentiment_amp=mean(sentiment_amp.y), sentiment_mean=mean(sentiment.y), sentiment_variance=var(sentiment.y))
  
  ranking_aux_r<-ranking_aux_r %>%
    mutate(name_r=r) %>%
    mutate(occurrence=nrow(dish_filter_r)) %>%
    mutate(occurrence_norm=(occurrence/num_total_occ))
  
  ranking_r<-rbind(ranking_r, ranking_aux_r)
  
}

View(ranking_r)

View(top_ranking)

top_ranking<-ranking_r %>%
  mutate(rank=(0.8*sentiment_amp + 0.2*occurrence_norm)) %>%
  mutate(name_r=paste(name_r, " - ", occurrence)) %>%
  top_n(100, rank) %>%
  arrange(-rank)

#Los campos que utilizaré para el ranking son: sentiment_amp, occurrence_norm y exclusive_norm
#Hago un gráfico de araña para entender la distribución y el comportamiento de estos campos

top_ranking %>%
  mutate(name_r=reorder(name_r, rank)) %>%
  ggplot(aes(x=name_r, y=rank, fill=sentiment_amp)) +
  geom_col() +
  scale_fill_gradient(low="Red", high="Green") +
  theme(text=element_text(size=8),
        axis.text.x=element_text(angle=90, hjust=1))

top_ranking %>%
  mutate(name_r=reorder(name_r, rank)) %>%
  ggplot(aes(x=name_r, y=rank, fill=sentiment_amp)) +
  geom_col() +
  scale_fill_gradient(low="Red", high="Green") +
  theme(text=element_text(size=8),
        axis.text.x=element_text(angle=90, hjust=1)) +
  geom_point(aes(x=name_r, y=occurrence_norm), shape=17)




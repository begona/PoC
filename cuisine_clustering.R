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
#library(rjson)

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

d<-Rev_Buss_111 %>%
dplyr::group_by(type) %>%
dplyr::summarise(num=dplyr::n())

View(d)

dd<-Rev_Buss_111 %>%
  dplyr::group_by(city) %>%
  dplyr::summarise(num=dplyr::n()) %>%
  arrange(desc(num))

View(dd)

#Las Vegas es la ciudad con más reviews. Analizo si es un buen subset para el assignment, viendo si tiene de todas las categorías de cocina

ddd<-Rev_Buss_111 %>%
  filter(city=="Las Vegas") %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(num=dplyr::n()) %>%
  arrange(desc(num))

#Si, tiene suficiente representación de todas las categorías. Lo tomo como subset

#Rev_Buss_1111<-Rev_Buss_111 %>%
#  filter(city=="Las Vegas")

Rev_Buss_1111<-Rev_Buss_111

#Voy a identificar tópicos utilizando el método LDA
#Debo primero obtener un elemento DocumentMatrix, para ello obtengo las reviews por establecimiento

p<-Rev_Buss_1111 %>%
  select(business_id, text, categories, type, user_id) %>%
  filter(type=="Mexican")

by_business<-p %>%
  unnest_tokens(word, text)


word_counts<-by_business %>%
  anti_join(stop_words) %>%
  dplyr::count(type, word, sort=TRUE) %>%
  ungroup()

dTypes<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis")

for(i in dTypes) {
  aux<-Rev_Buss_1111 %>%
    select(business_id, text, categories, type, user_id) %>%
    filter(type==i)
  
  by_business_aux<-aux %>%
    unnest_tokens(word, text)
  
  word_counts_aux<-by_business_aux %>%
    anti_join(stop_words) %>%
    dplyr::count(type, word, sort=TRUE) %>%
    ungroup()
  
  
  word_counts<-rbind(word_counts, word_counts_aux)
}


business_dtm<-word_counts %>%
  cast_dtm(type, word, n)

business_lda <- LDA(business_dtm, k = 10, control = list(seed = 1234))

business_topics<-tidy(business_lda, matrix="beta")

#Lo guardo para no tener que ejecutar el proceso de cálculo cada vez que abro una sesión nueva
write.csv(business_topics, file="topicsLV.txt")

#business_topics<-read.csv(file="topicsLV.txt")

#View(business_topics)

top_terms<-business_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

View(top_terms)

top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()

top_terms_1<-business_topics %>%
  filter(topic==1) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_1$term, freq = top_terms_1$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_2<-business_topics %>%
  filter(topic==2) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_2$term, freq = top_terms_2$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_3<-business_topics %>%
  filter(topic==3) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_3$term, freq = top_terms_3$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))


top_terms_4<-business_topics %>%
  filter(topic==4) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_4$term, freq = top_terms_4$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_5<-business_topics %>%
  filter(topic==5) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_5$term, freq = top_terms_5$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_6<-business_topics %>%
  filter(topic==6) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_6$term, freq = top_terms_6$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_7<-business_topics %>%
  filter(topic==7) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_7$term, freq = top_terms_7$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_8<-business_topics %>%
  filter(topic==8) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_8$term, freq = top_terms_8$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_9<-business_topics %>%
  filter(topic==9) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_9$term, freq = top_terms_9$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_10<-business_topics %>%
  filter(topic==10) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_10$term, freq = top_terms_10$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#Creo una visualización de red

top_terms<-business_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

term <-c("topic1", "topic2", "topic3", "topic4", "topic5","topic6","topic7","topic8","topic9","topic10")
topic<-c(161,161,161,161,161,161,161,161,161,161)
beta<-c(1,1,1,1,1,1,1,1,1,1)
topics<-data.frame(topic, term, beta)
top_terms<-rbind(topics, top_terms)

top_terms<-top_terms %>%
  mutate(id=1:160) %>%
  mutate(colr="black") %>%
  mutate(trasparencia=1)

filas_users<-nrow(top_terms)
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==1 | top_terms$term[i]=="topic1") {
  top_terms$colr[i]<-"orange"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==2 | top_terms$term[i]=="topic2") {
  top_terms$colr[i]<-"red"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==3 | top_terms$term[i]=="topic3") {
  top_terms$colr[i]<-"brown"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==4 | top_terms$term[i]=="topic4") {
  top_terms$colr[i]<-"gold"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==5 | top_terms$term[i]=="topic5") {
  top_terms$colr[i]<-"aquamarine3"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==6 | top_terms$term[i]=="topic6") {
  top_terms$colr[i]<-"blue"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==7 | top_terms$term[i]=="topic7") {
  top_terms$colr[i]<-"gray50"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==8 | top_terms$term[i]=="topic8") {
  top_terms$colr[i]<-"black"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==9 | top_terms$term[i]=="topic9") {
  top_terms$colr[i]<-"forestgreen"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==10 | top_terms$term[i]=="topic10") {
  top_terms$colr[i]<-"green"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}


nodes<-distinct(top_terms, id, term, colr, trasparencia)
nodes<-add_row (nodes, id=161, term="topics", colr="black", trasparencia=1)
edges<-select(top_terms, topic, id)

View(top_terms)
View(nodes)
View(edges)

#creamos el objeto tipo igraph
net<-graph.data.frame (d=edges, vertices=nodes, directed=T)

V(net)$color=V(net)$colr

#plot(net, edge.arrow.size=0,2,vertex.label=NA, vertex.size=2, layout=layout.fruchterman.reingold, main="fruchterman.reingold", usercurve=TRUE, edge.curve=TRUE)

ggraph(net, 'igraph', algorithm = 'tree', circular = TRUE) + 
 # geom_edge_diagonal(aes(alpha = ..index..)) +
  coord_fixed() + 
  scale_edge_alpha('Direction', guide = 'edge_direction') +
  geom_edge_link0(edge_alpha = 0.2)  +
  geom_node_point(color = V(net)$color, alpha=(V(net)$trasparencia), size = 2) +
  geom_node_text(aes(label = term), color = V(net)$color, size = 3, repel=TRUE) +
  ggforce::theme_no_axes()

#Assignment2
#Repito la operación distinguiendo los positivos de los negativos. Positivos 4 y 5, negativos 1, 2 y 3

#Analizo primero cómo es la distribución de positivos y negativos por localización

a<-Rev_Buss_111 %>%
  dplyr::filter(is.na(stars.x))
  
View(a)

location_top10<-Rev_Buss_111 %>%
  dplyr::group_by(city) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  top_n(10, n)

View(location_top10)

city_vector <- as.vector(location_top10$city)

View(city_vector)

by_rating_location<-Rev_Buss_111 %>%
  dplyr::group_by(city, stars.x) %>%
  dplyr::summarise(num_reviews=dplyr::n()) %>%
  dplyr::filter(city %in% location_top10$city)

View(by_rating_location)

colnames(by_rating_location)[colnames(by_rating_location)=="stars.x"]<-"rating"

ggplot(by_rating_location, aes(x=city, y= num_reviews, fill=rating)) + geom_col()

#Calculo ahora los porcentajes de positivos vs negativos para ver qué localización está más equilibrada

by_rating_location_percentage <- left_join(location_top10, by_rating_location, by="city") %>%
  mutate(percent=(num_reviews/n)*100)

View(by_rating_location_percentage)

ggplot(by_rating_location_percentage, aes(x=city, y= percent, fill=rating)) + geom_col()

#Ya que los porcentajes son parecidos, decido quedarme con Las Vegas, por tener más reviews. Hago el mismo análisis, por tipo de cocina

Rev_Buss_1111<-Rev_Buss_111 %>%
  filter(city=="Las Vegas")

cousine_top10<-Rev_Buss_1111 %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  top_n(10, n)

View(cousine_top10)

by_rating_cousine<-Rev_Buss_1111 %>%
  dplyr::group_by(type, stars.x) %>%
  dplyr::summarise(num_reviews=dplyr::n()) %>%
  dplyr::filter(type %in% cousine_top10$type)

View(by_rating_cousine)

ggplot(by_rating_cousine, aes(x=type, y= num_reviews, fill=stars.x)) + geom_col()

#Calculo ahora los porcentajes de positivos vs negativos para ver qué localización está más equilibrada

by_rating_cousine_percentage <- left_join(cousine_top10, by_rating_cousine, by="type") %>%
  mutate(percent=(num_reviews/n)*100)

View(by_rating_cousine_percentage)

ggplot(by_rating_cousine_percentage, aes(x=type, y=percent, fill=stars.x)) + geom_col()

#Parece que en el tipo de cocina buffets hay proporcionalmente peores valoraciones. Decido analizar este subset: Las Vegas + Buffets

Rev_Buss_11111<-Rev_Buss_1111 %>%
  filter(type=="Mexican")

Rev_Buss_OK<-Rev_Buss_1111 %>%
  filter(stars.x==4|stars.x==5)

dddd<-Rev_Buss_OK %>%
  dplyr::group_by(stars.x) %>%
  dplyr::summarise(num=dplyr::n()) %>%
  arrange(desc(num))

View(dddd)

#Voy a identificar tópicos utilizando el método LDA
#Debo primero obtener un elemento DocumentMatrix, para ello obtengo las reviews por establecimiento

p<-Rev_Buss_OK %>%
  select(business_id, text, categories, type, user_id) %>%
  filter(type=="Mexican")

by_business<-p %>%
  unnest_tokens(word, text)

word_counts<-by_business %>%
  anti_join(stop_words) %>%
  dplyr::count(type, word, sort=TRUE) %>%
  ungroup()

dTypes<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis")

for(i in dTypes) {
  aux<-Rev_Buss_OK %>%
    select(business_id, text, categories, type, user_id) %>%
    filter(type==i)
  
  by_business_aux<-aux %>%
    unnest_tokens(word, text)
  
  word_counts_aux<-by_business_aux %>%
    anti_join(stop_words) %>%
    dplyr::count(type, word, sort=TRUE) %>%
    ungroup()
  
  
  word_counts<-rbind(word_counts, word_counts_aux)
}


business_dtm_ok<-word_counts %>%
  cast_dtm(type, word, n)

business_lda_ok <- LDA(business_dtm_ok, k = 5, control = list(seed = 1234))

business_topics_ok<-tidy(business_lda_ok, matrix="beta")

write.csv(business_topics_ok, file="TopicsLS_OK.txt")

top_terms<-business_topics_ok %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

View(top_terms)

top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()

top_terms_1_ok<-business_topics_ok %>%
  filter(topic==1) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_1_ok$term, freq = top_terms_1_ok$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_2_ok<-business_topics_ok %>%
  filter(topic==2) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_2_ok$term, freq = top_terms_2_ok$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_3_ok<-business_topics_ok %>%
  filter(topic==3) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_3_ok$term, freq = top_terms_3_ok$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_4_ok<-business_topics_ok %>%
  filter(topic==4) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_4_ok$term, freq = top_terms_4_ok$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_5_ok<-business_topics_ok %>%
  filter(topic==5) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_5_ok$term, freq = top_terms_5_ok$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#Repito la operación ahora con los negativos, 1, 2 y 3

Rev_Buss_KO<-Rev_Buss_1111 %>%
  filter(stars.x==1|stars.x==2|stars.x==3)

dddd<-Rev_Buss_KO %>%
  dplyr::group_by(stars.x) %>%
  dplyr::summarise(num=dplyr::n()) %>%
  arrange(desc(num))

View(dddd)

#Voy a identificar tópicos utilizando el método LDA
#Debo primero obtener un elemento DocumentMatrix, para ello obtengo las reviews por establecimiento

p<-Rev_Buss_KO %>%
  select(business_id, text, categories, type, user_id) %>%
  filter(type=="Mexican")

by_business<-p %>%
  unnest_tokens(word, text)

word_counts<-by_business %>%
  anti_join(stop_words) %>%
  dplyr::count(type, word, sort=TRUE) %>%
  ungroup()

dTypes<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis")

for(i in dTypes) {
  aux<-Rev_Buss_KO %>%
    select(business_id, text, categories, type, user_id) %>%
    filter(type==i)
  
  by_business_aux<-aux %>%
    unnest_tokens(word, text)
  
  word_counts_aux<-by_business_aux %>%
    anti_join(stop_words) %>%
    dplyr::count(type, word, sort=TRUE) %>%
    ungroup()
  
  
  word_counts<-rbind(word_counts, word_counts_aux)
}


business_dtm_KO<-word_counts %>%
  cast_dtm(type, word, n)

business_lda_KO <- LDA(business_dtm_KO, k = 5, control = list(seed = 1234))

business_topics_KO<-tidy(business_lda_KO, matrix="beta")

write.csv(business_topics_KO, file="LV_KO.txt")

top_terms<-business_topics_KO %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()

top_terms_1_ko<-business_topics_KO %>%
  filter(topic==1) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_1_ko$term, freq = top_terms_1_ko$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_2_ko<-business_topics_KO %>%
  filter(topic==2) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_2_ko$term, freq = top_terms_2_ko$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_3_ko<-business_topics_KO %>%
  filter(topic==3) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_3_ko$term, freq = top_terms_3_ko$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_4_ko<-business_topics_KO %>%
  filter(topic==4) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_4_ko$term, freq = top_terms_4_ko$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

top_terms_5_ko<-business_topics_KO %>%
  filter(topic==5) %>%
  top_n(100, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

wordcloud(words = top_terms_5_ko$term, freq = top_terms_5_ko$beta, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#Creo la visualización jerárquica

top_terms_ok<-business_topics_ok %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_ko<-business_topics_KO %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_ko$topic<-top_terms_ko$topic+5

top_terms<-rbind(top_terms_ok, top_terms_ko)

View(top_terms)

term <-c("topic_ok_1", "topic_ok_2", "topic_ok_3", "topic_ok_4", "topic_ok_5","topic_ko_1","topic_ko_2","topic_ko_3","topic_ko_4","topic_ko_5")
topic<-c(161,161,161,161,161,162,162,162,162,162)
beta<-c(1,1,1,1,1,1,1,1,1,1)
topics<-data.frame(topic, term, beta)
top_terms<-rbind(topics, top_terms)

top_terms<-top_terms %>%
  mutate(id=1:160) %>%
  mutate(colr="black") %>%
  mutate(trasparencia=1)

filas_users<-nrow(top_terms)
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==1 | top_terms$term[i]=="topic_ok_1") {
  top_terms$colr[i]<-"green3"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==2 | top_terms$term[i]=="topic_ok_2") {
  top_terms$colr[i]<-"green4"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==3 | top_terms$term[i]=="topic_ok_3") {
  top_terms$colr[i]<-"seagreen3"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==4 | top_terms$term[i]=="topic_ok_4") {
  top_terms$colr[i]<-"seagreen4"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==5 | top_terms$term[i]=="topic_ok_5") {
  top_terms$colr[i]<-"chartreuse3"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==6 | top_terms$term[i]=="topic_ko_1") {
  top_terms$colr[i]<-"red"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==7 | top_terms$term[i]=="topic_ko_2") {
  top_terms$colr[i]<-"red4"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==8 | top_terms$term[i]=="topic_ko_3") {
  top_terms$colr[i]<-"tomato"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==9 | top_terms$term[i]=="topic_ko_4") {
  top_terms$colr[i]<-"tomato4"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}
j<-1
for (i in 1:filas_users) if (top_terms$topic[i]==10 | top_terms$term[i]=="topic_ko_5") {
  top_terms$colr[i]<-"orangered"
  top_terms$trasparencia[i]<-j
  j<-j-0.05
}


nodes<-distinct(top_terms, id, term, colr, trasparencia)
nodes<-add_row (nodes, id=161, term="positive", colr="green3", trasparencia=1)
nodes<-add_row (nodes, id=162, term="negative", colr="red", trasparencia=1)
nodes<-add_row (nodes, id=163, term="topics", colr="black", trasparencia=1)
edges<-select(top_terms, topic, id)
edges<-add_row(edges, topic=163, id=161)
edges<-add_row(edges, topic=163, id=162)

View(top_terms)
View(nodes)
View(edges)

#creamos el objeto tipo igraph
net<-graph.data.frame (d=edges, vertices=nodes, directed=T)

V(net)$color=V(net)$colr

#plot(net, edge.arrow.size=0,2,vertex.label=NA, vertex.size=2, layout=layout.fruchterman.reingold, main="fruchterman.reingold", usercurve=TRUE, edge.curve=TRUE)

ggraph(net, 'igraph', algorithm = 'tree', circular = TRUE) + 
  geom_edge_diagonal(aes(alpha = ..index..)) +
  coord_fixed() + 
  scale_edge_alpha('Direction', guide = 'edge_direction') +
  #geom_edge_link0(edge_alpha = 0.2)  +
  geom_node_point(color = V(net)$color, alpha=(V(net)$trasparencia), size = 2) +
  geom_node_text(aes(label = term), color = V(net)$color, size = 3, repel=TRUE) +
  ggforce::theme_no_axes()



#Voy a analizar el contexto de palabras que se repiten en ambos subsets que me resultan llamativas haciendo un análisis bigram
#Para analizar correctamente el contexto debemos estudiar los bigrams sin aplicar stop-words
#Estudiamos por separado el subset OK y el KO

#Bigrams OK
p<-Rev_Buss_OK %>%
  select(business_id, text, categories, type, user_id) %>%
  filter(type=="Mexican")

by_business_bigrams_ok<-p %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3, collapse = FALSE)

bigram_counts_ok<-by_business_bigrams_ok %>%
  dplyr::count(type, bigram, sort=TRUE) %>%
  ungroup()

dTypes<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis")

for(i in dTypes) {
  aux<-Rev_Buss_OK %>%
    select(business_id, text, categories, type, user_id) %>%
    filter(type==i)
  
  by_business_aux<-aux %>%
    unnest_tokens(bigram, text, token = "ngrams", n=3, collapse = FALSE)
  
  bigram_counts_aux_ok<-by_business_aux %>%
    dplyr::count(type, bigram, sort=TRUE) %>%
    ungroup()
  
  
  bigram_counts_ok<-rbind(bigram_counts_ok, bigram_counts_aux_ok)
}

bigram_counts_ok_II<-bigram_counts_ok %>%
  dplyr::select(bigram, n) %>%
  dplyr::group_by(bigram) %>%
  dplyr::summarise(number=sum(n)) %>%
  arrange(desc(number))

bigram_separate_ok<-bigram_counts_ok_II %>%
  separate(bigram, c("word1", "word2", "word3"), sep=" ")

#Bigrams KO
p<-Rev_Buss_KO %>%
  select(business_id, text, categories, type, user_id) %>%
  filter(type=="Mexican")

by_business_bigrams_ko<-p %>%
  unnest_tokens(bigram, text, token = "ngrams", n=3, collapse = FALSE)

bigram_counts_ko<-by_business_bigrams_ko %>%
  dplyr::count(type, bigram, sort=TRUE) %>%
  ungroup()

dTypes<-c("American (New)", "American (Traditional)", "Irish", "Korean", "Indian", "Latin American", "Seafood", "Filipino", "Steakhouses", "Italian", "Pizza", "Sandwiches", "Burgers", "Japanese", "Chinese", "Sushi Bars", "Buffets", "French", "Vietnamese", "Thai", "Asian Fusion", "Mediterranean", "Vegetarian", "Delis")

for(i in dTypes) {
  aux<-Rev_Buss_KO %>%
    select(business_id, text, categories, type, user_id) %>%
    filter(type==i)
  
  by_business_aux<-aux %>%
    unnest_tokens(bigram, text, token = "ngrams", n=3, collapse = FALSE)
  
  bigram_counts_aux_ko<-by_business_aux %>%
    dplyr::count(type, bigram, sort=TRUE) %>%
    ungroup()
  
  
  bigram_counts_ko<-rbind(bigram_counts_ko, bigram_counts_aux_ko)
}

bigram_counts_ko_II<-bigram_counts_ko %>%
  dplyr::select(bigram, n) %>%
  dplyr::group_by(bigram) %>%
  dplyr::summarise(number=sum(n)) %>%
  arrange(desc(number))
View(bigram_counts_ko_II)

bigram_separate_ko<-bigram_counts_ko_II %>%
  separate(bigram, c("word1", "word2", "word3"), sep=" ")

View(bigram_separate_ko)
#Una vez obtenidos los bigrams OK y KO, busco las palabras más llamativas: service, time, price, nice, pretty, awsome, delicious

bigram_filtered_ok<-bigram_separate_ok %>%
  filter((word1=="customer" & !word2 %in% stop_words$word & !word3 %in% stop_words$word) | (word2=="customer" & !word1 %in% stop_words$word & !word3 %in% stop_words$word) | (word3=="customer" & !word2 %in% stop_words$word & !word1 %in% stop_words$word)) %>%
  top_n(10, number)

bigram_filtered_ko<-bigram_separate_ko %>%
  filter((word1=="customer" & !word2 %in% stop_words$word & !word3 %in% stop_words$word) | (word2=="customer" & !word1 %in% stop_words$word & !word3 %in% stop_words$word) | (word3=="customer" & !word2 %in% stop_words$word & !word1 %in% stop_words$word)) %>%
  top_n(10, number)

bigram_filtered_ko$number<-bigram_filtered_ko$number*(-1)

bigram_filtered <- bigram_filtered_ok %>%
  rbind(bigram_filtered_ko) %>%
  mutate(trigram=str_c(word1, word2, word3, sep=" ")) %>%
  arrange(desc(number))

bigram_filtered %>%
  arrange(desc(number)) %>%
  mutate(tri=reorder(trigram, number)) %>%
  ggplot(aes(tri, number, fill=number>0)) +
  geom_col(show.legend = FALSE) +
  ylab("Count") +
  xlab("Trigrams")+
  coord_flip()

View(bigram_filtered_ok)
View(bigram_filtered_ko)
View(bigram_filtered)

#---------------------------------------------------------

#Week2. Comparación entre los distintos tipos de cocina

#Limpiamos el texto de las review
prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
Rev_Buss_111$textclean = prep_fun(Rev_Buss_111$text)

Rev_Buss_111$textclean = removeWords(Rev_Buss_111$textclean, stopwords("english"))

a<-select(Rev_Buss_111, text, textclean)

View(a)

#Agrupo los textos por tipo de cocina (documentos)

by_type<-select(Rev_Buss_111, type, textclean)
  
text_by_type<-stats::aggregate(formula=textclean ~ type, data=by_type, FUN=paste, collapse=" ")


it1<-itoken(text_by_type$textclean, progressbar = FALSE)

#Comparamos tipos de cocina (documentos) en un vector space. Debemos crearlo primero

v<-create_vocabulary(it1) %>%
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer<-vocab_vectorizer(v)

dtm1<-create_dtm(it1, vectorizer)
dim(dtm1)
dtm1[1:10, 1:10]

View(dtm1)

max (v$term_count)
min (v$term_count)
count(v$term=="lunch")

dtm2<-dtm1

rownames(dtm1)<-c("Asian F.", "Buffets", "Burg.", "Chinese", "Delis", "Filip.", "French", "Indian", "Irish", "Italian", "Japan.", "Korean", "Latin A.", "Med.", "Mexican", "Pizza", "Sandw.", "Seaf.", "Steakh.", "Sushi B.", "Thai", "Veget.", "Vietnam.")

#Jaccard
d1_d2_jacc_sim<-sim2(dtm1, method="jaccard", norm="none")


#Dibujamos la matriz de similaridad con una gráfica heatmap

d1_d2_matrix_jacc<-as.matrix(d1_d2_jacc_sim)

#Pongo nombre a las filas y columnas (types)
rownames(d1_d2_matrix_jacc)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
colnames(d1_d2_matrix_jacc)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
d1_d2_data_frame_jacc<-as.data.frame(d1_d2_matrix_jacc)
d1_d2_dist_jacc<-as.dist(d1_d2_data_frame_jacc)

fviz_dist(dist.obj = d1_d2_dist_jacc, order=FALSE, lab_size = NULL, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  theme(legend.position = "none")

coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(d1_d2_matrix_jacc, Colv = NA, Rowv = NA, col=coul)
heatmap(d1_d2_matrix_jacc, Colv = NA, Rowv = NA, col=cm.colors(256))
heatmap(d1_d2_matrix_jacc, Colv = NA, Rowv = NA, col=terrain.colors(256))


#Cosine
d1_d2_cos_sim<-sim2(dtm1, method="cosine", norm="l2")

#Dibujamos la matriz de similaridad con una gráfica heatmap

d1_d2_matrix_cos<-as.matrix(d1_d2_cos_sim)

#Pongo nombre a las filas y columnas (types)
rownames(d1_d2_matrix_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
colnames(d1_d2_matrix_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
d1_d2_data_frame_cos<-as.data.frame(d1_d2_matrix_cos)
d1_d2_dist_cos<-as.dist(d1_d2_data_frame_cos)

fviz_dist(dist.obj = d1_d2_dist_cos, order=FALSE, lab_size = NULL, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  theme(legend.position = "none")

coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(d1_d2_matrix_cos, Colv = NA, Rowv = NA, col=coul)
heatmap(d1_d2_matrix_cos, Colv = NA, Rowv = NA, col=cm.colors(256))
heatmap(d1_d2_matrix_cos, Colv = NA, Rowv = NA, col=terrain.colors(256))

#Cosine similarity con TF-IDF
dtm<-create_dtm(it1, vectorizer)
tfidf<-TfIdf$new()
dtm_tfidf<-fit_transform(dtm, tfidf)

d1_d2_tfidf_cos_sim<-sim2(x=dtm_tfidf, method="cosine", norm="none")

#Dibujamos la matriz de similaridad con una gráfica heatmap

d1_d2_matrix_tfidf_cos<-as.matrix(d1_d2_tfidf_cos_sim)

#Pongo nombre a las filas y columnas (types)
rownames(d1_d2_matrix_tfidf_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
colnames(d1_d2_matrix_tfidf_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
d1_d2_data_frame_tfidf_cos<-as.data.frame(d1_d2_matrix_tfidf_cos)
d1_d2_dist_tfidf_cos<-as.dist(d1_d2_data_frame_tfidf_cos)

fviz_dist(dist.obj = d1_d2_dist_tfidf_cos, order=FALSE, lab_size = NULL, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  theme(legend.position = "none")

coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(d1_d2_matrix_tfidf_cos, Colv = NA, Rowv = NA, col=coul)
heatmap(d1_d2_matrix_tfidf_cos, Colv = NA, Rowv = NA, col=cm.colors(256))
heatmap(d1_d2_matrix_tfidf_cos, Colv = NA, Rowv = NA, col=terrain.colors(256))

#Cosine similarity con tfidf y con lsa
lsa<-LSA$new(n_topics=22)
dtm_tfidf_lsa<-lsa$fit_transform(dtm_tfidf)

d1_d2_tfidf_lsa_cos_sim<-sim2(x=dtm_tfidf_lsa, method="cosine", norm="l2")

#Dibujamos la matriz de similaridad con una gráfica heatmap

d1_d2_matrix_tfidf_lsa_cos<-as.matrix(d1_d2_tfidf_lsa_cos_sim)

#Pongo nombre a las filas y columnas (types)
rownames(d1_d2_matrix_tfidf_lsa_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
colnames(d1_d2_matrix_tfidf_lsa_cos)<-c("Asian Fusion", "Buffets", "Burgers", "Chinese", "Delis", "Filipino", "French", "Indian", "Irish", "Italian", "Japanese", "Korean", "Latin American", "Mediterranean", "Mexican", "Pizza", "Sandwiches", "Seafood", "Steakhouses", "Sushi Bars", "Thai", "Vegetarian", "Vietnamese")
d1_d2_data_frame_tfidf_lsa_cos<-as.data.frame(d1_d2_matrix_tfidf_lsa_cos)
d1_d2_dist_tfidf_lsa_cos<-as.dist(d1_d2_data_frame_tfidf_lsa_cos)

fviz_dist(dist.obj = d1_d2_dist_tfidf_lsa_cos, order=FALSE, lab_size = NULL, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  theme(legend.position = "none")

coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(d1_d2_matrix_tfidf_lsa_cos, Colv = NA, Rowv = NA, col=coul)
heatmap(d1_d2_matrix_tfidf_lsa_cos, Colv = NA, Rowv = NA, col=cm.colors(256))
heatmap(d1_d2_matrix_tfidf_lsa_cos, Colv = NA, Rowv = NA, col=terrain.colors(256))

#Aplicamos k_means variando el número de clusters. Para representar los cluster, creo una matriz en la que, si dos cocinas coinciden en el cluster se colorea con el color de ese cluster, y si no, se pone a cero

rownames(d1_d2_matrix_jacc)<-c("Asian", "Buff", "Burg", "Chin", "Delis", "Filip", "French", "Indi", "Irish", "Ital", "Jap", "Kor", "Latin", "Med", "Mex", "Pizz", "Sandw", "Seaf", "Steakh", "Sushi", "Thai", "Veget", "Viet")

k2_1<-kmeans(d1_d2_matrix_jacc, centers=4, nstart=25)

dim<-dim(as.data.frame(k2_1$cluster))[1]

k2_1_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_1$cluster[j]    
    vi<-k2_1$cluster[i]
    k2_1_m[k,]<-c(names(k2_1$cluster[j]),names(k2_1$cluster[i]),0)
    if (vi==vj) k2_1_m[k,3]<-k2_1$cluster[i]
  }
}


colores.paleta<-c("white", "lightgoldenrod1", "brown1", "darkred", "mediumorchid1", "mediumpurple4", "skyblue", "slateblue", "lightslategrey")

ggplot(as.data.frame(k2_1_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_1<-kmeans(d1_d2_matrix_jacc, centers=6, nstart=25)

dim<-dim(as.data.frame(k2_1$cluster))[1]

k2_1_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_1$cluster[j]    
    vi<-k2_1$cluster[i]
    k2_1_m[k,]<-c(names(k2_1$cluster[j]),names(k2_1$cluster[i]),0)
    if (vi==vj) k2_1_m[k,3]<-k2_1$cluster[i]
  }
}

ggplot(as.data.frame(k2_1_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)


rownames(d1_d2_matrix_tfidf_lsa_cos)<-c("Asian", "Buff", "Burg", "Chin", "Delis", "Filip", "French", "Indi", "Irish", "Ital", "Jap", "Kor", "Latin", "Med", "Mex", "Pizz", "Sandw", "Seaf", "Steakh", "Sushi", "Thai", "Veget", "Viet")

k2_1<-kmeans(d1_d2_matrix_tfidf_lsa_cos, centers=4, nstart=25)

dim<-dim(as.data.frame(k2_1$cluster))[1]

k2_1_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_1$cluster[j]    
    vi<-k2_1$cluster[i]
    k2_1_m[k,]<-c(names(k2_1$cluster[j]),names(k2_1$cluster[i]),0)
    if (vi==vj) k2_1_m[k,3]<-k2_1$cluster[i]
  }
}

ggplot(as.data.frame(k2_1_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_1<-kmeans(d1_d2_matrix_tfidf_lsa_cos, centers=6, nstart=25)

dim<-dim(as.data.frame(k2_1$cluster))[1]

k2_1_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_1$cluster[j]    
    
    vi<-k2_1$cluster[i]
    k2_1_m[k,]<-c(names(k2_1$cluster[j]),names(k2_1$cluster[i]),0)
    if (vi==vj) k2_1_m[k,3]<-k2_1$cluster[i]
  }
}

ggplot(as.data.frame(k2_1_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)



#Analizamos la similitud basándonos en las preferencias por usuario (sitios a los que suele ir, por tipo de cocina)
User_by_type<-Rev_Buss_111 %>%
  dplyr::group_by(user_id, type) %>%
  dplyr::summarise(num=dplyr::n()) %>%
  arrange(desc(num))

user_by_type_matrix<-User_by_type %>%
  spread(key=type, value=num)

#Nos quedamos con los usuarios que han realizado un mínimo de 10 reviews
dim2<-dim(user_by_type_matrix)[2]
user_by_type_matrix_r<-user_by_type_matrix %>%
  mutate(total_reviews = rowSums(user_by_type_matrix[,2:dim2], na.rm = TRUE)) %>%
  mutate(max_reviews = apply(user_by_type_matrix[,2:dim2], 1, max, na.rm=TRUE)) %>%
  filter(total_reviews>10)

dim1<-dim(user_by_type_matrix_r)[1]

m<-array(NA, dim=c(dim2-1, dim2-1))
i<-NULL
j<-NULL
for (j in 2:dim2){
  for (i in 2:dim2){
    vj<-user_by_type_matrix_r[,j]    
    vi<-user_by_type_matrix_r[,i]
    v<-as.numeric(sum(vj*vi, na.rm=TRUE))
    dj<-as.numeric(sqrt(sum(vj*vj, na.rm=TRUE)))
    di<-as.numeric(sqrt(sum(vi*vi, na.rm=TRUE)))
    m[i-1,j-1]<-(v/(dj*di))
  }
}

#Preparo la matriz trasponiéndola y limpiando filas que no debo usar y gestionando los nulos
user_by_type_matrix_r_t<-t(user_by_type_matrix_r)
user_by_type_matrix_r_t<-user_by_type_matrix_r_t[-1,]
user_by_type_matrix_r_t<-user_by_type_matrix_r_t[-24,]
user_by_type_matrix_r_t<-user_by_type_matrix_r_t[-24,]
user_by_type_matrix_r_t<-as.data.frame(user_by_type_matrix_r_t, stringsAsFactors=FALSE)
user_by_type_matrix_r_t<-user_by_type_matrix_r_t %>%
  mutate_all(funs(replace(., is.na(.),0)))

dtm2<-as.matrix(user_by_type_matrix_r_t)

rownames(dtm2)<-c("Asian", "Buff", "Burg", "Chin", "Delis", "Filip", "French", "Indi", "Irish", "Ital", "Jap", "Kor", "Latin", "Med", "Mex", "Pizz", "Sandw", "Seaf", "Steakh", "Sushi", "Thai", "Veget", "Viet")

k2_2<-kmeans(dtm2, centers=4, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}


colores.paleta<-c("white", "lightgoldenrod1", "brown1", "darkred", "mediumorchid1", "mediumpurple4", "skyblue", "slateblue", "lightslategrey")

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=6, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

colores.paleta<-c("white", "lightgoldenrod1", "brown1", "darkred", "mediumorchid1", "mediumpurple4", "skyblue", "slateblue", "lightslategrey")

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=4, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=5, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=6, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=7, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile() + scale_fill_manual(values = colores.paleta)

k2_2<-kmeans(dtm2, centers=8, nstart=25)

dim<-dim(as.data.frame(k2_2$cluster))[1]

k2_2_m<-array(0, dim=c((dim*dim), 3))
i<-NULL
j<-NULL
k<-0
for (j in 1:dim){
  for (i in 1:dim){
    k<-k+1
    vj<-k2_2$cluster[j]    
    vi<-k2_2$cluster[i]
    k2_2_m[k,]<-c(names(k2_2$cluster[j]),names(k2_2$cluster[i]),0)
    if (vi==vj) k2_2_m[k,3]<-k2_2$cluster[i]
  }
}

colores.paleta<-c("white", "lightgoldenrod1", "brown1", "darkred", "mediumorchid1", "mediumpurple4", "skyblue", "slateblue", "lightslategrey")

ggplot(as.data.frame(k2_2_m), aes(x = V1, y = V2, fill = V3)) + geom_tile()  + scale_fill_manual(values = colores.paleta)

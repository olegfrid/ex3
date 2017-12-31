---
title: "EX3_Sol"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---
# Part 1
## Q1 a

setup home folder
```{r}
folder = 'C:/Users/oleg/Desktop/Study/year5/semesterA/dataAnalys/network analys'
setwd(folder)

#Or for all chuncks in this Rmarkdown:
knitr::opts_knit$set(root.dir = folder)
set.seed(123)

```
loading libraries
```{r}
library(igraph)
```

```{r}
ga.data <- read.csv('ga_edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('ga_actors.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)
plot(g)
```
![Image of Q1graph](/img/Q1Agraph.JPG)

the actor with the maximal betweenness centrality
```{r}
g$bet=betweenness(g, v = V(g), directed = FALSE, weights = NULL,
  nobigint = TRUE, normalized = FALSE)

which.max(g$bet)
```

the actor with the maximal closeness centrality
```{r}
g$clos=closeness(g, vids = V(g), mode = c("all"),
  weights = NULL, normalized = FALSE)
which.max(g$clos)
```

the actor with the maximal eigenvector centrality

```{r}
g$eig=eigen_centrality(g)
max=which.max(g$eig$vector)
max
```
## Q1 b
community detection by cluster edge betweenness
```{r}
gc1 <-  edge.betweenness.community(g)
memb1 <- membership(gc1)
plot(g, vertex.size=7, vertex.color=memb1, asp=FALSE)

```
![Image of Q1graph](/img/Q1BalgoGC1.JPG)
the number of communities for cluster edge betweenness
```{r}
length(gc1)
```
the size of each community by community index for cluster edge betweenness
```{r}
print (sizes(gc1))
```

the modularity value of gc1
```{r}
gc1$modularity
```
maximum modularity
```{r}
max(gc1$modularity)
```
the name of the actor with maximum modularity
```{r}
gc1$name[which.max(gc1$modularity)]
```

community detection using cluster walktrap
```{r}
gc2 <- walktrap.community(g)
memb2 <- membership(gc2)
plot(g, vertex.size=7, #vertex.label=NA,
     vertex.color=memb2, asp=FALSE)
```
![Image of Q1graph](/img/Q1BalgoGC2.JPG)

the number of communities for cluster walktrap
```{r}
length(gc2)
```
the size of each community by community index for cluster walktrap
```{r}
sizes(gc2)
```

the modularity value of gc2
```{r}
gc2$modularity
```
maximum modularity
```{r}
max(gc2$modularity)
```
the name of the actor with maximum modularity
```{r}
gc2$name[which.max(gc2$modularity)]
```




# Part 2

## Q2 a
In part 2 of the HW we will take a list of music genres and scrap twittes related to those genres.
after scarping we will make a graph of asociated genres and network analyze this graph.

## Q2 b
the nodes in the graph will be the music genres and the edges will be the asociations from twitts we scraped.

## Q2 c
initializing environment for twitter scraping
```{r}
library(twitteR)
library(httr)
library(jsonlite)
library(wordcloud)
library(jsonlite)
library(tm)
library(devtools)
#install.packages("base64enc")
library(base64enc)
library(XML)
library(RCurl)
library(igraph)
source("twitterOAuth.R")

myapp=oauth_app("twitter",key=consumer_key,secret=consumer_secret)
sig1=sign_oauth1.0(myapp,token=access_token,token_secret=access_secret)

```


```{r}
sig <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

musicGen= read.csv('musicGenres.csv', header=TRUE, stringsAsFactors=FALSE)
musicGen$Genres

```

scrap twitter for related words and create the asociation graph
```{r}
newEdges=c()
for (genre in musicGen$Genres){
  searchRes <- searchTwitter(genre, n=200)
  searchDF <- twListToDF(searchRes)
  for (twitts in searchDF$text){
    tokensForGenre <- strsplit(twitts," ")[[1]]
    tokensForGenre<-unique(tokensForGenre)
    for (token in tokensForGenre){
      if (token !=genre && token %in% musicGen$Genres){
        newEdges=c(newEdges,genre,token)
      }
    }
  }
}
g<- graph(newEdges)
A<-get.adjacency(g)
A[A>1]<-1
newg<- graph.adjacency(A)
g<- as.undirected(newg)
plot(g)
```
![Image of Q1graph](/img/Q2graph.JPG)

## Q2 d
the genre with the maximal betweenness centrality
```{r}
g$bet=betweenness(g, v = V(g), directed = FALSE, weights = NULL,
  nobigint = TRUE, normalized = FALSE)

which.max(g$bet)
```


the genre with the maximal closeness centrality
```{r}
g$clos=closeness(g, vids = V(g), mode = c("all"),
  weights = NULL, normalized = FALSE)
which.max(g$clos)
```

the genre with the maximal eigenvector centrality

```{r}
g$eig=eigen_centrality(g)
max=which.max(g$eig$vector)
max
```



### community detection by cluster edge betweenness
```{r}
gc3 <-  edge.betweenness.community(g)
memb3 <- membership(gc3)
plot(g, vertex.size=7,vertex.color=memb3, asp=FALSE)

```
![Image of Q1graph](/img/Q2DalgoGC3.JPG)

the number of communities for cluster edge betweenness
```{r}
length(gc3)
```

the size of each community by community index for cluster edge betweenness
```{r}
print (sizes(gc3))
```

the modularity value of gc3
```{r}
gc3$modularity
```
maximum modularity
```{r}
max(gc3$modularity)
```
the name of the genre with maximum modularity
```{r}
gc3$name[which.max(gc3$modularity)]
```


### community detection using cluster walktrap
```{r}
gc4 <- walktrap.community(g)
memb4 <- membership(gc4)
plot(g, vertex.size=7,vertex.color=memb4, asp=FALSE)
```
![Image of Q1graph](/img/Q2DalgoGC4.JPG)

the number of communities for cluster walktrap
```{r}
length(gc4)
```


the size of each community by community index for cluster walktrap
```{r}
sizes(gc4)
```
the modularity value of gc4
```{r}
gc4$modularity
```

maximum modularity
```{r}
max(gc4$modularity)
```
the name of the genre with maximum modularity
```{r}
gc4$name[which.max(gc4$modularity)]
```







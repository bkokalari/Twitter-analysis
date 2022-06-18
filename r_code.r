
data1<- read.csv("2009-07-01.txt", header=FALSE,fill=T, strip.white=T, na.strings=c(""," ","NA"), col.names=c(1:8),skipNul=T, sep = ",", stringsAsFactors = FALSE)
data2<- read.csv("2009-07-02.txt", header=FALSE,fill=T, strip.white=T, col.names=c(1:8),skipNul=T, sep = ",", stringsAsFactors = FALSE)
data3<- read.csv("2009-07-03.txt", header=FALSE,fill=T, strip.white=T, col.names=c(1:8),skipNul=T, sep = ",", stringsAsFactors = FALSE)
data4<- read.csv("2009-07-04.txt", header=FALSE,fill=T, strip.white=T, col.names=c(1:8),skipNul=T, sep = ",", stringsAsFactors = FALSE)
data5<- read.csv("2009-07-05.txt", header=FALSE,fill=T, strip.white=T, col.names=c(1:8),skipNul=T, sep = ",", stringsAsFactors = FALSE)

setwd("D:/MSc Business Analytics/Social Network Analysis/Assignment 2")

data<-read.csv("mention_graphs.csv", header= TRUE)
day1<-data[data$dayOfYear==182,]
day2<-data[data$dayOfYear==183,]
day3<-data[data$dayOfYear==184,]
day4<-data[data$dayOfYear==185,]
day5<-data[data$dayOfYear==186,]

day1$dayOfYear <- NULL
day2$dayOfYear <- NULL
day3$dayOfYear <- NULL 
day4$dayOfYear <- NULL
day5$dayOfYear <- NULL

library(igraph)
library(dplyr)
library(ggplot2)


#create an igraph graph
set.seed(123)
g1 <- graph_from_data_frame(day1, directed=TRUE)
g2 <- graph_from_data_frame(day2, directed=TRUE)
g3 <- graph_from_data_frame(day3, directed=TRUE)
g4 <- graph_from_data_frame(day4, directed=TRUE)
g5 <- graph_from_data_frame(day5, directed=TRUE)



############## Q2 ##################
### Number of vertices
Number_of_vertices<-c(gorder(g1),gorder(g2),gorder(g3),gorder(g4),gorder(g5))
Number_of_vertices 


###Number of edges
Number_of_edges<-c(gsize(g1),gsize(g2),gsize(g3),gsize(g4),gsize(g5))
Number_of_edges

###Diameter of the graph
is.weighted(g1)
E(g1)$weight 

Diameter_of_the_graph1<-diameter(g1, directed = TRUE,  weights= E(g1)$weight)
Diameter_of_the_graph2<-diameter(g2, directed = TRUE,  weights= E(g2)$weight)
Diameter_of_the_graph3<-diameter(g3, directed = TRUE,  weights= E(g3)$weight)
Diameter_of_the_graph4<-diameter(g4, directed = TRUE,  weights= E(g4)$weight)
Diameter_of_the_graph5<-diameter(g5, directed = TRUE,  weights= E(g5)$weight)
Diameter_of_the_graphs<-c(Diameter_of_the_graph1, Diameter_of_the_graph2, Diameter_of_the_graph3,
                        Diameter_of_the_graph4, Diameter_of_the_graph5)

Diameter_of_the_graphs

#in degree and out degree
avg_in_deg <-round(c(mean(degree(g1, mode="in")),mean(degree(g2, mode="in")),
               mean(degree(g3, mode="in")),mean(degree(g4, mode="in")),
               mean(degree(g5, mode="in"))), 2)
 
avg_in_deg

avg_out_deg <-round(c(mean(degree(g1, mode="out")),mean(degree(g2, mode="out")),
                mean(degree(g3, mode="out")),mean(degree(g4, mode="out")),
                mean(degree(g5, mode="out"))),2)
avg_out_deg

#ploting the metics
library(tibble)
x=data.frame(Number_of_edges, Number_of_vertices, Diameter_of_the_graphs, avg_in_deg,avg_out_deg)
days = seq(from = as.Date("2009-07-01"), to = as.Date("2009-07-05"), by = 'day')
add_column(x, days, .before = 1)


require(ggplot2)

plot1<-ggplot(x, aes(x=days, y=Number_of_vertices)) + ggtitle("Evolution of Number of Vertices")+
  geom_line(aes(group=1), colour="#000099") + 
  geom_point(size=4, colour="#CC0000") 

plot1

plot2<-ggplot(x, aes(x=days, y=Number_of_edges)) + ggtitle("Evolution of Number of Edges")+
  geom_line(aes(group=1), colour="darkorchid") +  
  geom_point(size=4, colour="violetred1") 

plot2

plot3<-ggplot(x, aes(x=days, y=avg_in_deg)) + ggtitle("Evolution of IN DEGREE")+
  geom_line(aes(group=1), colour="springgreen") + 
  geom_point(size=4, colour="mediumvioletred") 

plot3

plot4<-ggplot(x, aes(x=days, y=avg_out_deg)) + ggtitle("Evolution of OUT DEGREE")+
  geom_line(aes(group=1), colour="maroon1") + 
  geom_point(size=4, colour="olivedrab1") 

plot4

library(ggplot2)
require(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


ggplot(data=x, aes(x=days, y=Diameter_of_the_graphs)) +ggtitle("Evolution of Diameter of the graphs")+
  geom_bar(stat="identity", fill="magenta")+
  geom_text(aes(label=Diameter_of_the_graphs), vjust=1, color="blue", size=5)+
  theme_minimal()

########################## Q3 ##################################################

#Q3.1
###The top-10 characters of the g1 network as far as their IN degree is concerned
in1<-sort(degree(g1, mode="in"), decreasing=TRUE)
in1[c(1:10)]
topin1<-data.frame(in1[c(1:10)])

###The top-10 characters of the g2 network as far as their IN degree is concerned
in2<-sort(degree(g2, mode="in"), decreasing=TRUE)
in2[c(1:10)]
topin2<-data.frame(in2[c(1:10)])

###The top-10 characters of the g3 network as far as their IN degree is concerned
in3<-sort(degree(g3, mode="in"), decreasing=TRUE)
in3[c(1:10)]
topin3<-data.frame(in3[c(1:10)])

###The top-10 characters of the g4 network as far as their IN degree is concerned
in4<-sort(degree(g4, mode="in"), decreasing=TRUE)
in4[c(1:10)]
topin4<-data.frame(in4[c(1:10)])

###The top-10 characters of the g1 network as far as their IN degree is concerned
in5<-sort(degree(g5, mode="in"), decreasing=TRUE)
in5[c(1:10)]
topin5<-data.frame(in5[c(1:10)])
##################################################################################

#Q3.2
###The top-10 characters of the g1 network as far as their IN degree is concerned
out1<-sort(degree(g1, mode="out"), decreasing=TRUE)
out1[c(1:10)]
topout1<-data.frame(out1[c(1:10)])

###The top-10 characters of the g2 network as far as their IN degree is concerned
out2<-sort(degree(g2, mode="out"), decreasing=TRUE)
out2[c(1:10)]
topout2<-data.frame(out2[c(1:10)])

###The top-10 characters of the g3 network as far as their IN degree is concerned
out3<-sort(degree(g3, mode="out"), decreasing=TRUE)
out3[c(1:10)]
topout3<-data.frame(out3[c(1:10)])

###The top-10 characters of the g4 network as far as their IN degree is concerned
out4<-sort(degree(g4, mode="out"), decreasing=TRUE)
out4[c(1:10)]
topout4<-data.frame(out4[c(1:10)])

###The top-10 characters of the g1 network as far as their IN degree is concerned
out5<-sort(degree(g5, mode="out"), decreasing=TRUE)
out5[c(1:10)]
topout5<-data.frame(out5[c(1:10)])

##################################################################################

library(miniCRAN)
library(igraph)
library(magrittr)

###The top-10 characters of the g1 network as far as their PageRank is concerned
pr1 <- g1 %>%
  page.rank(directed = TRUE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank day1")

pr1<-head(pr1, 10)
pr1 <- as.data.frame(pr1)

###The top-2 characters of the g2 network as far as their PageRank is concerned
pr2 <- g2 %>%
  page.rank(directed = TRUE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank day2")

pr2<-head(pr2, 10)
pr2 <- as.data.frame(pr2)


###The top-10 characters of the g3 network as far as their PageRank is concerned
pr3 <- g3 %>%
  page.rank(directed = TRUE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank day3")

pr3<-head(pr3, 10)
pr3 <- as.data.frame(pr3)


###The top-10 characters of the g4 network as far as their PageRank is concerned
pr4 <- g4 %>%
  page.rank(directed = TRUE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank day4")

pr4<-head(pr4, 10)
pr4 <- as.data.frame(pr4)


###The top-10 characters of the g5 network as far as their PageRank is concerned
pr5 <- g5 %>%
  page.rank(directed = TRUE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank day5")

pr5<-head(pr5, 10)
pr5 <- as.data.frame(pr5)

##################################### Q4 ##################################################

# Make retweet_graph undirected
g1_undir <- as.undirected(g1)
g2_undir <- as.undirected(g2)
g3_undir <- as.undirected(g3)
g4_undir <- as.undirected(g4)
g5_undir <- as.undirected(g5)

# Find communities with fast greedy clustering
communities_fast_greedy1 <- cluster_fast_greedy(g1_undir)
communities_fast_greedy2 <- cluster_fast_greedy(g2_undir)
communities_fast_greedy3 <- cluster_fast_greedy(g3_undir)
communities_fast_greedy4 <- cluster_fast_greedy(g4_undir)
communities_fast_greedy5 <- cluster_fast_greedy(g5_undir)

# ... and again with infomap clustering
communities_infomap1 <- cluster_infomap(g1_undir)
communities_infomap2 <- cluster_infomap(g2_undir)
communities_infomap3 <- cluster_infomap(g3_undir)
communities_infomap4 <- cluster_infomap(g4_undir)
communities_infomap5 <- cluster_infomap(g5_undir)

# ... and again with louvain clustering
communities_louvain1 <- cluster_louvain(g1_undir)
communities_louvain2 <- cluster_louvain(g2_undir)
communities_louvain3 <- cluster_louvain(g3_undir)
communities_louvain4 <- cluster_louvain(g4_undir)
communities_louvain5 <- cluster_louvain(g5_undir)

#mashable
membership(communities_louvain1)["mashable"]
membership(communities_louvain2)["mashable"]
membership(communities_louvain3)["mashable"]
membership(communities_louvain4)["mashable"]
membership(communities_louvain5)["mashable"]

# Color vertices by community membership, as a factor
V(g1)$color <- factor(membership(communities_louvain1))
V(g2)$color <- factor(membership(communities_louvain2))
V(g3)$color <- factor(membership(communities_louvain3))
V(g4)$color <- factor(membership(communities_louvain4))
V(g5)$color <- factor(membership(communities_louvain5))

# Does the edge cross betwen commmunities?
is_crossing1 <- crossing(g1, communities = communities_louvain1)
is_crossing2 <- crossing(g2, communities = communities_louvain2)
is_crossing3 <- crossing(g3, communities = communities_louvain3)
is_crossing4 <- crossing(g4, communities = communities_louvain4)
is_crossing5 <- crossing(g5, communities = communities_louvain5)

# Set edge linetype: solid for crossings, dotted otherwise 
E(g1)$lty <- ifelse(is_crossing1, "solid", "dotted")
E(g2)$lty <- ifelse(is_crossing2, "solid", "dotted")
E(g3)$lty <- ifelse(is_crossing3, "solid", "dotted")
E(g4)$lty <- ifelse(is_crossing4, "solid", "dotted")
E(g5)$lty <- ifelse(is_crossing5, "solid", "dotted")


# Get the sizes of each community
community_size1 <- sizes(communities_louvain1)
community_size2 <- sizes(communities_louvain2)
community_size3 <- sizes(communities_louvain3)
community_size4 <- sizes(communities_louvain4)
community_size5 <- sizes(communities_louvain5)

# Some mid-size communities
in_mid_community1 <- unlist(communities_louvain1[community_size1 > 50 & community_size1 < 100])
in_mid_community2 <- unlist(communities_louvain2[community_size2 > 50 & community_size2 < 100])
in_mid_community3 <- unlist(communities_louvain3[community_size3 > 40 & community_size3 < 90])
in_mid_community4 <- unlist(communities_louvain4[community_size4 > 50 & community_size4 < 100])
in_mid_community5 <- unlist(communities_louvain5[community_size5 > 50 & community_size5 < 100])

# Induce a subgraph using in_mid_community
g1_subgraph <- induced.subgraph(g1, in_mid_community1)
g2_subgraph <- induced.subgraph(g2, in_mid_community2)
g3_subgraph <- induced.subgraph(g3, in_mid_community3)
g4_subgraph <- induced.subgraph(g4, in_mid_community4)
g5_subgraph <- induced.subgraph(g5, in_mid_community5)

# Plot those mid-size communities
plot(g1_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(g1_subgraph), margin = 0, vertex.size = 3, main="Communities on 01-07-2009")
plot(g2_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(g2_subgraph), margin = 0, vertex.size = 3, main="Communities on 02-07-2009")
plot(g3_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(g3_subgraph), margin = 0, vertex.size = 3, main="Communities on 03-07-2009")
plot(g4_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(g4_subgraph), margin = 0, vertex.size = 3, main="Communities on 04-07-2009")
plot(g5_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(g5_subgraph), margin = 0, vertex.size = 3, main="Communities on 05-07-2009")

plot(in_mid_community1, g1, layout=coords)

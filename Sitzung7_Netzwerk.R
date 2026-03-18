install.packages("igraph")
install.packages("ggraph")
install.packages("netrankr")
install.packages("tidygraph")
install.packages("here")

library(igraph)
library(ggraph)
library(tidygraph)
library(netrankr)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(here)


florence_URL <- "https://raw.githubusercontent.com/kosukeimai/qss/master/DISCOVERY/florentine.csv"

florence <- read.csv(florence_URL, row.names = 1, check.names = FALSE)

florence <- as.matrix(florence)

florence_g <- graph_from_adjacency_matrix(florence, mode = "undirected", diag = FALSE)

flo_net1 <- ggraph(florence_g, layout = "fr") +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(size = 5, color = "steelblue")+
  geom_node_text(aes(label = name), repel = TRUE)

flo_net1

florence_g <- delete_vertices(florence_g, which(degree(florence_g) == 0))



degree <- degree(florence_g)
betweenness <- betweenness(florence_g)
closeness <- closeness(florence_g)

flo_net1_degree <- ggraph(florence_g, layout = "fr")+
  geom_edge_link(alpha = 0.4)+
  geom_node_point(aes(size = degree), color = "steelblue") + 
  geom_node_text(aes(label = name), repel = TRUE)+
  theme_graph()+
  labs(size = "Degree - Zentralität")

flo_net1_degree


flo_net1_betweenness <- ggraph(florence_g, layout = "fr")+
  geom_edge_link(alpha = 0.4)+
  geom_node_point(aes(size = betweenness), color = "steelblue") + 
  geom_node_text(aes(label = name), repel = TRUE)+
  theme_graph()+
  labs(size = "Betweenness - Zentralität")

flo_net1_betweenness

flo_net1_closeness <- ggraph(florence_g, layout = "fr")+
  geom_edge_link(alpha = 0.4)+
  geom_node_point(aes(size = closeness), color = "steelblue") + 
  geom_node_text(aes(label = name), repel = TRUE)+
  theme_graph()+
  labs(size = "Closeness - Zentralität")

flo_net1_closeness










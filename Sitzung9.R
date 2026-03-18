library(ggraph)
library(tidygraph)
library(netrankr)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(here)
library(igraph)


edgelist <- data.frame(
  from = c(1,1,2,4,5,7,8),
  to = c(2,5,3,7,9,8,10)
)
edgelist <- edgelist %>%
  mutate(typ = c("freundschaft", "kollege", "freundschaft", "kollgege",
                 "freundschaft", "kollege", "freundschaft"))
nodes = data.frame(name = 1:10,
                   gender = c("m", "w", "w", "d", "m", "w", "d", "m", "d", "m"))
view(edgelist)
view(nodes)

graph <-graph_from_data_frame(edgelist, vertices = nodes, directed = FALSE)
graph1 <- ggraph(graph, layout = "fr") +
  geom_edge_link(width = 1) +
  geom_node_point(color = "steelblue", size = 5) +
  geom_node_text(aes(label = name), vjust = -1) + 
  theme_graph()

graph1

degree(graph)
graph <- delete_vertices(graph, V(graph)[degree(graph) == 0])
graph1 <- ggraph(graph, layout = "fr") +
  geom_edge_link(width = 1) +
  geom_node_point(color = "steelblue", size = 5) +
  geom_node_text(aes(label = name), vjust = -1) + 
  theme_graph()

graph1

transitivity(graph)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(linetype = typ), width = 0.8) + 
  geom_node_point(color = "steelblue", size = 5) + 
  geom_node_text(aes(label = name), vjust = -1) + 
  theme_graph()

vertex_attr(graph)

ggraph(graph, layout= "fr") +
  geom_edge_link(width = 1) +
  geom_node_point(color = "steelblue", size = 5) +
  geom_node_text(aes(label = name), vjust = -1) + 
  scale_color_manual(values = c("m" = "red", "w" = "blue", "d" = "purple"))+
  theme_graph()





edgelistNSU <- read_csv("edgelistNSU.csv") %>% select(-1)

nsu_graph <- graph_from_data_frame(edgelistNSU, directed = FALSE)


nsu_net1 <- ggraph(nsu_graph, layout ="fr") +
  geom_edge_link()+
  geom_edge_point()+
  geom_node_text(aes(label = name), vjust = -1)+
  theme_graph()

nsu_net1

nsu_net2 <- ggraph(nsu_graph, layout = "fr")+
  geom_edge_link(color = "lightgrey")+
  geom_node_point(color = "firebrick", alpha = 0.4)+
  geom_node_text(aes(label = name), vjust = -1, size = 1.4)+
  theme_graph()


nsu_net2

transitivity(nsu_graph)


edgelist.nsu_neu <- read_csv("edgelistNSU.csv") %>%
  select(-1)

nsu_graph_neu <- graph_from_data_frame(edgelist.nsu_neu, directed = FALSE)
nsu_betweenness <- betweenness(nsu_graph_neu)
nodes_bet <- 


edgelist.nsu_waffen <- read_csv("edgelist_waffen.csv") %>%
  select(-1)
nsu.waffen_graph <- graph_from_data_frame(edgelist.nsu_waffen, directed = TRUE)
nsu_net_weap <- ggraph(nsu.waffen_graph, layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(4, "mm")),
                 end_cap = circle(3, "mm"), color = "grey") + 
  geom_node_point(color = "firebrick", size = 1.7) +
  geom_node_text(aes(label = name), vjust = -1) +
  theme_graph()
nsu_net_weap


#betweenness
nsu_net_weap_1 <- ggraph(nsu.waffen_graph, layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(4, "mm")),
                 end_cap = circle(3, "mm"), color = "grey") + 
  geom_node_point(color = "firebrick", size = betweenness(nsu.waffen_graph)) +
  geom_node_text(aes(label = name), vjust = -1) +
  theme_graph()
nsu_net_weap_1

#closeness

nsu_net_weap_2 <- ggraph(nsu.waffen_graph, layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(4, "mm")),
                 end_cap = circle(3, "mm"), color = "grey") + 
  geom_node_point(color = "firebrick", size = closeness(nsu.waffen_graph)) +
  geom_node_text(aes(label = name), vjust = -1) +
  theme_graph()
nsu_net_weap_2

#degree
nsu_weap_degree <- degree(nsu.waffen_graph)
nsu_net_weap_3 <- ggraph(nsu.waffen_graph, layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(4, "mm")),
                 end_cap = circle(3, "mm"), color = "grey") + 
  geom_node_point(color = "firebrick", size = nsu_weap_degree) +
  geom_node_text(aes(label = name), vjust = -1) +
  theme_graph()
nsu_net_weap_3




nsu_weap <- as_data_frame(nsu.waffen_graph, what = "edges")
nsu_weap$typ <- "waffen"

nsu_gesamt <- as_data_frame(nsu_graph_neu, what = "edges")

nsu_weap_komb <- nsu_gesamt %>%
  left_join(nsu_weap %>%
              select(from, to, typ), by = c("from", "to"))

nsu_weap_komb <- nsu_weap_komb %>%
  mutate(typ_plot = ifelse(is.na(typ), "non", typ))

nsu_graph_komb <- ggraph(nsu_weap_komb, layout = "fr") + 
  geom_edge_link(arrow = arrow(length = unit(3, "mm")), end_cap = circle(2, "mm"), 
                 aes(color = typ_plot, alpha = typ_plot), width = 0.5, show.legend = FALSE) + 
  scale_edge_color_manual(values = c("Waffen" = "orange", "non" = "white")) + 
  geom_node_point(color = "firebrick", size = 0.4) + 
  geom_node_text(aes(label = name), vjust = -1, size = 1.4) + 
  theme_graph()
nsu_graph_komb

nsu_nodelist <- read_csv("attribute.csv") %>%
  select(-1)

nsu_graph_attr <- graph_from_data_frame(edgelist.nsu_neu, vertices = nsu_nodelist, directed = FALSE)
table(nsu_nodelist$Geschlecht)

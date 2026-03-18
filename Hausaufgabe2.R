library(tidyverse) 
library(igraph)
library(ggraph)
library(readr)
library(ggplot2)
library(ggrepel)





#Aufgabe 1-------------
#Die soziale Netzwerkanalyse geht davon aus, dass soziales Handeln und soziale Strukturen nicht isoliert, 
#sondern relational verstanden werden müssen. Zentrale Grundannahmen sind:
#1. Beziehungen stehen im Mittelpunkt
#2. Akteur*innen sind in Netzwerke eingebettet
#3. Soziale Strukturen entstehen aus Beziehungen

#Die soziale Netzwerkanalyse geht davon aus, dass soziales Handeln wesentlich durch 
#Beziehungen und Netzwerkstrukturen geprägt ist. Im Gegensatz zum methodologischen 
#Individualismus, der individuelles Handeln in den Mittelpunkt stellt, betrachtet die 
#SNA Akteur*innen als in soziale Netzwerke eingebettet und erklärt soziale Phänomene
#über relationale Strukturen statt ausschließlich über individuelle Eigenschaften.

#Aufgabe2----------
#(a)
twitter_URL <-
  "https://raw.githubusercontent.com/kosukeimai/qss/master/DISCOVERY/twitter-following.csv"

#(b)
twitter <- read_csv(twitter_URL )

head(twitter, 5)

#(c)
senator_URL <-
  "https://raw.githubusercontent.com/kosukeimai/qss/master/DISCOVERY/twitter-senator.csv"
senator <- read_csv(senator_URL)

#(d)
#Der Datensatz twitter enthält eine gerichtete Edgelist, die abbildet,
#welche Senatorinnen anderen auf Twitter folgen. Der Datensatz senator
#enthält Attribute zu den einzelnen Senatorinnen, wie Partei oder 
#Geschlecht, und stellt damit Knoteneigenschaften dar.

#(e)
graph <-graph_from_data_frame(twitter, vertices = senator, directed = TRUE)

#Aufgabe3-----

graph1 <- ggraph(graph, layout = "kk") +
  geom_edge_link(
    alpha = 0.08,
    colour = "grey50",
    arrow = arrow(length = unit(2, "mm"))) +
  geom_node_point(
    aes(size = degree(graph), color = party)) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3) +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = c(
    "D" = "#E41A1C",
    "R" = "#377EB8",
    "I" = "#4DAF4A")) +
  theme_void() +
  theme(legend.position = "right")
graph1


#Aufgabe 4-----
#Transitivität misst, wie stark ein Netzwerk zur Triadenbildung neigt.In gerichteten Netzwerken bedeutet Transitivität:
#Gibt es geschlossene Dreiecke mit konsistenter Richtung?
#Folgen Senator*innen den Personen, denen ihre Kontakte ebenfalls folgen? Das heißt:
#Hohe Transitivität → viele geschlossene Gruppen / Cliquen
#Niedrige Transitivität → eher lockeres, wenig geschlossenes Netzwerk
trans <- transitivity(graph, type = "global")
trans
#Das heißt, für das Twitter-Netzwerk der US-Senatorinnen ergibt sich eine mittlere Transitivität, 
#was auf eine moderate Gruppenbildung hindeutet. Dies legt nahe, dass Senatorinnen häufig Personen folgen, 
#denen auch ihre direkten Kontakte folgen, insbesondere innerhalb politischer Lager.

#Aufgabe 5--------
V(graph)$indegree <- degree(graph, mode = "in")
V(graph)$outdegree <- degree(graph, mode = "out")
V(graph)$betweenness <- betweenness(graph, directed = TRUE)
V(graph)$closeness <- closeness(graph, mode = "out")

graph_indegree <- ggraph(graph, layout = "kk") +
  geom_edge_link(
    alpha = 0.08,
    colour = "grey50",
    arrow = arrow(length = unit(2, "mm"))) +
  geom_node_point(
    aes(size = indegree, color = party)) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3) +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = c(
    "D" = "#E41A1C",
    "R" = "#377EB8",
    "I" = "#4DAF4A")) +
  theme_void() +
  theme(legend.position = "right")
graph_indegree

graph_outdegree <- ggraph(graph, layout = "kk") +
  geom_edge_link(
    alpha = 0.08,
    colour = "grey50",
    arrow = arrow(length = unit(2, "mm"))) +
  geom_node_point(
    aes(size = outdegree, color = party)) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3) +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = c(
    "D" = "#E41A1C",
    "R" = "#377EB8",
    "I" = "#4DAF4A")) +
  theme_void() +
  theme(legend.position = "right")
graph_outdegree

graph_betweenness <- ggraph(graph, layout = "kk") +
  geom_edge_link(
    alpha = 0.08,
    colour = "grey50",
    arrow = arrow(length = unit(2, "mm"))) +
  geom_node_point(
    aes(size = betweenness, color = party)) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3) +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = c(
    "D" = "#E41A1C",
    "R" = "#377EB8",
    "I" = "#4DAF4A")) +
  theme_void() +
  theme(legend.position = "right")
graph_betweenness

graph_closeness <- ggraph(graph, layout = "kk") +
  geom_edge_link(
    alpha = 0.08,
    colour = "grey50",
    arrow = arrow(length = unit(2, "mm"))) +
  geom_node_point(
    aes(size = closeness, color = party)) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 3) +
  scale_size_continuous(range = c(2, 6)) +
  scale_color_manual(values = c(
    "D" = "#E41A1C",
    "R" = "#377EB8",
    "I" = "#4DAF4A")) +
  theme_void() +
  theme(legend.position = "right")
graph_closeness
#Die Visualisierung zeigt, dass einige wenige Senator*innen besonders zentral sind 
#und das Netzwerk insgesamt stark nach Parteizugehörigkeit strukturiert ist.


#ufgabe 6-------
#(a)
degree_df <- data.frame(
  name = V(graph)$name,
  indegree = V(graph)$indegree,
  outdegree = V(graph)$outdegree
)

senator <- senator %>%
  left_join(degree_df, by = "name")
View(senator)

#(b)
senator %>%
  filter(name == "Bernard Sanders") %>%
  select(name, indegree, outdegree)

#(c)
senator %>%
  arrange(desc(indegree)) %>%
  select(name, indegree) %>%
  slice(1:5)

#Aufgabe 7----------

ggplot(senator, aes(x = outdegree, y = indegree, color = party)) +
  geom_point(alpha = 0.7) +
  theme_minimal()

#Die Ergebnisse zeigen keinen starken linearen Zusammenhang zwischen beiden Größen.
#Dies deutet darauf hin, dass Senatorinnen, die von vielen Kolleginnen auf Twitter verfolgt werden, 
#nicht zwangsläufig selbst besonders aktiv im Folgen anderer Senatorinnen sind. 
#Popularität und Aktivität stellen somit unterschiedliche Dimensionen der Netzwerkposition dar.


library(ggraph)
library(tidygraph)
library(netrankr)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(here)
library(igraph)

nsu_nodelist <- read.csv("attribute.csv") %>%
  select(-1)
edgelist <- read_csv("edgelist.csv") %>%
  select(-1)

nsu_edgelist_attr <- graph_from_data_frame(edgelist, vertices = nsu_nodelist, directed = FALSE)

names(nsu_nodelist)
table(nsu_nodelist$Geschlecht)
table(nsu_nodelist$THS)

V(nsu_edgelist_attr)$Geschlecht <- factor(V(nsu_edgelist_attr)$Geschlecht, 
                                          levels = c(1,2), 
                                          labels = c("male", "female"))


nsu_female <- ggraph(nsu_edgelist_attr, layout = "fr") + 
  geom_edge_link(color = "lightgrey") + 
  geom_node_point(aes(color = Geschlecht), alpha = 0.4, size = 3) + 
  scale_color_manual(values = c("female" = "red", "male" = "darkgreen")) + 
  geom_node_text(aes(label = name), vjust =-1, size = 1.4) + 
  theme_graph()
nsu_female

degree_f <- degree(nsu_edgelist_attr)
betweenness_f <- betweenness(nsu_edgelist_attr)
closeness_f <- closeness(nsu_edgelist_attr)

nsu_female_degree <- ggraph(nsu_edgelist_attr, layout = "fr") + 
  geom_edge_link(color = "lightgrey") + 
  geom_node_point(aes(color = Geschlecht, size = degree_f), alpha = 0.4) + 
  scale_color_manual(values = c("female" = "red", "male" = "darkgreen")) + 
  geom_node_text(aes(label = name), vjust =-1, size = 1.4) + 
  theme_graph()
nsu_female_degree

nsu_female_betweenness <- ggraph(nsu_edgelist_attr, layout = "fr") + 
  geom_edge_link(color = "lightgrey") + 
  geom_node_point(aes(color = Geschlecht, size = betweenness_f), alpha = 0.4) + 
  scale_color_manual(values = c("female" = "red", "male" = "darkgreen")) + 
  geom_node_text(aes(label = name), vjust =-1, size = 1.4) + 
  theme_graph()
nsu_female_betweenness

nsu_female_closeness <- ggraph(nsu_edgelist_attr, layout = "fr") + 
  geom_edge_link(color = "lightgrey") + 
  geom_node_point(aes(color = Geschlecht, size = closeness_f), alpha = 0.4) + 
  scale_color_manual(values = c("female" = "red", "male" = "darkgreen")) + 
  geom_node_text(aes(label = name), vjust =-1, size = 1.4) + 
  theme_graph()
nsu_female_closeness


install.packages("maps")
library(maps)
install.packages("ggthemes")
library(ggthemes)

elect_usa <- read.csv("president_electionsUSA.csv")
us_states <- map_data("state")

elect_usa_20 <- elect_usa %>%
  filter(year == 2020) %>%
  select(state, year, candidate, party_simplified, totalvotes, candidatevotes)
View(elect_usa_20)

us_states_map <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")
us_states_map

elect_usa_20$state <- tolower(elect_usa_20$state)

elect_usa_20 <- elect_usa_20 %>%
  rename(region = state)

us_states_elec <- left_join(us_states, elect_usa_20)

us_states_elec_winner <- us_states_elec %>%
  group_by(region) %>%
  slice_max(candidatevotes) %>%
  ungroup()

party_colors <- c("#2E74CD", "#CB454A")

map_elec20 <- ggplot(data = us_states_elec_winner, 
                     mapping = aes(x = long, y = lat, group = region, fill = party_simplified)) + 
  geom_polygon(color = "grey90", size = 0.1) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_manual(values = party_colors) + 
  labs(titel = "Election Results 2020", fill = NULL) +
  theme_map()
map_elec20

us_states_elec_winner <- us_states_elec_winner %>%
  group_by(region) %>%
  slice_max(candidatevotes) %>%
  mutate(vote_share = candidatevotes / totalvotes) %>%
  ungroup()

maps_elec_voteshare <- ggplot(us_states_elec_winner,
                              aes(long, lat, group = region, fill = voteshare)) + 
  geom_polygon(color = "grey90") + 
  coord_map("albers", lat0 = 39, lat1 = 45) + 
  scale_fill_viridis_c(name = "vote share") + 
  theme_map()

maps_elec_voteshare


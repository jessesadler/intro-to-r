### Introduction to Network Analysis with R ###

# This script goes along with the blog post of the same name,
# which can be found at https://www.jessesadler.com/post/network-analysis-with-r/

library(tidyverse)

# Load data
letters <- read_csv("data/correspondence-data-1585.csv")

################################
## Create node and edge lists ##
################################

### Node list ###
sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")

# Create id column and reorder columns
nodes <- add_column(nodes, id = 1:nrow(nodes)) %>% 
  select(id, everything())

### Edge list ###
per_route <- letters %>%  
  group_by(source, destination) %>%
  summarise(weight = n()) %>% 
  ungroup()

# Join with node ids and reorder columns
edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

#####################
## network package ##
#####################

library(network)

# network object
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

# Print network object
routes_network

# network plot
plot(routes_network, vertex.cex = 3)

# network circle plot
plot(routes_network, vertex.cex = 3, mode = "circle")

####################
## igraph package ##
####################

# Clean environment
detach(package:network)
rm(routes_network)
library(igraph)

# igraph object
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Print igraph object
routes_igraph

# igraph plot
plot(routes_igraph, edge.arrow.size = 0.2)

# igraph-graphopt-plot
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

##########################
## tidygraph and ggraph ##
##########################

# Load libraries
library(tidygraph)
library(ggraph)

# edge list and node list to tbl_graph
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# igraph to tbl_graph
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

# Show classes of objects
class(routes_tidy)
class(routes_igraph_tidy)
class(routes_igraph)

# Print routes tidy
routes_tidy

# Activate edges
routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

# Basic ggraph plot
ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

# More complex ggraph plot
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

# ggraph arc plot
ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

################
## visNetwork ##
################

library(visNetwork)

# Simple interactive plot
visNetwork(nodes, edges, width = "100%")

# Width attribute
edges <- mutate(edges, width = weight/5 + 1)

# visNetwork edge width plot
visNetwork(nodes, edges, width = "100%") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

###############
## networkD3 ##
###############

library(networkD3)

# Redo id numbers to have them begin at 0
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

# d3 force-network plot
forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

# Sankey diagram
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

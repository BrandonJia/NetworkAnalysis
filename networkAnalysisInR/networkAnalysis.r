# load packages for network exploration
library(igraph)
library(readr)
library(ggraph)
library(dplyr)

setwd('C:/Users/H.J. Jia/OneDrive/MOOC/datacamp/materials/network in r/data/')

# read nodes and ties data into variables
nodes <- read_csv('nodes.csv')
ties <- read_csv('ties.csv')

# build a network from data frames
g <- graph_from_data_frame(d = ties, 
                           directed = FALSE,
                           vertices = nodes)


# explore the set of nodes and print the number of nodes
V(g)
vcount(g)

E(g)
ecount(g)

# add node attribute id and print the note 'id' attribute
g$name <- 'Madrid network'
g$name

# add node attribute id and node 'id' attribute
V(g)$id <- 1:vcount(g)

# print the tie 'weight' attribute
E(g)$weight


# Visualize the network with the Kamada-Kawai layout 
ggraph(g, layout = "with_kk") + 
  # Add an edge link geometry mapping transparency to weight 
  geom_edge_link(aes(alpha = weight)) + 
  # Add a node point geometry
  geom_node_point() + 
  # Add a node text geometry, mapping label to id and repelling
  geom_node_text(aes(label = id), repel = TRUE)

# Visualize the network in a circular layout
ggraph(g, layout = "in_circle") + 
  # Map tie transparency to its weight
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()

# Change the layout so points are on a grid
ggraph(g, layout = "on_grid") + 
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()

# The degree of each node is the number of adjacent ties it has. In the context of this dataset, that means the number of other people that person is connected to.

nodes_with_centrality <- nodes %>%
  # Add a column containing the degree of each node
  mutate(degree = degree(g)) %>%
  # Arrange rows by descending degree
  arrange(desc(degree))

# See the result
nodes_with_centrality

# Strength: Summing up the edge weights of the adjacent edges for each vertex.

nodes_with_centrality <- nodes %>%
  mutate(
    degree = degree(g),
    # Add a column containing the strength of each node
    strength = strength(g)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))

# See the result
nodes_with_centrality

# betweenness(): a measure that quantifies how often a node lies on the shortest path between other nodes.
# closeness(): a measure that quantifies how close a node is to all other nodes in the network in terms of shortest path distance.

# Calculate the reciprocal of the tie weights
dist_weight <- 1 / E(g)$weight

ties_with_betweenness <- ties %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweenness = edge_betweenness(g, weights = dist_weight))

# Review updated ties
ties_with_betweenness

ties_joined <- ties_with_betweenness %>% 
  # Left join to the nodes matching 'from' to 'id'
  left_join(nodes, by = c("from" = "id")) %>% 
  # Left join to nodes again, now matching 'to' to 'id'
  left_join(nodes, by = c('to' = 'id'))

# See the result
ties_joined

# Select only relevant variables
ties_selected <- ties_joined %>% 
  select(from, to, name_from = name.x, name_to = name.y, betweenness)

# See the result
ties_selected

ties_selected %>%
  # Arrange rows by descending betweenness
  arrange(desc(betweenness))

g <- graph_from_data_frame(d = ties, 
                           directed = FALSE,
                           vertices = nodes_with_centrality)

# Plot with the Kamada-Kawai layout 
ggraph(g, layout = 'with_kk') + 
  # Add an edge link geom, mapping alpha to weight
  geom_edge_link(aes(alpha = weight)) + 
  # Add a node point geom, mapping size to degree
  geom_node_point(aes(size = degree))

# Update the previous plot, mapping node size to strength
ggraph(g, layout = "with_kk") + 
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point(aes(size = strength))

g <- graph_from_data_frame(d = ties_with_betweenness, 
                           directed = FALSE,
                           vertices = nodes_with_centrality)

ggraph(g, layout = "with_kk") + 
  # Add an edge link geom, mapping the edge transparency to betweenness
  geom_edge_link(aes(alpha = betweenness))

ggraph(g, layout = "with_kk") + 
  geom_edge_link(aes(alpha = betweenness)) + 
  # Add a node point geom, mapping size to degree
  geom_node_point(aes(size = degree))

# Calculate the median betweenness
median_betweenness = median(E(g)$betweenness)

ggraph(g, layout = "with_kk") + 
  # Filter ties for betweenness greater than the median
  geom_edge_link(aes(alpha = betweenness, filter = betweenness > median_betweenness)) + 
  theme(legend.position="none")

tie_counts_by_weight <- ties %>% 
  # Count the number of rows with each weight
  count(weight) %>%
  # Add a column of the percentage of rows with each weight
  mutate(percentage = 100 * n / nrow(ties)) 

# See the result
tie_counts_by_weight


# Make is_weak TRUE whenever the tie is weak
is_weak <- ifelse(E(g)$weight == 1, TRUE, FALSE)

# Check that the number of weak ties is the same as before
sum(is_weak)

ggraph(g, layout = "with_kk") +
  # Add an edge link geom, mapping color to is_weak
  geom_edge_link(aes(color = is_weak))



# Draw raster plot
# raster plot: plot to visualize the ties between nodes in a network. The idea is to draw a point in the plot at position (x, y) if there is a tie that connects the nodes x and y.
ties_swapped <- ties %>%
  # Swap the variables from and to 
  mutate(temp = to, to = from, from = temp) %>% 
  select(-temp)

# Bind ties and ties_swapped by row
ties_bound <- bind_rows(ties, ties_swapped)

# Using ties_bound, plot to vs. from, filled by weight
ggplot(ties_bound, aes(x = from, y = to, fill = factor(weight))) +
  # Add a raster geom
  geom_raster() +
  # Label the color scale as "weight"
  labs(fill = 'weight')

# Adjacency matrix
# Get the weighted adjacency matrix
A <- as_adjacency_matrix(g, attr = 'weight', names = FALSE)

# See the results
A

# Pearson similarity
# Compute the Pearson correlation matrix of A
S <- cor(A)

# Set the diagonal of S to 0
diag(S) <- 0

# Flatten S to be a vector
flat_S <- as.vector(S)

# Plot a histogram of similarities
hist(flat_S, xlab = "Similarity", main = "Histogram of similarity")

# Explore correlation between degree and strength
# Using nodes, plot strength vs.degree
ggplot(nodes, aes(x = degree, y = strength)) +
  # Add a point geom
  geom_point() +
  # Add a smooth geom with linear regression method
  geom_smooth(method = "lm", se = FALSE)

# Calculate the Pearson correlation coefficient  
cor(nodes$degree, nodes$strength)

# Convert weighted similarity matrix to a graph
h <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE)

# See the results
plot(h)

# Transforming the similarity matrix
# Convert weighted similarity matrix to a graph
h <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE)

# See the results
plot(h)

# Convert h to a data.frame
sim_df <- igraph::as_data_frame(h)

# See the result
head(sim_df)

# Notice that this is a base-R data.frame
class(sim_df)

# Convert sim_df to a tibble
sim_tib <- as_tibble(sim_df)

# See the results
sim_tib


sim_joined <- sim_df %>% 
  # Left join to nodes matching "from" to "id"
  left_join(nodes, by = c("from" = "id")) %>% 
  # Left join to nodes matching "to" to "id", setting suffixes
  left_join(nodes, by = c('to'='id'), suffix = c('_from','_to')) 

# See the results
sim_joined

sim_joined %>% 
  # Arrange by descending similarity
  arrange(desc(similarity))


sim_joined %>%
  # Filter for degree from & degree to greater than or equal to 10
  filter(degree_from>=10 & degree_to >= 10) %>%
  arrange(desc(similarity))

sim_filtered <- sim_joined %>% 
  # Filter on similarity greater than 0.6
  filter(similarity > 0.6)

# Convert to an undirected graph
filtered_network <- graph_from_data_frame(sim_filtered,directed = FALSE)

# Plot with Kamada-Kawai layout
ggraph(filtered_network,layout = 'with_kk') + 
  # Add an edge link geom, mapping transparency to similarity
  geom_edge_link(aes(alpha = similarity))


# Compute a distance matrix
D <- 1 - S

# Obtain a distance object 
d <- as.dist(D)

# Run average-linkage clustering method and plot the dendrogram 
cc <- hclust(d, method = "average")
plot(cc)

# Find the similarity of the first pair of nodes that have been merged 
S[40, 45]

# Cut the dendrogram tree into 4 clusters
cls <- cutree(cc, k = 4)

# Add cluster information to nodes
nodes_with_clusters <- nodes %>% mutate(cluster = cls)

# See the result
nodes_with_clusters


# Add cluster information to the network's nodes
V(g)$cluster <- nodes$cluster

# Plot the graph
ggraph(g, layout = "with_kk") + 
  # Add an edge link geom with alpha mapped to weight
  geom_edge_link(aes(alpha = weight), show.legend=FALSE) + 
  # Add a node point geom, colored by cluster as a factor
  geom_node_point(aes(color = factor(cluster))) +
  labs(color = "cluster")

# Update the plot
ggraph(g, layout = "with_kk") + 
  geom_edge_link(aes(alpha = weight), show.legend=FALSE) +  
  geom_node_point(aes(color = factor(cluster))) + 
  labs(color = "cluster") +
  # Facet the nodes by cluster, with a free scale
  facet_nodes(~cluster, scales = 'free')

library(visNetwork)
data <- toVisNetworkData(g)

# Add to the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  # Set the layout to Kamada-Kawai
  visIgraphLayout(layout = 'layout_with_kk')

# See a list of possible layouts
ls("package:igraph", pattern = "^layout_.")

# Update the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  # Change the layout to be in a circle
  visIgraphLayout(layout = "layout_in_circle")

# See a list of possible layouts
ls("package:igraph", pattern = "^layout_.")

# Update the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  # Change the layout to be on a grid
  visIgraphLayout(layout = "layout_on_grid")

# Add to the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  # Choose an operator
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change the options to highlight the nearest nodes and ties
  visOptions(highlightNearest = TRUE)

# Update the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change the options to allow selection of nodes by ID
  visOptions(nodesIdSelection = TRUE)

# Copy cluster node attribute to color node attribute
V(g)$color <- V(g)$cluster

# Convert g to vis network data
data <- toVisNetworkData(g)

# Update the plot
visNetwork(nodes = data$nodes, edges = data$edges, width = 300, height = 300) %>%
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change options to select by group
  visOptions(selectedBy = "group")


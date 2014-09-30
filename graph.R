# visualizing the network; adjacency matrix (with weights) has already been constructed

bob <- read.csv("adjacent.csv", header = TRUE, row.names = 1, check.names = TRUE)

# what libraries do you need?
  
library("igraph")
library("plyr")
library("HiveR")
library("RColorBrewer")

# create the weighted edge list

ls.str(bob) # just checking to see what's under the hood

fred <- t(as.matrix(bob)) # transposing bob so that he becomes fred

greg <- graph.adjacency(fred, weighted = TRUE)

ella <- get.data.frame(greg) # now finally we can make a weighted eged list, calling her ella

head(ella) # we can see that these are some huge weights

# the largest "edge count" is 804; since no organizations are "truly" comparable,
# I'm dividing by the number of individual questions (columns) in the datset: 228

ella$weighted <- ella$weight/228 # taking a proportion of the edge numbers over the number of questions

ella$weight <- NULL

ella$weight <- ella$weighted

ella$weighted <- NULL

head(ella) # check

  # write.table(ella, file = "ella.txt", append = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  # bella<- read.table("ella.txt", header = FALSE, sep = "\t")

  # head(ella.df)

# For the below, I'm improvising; code based off of R-blogger Vessy (http://www.vesnam.com/Rblog/viznets2/)

############################################################################################

# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
gD <- simplify(graph.data.frame(ella, directed = FALSE))

# Print number of nodes and edges
vcount(gD)
ecount(gD)

# Calculate some node properties and node similarities that will be used to illustrate 
# different plotting abilities

    # Calculate degree for all nodes (This is from Vessy's Les Mix example; not sure degrees makes sense here)
    # degAll <- degree(gD, v = V(gD), mode = c("all"), loops = FALSE) 

# Using this instead: Calculate Eigen centrality for all nodes
eigen <- alpha.centrality(gD, nodes = V(gD), alpha=.2, weights = NULL)

# Calculate betweenness for all nodes
betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
rm(betAll)

# Calculate Dice similarities between out pairs of nodes
    # not sure this makes sense since the DICE similarities are coming up as all the same
      # dsAll <- similarity.dice(gD, vids = V(gD), mode = c("out"), loops = FALSE)

# try closeness

# close <- closeness(gD, vids=V(gD), mode = c("all"), normalized = FALSE)

# bib <- bibcoupling(gD, v = V(gD))

short <- shortest.paths(gD, v=V(gD), to=V(gD), mode = c("all"), weights = NULL, algorithm = c("automatic"))



# Add new node/edge attributes based on the calculated node properties/similarities

gD <- set.vertex.attribute(gD, "eigen", index = V(gD), value = eigen)
gD <- set.vertex.attribute(gD, "betweenness", index = V(gD), value = betAll.norm)

# Check the attributes
summary(gD)

# bella <- data.frame(ella)

ella$V1 <- ella$from
ella$V2 <- ella$to
ella$V3 <- ella$weight
ella$from <- NULL
ella$to <- NULL
ella$weight <- NULL

F1 <- function(x) {data.frame(V4 = short[which(V(gD)$name == as.character(x$V1)), which(V(gD)$name == as.character(x$V2))])}
ella.ext <- ddply(ella, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))

gD <- set.edge.attribute(gD, "weight", index = E(gD), value = 0)
gD <- set.edge.attribute(gD, "shortest", index = E(gD), value = 0)

# The order of interactions in gD is not the same as it is in dataSet or as it is in the edge list,
# and for that reason these values cannot be assigned directly

E(gD)[as.character(ella.ext$V1) %--% as.character(ella.ext$V2)]$weight <- as.numeric(ella.ext$V3)
E(gD)[as.character(ella.ext$V1) %--% as.character(ella.ext$V2)]$shortest <- as.numeric(ella.ext$V4)


# Check the attributes
summary(gD)

####################################

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(gD)), NAME = V(gD)$name)
# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(gD, c(1:ecount(gD))))

# Define node and edge attributes - these attributes won't be directly used for network visualization, but they
# may be useful for other network manipulations in Gephi
#
# Create a dataframe with node attributes: 1st column - attribute 1 (degree), 2nd column - attribute 2 (betweenness)
nodes_att <- data.frame(EIG = V(gD)$eigen, BET = V(gD)$betweenness) 
#
# Create a dataframe with edge attributes: 1st column - attribute 1 (weight), 2nd column - attribute 2 (similarity)
edges_att <- data.frame(WGH = E(gD)$weight, SHO = E(gD)$shortest) 

# Define node/edge visual attributes - these attributes are the ones used for network visualization
#
# Calculate node coordinate - needs to be 3D
#nodes_coord <- as.data.frame(layout.fruchterman.reingold(gD, weights = E(gD)$similarity, dim = 3, niter = 10000))
# We'll cheat here, as 2D coordinates result in a better (2D) plot than 3D coordinates
nodes_coord <- as.data.frame(layout.kamada.kawai(gD, weights = E(gD)$shortest, dim = 2, niter = 10000))
nodes_coord <- cbind(nodes_coord, rep(0, times = nrow(nodes_coord)))
#
# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
approxVals <- approx(c(1, 5), n = length(unique(V(gD)$betweenness)))
# And we will assign a node size for each node based on its betweenness centrality
nodes_size <- sapply(V(gD)$eigen, function(x) approxVals$y[which(sort(unique(V(gD)$eigen)) == x)])
#
# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")
# This function returns a function corresponding to a color palete of "bias" number of elements
F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"), bias = length(unique(V(gD)$eigen)), space = "rgb", interpolate = "linear")
# Now we'll create a color for each degree
colCodes <- F2(length(unique(V(gD)$eigen)))
# And we will assign a color for each node based on its degree
nodes_col <- sapply(V(gD)$eigen, function(x) colCodes[which(sort(unique(V(gD)$eigen)) == x)])
# Transform it into a data frame (we have to transpose it first)
nodes_col_df <- as.data.frame(t(col2rgb(nodes_col, alpha = FALSE)))
# And add alpha (between 0 and 1). The alpha from "col2rgb" function takes values from 0-255, so we cannot use it
nodes_col_df <- cbind(nodes_col_df, alpha = rep(1, times = nrow(nodes_col_df)))
# Assign visual attributes to nodes (colors have to be 4dimensional - RGBA)
nodes_att_viz <- list(color = nodes_col_df, position = nodes_coord, size = nodes_size)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(E(gD)$weight)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(gD)$weight)))
edges_col <- sapply(E(gD)$weight, function(x) colCodes[which(sort(unique(E(gD)$weight)) == x)])
edges_col_df <- as.data.frame(t(col2rgb(edges_col, alpha = FALSE)))
edges_col_df <- cbind(edges_col_df, alpha = rep(1, times = nrow(edges_col_df)))
edges_att_viz <-list(color = edges_col_df)

############################################################
# rgexf appears to be glitchy at the moment; blame the development

# Write the network into a gexf (Gephi) file
#write.gexf(nodes = nodes_df, edges = edges_df, nodesAtt = nodes_att, edgesWeight = E(gD)$weight, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "prepare.gexf")
# And without edge weights
# write.gexf(nodes = nodes_df, edges = edges_df, nodesAtt = nodes_att, edgesAtt = edges_att, nodesVizAtt = nodes_att_viz, edgesVizAtt = edges_att_viz, defaultedgetype = "undirected", output = "lesmis.gexf")

############################################################

# good news is, we have almost everything we need for visualization a hive plot
# this is going to be another Vessy production, though with different analyses

rm(eigen, betAll, betAll.norm, F1)



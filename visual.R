# visualizing the network; adjacency matrix (with weights) has already been constructed
rm(list = ls())

hammond <- read.csv("index.csv", header = TRUE, row.names = 1, check.names = TRUE)

# what libraries do you need?

library("igraph")
library("plyr")
library("HiveR")
library("RColorBrewer")

# create the weighted edge list

ls.str(hammond) # just checking to see what's under the hood

hammond <- as.matrix(hammond)

grant <- graph.adjacency(hammond, weighted = TRUE)

satler <- get.data.frame(grant) # now finally we can make a weighted eged list

head(satler) # we can see that these are some huge weights

# I'm normalizing dr. satler

satler$weight <- (satler$weight - min(satler$weight))/(max(satler$weight) - min(satler$weight))

head(satler)

  # View(satler)
  # you'll notice that these are all directed edges

rm(hammond, grant) # and woman shall inherit the earth

# For the below, I'm improvising; code based off of R-blogger Vessy (https://gist.githubusercontent.com/Vessy/6562505/raw/exampleForModEdge2HPD_HiveR.R)

############################################################################################

# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
tRex <- simplify(graph.data.frame(satler, directed = FALSE))

# Print number of nodes and edges
vcount(tRex)
ecount(tRex)

# Vessy's example calculates degrees; but within this graph, all nodes share edges
  # degAll <- degree(graph, v = V(graph), mode = c("all"), loops = FALSE) 

# I think alpha is misbehaving
# alpha <- alpha.centrality(tRex, nodes = V(tRex), alpha=1, weights = NULL)

eigen <- evcent(tRex, directed = FALSE, scale = TRUE, weights = NULL)
eigen <- data.frame(eigen)

# Vessy's example calculates betweenness
# however, the algorithm appears to have been affected by normalized edge weights
  # betAll <- betweenness(tRex, v = V(tRex), directed = FALSE) / (((vcount(tRex) - 1) * (vcount(tRex)-2)) / 2)
  # betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))

# Calculate closeness centrality instead
close <- closeness(tRex, v = V(tRex), mode = c("all"), weights = NULL)
close <- (close - min(close))/(max(close) - min(close))

node.list <- data.frame(name = V(tRex)$name, eigen = eigen$vector, close = close)

# Calculate Dice similarities between out pairs of nodes
  # not sure this makes sense since the DICE similarities are coming up as all the same
  # dsAll <- similarity.dice(tRex, vids = V(tRex), mode = c("in"), loops = FALSE)

# Calculating shortest path instead
short <- shortest.paths(tRex, v=V(tRex), to=V(tRex), mode = c("out"), weights = NULL, algorithm = c("johnson"))

# data needs some massaging before setting the first function F1
satler.ext <- satler

satler.ext$V1 <- satler.ext$from
satler.ext$V2 <- satler.ext$to
satler.ext$V3 <- satler.ext$weight

satler.ext$from <- NULL
satler.ext$to <- NULL
satler.ext$weight <- NULL

F1 <- function(x) {data.frame(V4 = short[which(V(tRex)$name == as.character(x$V1)), which(V(tRex)$name == as.character(x$V2))])}
satler.ext <- ddply(satler.ext, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))

rm(eigen, close, F1)
############################################################################################
#Determine node/edge color based on the properties

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness (closeness) centrality

# we will let closeness centrality define node size
# since 1 is a small value for node size, just multiply by 2.5

nodes_size <- node.list$close*2.5
node.list <- cbind(node.list, size = nodes_size)
rm(nodes_size)

# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")
# This function returns a function corresponding to a collor palete of "bias" number of elements
F2 <- colorRampPalette(c("#ffeda0", "#b10026"), bias = length(unique(node.list$eigen)), space = "rgb", interpolate = "linear")
# Now we'll create a color for each degree (eigen)
colCodes <- F2(length(unique(node.list$eigen)))
# And we will assign a color for each node based on its degree (eigen)
nodes_col <- sapply(node.list$eigen, function(x) colCodes[which(sort(unique(node.list$eigen)) == x)])
node.list <- cbind(node.list, color = nodes_col)
rm(F2, colCodes, nodes_col)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#edf8b1", "#0c2c84"), bias = length(unique(satler.ext$V4)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(satler.ext$V4)))
edges_col <- sapply(satler.ext$V4, function(x) colCodes[which(sort(unique(satler.ext$V4)) == x)])
satler.ext <- cbind(satler.ext, color = edges_col)
rm(F2, colCodes, edges_col)

############################################################################################
# Assign nodes to axes (randomly; from Vessy's example)

# Randomly
nodeAxis <- sample(3, nrow(node.list), replace = TRUE )
node.list <- cbind(node.list, axis = nodeAxis)
rm(nodeAxis)

############################################################################################
#Create a hive plot

source("mod.edge2HPD.R")

harry <- mod.edge2HPD(edge_df = satler.ext[, 1:2], edge.weight = satler.ext[, 3], edge.color = satler.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "eigen")], node.axis = node.list[,c("name", "axis")])
#sumHPD(harry)

################################
# next part has code from SO user 'exegetic' (http://stackoverflow.com/questions/25604399/cropping-hive-plot)

hermione <- mineHPD(harry, option = "remove zero edge")

horcrux1 = sumHPD(hermione, chk.all = TRUE, chk.sm.pt = TRUE, chk.sm.ax = TRUE, plot.list = TRUE)
#
occluding = subset(horcrux1, n1.ax == n2.ax & n1.rad == n2.rad)
occluding = unique(c(as.character(occluding$n1.lab), as.character(occluding$n2.lab)))
#
hermione$nodes$radius = ifelse(hermione$nodes$lab %in% occluding, jitter(hermione$nodes$radius), hermione$nodes$radius)

################################

library(grid)

plotHive(hermione, ch = 0, method = "abs", axLabs.pos = .225, axLabs = c("1", "2", "3"), bkgnd = "white")
# note that 'method = "ranknorm"' produces an identical graph to 'method = "abs"'
# plotHive(hermione, method = "abs", bkgnd = "white")

########################################
# Based on hierarchical clustering
d <- dist(short)
hc <- hclust(d)
#plot(hc)
nodeAxis <- cutree(hc, k = 3)
node.list <- cbind(node.list, axisCl = nodeAxis)
rm(nodeAxis)

albus <- mod.edge2HPD(edge_df = satler.ext[, 1:2], edge.weight = satler.ext[, 3], edge.color = satler.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "eigen")], node.axis = node.list[,c("name", "axisCl")])
#sumHPD(albus)

################################
# next part has code from SO user 'exegetic' (http://stackoverflow.com/questions/25604399/cropping-hive-plot)

snape <- mineHPD(albus, option = "remove zero edge")

horcrux2 = sumHPD(snape, chk.sm.pt = TRUE, chk.sm.ax = TRUE, plot.list = TRUE)
#
occluding = subset(horcrux2, n1.ax == n2.ax & n1.rad == n2.rad)
occluding = unique(c(as.character(occluding$n1.lab), as.character(occluding$n2.lab)))
#
snape$nodes$radius = ifelse(snape$nodes$lab %in% occluding, jitter(snape$nodes$radius), snape$nodes$radius)

################################

plotHive(snape, ch = 0, method = "abs",  axLab.pos = .225, anNodes = NULL, axLabs = c("1","2","3"), bkgnd = "white")

########################################
# Based on hierarchical clustering
d <- dist(eigen)
hc <- hclust(d)
#plot(hc)
nodeAxis <- cutree(hc, k = 3)
node.list <- cbind(node.list, eigenCl = nodeAxis)
rm(nodeAxis)

fish <- mod.edge2HPD(edge_df = satler.ext[, 1:2], edge.weight = satler.ext[, 3], edge.color = satler.ext[, 5], node.color = node.list[,c("name", "color")], node.size = node.list[,c("name", "size")], node.radius = node.list[,c("name", "eigen")], node.axis = node.list[,c("name", "eigenCl")])
#sumHPD(albus)

################################
# next part has code from SO user 'exegetic' (http://stackoverflow.com/questions/25604399/cropping-hive-plot)

land <- mineHPD(fish, option = "remove zero edge")

air = sumHPD(land, chk.sm.pt = TRUE, chk.sm.ax = TRUE, plot.list = TRUE)
#
occluding = subset(horcrux2, n1.ax == n2.ax & n1.rad == n2.rad)
occluding = unique(c(as.character(occluding$n1.lab), as.character(occluding$n2.lab)))
#
land$nodes$radius = ifelse(land$nodes$lab %in% occluding, jitter(land$nodes$radius), land$nodes$radius)

################################

darwin <- plotHive(land, ch = 1, method = "abs",  axLab.pos = .225, anNodes = NULL, axLabs = c("1","2","3"))


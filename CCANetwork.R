
#CCA NETWORK 


##############################################################################################################


#import adjacency matrix
dat <- read.csv("adjMat3.csv", header=TRUE, row.names=1, check.names=FALSE)
mat <- as.matrix(dat)
net <- graph.adjacency (mat, mode="undirected", weighted=NULL)
net
plot(net)

#import attributes
a <- read.csv("attrList.csv")
V(net)$case <- as.character(a$case[match(V(net)$name, a$name)])
V(net)$case
V(net)$convo <- as.character(a$convo[match(V(net)$name, a$name)])
V(net)$convo
net
V(net)$name


#eigencentrality
degree(net)
lo <- layout_components(net)
plot(net, vertex.size=degree(net), vertex.label.dist=1, vertex.label.cex=0.3, layout=lo)
closeness(net)
plot(net, vertex.size=closeness(net)*500, vertex.label.dist=1, vertex.label.cex=0.3, layout=lo)
betweenness(net)
plot(net, vertex.size=betweenness(net)*0.03, vertex.label.dist=1, vertex.label.cex=0.3, layout=lo)
eigen_centrality(net)
plot(net, vertex.size=eigen_centrality(net)$vector*10, vertex.label.dist=1, vertex.label.cex=0.5, layout=lo)
eigen_centrality(finalNet)


##############################################################################################################

#largest clique
cliques(net, min=3) 
largest_cliques(net) 
vcol <- rep("grey80", vcount(net))
vcol[unlist(largest_cliques(net))] <- "gold"
plot(net, vertex.color=vcol, vertex.label.dist=1, vertex.label.cex=0.3, layout=lo)
components(net)
groups(components(net))


# dense subgraph, fastgreedy algorithm
fgc <- fastgreedy.community(net, merges=TRUE, modularity=TRUE)
plot(fgc, net, vertex.label.dist=1, vertex.label.cex=0.3, layout=lo)




##############################################################################################################

#best practices groups
V(net)$color <- V(net)$convo
V(net)$color <- gsub("1", "orange", V(net)$color)
V(net)$color <- gsub("2", "mediumslateblue", V(net)$color)
V(net)$color <- gsub("3", "pink", V(net)$color)
V(net)$color <- gsub("4", "cyan", V(net)$color)
V(net)$color <- gsub("5", "green", V(net)$color)
V(net)$color <- gsub("6", "red", V(net)$color)
plot(net, vertex.label.dist=1, vertex.label.cex=0.5, layout=lo)


#case comp groups
V(net)$color <- V(net)$case
V(net)$color <- gsub("1", "orange", V(net)$color)
V(net)$color <- gsub("2", "mediumslateblue", V(net)$color)
V(net)$color <- gsub("3", "pink", V(net)$color)
V(net)$color <- gsub("4", "cyan", V(net)$color)
V(net)$color <- gsub("5", "green", V(net)$color)
V(net)$color <- gsub("6", "green", V(net)$color)
V(net)$color <- gsub("7", "grey", V(net)$color)
V(net)$color <- gsub("8", "yellow", V(net)$color)
V(net)$color <- gsub("9", "red", V(net)$color)
plot(net, vertex.label.dist=1, vertex.label.cex=0.5, layout=lo)


##############################################################################################################


#cumulative frequency plot
edge_density(net)
centr_degree(net)$centralization
deg <- degree(net)
hist(deg, breaks=1:vcount(net)-1, xlab="Degree")
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot(x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
     xlab="Degree", ylab="Cumulative Frequency")


#network diameter
diameter(net, directed=F) # longest geodesic distance
mean_distance(net, directed=F) # mean distance
distances(net) # length of all shortest paths between nodes
diam <- get_diameter(net)
diam
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange"
plot(net, vertex.color=vcol, edge.color=ecol, vertex.label.dist=1, vertex.label.cex=0.5, layout=lo)
edge_density(finalNet)
##############################################################################################################


#case comp network

net1 <- graph.full(length(V(net)[V(net)$case=="1"]))
V(net1)$name <- V(net)[V(net)$case=="1"]$name
net2 <- graph.full(length(V(net)[V(net)$case=="2"]))
V(net2)$name <- V(net)[V(net)$case=="2"]$name
net3 <- graph.full(length(V(net)[V(net)$case=="3"]))
V(net3)$name <- V(net)[V(net)$case=="3"]$name
net4 <- graph.full(length(V(net)[V(net)$case=="4"]))
V(net4)$name <- V(net)[V(net)$case=="4"]$name
net5 <- graph.full(length(V(net)[V(net)$case=="5"]))
V(net5)$name <- V(net)[V(net)$case=="5"]$name
net6 <- graph.full(length(V(net)[V(net)$case=="6"]))
V(net6)$name <- V(net)[V(net)$case=="6"]$name
net7 <- graph.full(length(V(net)[V(net)$case=="7"]))
V(net7)$name <- V(net)[V(net)$case=="7"]$name
net8 <- graph.full(length(V(net)[V(net)$case=="8"]))
V(net8)$name <- V(net)[V(net)$case=="8"]$name

netNet <- union(net, net1, net2, net3, net4, net5, net6, net7, net8) 
plot(netNet)

#best practices network
g1 <- graph.full(length(V(net)[V(net)$convo=="1"]))
V(g1)$name <- V(net)[V(net)$convo=="1"]$name
g2 <- graph.full(length(V(net)[V(net)$convo=="2"]))
V(g2)$name <- V(net)[V(net)$convo=="2"]$name
g3 <- graph.full(length(V(net)[V(net)$convo=="3"]))
V(g3)$name <- V(net)[V(net)$convo=="3"]$name
g4 <- graph.full(length(V(net)[V(net)$convo=="4"]))
V(g4)$name <- V(net)[V(net)$convo=="4"]$name
g5 <- graph.full(length(V(net)[V(net)$convo=="5"]))
V(g5)$name <- V(net)[V(net)$convo=="5"]$name


netNet2 <- union(net, g1, g2, g3, g4, g5) 
plot(netNet2)

finalNet <- union(netNet, netNet2)
plot(finalNet)
plot(finalNet, vertex.size=eigen_centrality(net)$vector*10, vertex.label.dist=1, vertex.label.cex=0.5, layout=lo)





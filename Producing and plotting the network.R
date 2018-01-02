library("asnipe")
library("igraph")
library("sp")
library("plyr")
library("rgdal")
library("maptools")

#Loading the data (after removing nodes within a certain distance from each other)
DATA <- read.csv("DATA with reduced node number.csv", header=TRUE, stringsAsFactors = FALSE)

#Preparing the data
AID = DATA$transmitter_id
LOC = DATA$Station_code
ping = DATA$datetime_local

DATA$diff_ID <- c(0, diff(AID))
DATA$diff_loc <- c(0, diff(as.factor(LOC)))
DATA$T <- as.numeric(strptime(DATA$datetime_local, format = "%Y/%m/%d %H:%M"))
DATA$time_diff <- c(0, diff(DATA[,22])/60)

move <- (DATA[,20]==0 & DATA[,21]!=0)
b <- rep(NA, length(move))
a <- rep(NA, length(move))
c <- rep(NA, length(move))
d <- rep(NA, length(move))
e <- rep(NA, length(move))

b[move] <- DATA[move, 5] 
a[move] <- DATA[(which(move)-1), 5] 
c[move] <- c(1:length(which(move)))
d[move] <- DATA[move, 1]
e[move] <- DATA[(which(move)-1), 1]

DATA$From <- a
DATA$To <- b
DATA$Movement <- c

#Number of movements less than or equal to 110 hours
subset<-subset(DATA,time_diff<=6600)

net <- subset[,c(24,25)]
net <- na.omit(net)
G1 <- graph.data.frame(net, directed = FALSE)
G2 <- as_adjacency_matrix(G1)
G3 <- as.matrix(G2)
G <- graph.adjacency(G3, mode = "undirected", diag = FALSE, weighted = TRUE)

#Location attributes
#Plotting receivers 
locatt<-readOGR("Receiver shape file","AllReceivers")
locatt2<-spTransform(locatt,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
summary(locatt2)
#Plotting Palmyra Atoll
palmyra<-readOGR("Shape files","Palmyra_Habitat_Map_Final")
palmyra2<-spTransform(palmyra,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(palmyra2)
summary(palmyra2)

#Assigning network to coordinates 
names(locatt2)
poss <- match(V(G)$name, locatt2$Location.C)
V(G)$long <- locatt2$Station_Lo[poss]
V(G)$lat <- locatt2$Station_La[poss]
V(G)$name <- locatt2$Location.C[poss]
lay <- matrix(c(V(G)$long, V(G)$lat), ncol=2)

plot(palmyra2,border="gray76")
plot(G, layout=lay, vertex.size=0.1, edge.curved=0.2, vertex.label= NA,
     vertex.color="mistyrose1", edge.color = "darkred",
     add=TRUE, rescale=FALSE, title(main="Grey Reef Shark movement network"))

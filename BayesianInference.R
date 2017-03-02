#
library(bnlearn)


##################
## DAG creation ##
##################

##creating an empty DAG with 6 nodes
dag1 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#add arc by arc to empty graph
dag1 <- set.arc(dag1, from = "A", to = "E")
dag1 <- set.arc(dag1, from = "S", to = "E")
dag1 <- set.arc(dag1, from = "E", to = "O")
dag1 <- set.arc(dag1, from = "E", to = "R")
dag1 <- set.arc(dag1, from = "O", to = "T")
dag1 <- set.arc(dag1, from = "R", to = "T")

##A different way to add arcs
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#adding arcs by matrix
arc2.set <- matrix(c("A", "E",
                     "S", "E",
                     "E", "O",
                     "E", "R",
                     "O", "T",
                     "R", "T"),
                     byrow = TRUE,
                     ncol = 2,
                     dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc2.set

##A shorter way for greating graphs
dag3 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
#add arcs to DAG using graph model
modelstring(dag3) <- "[A][S][E|A:S][O|E][R|E][T|O:R]"
#the result is the same as before

##The shortest way
dag4 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

##Testing if they are equal
all.equal(dag1, dag2)
all.equal(dag1, dag3)
all.equal(dag1, dag4)

##All this ways to create DAGs guarantee that there are no 
## cycles inside the graph. If you try to introduce a cycle,
## it trows an error.

##Tools
#show nodes
nodes(dag1)
#show graph model
modelstring(dag1)
#show arcs
arcs(dag1)


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


#######################
## DAG visualization ##
#######################

##simple plot DAG
plot(dag1)

##Rgraphviz plot, need library download
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
graphviz.plot(dag1)
#this plot function sort nodes by reducing arcs and nodes overlap


################################
## Probability especification ##
################################

##Establis variable domains
A.st <- c("young", "adult", "old")
S.st <- c("M", "F")
E.st <- c("high", "uni")
O.st <- c("emp", "self")
R.st <- c("small", "big")
T.st <- c("car", "train", "other")

##The DAG have this factorized distribution:
## P(A,S,E,O,R,T) = P(A)P(S)P(E|A,S)P(O|E)P(R|E)P(T|O,R)

##Asign probabilities:
# -to non-Parents variables, P(A) P(S)
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3,
                dimnames = list (A = A.st))
S.prob <- array(c(0.60, 0.40), dim = 2,
                dimnames =  list(S = S.st))
# -to mono-Parents variables, P(O|E) P(R|E)
#  (can be done with matrix or array)
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim=c(2,2),
                dimnames = list(O = O.st, E = E.st))
R.prob <- matrix(c(0.25, 0.75, 0.20, 0.80), ncol = 2,
                 dimnames = list(R = R.st, E = E.st))
# -to multiple-Parents variables, P(E|A,S) (T|O,R)
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64, 
                  0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2), 
                dimnames = list(E = E.st, A = A.st, S = S.st))
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2),
                dimnames = list(T = T.st, O = O.st, R = R.st))


###############################
## Bayesian Network creation ##
###############################

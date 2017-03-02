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

##Create bayesian network
#First, add probabilities to a list
cpt <- list(A=A.prob, S=S.prob, E=E.prob, O=O.prob,
            R=R.prob, T=T.prob)
#then, combine with DAG to create
bn <- custom.fit(dag1,cpt)

##tools
#Show number of parameters on Bayesian Network
nparams(bn)
#show arcs on his DAG
arcs(bn)
#Show parameters of an specific variable
bn$R
#Extract conditional probability distribution of a variable
# from Bayesian Network (note that its the same as R.prob)
R.cpt = coef(bn$R)
#Show every conditional probability distribution of this
# bayesian network
bn


#################################################################
## Parameter estimation:  conditional probability distribution ##
#################################################################

##Read survey file
survey <- read.table("survey.txt", header = TRUE)

##Now we associate this dataset to our DAG, to obtain
## the conditional probability distribution
## We use MLE method (maximun likelihood)
bn.mle <- bn.fit(dag1, data = survey, method = "mle")
## Or we can also use Bayes
bn.bayes <- bn.fit(dag1, data = survey, method = "bayes",
                   iss = 10)

##Its possible to calculate each variable manually
# For example, P(O|E)
prop.table(table(survey[, c("O", "E")]), margin = 2)
# Its the same as before
bn.mle$O


####################################
## Learning DAG: Tests and Scores ##
####################################

##Estimate DAG using survey dataset, using
## Hill-Climbing method (hc)
learned1 <- hc(survey)
modelstring(learned1)
score(learned1, data = survey, type = "bic")

##Estimate DAG using survey dataset, using
## Hill-Climbing method (hc)
learned2 <- hc(survey, score = "bde")
modelstring(learned2)


####################################
## Inference on Bayesian Networks ##
####################################

#####################
## Obtaining independencies showed on DAG

##Check dependencies between two variables. True = independent,
## False = dependent
dsep(dag1, x = "S", y = "R")

##Chek dependencies, contitioning by knowing a third variable
dsep(dag1, x = "S", y = "R", z = "E")

##Chek dependencies, contitioning by knowing a set of variables
dsep(dag1, x="S", y="T",z=c("O", "R"))

##Check if there is a directed path between to variables
path(dag1 , from = "S", to = "R")


#########
## Exact inference

##Needs librares download
source("http://bioconductor.org/biocLite.R")
biocLite(c("graph","RBGL","Rgraphviz"))
#install.packages("gRain")

##
library(gRain)

##Convert our Bayesian Network into a grain item type
junction <- compile(as.grain(bn))

##Get probability distribution, posteriori way
querygrain(junction, nodes="T")$T

##Set new evidence and show probabilities distribution
## evidente --> S = F, watch its influence on T 
## P(T|S = "F")
jsex <- setEvidence(junction, nodes="S", states="F")
querygrain(jsex,nodes="T")$T

##Set another evidence
## R = "small", watch its influence on T
## P(T|R = "small")
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jsex,nodes="T")$T

##E = "high", watch its influence on S and T
# P(S,T|R = "high")
jedu <- setEvidence(junction, nodes = "E", states = "high")

# Check its influence in 2 combined variables
querygrain(jedu, nodes = c("S", "T"), type = "joint")

# Check influence on 2 variables, but separated (marginal)
# P(S|E = "high") and P(T|E = "high")
querygrain(jedu, nodes = c("S", "T"), type = "marginal")

# Check its influence on both conditional
# P(S|T,E = "high") and P(T|S,E = "high")
querygrain(jedu, nodes = c("S", "T"), type = "conditional")


##############
## Approximate inference

##Logical sampling
# S = "M", T = "car"  for P(S = "M",T = "car"|E = "high")
cpquery(bn, event = (S == "M") & (T == "car"), 
        evidence = (E == "high"))
##Its possible to  the aproximation by modifying the n value.
# This value represents the number of random samples we use
cpquery(bn, event = (S == "M") & (T == "car"), 
        evidence = (E == "high"), n = 1000000)

##We can also use a different method, likelihood weighting.
# The logical sampling doesnt works well with low probabilities,
# so we can use this one.
cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = list(E = "high"), method = "lw")

##gRain let us make more complex queries, like this one
# P(S = "M", T = "car"|{A = "young",E = "uni"}U{A = "adult"})
cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = ((A == "young") & (E == "uni"))
        | (A == "adult"))

##Create samples that let this evidence take place. This samples
# have ocurrences of nodes
SxT <- cpdist(bn, nodes = c("S", "T"),
              evidence = (E == "high"))



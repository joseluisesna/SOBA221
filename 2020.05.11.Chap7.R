########################################################################################################################
## SOCIAL NETWORKS (SOBA221, 2019-20)
## Chapter 7, Visualisation
## R script written by J. Luis Estevez (University of Groningen)
## Date: May 11th, 2020
########################################################################################################################

# 1. GENERAL INFORMATION

# In this script we will learn how to visualise networks using the package 'igraph'. Specifically, we will learn how to
# use different algorithms for network layouts, as well as how to embed both node and tie attributes.  The contents here 
# corresponds to chapter 7 of the book 'Analyzing Social Networks' (second edition, 2017), by Borgatti, Everett and 
# Johnson. 

library('igraph') # load the package 'igraph'

# 2. NETWORK LAYOUTS

# Network layout is how we want the nodes to be displayed in the graph. This allocation can be done in multiple ways,
# ranging from one having total control (assigning coordinates in the Cartesian plane (x, y) for each node by hand), to 
# completely at random. Most times, however, we will rely on some existing algorithm that arranges the nodes for us. 

# First, let us create a matrix
N <- 16 # Number of nodes in the network
names <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P') # Names of the nodes
set.seed(123) # for replication purposes
mtx <- matrix(data = rbinom(n = N^2, size = 1, prob = .08),
              nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names)) 
diag(mtx) <- NA # Removal of the diagonal

# And turn this matrix into an igraph-class object
grp <- graph_from_adjacency_matrix(mtx, mode = 'directed')

# If we just plot out igraph object (with no extra arguments), we obtain some 'nice' but apparently random layout 
plot(grp) # try running this same command several times

# What we often do is choosing a pre-existing algorithm with the argument 'layout'. Try the following algorithms:
plot(grp, layout = layout_randomly)
plot(grp, layout = layout_in_circle) # nodes forming a ring
plot(grp, layout = layout_as_tree) # interesting when there exists a hierarchical structure (e.g., who commands whom)
plot(grp, layout = layout_as_star) 
plot(grp, layout = layout_nicely) 
plot(grp, layout = layout_with_kk) 

# What all these algorithms above do is to generate a pair of coordinates for each node. We can obtain the exact 
# coordinates in the plane as follows:
layout_with_kk(grp) # the first column contains the x-coord (horizontal), the second column the y-coord (vertical)
coords <- layout_with_kk(grp)
coords[1,2] <- coords[1,2] - 1 # Ww can modify by hand the coordinates (node A goes one unit less vertically)
plot(grp, layout = coords)

# Saving coordinates comes in very handy when visualising network change
# Let's create two other graphs (say, these are the same network before but observed at time 2 and time 3)
mtx_2 <- mtx
mtx_3 <- mtx

# In mtx_2, I will only change the outdegree of 'L'. In mtx_3, I will change the indegree of 'L'
set.seed(123)
mtx_2['L',] <- rbinom(n = N, size = 1, prob = .1)  
mtx_3[,'L'] <- rbinom(n = N, size = 1, prob = .1) 
diag(mtx_2) <- NA
grp_2 <- graph_from_adjacency_matrix(mtx_2, mode = 'directed') 
diag(mtx_3) <- NA
grp_3 <- graph_from_adjacency_matrix(mtx_3, mode = 'directed') 

# Even using the same algorithm, the position of the nodes changes
plot(grp_2, layout = layout_nicely)
plot(grp_3, layout = layout_nicely)

# We cansave the layout of the first observation (for instance), and keep it for observations later on
mylayout <- layout_nicely(grp) 
par(mfrow = c(1,3), mar = rep(2,4)) # This is to show all networks in the same plot
plot(grp, layout = mylayout)
plot(grp_2, layout = mylayout)
plot(grp_3, layout = mylayout)

par(mfrow = c(1,1)) # back to one plot

# 3. NODE CHARACTERISTICS

# Let's create some dataset of node-level attributes
attr <- data.frame(names)
set.seed(123)
attr$gender <- sample(x = c('male', 'female'), size = N, replace = TRUE) # gender: dichotomous
attr$age <- round(rnorm(N, mean = 21, sd = 4), digits = 0) # age: quantitative 
attr$nation <- sample(x = c('ND', 'DE', 'UK'), size = N, replace = TRUE) # nation: categorical
attr$ideology <- sample(x = c('left', 'centre-left', 'centre', 'centre-right', 'right'), 
                        size = N, replace = TRUE) # ideology: scale

print(attr)

# 3.1. Qualitative attributes: shape, colour
plot(grp, layout = mylayout,
     vertex.shape = ifelse(attr$gender == 'female', 'circle', 'square')) # women circles, men squares
plot(grp, layout = mylayout,
     vertex.color = ifelse(attr$nation == 'ND', 'orange', # dutch orange
                           ifelse(attr$nation == 'DE', 'white', 'blue'))) # german white, belgian  
# We can combine both shape and colour
plot(grp, layout = mylayout,
     vertex.shape = ifelse(attr$gender == 'female', 'circle', 'square'),
     vertex.color = ifelse(attr$nation == 'ND', 'orange', 
                           ifelse(attr$nation == 'DE', 'white', 'blue'))) 

# 3.2. Quantitative attributes: size, colour-gradient
plot(grp, layout = mylayout,
     vertex.size = attr$age)

gradient <- colorRampPalette(c('red','blue')) # create a gradient between red (left) and blue (right)
gradient(5) # Our gradient with 5 breaks
plot(x = 1:5, y = rep(1, 5), pch = 19, cex = 8, col = gradient(5)) 

plot(grp, layout = mylayout,
     vertex.color = ifelse(attr$ideology == 'left', gradient(5)[1], 
                           ifelse(attr$ideology == 'centre-left', gradient(5)[2],
                                  ifelse(attr$ideology == 'centre', gradient(5)[3],
                                         ifelse(attr$ideology == 'centre-right', gradient(5)[4], gradient(5)[5])))))

# Other aspects: vertex.frame.color, vertex.label.color, etc.
plot(grp, layout = mylayout,
     vertex.color = ifelse(attr$ideology == 'left', gradient(5)[1], 
                           ifelse(attr$ideology == 'centre-left', gradient(5)[2],
                                  ifelse(attr$ideology == 'centre', gradient(5)[3],
                                         ifelse(attr$ideology == 'centre-right', gradient(5)[4], gradient(5)[5])))),
     vertex.label.color = 'white')

# 4. TIE CHARACTERISTICS

# 4.1. Directed / Undirected ties
# We need an undirected network
library(sna)
mtx_s <- symmetrize(mtx, rule = 'weak')
rownames(mtx_s) <- names
colnames(mtx_s) <- names

# Defined the igraph object as undirected
grp_s <- graph_from_adjacency_matrix(mtx_s, mode = 'undirected')
# Observe that the arrows (->) turned into (--)
grp
grp_s

# Now, when we plot, we see that the ties have no longer arrowheads
plot(grp_s, layout = mylayout)

# 4.2. Qualitative characteristics: colour, style (line type: solid, dashed, dotted)
# I will create a signed matrix (0, 1, -1)
mtx_sign <- mtx
mtx_sign[9:16,] <- mtx_sign[9:16,] * (-1)
mtx_sign

# Now I create an igraph object, with argument weighted = TRUE
grp_sign <- graph_from_adjacency_matrix(mtx_sign, mode = 'directed', weighted = TRUE, diag = FALSE)
grp
grp_sign
# We can see, the difference between grp and grp_sign is that we have now an edge attribute: weight (e/n)
E(grp_sign)$weight # And we can see the sign of the ties

# Now, let's add this information to our network visually
E(grp_sign)$color <- as.character(factor(E(grp_sign)$weight, levels = c(1, -1), labels = c('green', 'red')))
plot(grp_sign, layout = mylayout)

# We can also change the edge line type ($lty) in addition to the colour
E(grp_sign)$lty <- as.character(factor(E(grp_sign)$weight, levels = c(1, -1), labels = c('solid', 'dashed')))
plot(grp_sign, layout = mylayout)

# 4.3. Quantitative characteristics: width

# Let us create a valued matrix 
diag(mtx_s) <- 0
mtx_v <- mtx_s %*% mtx_s %*% mtx_s # this is a matrix of degree-three, using our undirected network above
diag(mtx_v) <- NA
mtx_v

grp_v <- graph_from_adjacency_matrix(mtx_v, mode = 'undirected', weighted = TRUE, diag = FALSE)
E(grp_v)$width <- E(grp_v)$weight # We add the weights as the width of the edges
plot(grp_v, layout = mylayout)

# Other aspects: edge labels, etc.
E(grp_v)$label <- E(grp_v)$weight 
plot(grp_v, layout = mylayout)

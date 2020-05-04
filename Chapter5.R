########################################################################################################################
## SOCIAL NETWORKS (SOBA221, 2019-20)
## Chapter 5, Data management
## R script written by J. Luis Estevez (University of Groningen)
## Date: May 4th, 2020
########################################################################################################################

# 1. GENERAL INFORMATION
# In this script we will learn how to perform some basic data transformations frequently used in social network analysis
# using R. The contents here corresponds to chapter 5 of the book 'Analyzing Social Networks' (second edition, 2017), by
# Borgatti, Everett and Johnson. We will not cover all the transformations in the book, but only those we judge as the 
# most important. For instance, we entirely omit sections 5.2 and 5.3, about how to import network data from other 
# pieces of software, or data with a format different than a matrix form (i.e., a node list or an edge list).
# Likewise, we won't cover sections 5.4.2 about imputing missing data, section 5.4.6 on combining nodes, section 5.5 on
# normalisation, and section 5.6 about cognitive network structures, 

########################################################################################################################

# 2. CREATING A NETWORK OBJECT
# We will start by 'making up' a network. Obviously, in actual research you will collect your data, not concoct it. 
# To make things simple for us, we will create a network of 10 individuals, and call those individuals 'a', 'b', ..., 'j'.
N <- 10 # Number of nodes in the network
names <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J') # Names of the nodes

set.seed(123) # this is only for all of us to have the same network
ties <- rbinom(n = N^2, size = 1, prob = .2) # We create a vector of ties randomly, drawing from a binomial distribution.
mtx <- matrix(data = ties, nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names)) # We arrange ties in a matrix
diag(mtx) <- NA # And we remove the diagonal (let's assume that the type of tie we are capturing cannot be to oneself)

# Let us see the matrix
print(mtx)

# We can observe that, in our matrix, node A chooses D, E and H (the 1s in A's row). Similarly, A is chosen by B, D and
# C (the 1s in a's column).

# To visualise our network, we can use the package 'igraph', for example. 
# If you have not installed this package yet, please, type in the console: install.packages('igraph')

library('igraph') # First, we load the package
grp <- graph_from_adjacency_matrix(mtx, mode = 'directed') # We transform the matrix into an igraph-class object
class(grp)
plot(grp, layout = layout_in_circle) # And, then, plot the network
# The layout in a circle is just for comparability reasons

########################################################################################################################

# 3. DATA TRANSFORMATIONS
# Let us now starts with some data-transformation

# 3.1. Matrix transposition
# Matrix transposition consists of switching rows for columns. In other words, for all the cells in a matrix, if (x, y)
# stands for the value that is in row x and column y, what we do is changed this values for the value that is in (y, x). 
# Visually, we flip the matrix over its diagonal.
# For matrix transposition, we need not use any addition package as the function 't()' does this for us.

mtx_t <- t(mtx) # mtx_t is the transposed of mtx
t(t(mtx)) # Obviously, the transposed of the transposed is the original matrix

# Visually we can see that the direction of the ties has been reverted. 
plot(graph_from_adjacency_matrix(mtx_t, mode = 'directed'), layout=layout_in_circle)

# 3.2. Symmetrising
# For symmetrising matrices, we will use the function 'symmetrize()', as defined in the package 'sna'.
# If you have not installed this package yet, please, type in the console: install.packages('sna')
library('sna')

# By symmetrising, what we do is getting rid of the directionality of ties. Rather than knowing whether A chooses B, or
# whether B chooses A, we simplify things to 'there is a tie between A and B'.
# As we saw in the lecture, there're several ways of symmetrising. For non-valued networks, two options are possible: 
# max-symmetrising (if at least one of the two actors reported a tie), and min-symmetrisising (only when both parties 
# reported the tie). In 'sna', we add the argument rule = 'weak' when we want max-symmetrising; and rule = 'strong' for 
# min-symmetrising.
# Let us max-symmetrise our network and visualise it:
mtx_sw <- symmetrize(mtx, rule = 'weak')
# For some reason, the function 'symmetrize()' removes the names of the nodes, so we add these again
rownames(mtx_sw) <- names
colnames(mtx_sw) <- names
plot(graph_from_adjacency_matrix(mtx_sw, mode = 'undirected'), layout=layout_in_circle)
# As we see, we have lost the direction of the ties

# Now, let us min-symmetrise the network
mtx_ss <- symmetrize(mtx, rule = 'strong')
rownames(mtx_ss) <- names
colnames(mtx_ss) <- names
plot(graph_from_adjacency_matrix(mtx_ss, mode = 'undirected'), layout = layout_in_circle)
# In this network, there is one single tie: A-D because in the original network this was the only reciprocated tie.

# We valued networks, we can decide whether we want the sum of (x,y) and (y,x), or the mean/minimum/maximum of the two 
# values, for instance. Unfortunately, the function 'symmetrize()' seems not be written to do this. In R we can always 
# write a function ourselves though.

# Let us create a valued network
set.seed(123) 
ties2 <- rbinom(n = N^2, size = 5, prob = .1) 
mtx_v <- matrix(data = ties2, nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names)) 
diag(mtx_v) <- NA 
mtx_v

# Hypotetically, in this network there may be values between 0 and 5. 
range(mtx_v, na.rm = TRUE) # In this particular case, we only have values between 0 and 3. 

grp_v <- graph_from_adjacency_matrix(mtx_v, mode = 'directed', weighted = TRUE, diag = FALSE)
plot(grp_v, edge.label = E(grp_v)$weight, edge.width = E(grp_v)$weight,  layout = layout_in_circle)

# Let's say, the number over the ties represents phone calls. For instance, A called B once, and B called A twice.
# If we wanted to dichotomised this network, we could decide that the tie between A and B just captures the number of 
# calls between the two, no matter who called whom. We can do this by simply adding the matrix with its transposed:
mtx_v_sum <- mtx_v + t(mtx_v)
grp_v_sum <- graph_from_adjacency_matrix(mtx_v_sum, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(grp_v_sum, edge.label = E(grp_v_sum)$weight, edge.width = E(grp_v_sum)$weight, layout = layout_in_circle)

# Let us say, however, that numbers capture the strength of the relation between two actors. In this case, we may
# be interested in the mean value of the tie between two actors:
mtx2 <- array(data = NA, dim = c(N, N, 2)) # this creates two matrices stuck to each other on a third dimension
mtx2[,,1] <- mtx_v # In a first layer with put our matrix
mtx2[,,2] <- t(mtx_v) # In the second layer, its transposed
mtx_v_mean <- apply(mtx2, c(1,2), mean, na.rm = TRUE)
diag(mtx_v_mean) <- NA
rownames(mtx_v_mean) <- names
colnames(mtx_v_mean) <- names
grp_v_mean <- graph_from_adjacency_matrix(mtx_v_mean, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(grp_v_mean, edge.label = E(grp_v_mean)$weight, edge.width = E(grp_v_mean)$weight, layout = layout_in_circle)

# Likewise, we could obtain the minimun value, or the maximum, for example.

# 3.3. Dichotomising
# Dichotomising is nothing but turning a valued network into a dichotomous network (0, 1). To do so, we need to decide 
# on a threehold value. For example, going back to our network of phone calls...
plot(grp_v_sum, edge.label = E(grp_v_sum)$weight, edge.width = E(grp_v_sum)$weight, layout = layout_in_circle)
# we may decide that we will consider that there is a tie between two actors if, at least, they shared two phone calls.

# Here, we write for you a function to dichotomise networks. Don't worry if you don't understand what it means  
dichotomise <- function(matrix,threshold){
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      if(!is.na(matrix[i,j])){
        if(matrix[i,j] < threshold){
          matrix[i,j] <- 0
        }else{
          matrix[i,j] <- 1
        }
      }
    }
  }
  return(matrix)
}

# The important thing for us is that now we have a function in our enviroment that dichotomises matrices. This function
# has two arguments: matrix (the matrix we want to dichotomise), and thresold (the cut-point value).
# So, let's dichotomise our network of phone calls
mtx_dich <- dichotomise(mtx_v_sum, threshold = 2)
plot(graph_from_adjacency_matrix(mtx_dich, mode = 'undirected'), layout = layout_in_circle)

# 3.4 Combining networks
# In some cases, we have several relational measurements from the same set of nodes, and we may want to reduce the 
# information into one single measure. Say, for instance, that we have 3 networks capturing who beats whom, who mocks
# whom, and who insults whom. The three measures can be seen as different forms of bullying, so we decide to use the 
# three together to create a network of bullying.
set.seed(123)
beat <- matrix(rbinom(n = N^2, size = 1, prob = .05), nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names))   
plot(graph_from_adjacency_matrix(beat, mode = 'directed'), layout = layout_in_circle, diag = FALSE)
mock <- matrix(rbinom(n = N^2, size = 1, prob = .1), nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names))   
plot(graph_from_adjacency_matrix(mock, mode = 'directed'), layout = layout_in_circle, diag = FALSE)
# Because we did not remove the diagonal, observe that actor I mocks himself/herself
insult <- matrix(rbinom(n = N^2, size = 1, prob = .1), nrow = N, ncol = N, byrow = TRUE, dimnames = list(names, names))   
plot(graph_from_adjacency_matrix(insult, mode = 'directed'), layout = layout_in_circle, diag = FALSE)

# We can proceed in the same way as when we symmetrised networks. As matrix addition works cellwise, we can add the 
# three matrices together:
bully <- beat + mock + insult
# And then dichotomise
bully <- dichotomise(bully,1)  
plot(graph_from_adjacency_matrix(bully, mode = 'directed'), layout = layout_in_circle, diag = FALSE)

########################################################################################################################

# 4. MATCHING ATTRIBUTES AND NETWORKS
# Thus far, we have totally disregarded information about our actors themselves ('A', 'B', etc.). We don't know if A is 
# a man or a woman, which age s/he has, or whether s/he is Dutch or not, just to mention some common socio-demographic 
# variables. Let's create some information about these:
attr <- data.frame(names)
set.seed(123)
attr$gender <- sample(x = c('male', 'female'), size = N, replace = TRUE)
attr$age <- round(rnorm(N, mean = 21, sd = 4), digits = 0)
attr$dutch <- sample(x = c('yes', 'no'), size = N, replace = TRUE)
print(attr)
# Now, we know that A is a male, 28, and he is Dutch. D is a female, 18, and she is not Dutch.

# Let's use matrix mtx as a friendship network hereon
friend <- mtx        

# In igraph, we can add attributes to networks using: V(graph)$attribute.
fr_igraph <- graph_from_adjacency_matrix(friend, mode = 'directed') # First, we create an igraph-class object
V(fr_igraph)$gender <- attr$gender # Then, we add the attribute (e.g., gender)
V(fr_igraph)$age <- attr$age
V(fr_igraph)$dutch <- attr$dutch 

# Now, we can also plot the gender of each subject, their age, and whether s/he is Dutch or not:
fr_igraph
plot(fr_igraph, 
     vertex.color = ifelse(V(fr_igraph)$dutch == 'yes','orange','gray'),
     vertex.shape = ifelse(V(fr_igraph)$gender == 'female','circle','square'),
     vertex.size = V(fr_igraph)$age,
     layout = layout_in_circle)

# In sna, attributes are added using set.vertex.attribute().
fr_sna <- network(friend) # First, we create a network-class object
set.vertex.attribute(fr_sna, 'gender', attr$gender)
set.vertex.attribute(fr_sna, 'age', attr$age)
set.vertex.attribute(fr_sna, 'dutch', attr$dutch)
fr_sna

########################################################################################################################

# 5. CONVERTING ATTRIBUTES TO NETWORKS
# Finally, we will see how to convert attributes to networks.
# When we have an attribute like gender, we can easily create a matrix of ego's gender. This is done by repeating the 
# gender of the subject N (the number of nodes in the network) times by column:
matrix(rep(attr$gender, N), nrow = N, ncol = N, byrow = FALSE,  dimnames = list(names, names))
# To create a matrix of alter's gender, we just change the argument byrow to TRUE.
matrix(rep(attr$gender, N), nrow = N, ncol = N, byrow = TRUE,  dimnames = list(names, names))

# To create a matrix of same gender (1 if both actors are same sex, 0 otherwise), we wrote the following function you
# can use. Again, don't worry if you don't understand what it means.  
convert_mtx <- function(attribute,type = c('same','difference','abs. difference')){
  output <- matrix(data = NA, nrow = length(attribute), ncol = length(attribute), dimnames = list(names, names))
  for(i in 1:length(attribute)){
    for(j in 1:length(attribute)){
      
      if(type == 'same'){
        if(attribute[i] == attribute[j]){
          output[i,j] <- 1
        }else{
          output[i,j] <- 0
        }  
      }
      
      if(type == 'difference'){
        output[i,j] <- attribute[i] - attribute[j]
      }
      
      if(type == 'abs. difference'){
        output[i,j] <- abs(attribute[i] - attribute[j])
      }
      
    }
  }
  diag(output) <- NA
  return(output)
}

# You only need to know that you have a function 'convert_mtx' that needs of two arguments, the attribute you want to 
# turn into a matrix, and the type of conversion that you want to perform ('same' ,'difference' ,'abs. difference'). For 
# gender, we use 'same':
convert_mtx(attribute = attr$gender, type = 'same')

# If we wanted a network capturing the difference in age between every subject, we use the type 'abs. difference':
convert_mtx(attribute = attr$age, type = 'abs. difference')

# Finally, if we preferred the simple difference in age, we would have used 'difference':
convert_mtx(attribute = attr$age, type = 'difference')

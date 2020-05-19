########################################################################################################################
## SOCIAL NETWORKS (SOBA221, 2019-20)
## Chapter 6, Multivariate techniques in SNA; and Chapter 8, Testing hytotheses
## R script written by J. Luis Estevez (University of Groningen)
## Date: May 19th, 2020
########################################################################################################################

# 1. GENERAL INFORMATION

# In this script we will learn how to perform hierarchical clustering (HC) and logistic regression with quadratic 
# assignment procedure (LR-QAP). Both techniques are presented in the reference book of this course, Borgatti, Everett &
# Johnson (2017), 'Analyzing Social Networks'. For HC, see chapter 6, pp. 110-113. For QAP test (correlation, multiple
# regression, logistic regression), see chapter 8, pp. 147-157. Because in this course we will work only with non-valued
# networks, I focus on LR-QAP.

########################################################################################################################

# 2. HIERARCHICAL CLUSTERING (HC)

# Clustering methods are a form of data exploration. They can help us understand better our data by detecting grouping 
# patterns. HC in particular, by definition, is a method that iteratively groups observations (here nodes) based on 
# their pair-wise 'distance', until every observation is linked into a large unique group. Eventually, all nodes become 
# part of the same large group. However, in the process of grouping, HC can provide interesting insights.

# One thing that it is important noticing is that HC is not a network technique itself, but can be used for exploring 
# network data. To perform a HC, what we need is a matrix of distances or dissimilarities, for instance, a geodesic 
# distance matrix capturing how far away (how many steps away) each pair of node is. For illustration purposes, here I 
# use a geographical distances between cities in the Netherlads. The philosophy of the method, nonetheless, is 
# applicable to any feature from which we can obtain 'distances' or 'dissimilarities'.

# Data loading (this dataset is freely available at: https://simplemaps.com/data/nl-cities )
cities <- read.csv('X:/My Desktop/dutch_cities.csv') 
str(cities)
# Select only cities that are categorised as 'primary' or 'admin'
cities <- cities[cities$capital %in% c('primary', 'admin'), ] 
View(cities)

# Variables cities$lat and cities$lng contain the lattite and longitude of each city. We can plot this:
library(ggplot2) # this package is only for plotting purposes
ggplot(data = cities) +
  geom_point(aes(x = lng, y = lat)) +
  geom_text(aes(x = lng, y = lat, label = city), size = 3)

# Using the lattitude and longitude, we derive the (Euclidean) between every city
distances <- dist(cities[, c('lat', 'lng')], method = 'euclidean')
distances # this matrix of distance is the imput for our HC function: hclust()

# It remains for deciding the 'linkage criterion'. Any time that we group two nodes into a group, we need to recalculate 
# the distance between this new group and the remaining nodes. The decision about which method are we going to use to 
# find the next closest observation is the linkage criterion. Here, we will use 3: the maximum (complete), the minimum 
# (single) and the mean (average).
hc_max <- hclust(distances, method = 'complete')
hc_min <- hclust(distances, method = 'single')
hc_mean <- hclust(distances, method = 'average')

# Let's see the results visually (complete)
plot(hc_max)
hc_max$labels <- cities$city # we need to add the labels to the dendrogram
plot(hc_max)

# And let's compared the three methods
hc_min$labels <- cities$city 
hc_mean$labels <- cities$city 

par(mfrow = c(1,3)) 
plot(hc_max)
plot(hc_min)
plot(hc_mean)

# To obtain the groups, we use the function cutree, and set the argument k equals to the number of groups that we want
cities$max_3groups <- cutree(hc_max, k = 3)
# If we check the dataset, we have a new variable (max_3groups) containing in which group each city falls
View(cities)

########################################################################################################################

# 3. LOGISTIC REGRESSION WITH QUADRATIC ASSIGNMENT PROCEDURE (LR-QAP)

load('X:/My Desktop/extra3.RData') # Data loading

# 3.1. Multiple and logistic regression models (quick review)

# Example of a multiple regression test
drink_att <- dri[,2] # Attitude towards drinking from very stupid (1) to very good (5)
female <- fem[,1] - 1 # Whether the actor is a woman
sport_beh <- spo[,1] # Whether the actor is very sporty (1) to not sporty at all (5)
View(cbind(drink_att, female, sport_beh))

# Multiple regression (drinking attitude as a linear function of gender and sport behaviour)
model1 <- glm(formula = drink_att ~ female + sport_beh,
              family = gaussian(link = 'identity')) # a normal distribution, the mean
summary(model1)

# Example of a logistic regression test
# Say our response variable was dichotomous though
drink_att_dich <- 1*(drink_att >= 3) # 3 or above -> 1; 0 otherwise
drink_att_dich # Now 1 stands for a positive attitude, 0 for a negative attitude

model2 <- glm(formula = drink_att_dich ~ female + sport_beh,
              family = binomial(link = 'logit')) # a binomial distribution, the logit
summary(model2)

# 3.2. LR-QAP

# QAP enables running logistic regression tests when the data is not a vector of attributes (like attitudes towards 
# drinking), but a matrix of relations, like a friendship network. How does it do it? The LR-QAP algorithm proceeds in
# two steps. In step 1, it performs a regular logistic regression across the cells of the response matrix and the 
# predicting matrices. In step 2, it randomly permutes both rows and columns of the response matrix and recomputes the 
# regression, storing resultant values of all coefficients. This is repeated x times (often thousands) to estimate  
# standard errors for the statistics of interest. Then for each coefficient, the programme counts the proportion of 
# random permutations that yielded a coefficient as extreme as the one computed in step 1, to test its significance. 

# For us, it is important to recall that:
# a) As the assumption of independent observations does not hold, to test the significance of the results we create a 
# sampling distribution based on permutations of the existing data
# b) At a more practical level, to run a LR-QAP, we must transform all our variables to a matrix form

# Let's predict friendship as a function of gender homophily and resemblence in sport behaviour

fri # our response variable is already in the right form

# We create a same gender matrix based on gender
same_gender <- 1 * outer(female, female, '==')
# And a absolute difference matrix based on sport behaviour
sim_sport <- abs(outer(sport_beh, sport_beh, '-'))

# Once we hava all variables in a matrix form, we can run our LR-QAP using the function netlogit() in the package sna

library('sna') 
model3 <- netlogit(y = fri, x = list(same_gender, sim_sport),
                   nullhyp = 'qap', # By default, netlogit uses Dekker's semi-partialling routine (qapspp)
                   reps = 100) # Number of permutations: increase to 100 to reach a precision of 3 decimals (p-values)
summary(model3)

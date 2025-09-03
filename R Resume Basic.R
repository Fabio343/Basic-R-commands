# R : Basic Script to use 
# install packages use -> install.packages('name package')
# load a package -> library('name package')
# load a data set from a library -> data(data set name)
# to view the data set -> View(data set name)

# use ls() function to see variables in the work area
# use print() function to see the data or variable
# use help(function) or ?function to see details about a function

# use class function to check the type of date
# use str() to describe a data frame
# use head() to see a x number of lines

# Some practical examples
library(dplyr)
library(dslabs)
data(heights)

#use $ to access some column in a data frame
#EX:
heights$sex # In this case I can access the variable sex in the data set
Sex<- heights$sex

# To check the length of my column
length(Sex)

# vectors
# I can define a variable as a vector of length N, and put information
# in a for loop for example
vector1 <- vector(length = 10) #logical vector
vector2 <- c(1,10,100,1000,10000,100000,1000000) # numerical vector
vector2[3] # acess the element in the position 3 in the vector

# We can use as.character() or as.numeric() for example to force the 
# vector be a string or numerical vector
vector3 <- as.character(vector2)
vector3
vector4 <- as.numeric(vector3)
vector4

# Sorting vectors
# sort() change the order
# order() return the index that order the vector base on permutation
# rank() similar with order, but show the order the element vector
#EX:
vector5 <- c(31,3,25,90,56)
sort(vector5) # return 3,25,31,56,90
order(vector5)# return 2,3,1,5,4
rank(vector5) # return 3,1,2,5,4

# Vectorial Aritmetic
# We can do some aritimetic actions using vectors
#EX
data(murders)
murder_rate <- (murders$total/murders$population)*100000
# Now I have the rate murders based in a condition 
index <- murder_rate<=0.5
States <- murders$state[index] 
# and now I select the informations based on my condition

####
####indexing
####
#funtions
# which() return positions that a value is True
# match() return index to acess a element 
# %in% check if an element A is present in B
#EX:
vector6 <- c(FALSE,TRUE,TRUE,FALSE,TRUE,TRUE)
which(vector6)
match(c('Arizona','Texas'),murders$state)
x<-c('a','b','c','d')
y<-c('b','d','e','f')
y %in% x # return a logical expression 

#####
## PLOTS
####
# Very usefull to plot using the function plot(x,y) or his(x) or 
# bloxplot(x~a, data=x)
#EX:
plot(murder_rate,murders$population)
hist(murder_rate)
boxplot(population~state,murders)


#####
## Transform data
####
## functions
#  mutate() add a new column
#  filter() filter based on a condition
#  select() is like a filter but select only a group of variables
# EX:
murder <- mutate(murders,rate=(murders$total/murders$population)*100000)
head(murder)

murder_filter <- filter(murder,rate<=0.5)
murder_filter

murder_select <- select(murder,'state','region','rate')
murder_select

####
## Using the pipe (%>%)
## Way to connect actions
#EX:
murders %>%
  filter(state != "Arizona") %>%
  select(state, region)

####
## Dataframes
##
## To create a data frame use: data.frame(name1=v1,name2=v2...namen=vn)
data.frame(variavel_x=x,variavel_y=y)

####################
## Summarise and group by data ##
####################
murders %>% group_by(murders$state)
us_murder_rate <- murders %>% 
  summarise(rate=(sum(total)/sum(population))*100000)
us_murder_rate

#######
## quantile(x,y)
#######
my_quantile <- function(x){
  r<-quantile(x,c(0,0.5,1))
  data.frame(minimum=r[1],median=r[2],maximum=r[3])
}
my_quantile(murders$total)

#########
### pull function access values in pipes
#########
us_murder_rate <- murders %>% 
  summarise(rate=(sum(total)/sum(population))*100000)%>%pull(rate)
us_murder_rate

###
## Arrange() function - use to order a dataframe
###
murders %>% arrange(total)

### 
## Data.table - matrix operations 
### 
#install.packages('data.table')
library(data.table)
murders2 <- copy(murders)
setDT(murders2)              

## Now I can make operations and conditions using this way:
murders2[region == "South" & total > 100]


######################
## R conditionals#####
######################
######################
#if else
# if (condition){
#    operation a
#} else {
#     operation b
#}
#####################
###
#ifelse(condition,value1,value2) - if condition True apply value1 if not value2
###
###

############################
#### Other conditionals ####
############################
#function any() if 1 element is True, return True
#function all() return True only if all elements are True

#########################################
########  Functions #####################
#########################################

### functionA <- function (x){
#               B <- operations in x
#               return B
#               }


#########################################
######### For Loop ######################
#########################################

## for ( i in range of values){
#      operations
#      }

#EX:
compute_n_s <- function(n){
  x<-1:n
  return(sum(x))
}

n <- 10
for (i in 1:n){
  print(compute_n_s(i))
}

n <- 50
vector7 <- vector(length = n)
for (i in 1:n){
  vector7[i] <- compute_n_s(i)
}
vector7



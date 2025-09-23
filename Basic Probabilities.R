#################################################################
########### Basic R - Probabilities #############################
#The Birthday Problem
# Checking for duplicate birthdays in a group of 50 people
n <- 50
bdays <- sample(1:365, n, replace = TRUE)   # generate n random birthdays
any(duplicated(bdays)) # check if any birthday is duplicated

# Monte Carlo simulation with B = 10,000 repetitions
B <- 10000
results <- replicate(B, { # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results) # calculates the proportion of groups with duplicate birthdays

##################################################
##################################################
#Monte Carlo simulation of the 3-door problem if holding
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) # arrange prizes in random order
  prize_door <- doors[prize == "car"] # note which door has the prize
  my_pick <- sample(doors, 1) # note which door was chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1) # open the non-prize door that was not chosen
  stick <- my_pick # keep the original door
  stick == prize_door # test if the original door has the prize
})
mean(stick) # probability of choosing the prize door when holding

##################################################
#Monte Carlo simulation of the switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) # arrange prizes in random order
  prize_door <- doors[prize == "car"] # note which door has the prize
  my_pick <- sample(doors, 1) # note which door was chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1) # open the non-prize door that was not chosen
  switch <- doors[!doors %in% c(my_pick, show)] # switch to the door that was not chosen or opened
  switch == prize_door # test if the switched door has the prize
})
mean(switch) # probability of choosing the prize door when switching
###################################################

###################################################
### Odds for medalists
library(gtools)
library(tidyverse)

# Vector of runners
runners <- c("Jamaica", "Jamaica", "Jamaica",
             "USA", "Ecuador", "Netherlands", "France", "South Africa")

# Set the seed
set.seed(1)

# Monte Carlo simulation
simulation <- replicate(10000, {
  medalists <- sample(runners, 3, replace = FALSE)
  all(medalists == "Jamaica")
})

# Calculate the estimated probability
probability <- mean(simulation)

# Print the result
print(probability)


#########################################
#########################################

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
F <- function(a) mean(x <= a)
1 - F(70)   # probability of a male being taller than 70 inches

#### Plotting a density function
x<- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#########################################
#########################################
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)

cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
qnorm(.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()


#############################################
#OBS:
# E[x]=mu
# to 2 results a and b we have p, 1-p as prob.
# E[x]= a*p + b*(1-p)
# nE[x] = n(a*p+b*(1-p))
# standard deviation -> abs(b-a)*sqrt(p*(1-p))
# n standard deviation -> sqrt(n)*abs(b-a)*sqrt(p*(1-p))
# E[x1...xn]= n*mu
# E[ax]=a*E[x]
# E[(x1...xn)/n] = E[x1...xn]/n = mu]
# SE[x1...xn] = sqrt(sum(SE[x1]**2)) = variance
# SE[ax1] = a*SE[x] = asigma
# 
# Law of large numbers: A large N makes SE[x] denter to zero and the average of 
# the extractions converges to the general average
#
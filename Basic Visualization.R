#####################################################
########### Visualization using R ###################
#####################################################

######### Simple bar chart data visualization
library(dplyr)
library(ggplot2)
library(dslabs)
ds_theme_set()
data(murders)

#creating a new column in the data that indicates the 
#proportion of events and I can then group them by region
murders %>% group_by(region) %>%
  summarize(n = n()) %>%
  mutate(Proportion = n/sum(n), 
         region = reorder(region, Proportion)) %>%
  ggplot(aes(x=region, y=Proportion, fill=region)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  xlab("")

#############################################################
#############################################################
### Line Graph ##############################################
murders %>% mutate(murder_rate = total/population * 10^5) %>%
  ggplot(aes(murder_rate)) + 
  stat_ecdf() +
  ylab("F(a)") + xlab("a")

################################################################
######## Density Graph #########################################
murders %>% ggplot(aes(x=population/10^6)) +
  geom_density(fill = "grey") + 
  scale_x_log10() + 
  xlab("Population in Millions")

###############################################################
################## Mult Graphs using the gridExtra package  ###
library(gridExtra)
p1 <- murders %>% ggplot(aes(x=population/10^6)) + geom_density(fill = "grey", bw = 5) + xlab("População em milhões") + ggtitle("1")
p2 <- murders %>% ggplot(aes(x=population/10^6)) + geom_density(fill = "grey", bw = .05) + scale_x_log10() + xlab("População em milhões") + ggtitle("2")
p3 <- murders %>% ggplot(aes(x=population/10^6)) + geom_density(fill = "grey", bw = 1) + scale_x_log10() + xlab("População em milhões") + ggtitle("3")
grid.arrange(p1,p2,p3,ncol=2)

####################################################################
################ More Visualizations ###############################
# use the new dataset gapminder ####################################
data(gapminder)
# reorder by median income and color by continent
past_year <- 1970

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp / population)

p <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")

p
##Can I use the "object" p to assign chart actions ###############

##############################################
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

#reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))


##########################################################

# note that you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

#############################################################

# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

###############################################################

## Slope graph
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>% 
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>% 
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

#######################################################################
# Using library(titanic)
library(titanic)
# Prepare the data
data <- titanic::titanic_train %>%
  filter(!is.na(Survived), !is.na(Pclass)) %>%
  mutate(Survived = factor(Survived, labels = c("Did Not Survive", "Survive")),
         Pclass = factor(Pclass, labels = c("1ª Class", "2ª Class", "3ª Class")))

# Chart 1: Basic Class Bars Filled by Survival
ggplot(data, aes(x = Pclass, fill = Survived)) +
  geom_bar() +
  labs(title = "Distribution of Passengers by Class (Absolute Count)",
       x = "Passenger Class", y = "Count") +
  theme_minimal()

# Graph 2: Relative bars (proportions by class)
ggplot(data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Survivors by Class (Percentage)",
       x = "Passenger Class", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# Graph 3: Relative survival bars filled by class
ggplot(data, aes(x = Survived, fill = Pclass)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Passengers by Class within each Survival Status",
       x = "Survival Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


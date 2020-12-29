#Instantiate packages
library(dplyr)
library(tidyverse)
library(visdat)
library(GGally)
library(usmap)
library(mice)
library(VIM)
library(ggthemes)
library(ggplot2)

#Import csv
beers = read.csv("Beers.csv", header = TRUE)
breweries = read.csv("Breweries.csv", header = TRUE)

#1 Breweries per states
#Map with brews per state
brews_per_state <-breweries %>% group_by(State) %>% tally()
print(as_tibble(brews_per_state), n = 50)

#2 Merge beers and brews into beerbrew df
beers <- beers %>% rename(Beer_Name = Name) #both dataframes have "Name"
breweries <- breweries %>% rename(Brewery_Name = Name)

beerbrew <- merge(beers,breweries, by.x = "Brewery_id", by.y = "Brew_ID")

#first and last 6 rows
tail(beerbrew,6)
head(beerbrew,6)

#3 missing values in each column
beerbrew_drop <-beerbrew %>%
  na.omit()

md.pattern(beerbrew)

mice_plot <- aggr(beerbrew, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(beerbrew), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#4 Median and ABV Per State and comparison of two metrics
ABV_PerState <- beerbrew %>%
  na.omit() %>%
  group_by(State) %>%
  summarise(Median = median(ABV)) %>%
  arrange(Median)
print(as_tibble(ABV_PerState), n = 50)

IBU_PerState <- beerbrew %>%
  na.omit() %>%
  group_by(State) %>%
  summarise(Median = median(IBU)) %>%
  arrange(Median)
print(as_tibble(IBU_PerState), n = 50)



ggplot(data=ABV_PerState, aes(x=State, y=Median)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(0.8), angle=90)) +
  ggtitle("Median ABV by State") +
  labs(x="State",y="ABV")

ggplot(data=IBU_PerState, aes(x=State, y=Median)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(0.8), angle=90))+
  ggtitle("Median IBU by State") +
  labs(x="State",y="IBU")


#5 Max ABV/IBU
beerbrew[which.max(beerbrew$ABV),]
beerbrew[which.max(beerbrew$IBU),]


#6 Summary ABV and Distribution/Histogram of value
summary(beerbrew$ABV)

#7 Filled Density Plot
p <- ggplot(beerbrew, aes(x=ABV)) + 
  geom_density()
p

ggplot(beerbrew, aes(x=ABV)) + geom_histogram()


#8 Scatter Plot
ggplot(beerbrew, aes(x=IBU, y= ABV)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + # add linear regression line
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(1.0)))+
  ggtitle("Correlation between IBU and ABV ") +
  labs(x="IBU",y="ABV")





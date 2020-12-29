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
library (ggpubr)
library(caret)
library(e1071)
library(class)

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

#Filled Density Plot
p <- ggplot(beerbrew, aes(x=ABV)) + 
  geom_density()
p

ggplot(beerbrew, aes(x=ABV)) + geom_histogram()


#7 Scatter Plot

r_test = cor.test(beerbrew$IBU, beerbrew$ABV)
r_test

beerbrew.mod <- lm(IBU ~ ABV, data = beerbrew) 
summary(beerbrew.mod)

ggscatter(beerbrew, x = "IBU", y = "ABV", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IBU", ylab = "ABV") +
  theme_economist()



# ggplot(beerbrew, aes(x=IBU, y= ABV)) +
#   geom_point(shape=1) +
#   geom_smooth(method=lm) + # add linear regression line
#   theme_economist() + 
#   scale_color_economist()+
#   theme(axis.text.x=element_text(size=rel(1.0)))+
#   ggtitle("Correlation between IBU and ABV ") +
#   labs(x="IBU",y="ABV")



#8 KNN 

trainIndices = sample(seq(1:length(beerbrew$IBU)),round(.7*length(beerbrew$IBU)))
trainBeers = beerbrew[trainIndices,]
testBeers = beerbrew[-trainIndices,]

knn_train <- trainBeers
knn_test <- testBeers



# Filter out any beer style that isnt and IPA or an Ale.
beers_IPAALE <- filter(beerbrew,grepl('IPA|Ale',Style))
beers_IPAALE <- beers_IPAALE[,-8]
# Create a new dataframe that only holds the ABV,IBU, and beer style.
beers_IPAALE_sub <- select(beers_IPAALE,ABV,IBU,Style)
beers_IPAALE_sub$Style <- as.character(beers_IPAALE_sub$Style)

# Normalize the Beer styles to IPA or ALE
# This loop is used to iterate through each row and then to normalize the styles to simply say IPA or Ale.

for (i in 1:1534) {
  if (is.na(str_match(beers_IPAALE_sub[i,3],".Ale"))) {
    beers_IPAALE_sub[i,3] <- "IPA"
  } else {
    beers_IPAALE_sub[i,3] <- "ALE" 
    
  }
}


set.seed(877)
splitpale = .70
beerbrewsample <- sample(1:dim(beers_IPAALE_sub)[1],round(splitpale * dim(beers_IPAALE_sub)[1]))
# ipasamp

# The following code separates the randomly selected values into the 70 to 30 split.
knn_train <- beers_IPAALE_sub[beerbrewsample,]
knn_test <- beers_IPAALE_sub[-beerbrewsample,]

knn_train = na.omit(knn_train)
knn_test = na.omit(knn_test)

#md.pattern(knn_train)



# Using only the IBU and ABV values from both the training and test sets
# I use the beer style as the class against which the knn will search.
knnbeer <- knn(knn_train[,1:2],knn_test[,1:2],cl=knn_train$Style,k=24,prob = TRUE)
# The confusion matrix is used for calibrating the output of a model and examining all possible outcomes of the predictions
ipamatrix <- confusionMatrix(table(knnbeer,knn_test$Style))
ipamatrix





setwd("H:/INFO264/Exercise 1")
library(rpart)
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(clValid)
library(factoextra)
library(dplyr)
library(fpc)
library(NbClust)
library(ggthemes) # Load

workshop <- read.csv("under-five deaths africa.csv")

workshop$Year <- NULL

cor(workshop)

pairs(workshop, main="Relationship between variables")






plot(workshop$Number.of.under.five.deaths..thousands., workshop$HIV.AIDS.Related.Deaths)
fit <- lm(workshop$HIV.AIDS.Related.Deaths~workshop$Number.of.under.five.deaths..thousands.)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Number.of.under.five.deaths..thousands., workshop$HIV.AIDS.Related.Deaths, 
     main="Relationship between Number of Under-Five Deaths and HIV/AIDS Related Deaths", 
     sub="y = 117.51x - 251583", 
     xlab="Number of Under-Five Deaths", 
     ylab="HIV/AIDS Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)





plot(workshop$Number.of.under.five.deaths..thousands., workshop$Prematurity.Related.Deaths)
fit <- lm(workshop$Prematurity.Related.Deaths~workshop$Number.of.under.five.deaths..thousands.)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Number.of.under.five.deaths..thousands., workshop$Prematurity.Related.Deaths, 
     main="Relationship between Number of Under-Five Deaths and Prematurity Related Deaths", 
     sub="y = 47.209x + 206330",
     xlab="Number of Under-Five Deaths", 
     ylab="Prematurity Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)







plot(workshop$Number.of.under.five.deaths..thousands., workshop$Measles.Deaths)
fit <- lm(workshop$Measles.Deaths~workshop$Number.of.under.five.deaths..thousands.)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Number.of.under.five.deaths..thousands., workshop$Measles.Deaths, 
     main="Relationship between Number of Under-Five Deaths and Measles Related Deaths", 
     sub="y = -44.245x + 202770", 
     xlab="Number of Under-Five Deaths", 
     ylab="Measles Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)






plot(workshop$Number.of.under.five.deaths..thousands., workshop$Malaria.Deaths)
fit <- lm(workshop$Malaria.Deaths~workshop$Number.of.under.five.deaths..thousands.)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Number.of.under.five.deaths..thousands., workshop$Malaria.Deaths, 
     main="Relationship between Number of Under-Five Deaths and Malaria Related Deaths", 
     sub="y = 314.25x - 582014", 
     xlab="Number of Under-Five Deaths", 
     ylab="Malaria Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)






plot(workshop$Number.of.under.five.deaths..thousands., workshop$Tetanus.Deaths)
fit <- lm(workshop$Tetanus.Deaths~workshop$Number.of.under.five.deaths..thousands.)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Number.of.under.five.deaths..thousands., workshop$Tetanus.Deaths, 
     main="Relationship between Number of Under-Five Deaths and Tetanus Related Deaths", 
     sub="y = 16.545x - 30275", 
     xlab="Number of Under-Five Deaths", 
     ylab="Tetanus Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)






  
  
  
  
  
setwd("H:/INFO264/Exercise 1")
library(rpart)
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(clValid)
library(factoextra)
library(dplyr)
library(fpc)
library(NbClust)
library(ggthemes) # Load

workshop <- read.csv("under-five deaths africa.csv")

workshop$Year <- NULL

cor(workshop)

pairs(workshop, main="Relationship between variables")






plot(workshop$HIV.AIDS.Related.Deaths, workshop$Number.of.under.five.deaths..thousands.)
fit <- lm(workshop$Number.of.under.five.deaths..thousands.~workshop$HIV.AIDS.Related.Deaths)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$HIV.AIDS.Related.Deaths, workshop$Number.of.under.five.deaths..thousands., 
     main="Relationship between Number of Under-Five Deaths and HIV/AIDS Related Deaths", 
     sub="y = 0.0083x + 2154.4", 
     ylab="Number of Under-Five Deaths", 
     xlab="HIV/AIDS Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)




plot(workshop$Prematurity.Related.Deaths, workshop$Number.of.under.five.deaths..thousands.)
fit <- lm(workshop$Number.of.under.five.deaths..thousands.~workshop$Prematurity.Related.Deaths)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Prematurity.Related.Deaths, workshop$Number.of.under.five.deaths..thousands., 
     main="Relationship between Number of Under-Five Deaths and Prematurity Related Deaths", 
     sub="y = 0.0198x - 3887.3", 
     ylab="Number of Under-Five Deaths", 
     xlab="Prematurity Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)





plot(workshop$Measles.Deaths, workshop$Number.of.under.five.deaths..thousands.)
fit <- lm(workshop$Number.of.under.five.deaths..thousands.~workshop$Measles.Deaths)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Measles.Deaths, workshop$Number.of.under.five.deaths..thousands., 
     main="Relationship between Number of Under-Five Deaths and Measles Related Deaths", 
     sub="y = -0.0018x + 2988.9", 
     ylab="Number of Under-Five Deaths", 
     xlab="Measles Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)





plot(workshop$Malaria.Deaths, workshop$Number.of.under.five.deaths..thousands.)
fit <- lm(workshop$Number.of.under.five.deaths..thousands.~workshop$Malaria.Deaths)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Malaria.Deaths, workshop$Number.of.under.five.deaths..thousands., 
     main="Relationship between Number of Under-Five Deaths and Malaria Related Deaths", 
     sub="y = 0.0032x + 1853.5", 
     ylab="Number of Under-Five Deaths", 
     xlab="Malaria Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)





plot(workshop$Tetanus, workshop$Number.of.under.five.deaths..thousands.)
fit <- lm(workshop$Number.of.under.five.deaths..thousands.~workshop$Tetanus.Deaths)	
abline(fit)
#Shows the least squares line onto the scatterplot (best line of fit).


plot(workshop$Tetanus.Deaths, workshop$Number.of.under.five.deaths..thousands., 
     main="Relationship between Number of Under-Five Deaths and Tetanus Related Deaths", 
     sub="y = 0.0601x + 1835.8", 
     ylab="Number of Under-Five Deaths", 
     xlab="Tetanus Related Deaths")
#Replace X with the name of the variable.
#Add titles and labels.  To change colours of text, add 

abline(fit)


fit

summary(fit)

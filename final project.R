setwd("H:/INFO264/Final Project")

install.packages("extrafont")
install.packages("extrafontdb")
install.packages("ggthemes") # Install 
library(ggthemes) # Load
library(ggplot2)
library(extrafontdb)
font_import()

data <- read.csv("Number of under 5s deaths overall.csv") #Read data from the csv file and assigns to "data"

newdata <- subset(data, Year >= 2012 & Year < 2017)

ggplot(data=data, aes(x=Year, y=Number.of.under.five.deaths..thousands., label=Number.of.under.five.deaths..thousands.)) +
  ggtitle("Number of under-five deaths from 1990-2017 in the World") +
  labs(y = "Deaths(thousands)") +
  geom_bar(stat="identity") +
  geom_text(size = 4, vjust = 5, color = "white") +
  theme_stata() + scale_color_stata()
  
theme_economist() + scale_color_economist()


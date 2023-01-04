## install packages 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

load("E.RData")
head(lead)
names(lead)


#1# Create the bar chart
ggplot(lead, aes(x = Sex)) +
  geom_bar()
#######################
##Generate a bar chart graph with mean MAXWT in  males and females 
View (lead)
lead$MAXFWT <- as.numeric(lead$MAXFWT)
mean_maxfwt <- aggregate(MAXFWT ~ Sex, lead, mean)

ggplot(mean_maxfwt, aes(x = Sex, y = MAXFWT)) +
  geom_bar(stat = "identity")
##########################

##Make a histogram of a continuous variable: “age” as well as “MAXWT”.

# Create a histogram for age
ggplot(lead, aes(x = Age)) +
  geom_histogram()

# Create a histogram for MAXWT
ggplot(lead, aes(x = MAXFWT)) +
  geom_histogram()
##########################
###Make a scatterplot of 2 continuous variables Ld72 and MAXWT, and add the regression lines for each gender

# Convert the values in the Sex column to "Male" and "Female"
lead$Sex <- ifelse(lead$Id %% 2 == 1, "Male", "Female")

# Convert the Sex column to a factor
lead$Sex <- as.factor(lead$Sex)


# Create separate regression models for each gender
male_regression <- lm(Ld72 ~ MAXFWT, data = lead, subset = Sex == "Male")
female_regression <- lm(Ld72 ~ MAXFWT, data = lead, subset = Sex == "Female")



# Create the scatterplot
plot <- ggplot(lead, aes(x = MAXFWT, y = Ld72, color = Sex)) +
  geom_point()

# Add the regression lines
plot + abline(male_regression, col = "red") +
  abline(female_regression, col = "blue")
head(lead)


str(lead)



######################
####Make a boxplot of age  and a separate boxplots per Ld72 and per Ld73 (as.factors). 

# Convert Ld72 and Ld73 to factors
load("E.RData")
head(lead)
names(lead)
lead$Age <- as.numeric(lead$Age)
# Convert Ld72 and Ld73 to factors
lead$Ld72 <- as.factor(lead$Ld72)
lead$Ld73 <- as.factor(lead$Ld73)

boxplot(lead$Age, main = "Age")
# Create a boxplot for Ld72
ggplot(lead, aes(x = "Ld72", y = Ld72)) +
  geom_boxplot() +
  facet_wrap(~ Ld72)

# Create a boxplot for Ld73
ggplot(lead, aes(x = "Ld73", y = Ld73)) +
  geom_boxplot() +
  facet_wrap(~ Ld73)


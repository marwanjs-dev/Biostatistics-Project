
#### installed packages 

install.packages("lmtest")
install.packages("nortest")

###libraries
library(nortest)
library(ggplot2)
library(lmtest)
library(stats)
library(graphics)


#####data
load("E.RData")
head(lead)
names(lead)



##Check the homoscedasticity using two methods. 
############################
####Residual plot
############################

# Set the number of rows and columns in the plot grid
par(mfrow=c(2,2))

# Convert the data frame to a matrix
mat <- data.matrix(lead)

# Loop through the variables in the matrix
for(i in 1:ncol(mat)){
  y <- mat[,i]
  x <- mat[,setdiff(1:ncol(mat), i)]
  
  # Fit a linear regression model
  model <- lm(y ~ x)
  
  # Extract the residuals
  residuals <- model$residuals
  
  # Create the residual plot
  plot(model)
  
  # Add a title to the plot
  title(paste("Residual Plot for Variable", colnames(lead)[i]))
}
###################################
######Bartlett's test: 
##################################
# Set the alpha level
alpha <- 0.05

# Convert the data frame to a matrix
mat <- data.matrix(lead)

# Loop through the variables in the matrix
for(i in 1:ncol(mat)){
  y <- mat[,i]
  x <- mat[,setdiff(1:ncol(mat), i)]
  
  # Fit a linear regression model
  model <- lm(y ~ x)
  
  # Conduct Bartlett's test
  bartlett.test(model)
  
  # Check the p-value
  if(bartlett.test(model)$p.value < alpha){
    print(paste("Variable", colnames(lead)[i], "has non-homoscedastic residuals"))
  } else {
    print(paste("Variable", colnames(lead)[i], "has homoscedastic residuals"))
  }
}




################################################
####normality test 
########################################################

# Set the alpha level
alpha <- 0.05

# Loop through the variables in the data frame
for(i in 1:ncol(lead)){
  x <- lead[,i]
  
  # Conduct the Shapiro-Wilk test
  shapiro.test(x)
  
  # Check the p-value
  if(shapiro.test(x)$p.value < alpha){
    print(paste("Variable", colnames(lead)[i], "is not normal"))
  } else {
    print(paste("Variable", colnames(lead)[i], "is normal"))
  }
}
####################

# Loop through the variables in the data frame
for(i in 1:ncol(lead)){
  x <- lead[,i]
  
  # Create the Q-Q plot
  qqnorm(x)
  qqline(x)
  
  # Add a title to the plot
  title(paste("Q-Q Plot for Variable", colnames(lead)[i]))
}

###################


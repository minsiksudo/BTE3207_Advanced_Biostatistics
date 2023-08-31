
alpha <- 1# this is data element

alpha

beta <- c(1, 2) # this is vector

beta

gamma <- c(alpha, beta) # it is still a vector

gamma

delta <- c(beta, alpha) # it is still a vector

delta

epsilon <- data.frame(gamma, delta) # this is data frame

epsilon

#data frame : each column has one variable (vectors)

# we use dollar sign to select one variable from dataframe

ep_inha <- epsilon


epsilon

epsilon$gamma

epsilon$delta

#data frame example 2

subject <- c("Joe", "Trump", "Obama", "George")

height <- c(183, 190, 187, 182)
IsTall <- c("short", "tall", "tall", "short")

example_dataframe3 <- data.frame(subject, height, IsTall)


read.csv("/Users/minsikkim/Dropbox (Personal)/Inha/Lectures/Advanced biostatistics/datasets/sbp_dataset/sbp_dataset_korea_2013-2014.csv")

dataset_sbp <- read.csv("/Users/minsikkim/Dropbox (Personal)/Inha/Lectures/Advanced biostatistics/datasets/sbp_dataset/sbp_dataset_korea_2013-2014.csv")

# we use dollar sign to select one variable from dataframe
dataset_sbp$SBP

#Let's use computer to calculate large dataset!


#function for calculating mean: mean()
mean(dataset_sbp$SBP)

#function for calculating standard deviation: sd()
sd(dataset_sbp$SBP)

#function for calculating median: median()
median(dataset_sbp$SBP)

#function for calculating median: median()
quantile(dataset_sbp$SBP)

quantile(x = dataset_sbp$SBP, 0.5)

#use question mark to see what function is doing

?quantile


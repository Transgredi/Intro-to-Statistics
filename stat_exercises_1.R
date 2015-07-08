#David M. Lane. “Introduction to Statistics: An Interactive e-Book.”
#David Lane, 2013. iBooks. https://itun.es/pl/CJqXO.l

# 1. Make up a dataset of 12 numbers with a positive skew.
# Use a statistical program to compute the skew.
# Is the mean larger than the median as it usually is 
# for distributions with a positive skew? What is the value for skew?”

ds <- c(1, 1, 3, 3, 3, 4, 8, 9, 23, 42, 21, 23)
mean(ds)
median(ds)

skew_1 <- function(dataset){
  skew <- (3 * (mean(dataset) - median(dataset))) / sd(dataset);
  return(skew);
}

skew_2 <- function(dataset){
  skew <- sum(((dataset - mean(dataset))^3)/(sd(dataset))^3);
  return(skew);
}


kurt <- function(dataset){
  kurtosis <- sum((((dataset - mean(dataset))^4)/(sd(dataset))^4)-3);
  return(kurtosis);
}

skew_1(ds)
skew_2(ds)
kurt(ds)
library(e1071) 
kurtosis(ds)
skewness(ds)

# 2. Repeat Problem 1 only this time make the dataset have a negative skew.
ds2 <- c(1, 1, 3, 3, 34, 22, 35, 32, 23, 42, 21, 23)
mean(ds2)
median(ds2)
skew_1(ds2)
skew_2(ds2)
kurt(ds2)
kurtosis(ds2)
skewness(ds2)


# 3. Make up three data sets with 5 numbers each that have: 
# (a) the same mean but different standard deviations. 
# (b) the same mean but different medians. 
# (c) the same median but different means.



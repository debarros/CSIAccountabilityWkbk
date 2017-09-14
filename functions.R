#functions.R

library("abind")
library("ggplot2")
library("scales")
library("reshape2")
library("openxlsx")
library(dBtools)
library(googlesheets)
library(data.table)

# #This function takes the max value if there are any non NA values, and returns NA if there are only NA values
# betterMax = function(x){
#   y = x[!is.na(x)]
#   if(length(y)>0){
#     return(max(y))}
#   return(NA)
# }
# 
# #This function returns TRUE if x and y equal or if both are NA, and FALSE if they are not equal or if exaclty one NA
# #Instead of two arguments, this function will also accept a single vector of length 2 and compare the the two elements
# betterComp = function(x,y = NULL){
#   if(is.null(y)){
#     y = x[2]
#     x = x[1]
#   }
#   q = is.na(x) + is.na(y) 
#   if(q < 1){return(x == y)
#   } else if(q == 1){return(FALSE)
#   }else return(TRUE)
# }
# 
# 
# #This function checks to see if two matrices are exactly equal, where NA entries are considered equal to each other
# MbetterComp = function(x,y){
#   apply(X = abind(x,y,along = 3), MARGIN = c(1,2), FUN = betterComp)
# }
# 
# 
# #This function takes two vectors of equal length and finds the betterMax of each position
# VbetterMax = function(x,y){
#   apply(X = cbind(x,y), MARGIN = 1, FUN = betterMax)
# }
# 
# 
# #This function checks to see if two vectors are exactly equal, where NA entries are considered equal to each other
# VbetterComp = function(x,y){
#   apply(X = cbind(x,y), MARGIN = 1, FUN = betterComp)
# }
# 
# 
# #This functions collapses the planes of a 3-dimensional array, leaving a two dimensional matrix
# #Each element of the 2-d matrix is the betterMax of all the values in that cell location across all planes in the array
# #In otherwords, it collapses xyz space into the xy plane by taking the maximum value across the z dimension for each xy coordinate, ignoring NA's
# MbetterMax = function(x){
#   apply(X = x, MARGIN = c(1,2), FUN = betterMax)
# }

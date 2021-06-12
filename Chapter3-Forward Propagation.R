#================================================
### A Simple Neural Network Making a Prediction
# What is a Neural Network?
print(" Chapter 3: A Simple Neural Network Making a Prediction ")

# The network:
weight = 0.1
neural_network = function(input,weight) {
	prediction <- input*weight
	return(prediction)
	}

# How we use the network to predict something:

number_of_toes  <- c(8.5, 9.5, 10,9)
input = number_of_toes[1]
pred=neural_network(input,weight)
print("What is a Neural Network?")
print(pred)

# result: [1] 0.85

#================================================
### Making a Prediction with Multiple Inputs
# Complete Runnable Code

#R> install package(asserthat)

library(assertthat)
w_sum = function(a,b){
    assertthat::assert_that(length(a) == length(b))
    output <- 0
    for (i in seq(1,length(a))) {
        output <- output+(a[i] * b[i])
		}
    return(output)
	}

weights <- c(0.1, 0.2, 0)
    
neural_network = function(input, weights){
	    pred<- w_sum(input,weights)
	    return(pred)
		}

# This dataset is the current
# status at the beginning of
# each game for the first 4 games
# in a season.

# toes = current number of toes
# wlrec = current games won (percent)
# nfans = fan count (in millions)
toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65, 0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

# Input corresponds to every entry
# for the first game of the season.

input <- c(toes[1],wlrec[1],nfans[1])
pred  <- neural_network(input,weights)
print("Making a Prediction with Multiple Inputs")
print(pred)
#- result: [1] 0.98

#=======================================================
### NumPy code
# PYTHON: import numpy as np
#
weights = c(0.1, 0.2, 0)
neural_network= function(input, weights){
    pred = input %*% weights
    return(pred)
	}
    
toes =  c(8.5, 9.5, 9.9, 9.0)
wlrec = c(0.65, 0.8, 0.8, 0.9)
nfans = c(1.2, 1.3, 0.5, 1.0)

# Input corresponds to every entry
# for the first game of the season.

input = c(toes[1],wlrec[1],nfans[1])
pred = neural_network(input,weights)

print("NumPy code")
print(pred)
#     [,1]
#[1,] 0.98

#================================================
### Making a Prediction with Multiple Outputs
#

# Instead of predicting just 
# whether the team won or lost, 
# now we're also predicting whether
# they are happy/sad AND the percentage
# of the team that is hurt. We are
# making this prediction using only
# the current win/loss record.

ele_mul= function(number,vector){
    output <- c(0,0,0)
    assertthat::assert_that(length(output) == length(vector))
    for(i in seq(1,length(vector))){
        output[i] = number * vector[i]
	}
    return(output)
}

weights <- c(0.3, 0.2, 0.9)

neural_network= function(input, weights){
    pred = ele_mul(input,weights)
    return(pred)
   }
wlrec <- c(0.65, 0.8, 0.8, 0.9)
input = wlrec[1]
pred = neural_network(input,weights)
print("Making a Prediction with Multiple Outputs")
print(pred)
# [1] 0.195 0.130 0.585

#================================================
### Predicting with Multiple Inputs & Outputs
#

#toes %win #fans
weights <- matrix(c(0.1, 0.1, -0.3, # hurt?
            0.1, 0.2, 0.0,  # win?
            0.0, 1.3, 0.1)  # sad?
			,byrow=TRUE,nrow=3)

w_sum= function(a,b){
    assertthat::assert_that(length(a) == length(b))
    output = 0
    for(i in seq(1,length(a))){
        output = output+(a[i] * b[i])
	}
    return(output)
}

vect_mat_mul= function(vect,matrix){
    assertthat::assert_that(length(vect) == ncol(matrix))
    output <- c(0,0,0)
    for (i in seq(1,length(vect))){
        output[i] = w_sum(vect,matrix[i,1:ncol(matrix)])
	}
    return(output)
}

neural_network= function(input, weights){
    pred = vect_mat_mul(input,weights)
    return(pred)
}
# This dataset is the current
# status at the beginning of
# each game for the first 4 games
# in a season.

# toes = current number of toes
# wlrec = current games won (percent)
# nfans = fan count (in millions)

toes <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65,0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

# Input corresponds to every entry
# for the first game of the season.

input <- c(toes[1],wlrec[1],nfans[1])
pred = neural_network(input,weights)
print("Predicting with Multiple Inputs & Outputs ")
print(pred)
# [1] 0.555 0.980 0.965
# R> Of course one could easily get the same result via " input %*% t(weights) "

#===================================================
### Predicting on Predictions
#
  #toes %win #fans
ih_wgt <- matrix(c(0.1, 0.2, -0.1, #hid[0]
           -0.1,0.1, 0.9, #hid[1]
            0.1, 0.4, 0.1) #hid[2]
		   	,byrow=TRUE,nrow=3)

           #hid[0] hid[1] hid[2]
hp_wgt <- matrix(c(0.3, 1.1, -0.3, #hurt?
            0.1, 0.2, 0.0, #win?
            0.0, 1.3, 0.1) #sad?
		   	,byrow=TRUE,nrow=3)

#R> no need to define: weights <- list(ih_wgt, hp_wgt)

neural_network= function(input, weights1,weights2){
    hid = vect_mat_mul(input,weights1)
    pred = vect_mat_mul(hid,weights2)
    return(pred)
}

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65,0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

# Input corresponds to every entry
# for the first game of the season.

input <- c(toes[1],wlrec[1],nfans[1])
pred = neural_network(input,ih_wgt,hp_wgt)
print("Predicting on Predictions")
print(pred)
# [1] 0.2135 0.1450 0.5065

#================================
### R version
#
  #toes %win #fans
ih_wgt <- c(0.1, 0.2, -0.1, #hid[0]
           -0.1,0.1, 0.9, #hid[1]
            0.1, 0.4, 0.1) #hid[2]
		   	
           #hid[0] hid[1] hid[2]
hp_wgt <- c(0.3, 1.1, -0.3, #hurt?
            0.1, 0.2, 0.0, #win?
            0.0, 1.3, 0.1) #sad?
		  
weights <- c(ih_wgt,hp_wgt) #R:: all weights are gathered in one vector

neural_network= function(input, weights){
	n  = length(input)
	n2 = n^2
	w1 <- matrix(weights[1:n2],byrow=TRUE,nrow=n)
	w2 <- matrix(weights[(n2+1):(2*n2)],byrow=TRUE,nrow=n)
    hid = vect_mat_mul(input,w1)
    pred = vect_mat_mul(hid,w2)
    return(pred)
}

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65,0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

# Input corresponds to every entry
# for the first game of the season.

input <- c(toes[1],wlrec[1],nfans[1])
pred = neural_network(input,weights)
print("R-version")
print(pred)

print(" END of Chapter 3")

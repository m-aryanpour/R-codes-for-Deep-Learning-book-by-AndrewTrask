# Chaper 6
#===================================================
### R> utility functions
sprintVec= function(A,Aname){
	return(sprintf("%s = [%s]",Aname,paste(A,collapse=", ")))
}
printVec= function(A,Aname){
	print(sprintVec(A,Aname), quote=FALSE)
}
Print= function(what){
	print(what, quote=FALSE)
}
printsf= function(what){
	print(sprintf(what), quote=FALSE)
}
#==================================================
Print("BEGIN Chapter 6 ")
#==================================================

Print("Creating a Matrix or Two in R")

weights <- matrix(c(0.5,0.48,-0.7),byrow=FALSE) # make a vertical vector
alpha   = 0.1

streetlights <- matrix(c( 1, 0, 1 ,
                         0, 1, 1 ,
                         0, 0, 1 ,
                         1, 1, 1 ,
                         0, 1, 1 ,
                         1, 0, 1 ), byrow=TRUE,nrow=6)

walk_vs_stop <- c(0, 1, 0, 1, 1, 0)

input = streetlights[1,1:3] # [1,0,1]
goal_prediction = walk_vs_stop[1] # equals 0... i.e. "stop"

for (iteration in seq(1,20)){
    prediction <- input %*% weights
    error <- (goal_prediction - prediction) ** 2
    delta = c(prediction - goal_prediction)
    weights <- weights - (alpha * (input * delta))	

    print(sprintf(" Error: %f , Prediction: %f", error,prediction),quote=FALSE)
}
#========================================
###
Print("Building Our Neural Network")
a <- c(0,1,2,1)
b <- c(2,2,2,3)

print(a*b) #elementwise multiplication
print(a+b) #elementwise addition
print(a * 0.5) # vector-scalar multiplication
print(a + 0.5) # vector-scalar addition
#========================================
###
Print("Learning the whole dataset!")
weights <- matrix(c(0.5,0.48,-0.7),byrow=FALSE) # make a vertical vector
alpha   = 0.1

streetlights <- matrix(c( 1, 0, 1 ,
                         0, 1, 1 ,
                         0, 0, 1 ,
                         1, 1, 1 ,
                         0, 1, 1 ,
                         1, 0, 1 ), byrow=TRUE,nrow=6)

walk_vs_stop <- c(0, 1, 0, 1, 1, 0)

input = streetlights[1,1:3] # [1,0,1]
goal_prediction = walk_vs_stop[1] # equals 0... i.e. "stop"

nCols = ncol(streetlights)
for (iteration in seq(1,40)){
    error_for_all_lights = 0
    for (row_index in seq(1,length(walk_vs_stop))){
        input = streetlights[row_index,1:nCols]
        goal_prediction = walk_vs_stop[row_index]
        
        prediction = input %*% weights        
        error = (goal_prediction - prediction) ** 2
        error_for_all_lights = error_for_all_lights + error
        
        delta <- c(prediction - goal_prediction)
        weights = weights - (alpha * (input * delta))	
        printVec(prediction,"  Prediction:")
	}
    print(sprintf(" Error: %f",error_for_all_lights),quote=FALSE)
}

#====================================================
###
Print("Our First 'Deep' Neural Network")

set.seed(1)
relu= function(x){
    return((x > 0) * x )
}
alpha = 0.2
hidden_size = 4
streetlights <- matrix(c(1, 0, 1 ,
                         0, 1, 1 ,
                         0, 0, 1 ,
                         1, 1, 1 ),
						 byrow=TRUE, nrow=4)

walk_vs_stop <- c( 1, 1, 0, 0) # .T

weights_0_1 = matrix(runif(3*hidden_size,max=1,min=-1),ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size,max=1,min=-1),nrow=hidden_size)

layer_0 <- streetlights[1,1:ncol(streetlights)]
layer_1 <- relu(layer_0 %*% weights_0_1)
layer_2 <- layer_1 %*% weights_1_2

printVec(layer_0," layer_0")
printVec(layer_1," layer_1")
printVec(layer_2," layer_2")

#====================================================
###
Print("Backpropagation in Code")

set.seed(1)

relu= function(x){
    return((x > 0) * x) # returns x if x > 0
                       # return 0 otherwise
}

relu2deriv= function(output){
    return(output>0) # returns 1 for input > 0
                    # return 0 otherwise
}
alpha = 0.2
hidden_size = 4

weights_0_1 = matrix(runif(3*hidden_size,max=1,min=-1),ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size,max=1,min=-1),nrow=hidden_size)

for (iteration in seq(1,70)){
   layer_2_error = 0
   for (i in seq(1,nrow(streetlights))){
      layer_0 = streetlights[i,1:ncol(streetlights)]
      layer_1 = relu(layer_0 %*% weights_0_1)
      layer_2 = layer_1 %*% weights_1_2

      layer_2_error = layer_2_error + sum((layer_2 - walk_vs_stop[i]) ** 2)

      layer_2_delta = (walk_vs_stop[i] - layer_2)
      layer_1_delta = layer_2_delta %*% t(weights_1_2) * relu2deriv(layer_1)
	
      weights_1_2 = weights_1_2 + alpha * t(layer_1) %*% layer_2_delta
      weights_0_1 = weights_0_1 + (alpha * layer_0 %*% layer_1_delta)
	}

   if(iteration %% 10 == 9){
      printVec(layer_2_error, " Error: ")
	  }
}

#==================================================================
###
Print("One Iteration of Backpropagation")
set.seed(1)

relu= function(x){
  return((x > 0) * x) 
}

relu2deriv= function(output){
  return(output>0) 
}

lights = matrix(c(1, 0, 1,
                  0, 1, 1,
                  0, 0, 1,
                  1, 1, 1 ), byrow=TRUE, 4)

walk_stop <- c(1, 1, 0, 0)

alpha = 0.2
hidden_size = 3

weights_0_1 = matrix(runif(3*hidden_size,max=1,min=-1),ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size,max=1,min=-1),nrow=hidden_size)

layer_0 <- lights[1,1:ncol(lights)]
layer_1 <- layer_0 %*% weights_0_1
layer_1 = relu(layer_1)
layer_2 = layer_1 %*% weights_1_2

error = (layer_2-walk_stop[1])**2

layer_2_delta=(layer_2-walk_stop[1])

layer_1_delta= layer_2_delta %*% t(weights_1_2)
layer_1_delta = layer_1_delta*relu2deriv(layer_1)

weight_delta_1_2 = t(layer_1) %*% layer_2_delta
weight_delta_0_1 = (layer_0) %*% layer_1_delta

weights_1_2 = weights_1_2 - alpha * weight_delta_1_2
weights_0_1 = weights_0_1 - alpha * weight_delta_0_1


#==================================================================
###
Print("Putting it all Together")

set.seed(1)

relu= function(x){
  return((x > 0) * x) # returns x if x > 0
  # return 0 otherwise
}

relu2deriv= function(output){
  return(output>0) # returns 1 for input > 0
  # return 0 otherwise
}

streetlights = matrix(c(1, 0, 1,
                        0, 1, 1,
                        0, 0, 1,
                        1, 1, 1 ), byrow=TRUE, 4)

walk_stop <- c(1, 1, 0, 0)
alpha = 0.2
hidden_size = 4

weights_0_1 = matrix(runif(3*hidden_size,max=1,min=-1),ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size,max=1,min=-1),nrow=hidden_size)


for (iteration in seq(1,70)){
  layer_2_error = 0
  for (i in seq(1,nrow(streetlights))){
    layer_0 = streetlights[i,1:ncol(streetlights)]
    layer_1 = relu(layer_0 %*% weights_0_1)
    layer_2 = layer_1 %*% weights_1_2
    
    layer_2_error = layer_2_error + sum((layer_2 - walk_vs_stop[i]) ** 2)
    
    layer_2_delta = (layer_2 - walk_vs_stop[i])
    layer_1_delta = layer_2_delta %*% t(weights_1_2) * relu2deriv(layer_1)
    
    weights_1_2 = weights_1_2 - alpha * t(layer_1) %*% layer_2_delta
    weights_0_1 = weights_0_1 - (alpha * layer_0 %*% layer_1_delta)
  }
  
  if(iteration %% 10 == 9){
    printVec(layer_2_error, " Error: ")
  }
}


#========================================================
Print("END of Chapter 6")










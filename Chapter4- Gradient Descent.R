print(" BEGIN Chapter 4")
#
### Compare: Does our network make good predictions?
#
knob_weight = 0.5
input       = 0.5
goal_pred   = 0.8

pred = input * knob_weight
error = (pred - goal_pred) ** 2
print("Compare: Does our network make good predictions?")
print(error)
# [1] 0.3025

#======================================================
### What's the Simplest Form of Neural Learning?
# Learning using the Hot and Cold Method
#
# 1) An Empty Network

weight = 0.1 
lr = 0.01

neural_network= function(input, weight){
    prediction = input * weight
    return(prediction)
	}

# 2) PREDICT: Making A Prediction And Evaluating Error

number_of_toes <- c(8.5)
win_or_lose_binary <- c(1) #(won!!!)

input = number_of_toes[1]
true = win_or_lose_binary[1]

pred = neural_network(input,weight)
error = (pred - true) ** 2
print("Learning using the Hot and Cold Method")
print(error)

# 3) COMPARE: Making A Prediction With a *Higher* Weight And Evaluating Error

weight = 0.1 

neural_network= function(input, weight){
    prediction = input * weight
    return(prediction)
	}

number_of_toes <- c(8.5)
win_or_lose_binary <- c(1) #(won!!!)

input = number_of_toes[1]
true = win_or_lose_binary[1]

lr = 0.01
p_up = neural_network(input,weight+lr)
e_up = (p_up - true) ** 2
print("Making A Prediction With a *Higher* Weight And Evaluating Error")
print(e_up)

## 4) COMPARE: Making A Prediction With a *Lower* Weight And Evaluating Error

weight = 0.1 

neural_network= function(input, weight){
    prediction = input * weight
    return(prediction)
}

number_of_toes<- c(8.5)
win_or_lose_binary <- c(1) #(won!!!)

input = number_of_toes[1]
true = win_or_lose_binary[1]

lr = 0.01
p_dn = neural_network(input,weight-lr)
e_dn = (p_dn - true) ** 2
print("Making A Prediction With a *Lower* Weight And Evaluating Error")
print(e_dn)

#===================================================
### Hot and Cold Learning
#
weight = 0.5
input = 0.5
goal_prediction = 0.8

step_amount = 0.001
print(" START: Hot and Cold Learning")
for (iteration in seq(1,1101	)){

    prediction = input * weight
    error = (prediction - goal_prediction) ** 2

    print(sprintf("Error: %f  , Prediction: %f", error,prediction))
    
    up_prediction = input * (weight + step_amount)
    up_error = (goal_prediction - up_prediction) ** 2

    down_prediction = input * (weight - step_amount)
    down_error = (goal_prediction - down_prediction) ** 2

    if(down_error < up_error){
        weight = weight - step_amount
        }
    if(down_error > up_error){
        weight = weight + step_amount
	}
}
print(" END: Hot and Cold Learning")

#
### Calculating Both Direction and Amount from Error
#

print("Calculating Both Direction and Amount from Error")
weight = 0.5
goal_pred = 0.8
input = 0.5

for( iteration in seq(1,20)){
    pred = input * weight
    error = (pred - goal_pred) ** 2
    direction_and_amount = (pred - goal_pred) * input
    weight = weight - direction_and_amount
    print(sprintf("Error: %f  , Prediction: %f ", error,pred))
}

#============================================
### One Iteration of Gradient Descent
#
# 1) An Empty Network

weight = 0.1 
alpha = 0.01

neural_network= function(input, weight){
    prediction = input * weight
    return(prediction)
}

# 2) PREDICT: Making A Prediction And Evaluating Error

number_of_toes <- c(8.5)
win_or_lose_binary <- c(1) # (won!!!)

input = number_of_toes[1]
goal_pred = win_or_lose_binary[1]

pred = neural_network(input,weight)
error = (pred - goal_pred) ** 2

# 3) COMPARE: Calculating "Node Delta" and Putting it on the Output Node

delta = pred - goal_pred

# 4) LEARN: Calculating "Weight Delta" and Putting it on the Weight

weight_delta = input * delta

# 5) LEARN: Updating the Weight

alpha = 0.01 # fixed before training
weight = weight - weight_delta * alpha

weight = 0.0
goal_pred = 0.8
input = 0.5

#
### Learning is just Reducing Error
#
print("Learning is just Reducing Error")
for (iteration in seq(1,4)){    
    pred = input * weight
    error = (pred - goal_pred) ** 2
    delta = pred - goal_pred
    weight_delta = delta * input
    weight = weight - weight_delta
	print(sprintf("Error: %f  , Prediction: %f ", error,pred))
}

#
### Let's Watch Several Steps of Learning
#
print("Let's Watch Several Steps of Learning")

weight = 0.0
goal_pred = 0.8
input  =  1.1

for (iteration in seq(1,4)){
    print(sprintf("-----Weight: %f", weight))
    pred = input * weight
    error = (pred - goal_pred) ** 2
    delta = pred - goal_pred
    weight_delta = delta * input
    weight = weight - weight_delta
    print(sprintf("Error: %f  , Prediction: %f ", error,pred))
    print(sprintf("Delta: %f, Weight Delta: %f", delta,weight_delta))
}

	
#
### Why does this work? What really is weight_delta
#
print("Why does this work? What really is weight_delta")

weight = 0.5
goal_pred = 0.8
input = 0.5

for (iteration in seq(1,20)){
    pred = input * weight
    error = (pred - goal_pred) ** 2
    direction_and_amount = (pred - goal_pred) * input
    weight = weight - direction_and_amount

    print(sprintf("Error: %f , Prediction: %f", error,pred))
}

#
### How to use a Derivative to Learn
#
print("How to use a Derivative to Learn")

weight = 0.0
goal_pred = 0.8
input = 1.1

for (iteration in seq(1,4)){
    pred = input * weight
    error = (pred - goal_pred) ** 2
    delta = pred - goal_pred
    weight_delta = delta * input
    weight = weight - weight_delta
    print(sprintf("Error: %f , Prediction: %f", error,pred))
}

#
### Breaking Gradient Descent
#
print("Breaking Gradient Descent")
weight = 0.5
goal_pred = 0.8
input = 0.5

for (iteration in seq(1,20)){
    pred = input * weight
    error = (pred - goal_pred) ** 2
    delta = pred - goal_pred
    weight_delta = input * delta
    weight = weight - weight_delta
    print(sprintf("Error: %f , Prediction: %f", error,pred))
}
# 
print(" Now let's break it: ")

weight = 0.5
goal_pred = 0.8
input = 2

for (iteration in seq(1,20)){
    pred = input * weight
    error = (pred - goal_pred) ** 2
    delta = pred - goal_pred
    weight_delta = input * delta
    weight = weight - weight_delta
    print(sprintf("Error: %f , Prediction: %f", error,pred))
}

#
print(" introducing alpha")
weight = 0.5
goal_pred = 0.8
input = 2
alpha = 0.1

for (iteration in seq(1,20)){
    pred  = input * weight
    error = (pred - goal_pred) ** 2
    derivative = input * (pred - goal_pred)
    weight = weight - (alpha * derivative)
    print(sprintf("Error: %f , Prediction: %f", error,pred))
}    
print(" END of Chapter 4")  





































	
	
	
	
	
	
	
	










































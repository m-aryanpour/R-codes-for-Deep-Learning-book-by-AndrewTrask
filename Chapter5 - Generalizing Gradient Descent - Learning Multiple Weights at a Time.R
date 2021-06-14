# Chapter 5 - Generalizing Gradient Descent - Learning Multiple Weights at a Time


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
Print("BEGIN Chapter 5 ")
#==================================================
### Gradient Descent Learning with Multiple Inputs
#
Print("Gradient Descent Learning with Multiple Inputs")

w_sum= function(a,b){
    assertthat::assert_that(length(a) == length(b))
    output = 0
    for (i in seq(1,length(a))){
        output = output+ (a[i] * b[i])
	}
    return(output)
}
weights <- c(0.1, 0.2, -.1)

neural_network= function(input,weights){
    pred = w_sum(input,weights)
    return(pred)
}

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65, 0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

win_or_lose_binary <- c(1, 1, 0, 1)

true = win_or_lose_binary[1]

# Input corresponds to every entry
# for the first game of the season.

input <- c(toes[1],wlrec[1],nfans[1])

pred = neural_network(input,weights)
error = (pred - true) ** 2
delta = pred - true

ele_mul= function(number,vector){
    output <- c(0,0,0)
    assertthat::assert_that(length(output) == length(vector))
    for (i in seq(1,length(vector))){
        output[i] = number * vector[i]
	}
    return(output)
}

weight_deltas = ele_mul(delta,input)
#R> Instead of using function ele_mul() above, one can easily use: weight_deltas= delta * input

alpha = 0.01

for (i in seq(1,length(weights))){
    weights[i] = weights[i]- alpha * weight_deltas[i]
}
printVec(weights," Weights")
printVec(weight_deltas," Weight Deltas")
#=======================================================
### 
#
Print("Let's Watch Several Steps of Learning")
neural_network= function(input, weights){
  out = 0
  for (i in seq(1,length(input))){
    out = out+ (input[i] * weights[i])
	}
  return(out)
}

ele_mul= function(scalar, vector){
  out <- c(0,0,0)
  for (i in seq(1,length(out))){
    out[i] = vector[i] * scalar
	}
  return(out)
 }

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65, 0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

win_or_lose_binary <- c(1, 1, 0, 1)
true = win_or_lose_binary[1]

alpha = 0.01
weights <- c(0.1, 0.2, -.1)
input <- c(toes[1],wlrec[1],nfans[1])

for (iter in seq(1,3)){
  pred = neural_network(input,weights)
  error = (pred - true) ** 2
  delta = pred - true
  weight_deltas = ele_mul(delta,input)

  print(sprintf(" Iteration: %d",iter), quote=FALSE)
  print(sprintf("  Pred: %f",pred), quote=FALSE)
  print(sprintf("  Error: %f",error), quote=FALSE)
  print(sprintf("  Delta: %f",delta), quote=FALSE)
  printVec(weights, "  Weights")
  printVec(weight_deltas,"  Weight_Deltas ")


	for (i in seq(1,length(weights))){
		weights[i] = weights[i] - alpha*weight_deltas[i]
	}
 }
#
#==================================
#
Print("Freezing One Weight - What Does It Do?")

Print("Let's Watch Several Steps of Learning")
neural_network= function(input, weights){
  out = 0
  for (i in seq(1,length(input))){
    out = out+ (input[i] * weights[i])
	}
  return(out)
}

ele_mul= function(scalar, vector){
  out <- c(0,0,0)
  for (i in seq(1,length(out))){
    out[i] = vector[i] * scalar
	}
  return(out)
 }

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65, 0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.0)

win_or_lose_binary <- c(1, 1, 0, 1)
true = win_or_lose_binary[1]

alpha = 0.3
weights <- c(0.1, 0.2, -.1)
input <- c(toes[1],wlrec[1],nfans[1])

for (iter in seq(1,3)){
  pred = neural_network(input,weights)
  error = (pred - true) ** 2
  delta = pred - true
  weight_deltas = ele_mul(delta,input)
  weight_deltas[1] = 0.

  print(sprintf(" Iteration: %d",iter), quote=FALSE)
  print(sprintf("  Pred: %f",pred), quote=FALSE)
  print(sprintf("  Error: %f",error), quote=FALSE)
  print(sprintf("  Delta: %f",delta), quote=FALSE)
  printVec(weights, "  Weights")
  printVec(weight_deltas,"  Weight_Deltas ")


	for (i in seq(1,length(weights))){
		weights[i] = weights[i] - alpha*weight_deltas[i]
	}
 }

#
#==================================
#
Print("Gradient Descent Learning with Multiple Outputs")
# Instead of predicting just 
# whether the team won or lost, 
# now we're also predicting whether
# they are happy/sad AND the
# percentage of the team that is
# hurt. We are making this
# prediction using only
# the current win/loss record.

weights <- c(0.3, 0.2, 0.9)

neural_network= function(input, weights){
    pred = ele_mul(input,weights)
    return(pred)
}

wlrec <- c(0.65, 1.0, 1.0, 0.9)
hurt  <- c(0.1, 0.0, 0.0, 0.1)
win   <- c(  1,   1,   0,   1)
sad   <- c(0.1, 0.0, 0.1, 0.2)

input = wlrec[1]
true <- c(hurt[1], win[1], sad[1])

pred = neural_network(input,weights)

error <- c(0, 0, 0)
delta <- c(0, 0, 0)

for (i in seq(1,length(true))){
    error[i] = (pred[i] - true[i]) ** 2
    delta[i] = pred[i] - true[i]
}
    
scalar_ele_mul= function(number,vector){
    output <- c(0,0,0)
    assertthat::assert_that(length(output) == length(vector))
    for (i in seq(1,length(vector))){
        output[i] = number * vector[i]
	}
    return(output)
}

weight_deltas = scalar_ele_mul(input,delta)

alpha = 0.1
for (i in seq(1,length(weights))){
    weights[i] = weights[i]- (weight_deltas[i] * alpha)
}
    
printVec(weights, " Weights")
printVec(weight_deltas," Weight Deltas")

#
#==================================
#
Print("Gradient Descent with Multiple Inputs & Outputs")
 #toes %win #fans
weights <- matrix( c(0.1, 0.1, -0.3,#hurt?
            0.1, 0.2, 0.0, #win?
            0.0, 1.3, 0.1),#sad?
			byrow=TRUE,nrow=3)

w_sum= function(a,b){
    assertthat::assert_that(length(a) == length(b))
    output = 0	
    for (i in seq(1,length(a))){
        output = output+ (a[i] * b[i])
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

toes  <- c(8.5, 9.5, 9.9, 9.0)
wlrec <- c(0.65,0.8, 0.8, 0.9)
nfans <- c(1.2, 1.3, 0.5, 1.00)

hurt  <- c(0.1, 0.0, 0.0, 0.1)
win   <- c(  1,   1,   0,   1)
sad   <- c(0.1, 0.0, 0.1, 0.2)

alpha = 0.01

input <- c(toes[1],wlrec[1],nfans[1])
true  <- c(hurt[1], win[1], sad[1])

pred = neural_network(input,weights)

error <- c(0, 0, 0)
delta <- c(0, 0, 0)

for (i in seq(1,length(true))){
    error[i] = (pred[i] - true[i]) ** 2
    delta[i] = pred[i] - true[i]
}

outer_prod= function(a, b){
    # just a matrix of zeros
    out = matrix(rep(0,len=length(a)*length(b)),nrow=length(a))

    for (i in seq(1,length(a))){
        for (j in seq(1,length(b))){
            out[i,j] = a[i] * b[j]
		}
	}
    return(out)
}

weight_deltas = outer_prod(delta,input)

for (i in seq(1,nrow(weights))){
    for (j in seq(1,ncol(weights))){
        weights[i,j] = weights[i,j]- alpha * weight_deltas[i,j]
	}
}
printVec(weights," weights")

#========================================
Print("END of Chapter 5")























# Chapter 8
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
Print("BEGIN Chapter 8 ")
#==================================================
# R: load and save the MNIST dataset for later usage
# R> library(dslabs)
# R> read_mnist(download=TRUE,destdir = "../data/")

Print("learning signal and ignoring noise")
# choose a part to run
# part: 0=(load mnist)  1=(train on 1000)

part = 0
if (part==0) {
  mnist <- read_mnist(path ="../data/")
}




Print(" start: 3 Layer Network on MNIST- train")
x_train <- mnist$train$images
x_test  <- mnist$test$images
y_train <- mnist$train$labels
y_test  <- mnist$test$labels

nTrain = 1000
nTest  = length(y_test)
nWidth = 28
num_labels = 10
images <- matrix(x_train[1:nTrain,],nTrain)/255
labels <- matrix(y_train[1:nTrain],nTrain)

one_hot_labels = matrix(rep(0,num_labels*nTrain),ncol=num_labels)

for (i in seq(1,nTrain)){
  l = labels[i]
  one_hot_labels[i,l] = 1
}
labels = one_hot_labels

test_images <- matrix(x_test,nrow=nTest)/255
test_labels <- matrix(rep(0,nTest*num_labels),nrow=nTest)
for (i in seq(1,nTest)){
  l = y_test[i]
  test_labels[i,l] = 1
}

set.seed(1)
relu= function(x) (x>=0)*x
relu2deriv= function(x) (x>=0)
alpha        = 0.005
iterations   = 350
hidden_size  = 20
pixels_per_image = nWidth*nWidth
weights_0_1 = matrix(runif(pixels_per_image*hidden_size,min=-0.1,max=0.1)
                            ,nrow=hidden_size)
weights_1_2 = matrix(runif(hidden_size*num_labels,min=-0.1,max=0.1),
                         nrow=hidden_size)  


for (j in seq(1,iterations)){
  error = 0.0
  correct_cnt = 0
  for (i in seq(1,nTrain)){
    layer_0 <- images[i,]
    layer_1 <- relu(layer_0 %*% t(weights_0_1))
    layer_2 <- layer_1 %*% weights_1_2
    
    error = error + sum(labels[i,]-layer_2)**2
    correct_cnt = correct_cnt + 
      as.integer(which.max(labels[i,])==which.max(layer_2))
    
    layer_2_delta = labels[i,]-layer_2
    layer_1_delta = (layer_2_delta) %*% t(weights_1_2) * relu2deriv(layer_1)
   
    weights_1_2 = weights_1_2 + alpha* t(layer_1) %*% layer_2_delta
    weights_0_1 = weights_0_1 + alpha* t(layer_0 %*% layer_1_delta)
  }

  
  if (j %% 10 ==0 | j==iterations) {
    cat(sprintf(" I: %d, Train-Err: %g, Train-Acc: %f",
                j,error/as.double(nTrain),correct_cnt/as.double(nTrain)))    
    error2 = 0.
    correct_cnt2 = 0
    for (i in seq(1,nTest)) {
      layer_0 <- test_images[i,]
      layer_1 <- relu(layer_0 %*% t(weights_0_1))
      layer_2 <- layer_1 %*% weights_1_2
      error2 = error2 + sum(test_labels[i,]-layer_2)**2
      correct_cnt2 = correct_cnt2 + 
        as.integer(which.max(layer_2)==which.max(test_labels[i,]))
      
    } # FOR i 
    error2 = error2/as.double(nTest)
    correct_cnt2 = correct_cnt2/as.double(nTest)
    cat(sprintf(" Test-Err: %g , Test-Acc: %g  ",error2,correct_cnt2))
    print("",quote=F)
  } # IF (j
  
}
Print(" end: 3 Layer Network on MNIST")
#================================================
###
Print(" start: Dropout In Code")

set.seed(1)
relu= function(x){
  return((x >= 0) * x) # returns x if x > 0
  # returns 0 otherwise
}

relu2deriv= function(output){
  return(output >= 0) #returns 1 for input > 0
}

alpha       = 0.005
iterations  = 300 
hidden_size = 100
pixels_per_image = 784
num_labels       = 10
nTrain = dim(images)[1]
nTest = 1000

weights_0_1 = matrix(runif(pixels_per_image*hidden_size,min=-0.1,max=0.1)
                     ,nrow=hidden_size)
weights_1_2 = matrix(runif(hidden_size*num_labels,min=-0.1,max=0.1),
                     nrow=hidden_size) 


for (j in seq(1,iterations)){
  error = 0. 
  correct_cnt = 0
  for (i in seq(1, dim(images)[1])){
    layer_0 <- images[i,]
    layer_1 <- relu(layer_0 %*% weights_0_1)
    dropout_mask <- matrix(sample.int(2, length(layer_1), TRUE),ncol=hidden_size)-1
    
    layer_1 <- layer_1 * dropout_mask *2
    layer_2 <- layer_1 %*% weights_1_2
    
    error = error + sum(labels[i,]-layer_2)**2
    correct_cnt = correct_cnt + 
      as.integer(which.max(labels[i,])==which.max(layer_2))
    
    layer_2_delta = labels[i,]-layer_2
    layer_1_delta = (layer_2_delta) %*% t(weights_1_2) * relu2deriv(layer_1)
    
    layer_1_delta <- layer_1_delta * dropout_mask
    weights_1_2 = weights_1_2 + alpha* t(layer_1) %*% layer_2_delta
    weights_0_1 = weights_0_1 + alpha* (layer_0 %*% layer_1_delta)
  } # FOR i in seq
  if(j %% 10 == 0){
    error = error/as.double(nTrain)
    correct_cnt= correct_cnt/as.double(nTrain)
    test_error = 0.0
    test_correct_cnt = 0
    
    for (i in seq(1,nTest)){
      layer_0 = test_images[i,]
      layer_1 = relu(layer_0  %*% weights_0_1)
      layer_2 = layer_1 %*% weights_1_2
      test_error = test_error + sum(test_labels[i,]-layer_2)**2
      test_correct_cnt = test_correct_cnt + 
        as.integer(which.max(layer_2)==which.max(test_labels[i,]))
    } # FOR i
    test_error = test_error/as.double(nTest)
    test_correct_cnt= test_correct_cnt/as.double(nTest)
    
    print(sprintf("  I: %d, Test-Err: %g, Test-Acc= %g, Train-Err: %g, Train-Acc: %g",
                  j,test_error,test_correct_cnt,error,correct_cnt), quote= F) 
  } # IF  j %%
} # FOR j in seq






Print(" end: Dropout In Code")
#=========================================
Print("start: Batch Gradient Descent")
set.seed(1)

relu= function(x){
  return((x >= 0) * x) # returns x if x > 0
}

relu2deriv= function(output){
  return(output >= 0) # returns 1 for input > 0
}

batch_size = 100
alpha      = 0.001
iterations = 300
pixels_per_image = 784
num_labels       = 10 
hidden_size      = 100

weights_0_1 = matrix(runif(pixels_per_image*hidden_size,min=-0.1,max=0.1)
                     ,ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size*num_labels,min=-0.1,max=0.1),
                     nrow=hidden_size) 


for (j in seq(1,iterations)){
  error = 0.
  correct_cnt = 0
  for (i in seq(1,as.integer(nrow(images)/ batch_size))-1){
    batch_start = i*batch_size
    batch_end   = (i+1)*batch_size-1
    
    layer_0 <- images[batch_start:batch_end,]
    layer_1 <-  relu(layer_0 %*% weights_0_1)
    
    dropout_mask = matrix(sample.int(2,length(layer_1),TRUE),ncol=hidden_size)-1
    layer_1 <- layer_1 * dropout_mask * 2
    layer_2 <- layer_1 %*% (weights_1_2)
    
    error = error+ sum((labels[batch_start:batch_end,] - layer_2) ** 2)
    for (k in seq(1,batch_size)){
      correct_cnt = correct_cnt + as.integer(which.max(layer_2[k:k]) == which.max(labels[batch_start+k:batch_start+k]))
    } # FOR k
    
    layer_2_delta <- (labels[batch_start:batch_end]-layer_2)/batch_size
    layer_1_delta <- layer_2_delta %*% t(weights_1_2)* relu2deriv(layer_1)
    layer_1_delta <- layer_1_delta * dropout_mask
    
    weights_1_2 <- weights_1_2 + alpha * t(layer_1) %*% (layer_2_delta)
    weights_0_1 <- weights_0_1 + alpha * t(layer_0 ) %*% (layer_1_delta)
    
  } # FOR i
  if(j %% 10 == 0){
    error = error/as.double(nTrain)
    correct_cnt= correct_cnt/as.double(nTrain)
    test_error = 0.0
    test_correct_cnt = 0
    
    for (i in seq(1,nTest)){
      layer_0 = test_images[i,]
      layer_1 = relu(layer_0  %*% weights_0_1)
      layer_2 = layer_1 %*% weights_1_2
      test_error = test_error + sum(test_labels[i,]-layer_2)**2
      test_correct_cnt = test_correct_cnt + 
        as.integer(which.max(layer_2)==which.max(test_labels[i,]))
    } # FOR i
    test_error = test_error/as.double(nTest)
    test_correct_cnt= test_correct_cnt/as.double(nTest)
    
    print(sprintf("  I: %d, Test-Err: %g, Test-Acc= %g, Train-Err: %g, Train-Acc: %g",
                  j,test_error,test_correct_cnt,error,correct_cnt), quote= F) 
  } # IF  j %%
} # FOR j
Print("end: Batch Gradient Descent")
#=========================================
Print("END of Chapter 8")


















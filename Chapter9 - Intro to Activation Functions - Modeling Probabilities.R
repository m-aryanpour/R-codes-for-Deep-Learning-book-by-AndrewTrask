# Chapter 9
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
Print("BEGIN Chapter 9 ")
#==================================================

set.seed(1)

part = 0
if (part==0) {
  mnist <- read_mnist(path ="../data/")
}
x_train <- mnist$train$images
x_test  <- mnist$test$images
y_train <- mnist$train$labels
y_test  <- mnist$test$labels



images <- x_train[1:1000,]/255
labels <- y_train[1:1000]

one_hot_labels <- matrix(rep(0,length(labels)*10),ncol=10)


for (i in seq(1,length(labels))){
  l = labels[i]
  one_hot_labels[i,l] = 1
} # FOR i
labels <- one_hot_labels

test_images <- matrix(x_test, ncol=,28*28) / 255
test_labels <- matrix(rep(0,length(y_test)*10),ncol=10)


for (i in seq(1,length(y_test))){
  l = y_test[i]
  test_labels[i,l] = 1
} # FOR i

#R> no need to define function tanh
#tanh= function(x){
#  return(tanh(x)) # huge

tanh2deriv= function(output){
  return(1 - (output ** 2))
}

softmax= function(x){
  temp = exp(x)
  return(temp/sum(temp))
}

alpha       = 2
iterations  = 300
hidden_size = 100
pixels_per_image = 784 
num_labels       =  10
batch_size       = 100

weights_0_1 = matrix(runif(pixels_per_image*hidden_size,min=-0.01,max=0.01),ncol=hidden_size)
weights_1_2 = matrix(runif(hidden_size*num_labels,min=-0.1,max=0.1),nrow=hidden_size) 


for (j in seq(1,iterations)){

  for (i in seq(1,as.integer(nrow(images)/batch_size))){
    batch_start = (i-1)*batch_size+1
    batch_end   = (i)*batch_size
    layer_0 <- images[batch_start:batch_end,]
    layer_1 <- tanh(layer_0 %*% weights_0_1)
    dropout_mask <- matrix(sample.int(2,length(layer_1),TRUE),ncol=hidden_size)-1
    layer_1 <- layer_1 * dropout_mask * 2
    layer_2 <- softmax((layer_1) %*% weights_1_2)

    layer_2_delta <- (labels[batch_start:batch_end]-layer_2) / (batch_size* ncol(layer_2))
    layer_1_delta <- layer_2_delta  %*% t(weights_1_2) * tanh2deriv(layer_1)
    layer_1_delta <- layer_1_delta * dropout_mask
    
    weights_1_2 <- weights_1_2 + alpha * t(layer_1) %*% layer_2_delta
    weights_0_1 <- weights_0_1 + alpha * t(layer_0) %*% layer_1_delta
    
  } # FOR i


  
  if(j %% 10 == 0){
    correct_cnt = 0
    for (k in seq(1,batch_size)){
      correct_cnt = correct_cnt + as.integer(which.max(layer_2[k:k]) == which.max(labels[batch_start+k:batch_start+k]))
    } # FOR k
    correct_cnt = correct_cnt/nrow(images)
    
    test_correct_cnt = 0
    for (i in seq(1,nrow(test_images))){
      layer_0 = x_test[i,]
      layer_1 = tanh(layer_0  %*% weights_0_1)
      layer_2 = layer_1 %*% weights_1_2
      test_error = test_error + sum(test_labels[i,]-layer_2)**2
      test_correct_cnt = test_correct_cnt + 
        as.integer(which.max(layer_2)==which.max(test_labels[i,]))
    } # FOR i
    test_correct_cnt= test_correct_cnt/nrow(test_images)
    
    
    print(sprintf("  I: %d, Test-Acc= %g, Train-Acc: %g",
                  j,test_correct_cnt,correct_cnt), quote= F) 
  } # IF  j %%
  
} # FOR j

Print("END of Chapter 9 ")



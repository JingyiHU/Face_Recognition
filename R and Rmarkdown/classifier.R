classifieur_expressions = function(dataset) {
  # Load environment
  load("env.Rdata")
  # Remove the useless data for the trainning data  ===> data_expressions
  cleanData.train=data_expressions[,colSums(abs(data_expressions[,1:ncol(data_expressions)-1])) != 0]
  
  #FDA for the trainning data
  library("MASS")
  lda_data = lda(y~.,data = cleanData.train)
  S = lda_data$scaling
  X = as.matrix(cleanData.train[,1:ncol(cleanData.train)-1])
  Z = X%*%S
  Z = as.data.frame(Z)
  y = cleanData.train$y
  trainFDA = cbind(Z,y)
  
  #Remove column full of 0 of test data ===> dataset
  cleanData.test = dataset[,colSums(abs(dataset[,1:ncol(dataset)-1])) != 0]
  
  #Apply FDA on test data
  X = as.matrix(cleanData.test[,1:ncol(cleanData.test)-1])
  Z = X%*%S
  Z = as.data.frame(Z)
  y = cleanData.test$y
  testFDA = cbind(Z,y)
  
  #LDA
  lda_data = lda(y~.,data = trainFDA)
  predictions = predict(lda_data,newdata = testFDA)
  predictions = predictions$class
  
  return(predictions)
}


#########################################################################################################


classifieur_characters = function(dataset) {
  # Load environment
  load("env.Rdata")
  # Random Forest
  library(randomForest)
 
  RF = randomForest(Y~., data = data_characters,mtry = 4)
  predictions = predict(RF, newdata = dataset, type = 'response')
  return(predictions)
}


#########################################################################################################


classifieur_parole = function(dataset) {
  # Load environment
  load("env.Rdata")
  # Naive Bayesian 
  
  #FDA for the trainning data ===> data_parole
  library("MASS")
  lda_data = lda(y~.,data = data_parole)
  U = lda_data$scaling
  X = as.matrix(data_parole[,1:ncol(data_parole)-1])
  Z = X%*%U
  Z = as.data.frame(Z)
  y = data_parole$y
  trainFDA = cbind(Z,y)
  
  #Apply FDA on test data ===>  dataset
  X = as.matrix(dataset[,1:ncol(dataset)-1])
  Z = X%*%U
  Z = as.data.frame(Z)
  y = dataset$y
  testFDA = cbind(Z,y)
  
  library(klaR)
  
  NB = NaiveBayes(y~.,data=trainFDA)
  predictions = predict(NB,newdata = testFDA)
  predictions = predictions$class
  return(predictions)
}

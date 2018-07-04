# Auther: 	Yuqian Shi
# Date:		4 Jul 2018
# Name: 	randomForest  in R







require(randomForest)
require(MASS)# the Boston housing dataset
attach(Boston)

set.seed(101)

dim(Boston)


#select 300 instances for training
train=sample(1:nrow(Boston),300)
?Boston  #to search on the dataset

Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf

plot(Boston.rf)

oob.err	=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(medv ~ . , data = Boston , 
  	subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,Boston[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(Boston[-train,], mean( (medv - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}



matplot(
	1:mtry , cbind(oob.err,test.err), pch=19 , 
	col=c("red","blue"),type="b",
	ylab="Mean Squared Error",
	xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),
	pch=19, col=c("red","blue"))

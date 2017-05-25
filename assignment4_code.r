

main<-function(){
library("e1071")
	data.frame<-read.csv("abalone.data")
	data.frame1<-read.csv("Exercise-4.data")
	names(data.frame) <- c("sex","length","diameter","height","w_weight","s_weight","v_weight","shell_weight","rings")
	names(data.frame1) <- c("x","y")
	degree <- c(1,2,3)
	cost <- c(0.1, 1.0, 10.0, 100.0)
	epsilon <- c(0.1, 1.0, 1.5, 1.75)
	exercise1(data.frame,degree,cost,5)
	exercise2_3(data.frame,degree,cost,5)
	exercise4(data.frame1,cost,epsilon,5)
	exercise5(data.frame1,cost,epsilon,5)
	exercise6(data.frame,degree,cost,epsilon,5)
}

exercise2_3<-function(data.frame, degree, cost, n){
cat("****************************************************\n")
	data.frame.9_10 <- data.frame[data.frame$rings==9|data.frame$rings==10, ]
	cat("\n9 vs 10, Total rows:",nrow(data.frame.9_10))
	model.9_10 <- best.Nfold.model(data.frame.9_10, degree, cost, n)
	result<-data.frame$rings==fitted(model.9_10)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.9_10))*100)
	
	data.frame.10.11_12 <- data.frame[data.frame$rings==10|data.frame$rings==11|data.frame$rings==12, ]
	cat("\n10 vs 11 vs 12, Total rows:",nrow(data.frame.10.11_12))
	model.10.11_12 <- best.Nfold.model(data.frame.10.11_12, degree, cost, n)
	result<-data.frame$rings==fitted(model.10.11_12)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.10.11_12))*100)
	
	data.frame.12.13_14 <- data.frame[data.frame$rings==12|data.frame$rings==13|data.frame$rings==14, ]
	cat("\n12 vs 13 vs 14, Total rows:",nrow(data.frame.12.13_14))
	model.12.13_14 <- best.Nfold.model(data.frame.12.13_14, degree, cost, n)
	result<-data.frame$rings==fitted(model.12.13_14)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.12.13_14))*100)
	
	data.frame.12_13 <- data.frame[data.frame$rings==12|data.frame$rings==13, ]
	cat("\n12 vs 13, Total rows:",nrow(data.frame.12_13))
	model.12_13 <- best.Nfold.model(data.frame.12_13, degree, cost, n)
	result<-data.frame$rings==fitted(model.12_13)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.12_13))*100)
	
	data.frame.10_11 <- data.frame[data.frame$rings==10|data.frame$rings==11, ]
	cat("\n10 vs 11, Total rows:",nrow(data.frame.10_11))
	model.10_11 <- best.Nfold.model(data.frame.10_11, degree, cost, n)
	result<-data.frame$rings==fitted(model.10_11)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.10_11))*100)
	
	data.frame.7_8.9 <- data.frame[data.frame$rings==7|data.frame$rings==8|data.frame$rings==9, ]
	cat("\n7 vs 8 vs 9, Total rows:%f",nrow(data.frame.7_8.9))
	model.7_8.9 <- best.Nfold.model(data.frame.7_8.9, degree, cost, n)
	result<-data.frame$rings==fitted(model.7_8.9)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.7_8.9))*100)
	
	data.frame.8_9 <- data.frame[data.frame$rings==8|data.frame$rings==9, ]
	cat("\n8 vs 9, Total rows:",nrow(data.frame.8_9))
	model.8_9 <- best.Nfold.model(data.frame.8_9, degree, cost, n) 
	result<-data.frame$rings==fitted(model.8_9)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.8_9))*100)
	
	data.frame.5_6.7 <- data.frame[data.frame$rings==5|data.frame$rings==6|data.frame$rings==7, ]
	cat("\n5 vs 6 vs 7, Total rows:",nrow(data.frame.5_6.7))
	model.5_6.7 <- best.Nfold.model(data.frame.5_6.7, degree, cost, n)
	result<-data.frame$rings==fitted(model.5_6.7)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.5_6.7))*100)
	
	data.frame.6_7 <- data.frame[data.frame$rings==6|data.frame$rings==7, ]
	cat("\n6 vs 7, Total rows:",nrow(data.frame.6_7))
	model.6_7 <- best.Nfold.model(data.frame.6_7, degree, cost, n)
	result<-data.frame$rings==fitted(model.6_7)
	result<-length(result[result==TRUE])
	cat("\tTraining accuracy:",(result/nrow(data.frame.6_7))*100)
	
	data.frame1 <- subset(data.frame,select=-rings)
	predict_list<-c()
	for(i in 1:nrow(data.frame)){
		root_predict1 <- predict(model.9_10,data.frame1[i,])
		root_predict <- as.numeric(as.character(root_predict1))
		value<--1
		if(root_predict>10|root_predict==10){
			if(as.numeric(as.character(predict(model.10.11_12,data.frame1[i,])))>11){
				value<- as.numeric(as.character(predict(model.12.13_14,data.frame1[i,])))
				if(value<14){
					value<-as.numeric(as.character(predict(model.12_13,data.frame1[i,])))
					if(value<13){
						predict_list<-c(predict_list,value)
					}
					else{
						predict_list<-c(predict_list,value)
					}
				}	
				else{
					predict_list<-c(predict_list,14)
				}
			}
			else{
				value<-as.numeric(as.character(predict(model.10_11,data.frame1[i,])))
				if(value<11)
					predict_list<-c(predict_list,value)
				else
					predict_list<-c(predict_list,value)
			}
		}
		else{
			value<-as.numeric(as.character(predict(model.7_8.9,data.frame1[i,])))
			if(value>7){
				value<-as.numeric(as.character(predict(model.8_9,data.frame1[i,])))
				if(value<9){
					predict_list<-c(predict_list,value)
				}
				else{
					predict_list<-c(predict_list,value)
				}
			}
			else{
				value<-as.numeric(as.character(predict(model.5_6.7,data.frame1[i,])))
				if(value>5){
					value<- as.numeric(as.character(predict(model.6_7,data.frame1[i,])))
					if(value<7){
						predict_list<-c(predict_list,value)
					}
					else{
						predict_list<-c(predict_list,value)
					}
				}
				else{
					predict_list<-c(predict_list,5)
				}
			}
	
		}
	}
	class_rings <- as.numeric(data.frame$rings)
	count<-0
	difference_total <- 0
	difference_list <- c()
	for(i in 1:nrow(data.frame)){
		difference <- class_rings[i]-predict_list[i]
		difference_list <- c(difference_list,difference)
		difference_total <- difference_total + difference
		if(class_rings[i]==predict_list[i]){
		count<-count+1}
	}
	difference_avg <-  difference_total/nrow(data.frame)
	print(difference_avg)
	result<-(count/nrow(data.frame))*100
	print(result)
	hist(difference_list,right=FALSE)
}

exercise4<-function(data.frame,cost,e,n){
cat("****************************************************\n")	
	plot(data.frame$x,data.frame$y)
	q<-0
	best_model<--1
	lowest_avg_mse <- 1000000000
	mean_error_list <- c()
	low_cost<- -1
	low_epsilon<- -1
	
		for(j in cost){
			for(k in e)
			{	
				model <- svm(y ~.,
							data=data.frame,
							type="eps-regression",
							kernel="polynomial",
							degree=2,
							cost=j,
							cross=n,
							epsilon=k
						)
				predictions <-  predict(model,data.frame)
				predict_list <- c()
				predict_list <- c(predict_list,predictions)
				mean_error <- 0
				for(p in 1:nrow(data.frame)){
					mean_error = mean_error + (data.frame$y[p]-predict_list[p])^2
				}
				cat("\nmean error :",as.numeric(mean_error))
				mean_error_avg <- (mean_error/nrow(data.frame))
				cat("\tmean_error_avg :",as.numeric(mean_error_avg))
				
				if(lowest_avg_mse>mean_error_avg){
					lowest_avg_mse<-mean_error_avg
					best_model <- model	
					low_cost <- j
					low_epsilon <- k
				}
				
			}
		}
		cat("\nlowest average for best model : \t",lowest_avg_mse,"\tassociated")
		return(best_model)
	
}

exercise5<-function(data.frame1,cost,epsilon,n){
	best_model <- exercise4(data.frame1,cost,epsilon,5)
	predictions <- predict(best_model,data.frame1)
	points(data.frame1$x,predictions,col="green")
	points(data.frame1$x,predictions,col="green",pch=4)
	points(data.frame1$x,predictions,col="yellow")
}

exercise6<-function(data.frame,degree,cost,e,n){
cat("****************************************************\n")
	
	best_model <- -1
	min_error<-100000000000
	for(i in degree){
		for(j in cost){
			for(k in e)
			{	
				model <- svm(rings ~.,
							data=data.frame,
							type="eps-regression",
							kernel="polynomial",
							degree=i,
							cost=j,
							cross=n,
							epsilon=k
						)
				predictions <-  predict(model,data.frame)
				predict_list <- c()
				predict_list <- c(predict_list,predictions)
				mean_error <- 0
				mean_error_avg<-0
				for(p in 1:nrow(data.frame)){
					mean_error = mean_error + (data.frame$rings[p]-predict_list[p])
				}
				mean_error_avg<-mean_error/nrow(data.frame)
				if(min_error>mean_error_avg){
					min_error<-mean_error_avg
					best_model<-model
				}
			}
		}	
	}
	print(best_model)
	differences <- c()
	predict_list <- predict(best_model,data.frame)
	predictions<- c()
	predictions <- c(predictions,predict_list)
	mean_v <- 0
	for (i in 1:nrow(data.frame)){
			difference <- data.frame$rings[i]-predictions[i]
			mean_v <- mean_v + difference
			differences<- c(differences,difference)
	}
	result <- mean_v/nrow(data.frame)
	cat("Average distance of the predicted class from the true class : ")
	print(result)	
	hist(differences,right=FALSE)	
}

best.Nfold.model<-function(data.frame, degree, cost, n){
	max_accuracy<- 	-1
	best_model <- -1
	deg<--1
	cost1<--1
	
	for(i in degree){
		for(j in cost){	
			model <- svm(rings ~.,
						data=data.frame,
						type="C-classification",
						kernel="polynomial",
						degree=i,
						cost=j,
						cross=n
					)
			accuracy<-model$tot.accuracy
			
			if(accuracy>max_accuracy){
				max_accuracy<-accuracy
				best_model<-model
				deg<-i
				cost1<-j
			}
		}	
	}
	
	cat("\taccuracy of model :",as.numeric(max_accuracy),"\tdegree :",as.numeric(deg),"\tcost :",as.numeric(cost1))
	return(best_model)
}

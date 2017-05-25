	#SUMEET SUNIL GADKARI 016115942  ASSIGNMNET 3

#RUN MAIN FUNCTION. MAIN CALLS ON EVERY FUNCTION OF THE EXERCISE 
main<-function(){
	library("e1071")
	data.frame<-read.csv("car.data")
	names(data.frame) <- c("buying","maint","doors","persons","lug_boot","safety","Class")
	alpha <- 0.8
	degree <- c(1,2,3,4)
	cost <- c(0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0)
	best_svm(data.frame,alpha,degree,cost)
	cost_fixed <- 100000
	exercise4(data.frame,alpha,degree,cost_fixed)
	data.frame<-read.csv("breast-cancer-wisconsin.data")
	names(data.frame) <- c("Sample.code.number","Clump.Thickness","Cell.Size","Cell.Shape","Marginal.Adhesion","Epithelia.Cell.Size","Nuclei","Chromatin","Nucleoli","Mitoses","Class")
	best.svm.cross(data.frame,degree,cost,10)
	model<-best.Nfold.model(data.frame,degree,cost,10)
	bootstrap(data.frame, model, 0.90, 100)
}


#EXERCISE 1. THIS FUNCTION PARTIONS GIVEN DATA SET INTO TEST AND TRAIN
partition<-function(data.frame,alpha){
	count<-nrow(data.frame)
	
	trainIndex <- sample(1:count, size = round(alpha*count), replace=FALSE)
	data.train <- data.frame[trainIndex ,]
	data.test  <- data.frame[-trainIndex ,]
	result <- list(df1=data.train,df2=data.test)
}

#EXERCISE 2 AND 3. BEST_SVM . ANSWER TO THIS EXERCISE CHANGES ACCORDING TO THE TRAIN AND TEST DATASETS RETURNED BY PARTITION FUNCTION
#ONE OF THE ANSWER FOR EXERCISE 3 :  DEGREE=2 	COST=10000     ACCURACY= 99.71014
best_svm<- function(data.frame,alpha,degree,cost){
		result <- partition(data.frame,alpha)
		data.train <- result[[1]]
		data.test  <- result[[2]]
		count<-nrow(data.test)
		accuracy<--1
		degree_best<--1
		cost_best<--1
		for(i in degree){
			for(j in cost){
				model		<-	svm(Class ~.,data=data.train,kernel="polynomial",degree=i,type="C-classification",cost=j)
				prediction	<-	predict(model,data.test)
				result		<- (length(prediction[prediction==data.test$Class]))
				
				if(accuracy<result){
					accuracy	<-result
					degree_best	<-i
					cost_best	<-j
				}
			}
		}
		result<-c(degree_best,cost_best,(accuracy/count)*100)
		cat("Exercise 2 and 3 = \n")
		print(result)
		return(result)
}

#EXERCISE 4 : DEGREE = 2 GIVES 100% ACCURACY
exercise4<-function(data.frame,alpha,degree,cost){
		count<-nrow(data.frame)
		degree_best<- -1
		result1<- -1
		for(i in degree){
				model		<-	svm(Class ~.,data=data.frame,kernel="polynomial",degree=i,type="C-classification",cost=cost)
				prediction	<-	predict(model,data.frame)
				result		<- 	(length(prediction[prediction==data.frame$Class]))
				
				if((result/count)==1){
					degree_best	<- i
					break
			}
		}
		cat("Exercise 4, Best degree value = \n")
		print(degree_best)
}

#EXERCISE 5 AND 6. ANSWER :  DEGREE=1.0000 COST=10.0000 ACCURACY=96.5616
best.svm.cross<-function(data.frame, degree, cost, n){
	
	max_accuracy<- 	-1
	degree_best	<-	-1
	cost_best	<-	-1
	best_model <- -1
	
	for(i in degree){
		for(j in cost){	
			model <- svm(Class ~.,
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
				degree_best<-i
				cost_best<-j
				best_model<-model
			}
			
		}	
	}

	result <- c(degree_best,cost_best,max_accuracy)
	cat("Exercise 5 and 6 = \n")
	print(result)
	return(result)
	
}

#THIS FUNCTION GIVES BEST MODEL USING N-FOLD FOR BOOTSTRAP FUNCTION 
best.Nfold.model<-function(data.frame, degree, cost, n){
	max_accuracy<- 	-1
	best_model <- -1
	
	for(i in degree){
		for(j in cost){	
			model <- svm(Class ~.,
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
			}
		}	
	}
	return(best_model)
}


#EXERCISE 7 AND 8
bootstrap<-function(data.frame, model, p, n){
	accuracy.list <- c()
	for(i in 1:n){
		sample.df <- data.frame[sample(nrow(data.frame),replace=TRUE),]
		subset.df <- subset(sample.df,select=-Class)
		confusion.matrix<-table(predict(model,subset.df),sample.df$Class)
		accuracy <- ((confusion.matrix[1,1]+confusion.matrix[2,2])/length(data.frame$Class))*100
		accuracy.list <- c(accuracy.list,accuracy)
	}
	accuracy.list<-sort(accuracy.list)
	percentile <- (100-(p*100))/2
	lower.bound <- accuracy.list[0+percentile]
	upper.bound <- accuracy.list[100-percentile]
	result <- list(lb=lower.bound,ub=upper.bound)
	cat("Exercise 7 and 8, lower and upper bound = \n")
	return(result)

}




#Sumeet Sunil Gadkari

create_df <- function(){
	data.frame <- read.csv("Exercise-4.csv",header=FALSE,stringsAsFactors=FALSE)
	names(data.frame) <- c("x1","x2","x3","x4","x5","x6","label")
	return (data.frame)
}


#EXERCISE 1 : FOLLOWING FUNCTION RETURNS VALUES OF W AND B USING SIMPLE LEARNING ALGORITHM
simple_learner<- function(){
	simple.df <- create_df()
		
	cplus=c(colMeans(subset(simple.df,simple.df$label==1,select=x2:x6)))
	cminus=c(colMeans(subset(simple.df,simple.df$label==-1,select=x2:x6)))
	
	w.bar <- as.numeric(c(cplus-cminus))
	c.bar <- as.numeric(c((cplus+cminus)/2))

	b <- w.bar%*%c.bar
	 
	result <- list(w=w.bar,b=b)
	result
}

#EXERCISE 2 : FOLLOWING FUNCTION RETURNS VALUES OF W AND B USING PERCEPTRON LEARNING ALGORITHM
perceptron_learner<- function(){
	percep.df <- create_df()
	percep.values.df <- subset(percep.df,select=x2:x6)
	percep.label.df <- subset(percep.df,select=label)
	
	r <- 1
	cols <- length(percep.values.df)
	w.bar <- rep(0,cols)
	b <- 0
	eta <- 1
	X <- c()
	z<-0
	
	repeat{
		misclassified <- 0
		
			for(i in 1:nrow(percep.values.df)){
				X<-as.numeric(percep.values.df[i,1:cols])
				z <- sign((w.bar%*%X)-b)
				if(z>0){
					z<-1
				}
				else{
					z<- -1
				}
				if(z!=percep.df$label[i]){
					misclassified <- 1
					w.bar <- w.bar + (percep.df$label[i]*X)
					b <- b - percep.df$label[i]
				}
			}
			if(misclassified == 0){
				break
			}
	}
	result <- list(w=w.bar,b=b)
	result
}

#EXERCISE 3: FOLLOWING FUNCTION CLASSIFIES DATA FRAME GIVEN THE VALUES OF W AND B
classify<- function(){
	result <- simple_learner()	
	w.bar <- result[[1]]
	b <- result[[2]]
	classify.df <- create_df()
	classify.values.df <- subset(classify.df,select=x2:x6)
	cols <- length(classify.values.df)
	z.list <- c()
	count<-0
	for(i in 1:nrow(classify.values.df)){
				X<-as.numeric(classify.values.df[i,1:cols])
				z <- sign((w.bar%*%X)-b)
				if(z==0||z==1){
					z<-1
				}
				z.list <- c(z.list,z)	
			}	
	z.list
}

#EXERCISE 4
# w = (11.87250, 20.36818, -16.60066, 27.87608, -22.29279) 		b = -70.39144

#EXERCISE 5
# w = ( 409.2977, 757.0583, -722.5130, 908.0482, -929.8089)		b =  -278



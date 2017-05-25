create_dataframe <- function(){
	data.frame <- read.csv("mammographic_masses.data",header=FALSE,stringsAsFactors=FALSE)
	names(data.frame)<-c("Birads","Age","Shape","Margin","Density","Severity")
	data.frame[data.frame=="?"] <- NA
	return (data.frame)
}

create_dataframe_neg <- function(){
	data.frame <- read.csv("mammographic_masses.data",header=FALSE,stringsAsFactors=FALSE)
	names(data.frame)<-c("Birads","Age","Shape","Margin","Density","Severity")
	data.frame[data.frame=="?"] <- 
	data.frame[is.na(data.frame)]<- -1
	return (data.frame)
}

gettotalcount_dataframe <- function(){
	data.frame<-create_dataframe()
	data.frame <- data.frame[complete.cases(data.frame),]
	return (nrow(data.frame))
}

gettotalcount_dataframe_1 <- function(){
	data.frame<-create_dataframe_neg()
	return (nrow(data.frame))
}

linearmodel_1 <- function(){
	library(e1071)
	data.frame<-create_dataframe_neg()
	#print(data.frame)
	model<-svm(Severity ~.,data=data.frame,kernel="linear",type="C-classification")
	result<-data.frame$Severity==fitted(model)
	return (length(result[result==TRUE]))
}


linearmodel <- function(){
	library(e1071)
	data.frame<-create_dataframe()
	data.frame <- data.frame[complete.cases(data.frame),]
	#print(data.frame)
	model<-svm(Severity ~.,data=data.frame,kernel="linear",type="C-classification")
	result<-data.frame$Severity==fitted(model)
	return (length(result[result==TRUE]))
}

ans_linear <- function() {
	result <- linearmodel() / gettotalcount_dataframe()
	return (result*100)
}

ans_linear_1 <- function() {
	result <- linearmodel_1() / gettotalcount_dataframe_1()
	return (result*100)
}

polymodel <-  function(){
	library(e1071)
	data.frame<-create_dataframe()
	data.frame <- data.frame[complete.cases(data.frame),]
	#print(data.frame)
	model<-svm(Severity ~.,data=data.frame,kernel="polynomial",degree=2,type="C-classification")
	result<-data.frame$Severity==fitted(model)
	return (length(result[result==TRUE]))
}

ans_poly <- function() {
	result <- polymodel() / gettotalcount_dataframe()
	return (result*100)
}
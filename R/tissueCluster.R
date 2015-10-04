tissueCluster <-
function(component,type="response"){
#   library(randomForest)
#   data(classification)
#   read.csv("classification.txt",sep="\t",header=T,row.names=1)->classification
   set.seed(100)
   rf<-randomForest(subtype~.,data=classification,proximity=TRUE,importance=TRUE)
   pred<-predict(rf,component,type=type)
   return(pred)
}

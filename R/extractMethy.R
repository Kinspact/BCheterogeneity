extractMethy <-
function(file){ 
#data(cellmethy)
#data(gpl) 
gene<-rownames(cellmethy)
#read.csv("G:/R/gpl.txt",sep="\t",header=T)->gpl
num<-dim(file)[2]
matrix(nrow=length(gene),ncol=num)->result 
rownames(result)<-gene
colnames(result)<-colnames(file)
for(i in 1:length(gene))  {
  which(gpl[,2]==gene[i])->s 
  for(j in 1:num){       
    methy<-file[s,j]     
    methy<-methy[!is.na(methy)]       
    m<-mean(methy)
    result[i,j]<-m 
  }
}           
  return(result)
}

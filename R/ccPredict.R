ccPredict <-
function(methy,p=0.01,q=0.00001){ 
#  library(quadprog)
#  library(pls)  
#  data(cellmethy)
# read.csv("G:/R1/cellmethy.txt",sep="\t",header=T,row.names=1)->cellmethy 
  v<-colnames(methy)
  num<-dim(methy)[2]
  sam<-colnames(cellmethy)
  33->k
  matrix(0,nrow=num,ncol=33)->result
  colnames(result)<-colnames(cellmethy)
  rownames(result)<-v

for(i in 1:num){  
   tissue<-methy[,i]
   data1<-as.matrix(cellmethy)
   pls1<-plsr(tissue~data1,validation="LOO",jackknife=TRUE)
   RMSEP(pls1)$val->rmsep   
   matrix(rmsep,2)->rmsep
   cv<-rmsep[1,2:34]
   which(cv[]==min(cv)) ->ncom
   pls2<-plsr(tissue~data1,ncomp=ncom,validation="LOO",jackknife=TRUE)
   coefficients(pls2)->s2
   jack.test(pls2)[5]->pvalue
   pvalue[[1]]->pvalue  
   tissue->y
   x<-data1
   Dmat<-2*t(x)%*%x
   dvec<-2*t(y)%*%x
   Amat<-rep(-1,k)
   con<-matrix(0,k,k)
   diag(con)<-1
   rbind(Amat,con) ->Amat  
   bvec<-c(-1,rep(0,k))
   solve.QP(Dmat,t(dvec),t(Amat),bvec) ->quad
   quad$solution->coef  
   which(pvalue<p&coef>q)->re        
   result[i,re]<-coef[re]   
}
   return(result)
}

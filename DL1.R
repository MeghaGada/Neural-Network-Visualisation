TKS=c(20,10,30,20,80,30)
CSS=c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
df=data.frame(TKS,CSS,Placed)

#load library
library(neuralnet)
require(neuralnet)
nn=neuralnet(Placed~TKS+CSS,data=df,hidden=3,act.fct="logistic", linear.output=FALSE)
#nn=neuralnet(Placed~TKS+CSS,data=df,hidden=c(4,2),act.fct="logistic", linear.output=FALSE)
plot(nn)


TKS=c(50,30,65)
CSS=c(40,20,60)
test=data.frame(TKS,CSS)

Predict=compute(nn,test)
Predict$net.result

prob <- Predict$net.result
pred<-ifelse(prob>0.5,1,0)
pred
plot(pred)


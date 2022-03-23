#fcfps: Free cash flow per share(in$)
#earnings_growth: Earnings growth in the past year(in%)
#de:Debt to Equity Ratio
#mcap:Market Capitalization of the stock
#current_ratio:current ratio(or current assets/current liablities)
df <- read.csv("C:\\Users\\megha\\OneDrive\\Desktop\\Deep Learning\\dividendinfo.csv")
attach(df)
str(df)
scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))


trainset<-maxmindf[1:150,]
testset<-maxmindf[151:200,]

library(neuralnet)
nn<- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio,data=trainset,hidden=c(2,1),linear.output=FALSE,threshold=0.01)
nn$result.matrix
plot(nn)

nn$result.matrix
temp_test <- subset(testset,select = 
                      c("fcfps","earnings_growth","de","mcap","current_ratio"))
head(temp_test)
nn.results <- compute(nn,temp_test)
results <- data.frame(actual=testset$dividend,prediction=nn.results$net.result)

results                      


fcfps=c(1,2,3)
earnings_growth=c(10,20,18)
de=c(1,2,3)
mcap=c(551,620,195)
current_ratio=c(0,1,2)
test=data.frame(fcfps,earnings_growth,de,mcap,current_ratio)
Predict=compute(nn,test)
Predict$net.result
prob <- Predict$net.result
pred<-ifelse(prob>0.5,1,0)
pred
plot(pred)

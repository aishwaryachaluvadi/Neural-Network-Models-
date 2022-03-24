library(readr)
library(neuralnet)
tesla<- read_csv('/Users/aishwaryachaluvadi/Downloads/TSLA.csv')
attach(tesla)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
minmax_n<-as.data.frame(lapply(tesla,normalize))
train_n<-minmax_n[1:1813,]
test_n<- minmax_n[1814:2417,]
names(tesla)
nn<-neuralnet(Close~Low+High+Open,data = train_n,hidden = c(3,1),threshold = 0.01)
nn$result.matrix
plot(nn)

nn$result.matrix

t_test<-subset(test_n,select = c("Low","High","Open"))
nn.results<-compute(nn,t_test)

results<-data.frame(actual = test_n$Close,prediction=nn.results$net.result)
results

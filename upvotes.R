library(caret)

train<-read.csv('train_NIR5Yl1.csv')

head(train)

dv=train$Upvotes

# missing values
any(sapply(train,function(x) any(is.na(x))))

train2=train[,-match(c("ID","Username","Upvotes"),names(train))]

faclev<-data.frame(levels=levels(as.factor(train2$Tag)),labels=1:length(levels(as.factor(train2$Tag))))

train2$Tag<-unclass(as.factor(train2$Tag))

poisreg<-function(x){
  data<-train2[train2$Tag==x,]
  y<-dv[train2$Tag==x]
  i<-createDataPartition(y,p=0.7,groups=10)
  dev<-cbind(data[i[[1]],],y=y[i[[1]]])
  val<-cbind(data[-i[[1]],],y=y[-i[[1]]])
  poisreg<-glm(y~Reputation+Answers+Views,family='poisson',data=dev)
  pred<-predict(poisreg,newdata=val[,-match("y",names(val))],type="response")
  defaultSummary(data=data.frame(obs=val$y,pred=round(pred)))["RMSE"]
}

sapply(levels(as.factor(train2$Tag)),poisreg)

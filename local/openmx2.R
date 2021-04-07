N<-96
nc<-2
nr<-20
x<-rnorm(N*nr)
perClust<-data.frame(ID=1:N,int=rnorm(N,sd = 0),bx=rnorm(N,1),bm=rnorm(N,2))
perCase<-data.frame(ID=rep(1:N,each=nr),x=x)
data<-merge(perCase,perClust,by="ID")
data$m<-(data$bx)*data$x+1*data$int+rnorm(N*nr)
data$y<-data$bm*data$m+1*data$int+data$bx+data$x+rnorm(N*nr)
library(lmerTest)

wdata<-aggregate(data[,c("x","y","m")],list(data$ID),mean)
names(wdata)
mm<-lm(y~x+m,data=wdata)
summary(mm)
(mod0<-lmer(y~(1|ID)+x,data=data))

mod1<-lmer(m~(+x|ID)+x,data=data)
mod2<-lmer(y~(0+m+x|ID)+x+m,data=data)
summary(mod1)
summary(mod2)

cc<-cov(coef(mod1)$ID[,2],coef(mod2)$ID[,3])

(smm<-summary(mod1))
(smy<-summary(mod2))
(me<-smm$coefficients[2,1]*smy$coefficients[3,1]+cc)

mod3<-lmer(y~(0+m|ID)+m,data=data)
summary(mod3)

xdata<-data[,c("ID","x","m","y")]
head(xdata)
write.table(xdata,file="local/mydata.dat",row.names = F,col.names = F)

wdata<-reshape(xdata,timevar="x",idvar="ID",direction = "wide")
head(wdata)
cor(wdata$y.1,wdata$m.1)
wdata$msum<-wdata$m.1+wdata$m.0
wdata$msum<-wdata$msum-mean(wdata$msum)
wdata$mdiff<-wdata$m.1-wdata$m.0
wdata$ydiff<-wdata$y.1-wdata$y.0

(sm<-summary(lm(formula(ydiff~1+mdiff+msum),data=wdata)))
(sy<-summary(lm(formula(ydiff~1),data=wdata)))
2*sm$coefficients[2,1]
fixef(mod0)[2]-me
smy$coefficients[2,1]


bySubj <- mxModel(model='bySubj', type='RAM',
                  latentVars=c('slope'),
                  mxData(data.frame(ID=unique(data$ID)),
                         type='raw', primaryKey = 'ID'),
                  mxPath(from=c('slope'), arrows =2, values =1)
)

mainModel <- mxModel(
  model='main', type='RAM', bySubj,
  manifestVars=c('y'), latentVars = c('x',"m"),
  mxData(data, type='raw', sort=FALSE),
  mxPath(from='one', to='y', arrows =1, free=TRUE),
  mxPath(from='one', to='x', arrows =1, free=FALSE, labels='data.x'),
  mxPath(from='one', to='m', arrows =1, free=FALSE, labels='data.m'),
  mxPath(from='m', to='y', arrows =1, free=TRUE),
  mxPath(from='x', to='y', arrows =1, free=TRUE),
#  mxPath(from='x', to='m', arrows =2, free=TRUE,labels="cov"),
  mxPath(from='y', arrows =2, values =1),
  mxPath(from='m',  arrows =2, free=T,labels="errM"),
  mxPath(from='x', to='m', arrows =1, free=T,labels="Bmx"),
  
  mxPath(paste0('bySubj.', c('slope')),'y', arrows=1, free=FALSE, values=c(1),
         labels=c("data.x"), joinKey='ID'))
m1 <- mxRun(mainModel)
m1
summary(m1)
(mod0<-lmer(y~(0+x|ID)+x+m,data=data))
(mod1<-lm(m~x,data=data))
summary(mod1)
summary(mod0)
semPlot::semPaths(mainModel)

# dataset
dataRaw      <- mxData( observed=data, type="raw" )
# variance paths      
varPaths     <- mxPath( from=c("x","y","m"),  arrows=2, 
                        free=TRUE, values = c(1,1,1), labels=c("varx","res","varm") )
# covariance of x and z
covPaths     <- mxPath( from="x", to="m", arrows=1, 
                        free=TRUE, values=0.5, labels="betamx" )
# regression weights
regPaths     <- mxPath( from=c("x","m"), to="y", arrows=1, 
                        free=TRUE, values=1, labels=c("betax","betam") )
# means and intercepts
means        <- mxPath( from="one", to=c("y","m"), arrows=1, 
                        free=TRUE, values=c(1), labels=c("ay","am") )

multiRegModel <- mxModel("Multiple Regression Path Specification", type="RAM",
                         dataRaw, manifestVars=c("y","x","m"),
                         varPaths, covPaths, regPaths, means)
# Create an MxModel object
# -----------------------------------------------------------------------------
semPlot::semPaths(multiRegModel)
multiRegFit<-mxRun(multiRegModel)

lm(y~x+m,data=data)
lm(m~x,data=data)

summary(multiRegFit)
var(data$x)

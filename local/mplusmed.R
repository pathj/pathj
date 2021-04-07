data<-read.table("local/mydata.dat")
names(data)<-c("id","x","m","y")

mod1<-lmer(m~(1+x|id)+x,data=data)
mod0<-lmer(m~(1+x|id)+x,data=data)

mod2<-lmer(y~(1+x+m|id)+x+m,data=data)
mod3<-lmer(y~(1+m|id)+m,data=data)
mod4<-lmer(y~(1+x|id)+x,data=data)

(smm<-summary(mod1))
(smy<-summary(mod2))
cc<-cov(coef(mod1)$id[,2],coef(mod2)$id[,3])
cc
(me<-smm$coefficients[2,1]*smy$coefficients[3,1]+cc)

summary(mod0)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

cf<-coef(mod2)$id
names(cf)<-c("a","x","m")
cf1<-coef(mod1)$id
names(cf1)<-c("a","x")

summary(lm(wdata$m~cf$x))

wdata<-aggregate(data[,c("x","y","m")],list(data$id),mean)
names(wdata)
mm<-lm(y~x+m,data=wdata)
summary(mm)
mm<-lm(m~x,data=wdata)
summary(mm)

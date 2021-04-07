source("R/constants.R")
source("R/functions.R")
source("R/smart.R")

library(lavaan)

x1<-rnorm(100)
x2<-x1+rnorm(100)
x3<-rnorm(100)
x4<-rnorm(100)
fact1<-rep(c(1:4),25)
m1<-x1+rnorm(100)
m2<-x1+x2+rnorm(100)
m3<-x1+x2+x3+rnorm(100)
m4<-x1*x2+rnorm(100)
y<-m1+m2+m4+x3+x4*fact1+rnorm(100)
data<-data.frame(cbind(y,m1,m2,m3,m4,fact1,x1,x2,x3,x4))
data$fact1<-factor(data$fact1)
head(data)
#write.csv(data,"../jamm_docs/data/somedata.csv")
ff<-y~x1*x2+fact1
all.vars(ff)
mm<-lm(y~x1*x2+fact1)
ss<-summary(mm)
ss$df[2]
qq<-ss$coefficients
mm$coefficients
all.vars(as.formula("y~x1*x2+fact1"))
toscale<-names(which(sapply(names(data),function(a) class(data[[a]]))=="numeric"))
noscale<-names(which(sapply(names(data),function(a) class(data[[a]]))=="factor"))
zdata<-cbind(scale(data[,toscale]),data[,noscale])


lmeds<-list(
  list(dep="m1",ind=list("x1","x2","x3",list("x1","x2"),list("x1","x3"),list("x1","x2","x3")))
)
moderators<-list("x1")
full<-list(dep="y",ind=c("m1", "x1","x2","x3","m1:x1"))

(infos<-smartMediation$new(lmeds,full,moderators))


lmeds<-list(
  list(dep="m1",ind=c("m2", "x1")),
  list(dep="m2",ind=c("x1"))
)

full<-list(dep="y",ind=NULL)

med<-c("m1","m2")
dep<-c("y")
(infos<-smartMediation$new(lmeds,full))
infos$ieffects

infos$mediators
.indirect_effects(infos$M,infos$mediators,infos$dep)

infos$ieffects







aa<-.next[1,]
class(aa)
.names<-colnames(M)
toy<-.names[names(M[n,]) %in% med]
a<-cbind("y",toy)
for(x in a[,2])
  print(M[rownames(M)==x,])

infos$ieffects

infos$fullmodel

tab<-jmf.medialtionTable(infos,data = data)
tab<-tab[tab$op=="~" & tab$model=="med",]

tab
a<-"1....ciao"
rr<-strsplit(a,"....",fixed = T)[[1]]
rr[[2]]

library(jamm)
library(foreign)


data<-as.data.frame(read.spss("../jamm_docs/data/muller_mediation.sav"))
head(data)
write.csv(data,file = "data/coopmedmod.csv",row.names = F)
data<-read.csv(file = "data/coopmedmod.csv")
data("coopmedmod")
data<-coopmedmod
data$prime<-factor(data$prime)
ff<-  list(BEH~prime,EXP~prime,BEH~prime+EXP)
lm(BEH~prime,data=data)


class(data$cprime)
names(data)
ff<-  list(BEH~prime,EXP~prime,BEH~prime+EXP)
jamm::jammGLM(
  data = data,
  dep = BEH,
  mediators = SVO,
  factors = prime,
  mediatorsTerms = "prime",
  modelTerms = c("SVO","prime"))

jamm::jammGLM(
  data = data,
  dep = BEH,
  mediators = SVO,
  covs = EXP,
  mediatorsTerms = "EXP",
  modelTerms = c("SVO","EXP"))

jamm::jammGLM(
  formula = list( EXP ~ SVO,
                  BEH ~ EXP + SVO ),
  data = data,
  tableOptions = c("beta", "component", "regression"))


ff<-  list(EXP~prime+SVO+prime:SVO,
           BEH~prime+SVO+EXP+EXP:SVO+SVO:prime)
b<-jamm::jammGLM(
  formula = ff,
  data = data,
  moderatorsTerms = list(EXP="SVO"))
b

alist<-list(NULL,NULL)
alist
all(sapply(alist,is.null))
a<-jamm::jammGLM(
  formula = ff,
  data = data,
  moderatorsTerms = list(list("SVO"),list()))

sf<-lapply(ff,expand.formula)

f<-"y~x"
class(tryCatch(as.formula(f),
         error=function(e) FALSE
))

cbind(1:10,20:30)


p1=.5
e1=100
(u1<-(1-p1)*e1-e1*p1)
p2=.1
e2=50

c1<-c(rep(1,90),rep(-1,10))
c2<-c(rep(1,90),rep(-1,10))

dd<-expand.grid(c1,c2)
table(dd$w)
dd$w<-dd$Var1*e2+dd$Var2*e2
qq<-prop.table(table(dd$w))
apply(dd,2,mean)
qq*c(-100,0,100)
2*e2*(1-p2)*(1-p2)-2*e2*p2*p2

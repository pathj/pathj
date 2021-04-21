j_DEBUG=T
j_INFO=T


NOTES<-list()

NOTES[["ci"]]<-list("standard"="Standard (Delta method)",
                    "bca"="Bias corrected bootstrap",
                    "perc"="Bootstrap percentiles",
                    "norm"="Parametric bootstrap")


CONT_EXAMPLES<-list()
CONT_EXAMPLES[[1]]<-list("info"="Constraints",example="",com="")
CONT_EXAMPLES[[2]]<-list("info"="Equality constraint",example="p1==p2",com="Constrain the estimates of p1 and p2 to be equal")
CONT_EXAMPLES[[3]]<-list("info"="Linear constraint",example="p1+p2=2",com="Constrain the estimates of p1 and p2 to be equal to 2")
CONT_EXAMPLES[[4]]<-list("info"="Linear constraint",example="p1+p2+p3=2", com="Constrain the estimates for p1,p2, and p3")
CONT_EXAMPLES[[5]]<-list("info"="Linear constraint",example="p1+2*p2=0", com="Constrain the estimates of p1 plus twice p2 to be equal to 2")
CONT_EXAMPLES[[6]]<-list("info"="Non linear constraint",example="p1*p2=0", com="Constrain the estimates such that p1*p2 equals 0")

DP_EXAMPLES<-list()
DP_EXAMPLES[[1]]<-list("info"="Defined Parameters",example="",com="")
DP_EXAMPLES[[2]]<-list("info"="Linear estimates",example="p1+p2",com="p1 and p2 are free, and their sum is estimated and tested")
DP_EXAMPLES[[3]]<-list("info"="Linear estimates",example="(p1+p2)-p3",com="p1,p2, and p3 are free, and the specified function is estimated and tested")
DP_EXAMPLES[[4]]<-list("info"="Non linear estimates",example="p1*p2",com="p1 and p2 are free, and their product is estimated and tested")
DP_EXAMPLES[[5]]<-list("info"="Non linear estimates",example="ab:=p1*p2", com="Estimate and test the product p1*p2 and name it `ab`")
DP_EXAMPLES[[6]]<-list("info"="Non linear estimates",example="a2:=p1^2", com="Estimate and test the square of p1 and name it `a2`")


CONT_NOTE<-"All the parameters labels are in the form `pN`, where `N` is a number. 
The parameter labels can be found in the results tables. Please be sure to have the options `Show parameters labels` selected."
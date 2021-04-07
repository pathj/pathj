.indirect_effects<-function(M,mediators,dep) {
    res<-NULL
    M0<-M
    n<-dim(M0)[1]
    nj<-n+1
    target<-rownames(M0)[n]
    preds<-colnames(M0)[M0[n,]=="P"]
    res<-merge(target,preds)
    names(res)<-c("p0","last")
    j<-0
    while (n>2) {
        M0<-M0[-n,]
        n<-dim(M0)[1]
        j<-ntot-n
        target<-rownames(M0)[n]
        preds<-colnames(M0)[M0[n,]=="P"]
        res0<-merge(target,preds)
        names(res0)<-c("last",paste0("p",j))
        res<-merge(res,res0,by="last",all = T)
    }
    res<-res[c(2,1,3:dim(res)[2])]
    wie<-sapply(1:nrow(res), function(r){
          test1<- !all(!(med %in% unlist(res[r,])))
          test2<- !all(!(dep %in% unlist(res[r,])))
          all(test1,test2)
      })
    res<-res[wie,]
    ieffects<-lapply(1:nrow(res), function(r){
            paste(as.matrix(rev(res[r,!is.na(res[r,])])))
       })
    ieffects
}
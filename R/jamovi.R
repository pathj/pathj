tob64<- function(x,...) UseMethod(".tob64")

.tob64.default<-function(obj,ref=NULL) {
  
  if (is.null(ref)) {
    obj<-jmvcore::toB64(obj)
  } else {
    for (r in ref)
      obj<-gsub(r,jmvcore::toB64(r),obj,fixed = T)
  }
  obj
}

.tob64.list<-function(obj,ref=NULL) {
  lapply(obj,bogustob64,ref)
}

bogustob64<-function(obj,ref) tob64(obj,ref)

fromb64<- function(x,...) UseMethod(".fromb64")

.fromb64.default<-function(obj,ref=NULL) {
  if (is.null(ref)) {
    obj<-jmvcore::fromB64(obj)
  } else {
    for (r in ref)
      obj<-gsub(jmvcore::toB64(r),r,obj,fixed = T)
  }
  obj<-gsub(INTERACTION_SYMBOL,":",obj,fixed = T)
  obj<-gsub(FACTOR_SYMBOL,"",obj,fixed = T)
  obj
}

.fromb64.list<-function(obj,ref) {
  lapply(obj,bogusfromb64,ref)
}

bogusfromb64<-function(obj,ref) fromb64(obj,ref)



######### tables #########


j.init_table<-function(table,obj,ci=FALSE,ciroot="",ciformat="{}% Confidence Intervals",ciwidth,indent=NULL) {

  square<-length(dim(obj))>1
  if (ci) {
    l<-paste0(ciroot,"ci.lower")
    u<-paste0(ciroot,"ci.upper")
    table$getColumn(l)$setSuperTitle(jmvcore::format(ciformat, ciwidth))
    table$getColumn(u)$setSuperTitle(jmvcore::format(ciformat, ciwidth))
  }
  if (square)
     for (i in seq_len(nrow(obj))) {
         table$addRow(rowKey=i,obj[i,])
     }
  else
    for (i in seq_along(obj)) 
      table$addRow(rowKey=i,obj[[i]])

  if (!is.null(indent)) {
    trows<-1:i
    rows<-trows[indent]
    for (j in rows)
      table$addFormat(rowKey=j,col=1,jmvcore::Cell.INDENTED)
  }

}
j.init_table_append<-function(table,obj, indent=NULL) {
  
  last<-table$rowCount
  for (i in seq_along(obj)) 
    table$addRow(rowKey=last+i,obj[[i]])
  
  if (!is.null(indent)) {
    trows<-1:i
    rows<-trows[indent]
    for (j in rows)
      table$addFormat(rowKey=last+j,col=1,jmvcore::Cell.INDENTED)
  }
  
}
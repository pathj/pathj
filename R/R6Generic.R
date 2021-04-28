Dispatch <- R6::R6Class(
  "Dispatch",
  class=FALSE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
         warnings=list(),
         errors=list(),
         vars=NULL,
         initialize=function(vars) {
           self$vars<-vars
         }
         
  ),
  private = list(
    
    .warnings=function(warn) {
      
      if (is.null(warn))
        return()
      if (warn==FALSE)
        return()
      
      warn<-fromb64(warn,self$vars)
      self$warnings[[length(self$warnings)+1]]<-warn
      
    },
    .errors=function(err) {
      if (err==FALSE)
        return()
      mark(err)
      mark(self$vars)
      err<-fromb64(err,self$vars)
      mark(err)
      
      self$errors[[length(self$errors)+1]]<-err
    }
    
  )
)
    
    
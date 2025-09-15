## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                              run= function(data) {
                                jinfo("MODULE Pathj: run Runner")
                             ## prepare the options based on Syntax definitions
                                lavoptions<-list(model = private$.lav_structure, 
                                             data = data,
                                             se=self$options$se,
                                             bootstrap=self$options$bootN,
                                             estimator=self$options$estimator
                                 )
                               if (is.something(self$multigroup)) {
                                    lavoptions[["group"]]<-self$multigroup$var64
                                    lavoptions[["group.label"]]<-self$multigroup$levels
                                }
                                if (self$options$estimator=="ML") {
                                  lavoptions[["likelihood"]]<-self$options$likelihood
                                 }
                               jinfo("estimating the model...")
                            ## estimate the models
                               results<-try_hard({do.call(lavaan::lavaan,lavoptions)  })
                               jinfo("done")
                               self$warning <- list(topic="main",message=results$warning)
                               self$error  <- list(topic="main",message=results$error)
                               self$model <- results$obj
                            
                              },
                             run_info = function() {
                               
                                # fit indices
                                  alist<-list()
                                  ladd(alist)     <- c(info="Estimation Method",value=self$model@Options$estimator)
                                  ladd(alist)     <- c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                                  ladd(alist)     <- c(info="Free parameters",value=self$model@Fit@npar)
                                  ladd(alist)     <- c(info="Converged",value=self$model@Fit@converged) 
                                  ladd(alist)     <- c(info="",value="")
                                  try(ladd(alist) <- c(info="Loglikelihood user model",value=round(ff[["logl"]],digits=3) ))
                                  try(ladd(alist) <- c(info="Loglikelihood unrestricted model",value=round(ff[["unrestricted.logl"]],digits=3)))
                                  ladd(alist)      <-c(info="",value="")
                                  return(alist)

                               
                               
                             }

                          ), # end of public function estimate

                        private=list(
                          # do private stuff
                          
                        ) #end of private
)  # end of class



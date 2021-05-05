Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        list(
                          model=NULL,
                          ciwidth=NULL,
                          parameters=NULL,
                          coefficients=NULL,
                          correlations=NULL,
                          fit=NULL,
                          fitindices=NULL,
                          constfit=NULL,
                          info=NULL,
                          r2=NULL,
                          initialize=function(options=options,data=data) {
                            super$initialize(
                              options=options,
                              data=data)
                            self$ciwidth<-options$ciWidth/100
                          },
                          estimate=function(data) {
                            
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
                            
                            
                            
                            results<-try_hard({do.call(lavaan::lavaan,lavoptions)  })
                            
                            
                            #results<-try_hard({do.call(lavaan::lavaan,lavoptions)})
                            
                            
                            self$warnings<-list(topic="main",message=results$warning)
                            self$errors<-results$error
                            
                            if (is.something(self$errors))
                              return(self$errors)
                            self$model<-results$obj
                            
                            self$parameters<-lavaan::parameterestimates(
                              self$model,
                              ci=self$options$ci,
                              level = self$ciwidth,
                              standardized = T,
                              boot.ci.type = self$options$bootci
                            )
                            self$parameters$rhs<-fromb64(self$parameters$rhs,self$vars)
                            self$parameters$lhs<-fromb64(self$parameters$lhs,self$vars)
                            self$parameters$free<-(self$structure$free>0)
                            
                            self$parameters$endo<-FALSE
                            self$parameters$endo[self$structure$lhs %in% self$options$endogenous | self$structure$rhs %in% self$options$endogenous]<-TRUE
                            self$coefficients<-self$parameters[self$parameters$op=="~",]
                            self$correlations<-self$parameters[self$parameters$op=="~~",]
                            self$correlations$type<-ifelse(self$correlations$endo,"Residuals","Variables")
                            self$definedParameters<-self$parameters[self$parameters$op==":=",]
                            if (nrow(self$definedParameters)==0) self$definedParameters<-NULL
                            tab<-self$correlations
                            end<-tab[tab$lhs %in% self$options$endogenous & tab$lhs==tab$rhs,]
                            self$computeR2(end)

                            
                            
                            self$intercepts<-self$parameters[self$parameters$op=="~1",]
                            if (nrow(self$intercepts)==0) self$intercepts<-NULL
                            
                            
                            #### fit tests ###
                            alist<-list()
                            ff<-lavaan::fitmeasures(self$model)
                            alist<-list()
                            if (ff[["df"]]>0)
                              alist[[1]]<-list(label="User Model",chisq=ff[["chisq"]],df=ff[["df"]],pvalue=ff[["pvalue"]])
                            try(alist[[length(alist)+1]]<-list(label="Baseline Model",chisq=ff[["baseline.chisq"]],df=ff[["baseline.df"]],pvalue=ff[["baseline.pvalue"]]))
                            
                            self$fitindices<-as.list(ff)
                            
                            self$fit<-alist
                            
                            # fit indices
                            ff<-sapply(ff, round,3)
                            alist<-list()
                            alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist)+1]]<-c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist)+1]]<-c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist)+1]]<-c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist)+1]]<-c(info="",value="")
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value=ff[["logl"]]) )
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value=ff[["unrestricted.logl"]]))
                            alist[[length(alist)+1]]<-c(info="",value="")
                            
                            self$info<-alist
                            if (is.something(self$constraints)) {
                              check<-sapply(self$constraints,function(con) length(grep("<|>",con$value))>0,simplify = T)
                              if (any(check)) {
                                self$warnings<-list(topic="main",message=WARNS[["scoreineq"]])
                              } else {
                                tab<-lavaan::lavTestScore(self$model,
                                                          univariate = self$options$scoretest,
                                                          cumulative = self$options$cumscoretest)
                                
                                if (self$options$scoretest) {
                                  names(tab$uni)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  self$constfit<-tab$uni
                                  self$constfit$type="Univariate"
                                }
                                if (self$options$cumscoretest) {
                                  names(tab$cumulative)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  tab$cumulative$type<-"Cumulative"
                                  self$constfit<-rbind(self$constfit,tab$cumulative)
                                }
                                self$fit[[length(self$fit)+1]]<-list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value)
                                
                                
                              }
                            } # end of checking constraints
                            
                          }, # end of private function estimate
                          
                          computeR2=function(end) {
                            
                            end$var<-end$est/end$std.all
                            upper<-end$ci.upper
                            lower<-end$ci.lower
                            end$ci.upper<-1-(lower/end$var)
                            end$ci.lower<-1-(upper/end$var)
                            end$r2<-1-end$std.all
                            
                            if (self$options$r2ci=="fisher") {
                              ### https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3821705/
                              N<-lavaan::lavInspect(self$model,"ntotal")
                              r<-sqrt(end$r2)
                              f<-.5 * log((1 + r)/(1 - r))
                              zr<-f*sqrt((N-3))
                              z0<-qnorm((1-self$ciwidth)/2,lower.tail = F)
                              
                              lower<-zr-z0
                              upper<-zr+z0
                              flower<-lower/sqrt(N-3)
                              fupper<-upper/sqrt(N-3)
                              rupper<-(exp(2*fupper)-1)/(1+exp(2*fupper))
                              rupper<-rupper^2
                              rlower<-(exp(2*flower)-1)/(1+exp(2*flower))
                              rlower<-rlower^2
                              end$ci.upper<-rupper
                              end$ci.lower<-rlower
                              ####
                            }
                            
                                    end$chisq<-0
                                    end$df<-0
                                    end$pvalue<-0
                                    for (i in seq_len(nrow(end))) {
                                          labels<-self$structure[self$structure$lhs==end$lhs[i] & self$structure$op=="~" ,"label"]
                                          const<-paste(labels,0,sep="==",collapse = " ; ")
                                          results<-try_hard({tests<-lavaan::lavTestWald(self$model,const)})
                                          if (results$error!=FALSE) {
                                                  self$warnings<-list(topic="r2",message="Some inferential tests cannot be computed for this model")
                                                  end$chisq[i]<-NaN   
                                                  end$df[i]<-NaN
                                                  end$pvalue[i]<-NaN
                                            
                                          } else {
                                                end$chisq[i]<-tests$stat   
                                                end$df[i]<-tests$df
                                                end$pvalue[i]<-tests$p.value
                                          }
                                        }
                            self$r2<-end
                            
                          } ## end of r2
                            
                        ) # end of private
)  # end of class


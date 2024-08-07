## This class takes care of estimating the model and return the results. It inherit from Syntax, and define the same tables
## defined by Syntax, but it fill them with the results.

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        list(
                          model=NULL,
                          tab_fit=NULL,
                          tab_fitindices=NULL,
                          ciwidth=NULL,
                          tab_constfit=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(
                              options=options,
                              datamatic=datamatic)
                            self$ciwidth<-options$ciWidth/100
                          },
                          estimate=function(data) {
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
                            ginfo("estimating the model...")
                            ## estimate the models
                            results<-try_hard({do.call(lavaan::lavaan,lavoptions)  })
                            ginfo("done")
                            
                            

                            self$warnings<-list(topic="main",message=results$warning)
                            self$errors<-results$error
                            
                            if (is.something(self$errors))
                                return(self$errors)
                            
                            ## ask for the paramters estimates
                            self$model<-results$obj
                            .lav_params<-lavaan::parameterestimates(
                              self$model,
                              ci=self$options$ci,
                              standardized = T,
                              level = self$ciwidth,
                              boot.ci.type = self$options$bootci
                            )

                                                      
                            ## we need some info initialized by Syntax regarding the parameters properties
                            .lav_structure<-private$.lav_structure
                             sel<-grep("==|<|>",.lav_structure$op,invert = T)
                            .lav_structure<-.lav_structure[sel,]
                            ## make some change to render the results
                            
                            .lav_params$rhs<-fromb64(.lav_params$rhs,self$vars)
                            .lav_params$lhs<-fromb64(.lav_params$lhs,self$vars)
                            .lav_params$free<-(.lav_structure$free>0)
                            
                            .lav_params$endo<-FALSE
                            .lav_params$endo[.lav_params$lhs %in% self$options$endogenous | .lav_params$rhs %in% self$options$endogenous]<-TRUE
                            ## collect regression coefficient table
                            self$tab_coefficients<-.lav_params[.lav_params$op=="~",]

                            ## collect variances and covariances table
                            self$tab_covariances<-.lav_params[.lav_params$op=="~~",]
                            self$tab_covariances$type<-ifelse(self$tab_covariances$endo,"Residuals","Variables")
                            
                            ## collect defined parameters table
                            self$tab_defined<-.lav_params[.lav_params$op==":=",]
                            if (nrow(self$tab_defined)==0) self$tab_defined<-NULL
                            
                            # prepare and comput R2 and collect a table for them
                            tab<-self$tab_covariances
                            end<-tab[tab$lhs %in% self$options$endogenous & tab$lhs==tab$rhs,]
                            self$computeR2(end)

                            
                            ### collect intercepts
                            self$tab_intercepts<-.lav_params[.lav_params$op=="~1",]
                            if (nrow(self$tab_intercepts)==0) self$tab_intercepts<-NULL
                            
                            
                            #### fit tests ###
                            alist<-list()
                            results<-try_hard(lavaan::fitmeasures(self$model))
                     
                            if (is.something(results$obj)) {
                                    ff<-results$obj
                                    alist<-list()
                                    if (ff[["df"]]>0)
                                        alist[[1]]<-list(label="User Model",
                                               chisq=ff[["chisq"]],
                                               df=ff[["df"]],
                                               pvalue=ff[["pvalue"]],
                                               ..space..=1)
                                    
                                    self$tab_fitindices<-as.list(ff)
                                    
                                    if (is.something(self$options$tests)) {
                                      
                                      tests<-lavaan::lavTest(self$model,test = unlist(self$options$tests))
                                      ## if only an additional test is required, lavaan produces a list
                                      ## of properties of one test, not a list of tests
                                      if (length(tests)>5) tests<-list(tests)

                                      for (i in seq_along(tests)) {
                                        t<-tests[[i]]
                                        
                                        label  <-  strsplit(t$test,".",fixed = T)[[1]]
                                        label<-paste(lapply(label,function(x) stringr::str_to_title(x)),collapse = "-")
                                        chisq  <-  t$stat
                                        df     <-  t$df
                                        p      <-  t$pvalue
                                        alist[[length(alist)+1]]<-list(label=label,chisq=chisq,df=df,pvalue=p)
                                      }
                                      
                                    }

                                    try(alist[[length(alist)+1]]<-list(
                                      label="Baseline Model",
                                      chisq=ff[["baseline.chisq"]],
                                      df=ff[["baseline.df"]],
                                      pvalue=ff[["baseline.pvalue"]],
                                      ..space..=1))
                                    
                            
                                    if (is.something(self$multigroup)) {
                                       alist[[length(alist)+1]]<-list(label="Groups statistics",chisq="",df="",pvalue="",..space..=2)
                                       multitests<-self$multitest()
                                       for (r in seq_along(multitests))
                                            alist[[length(alist)+1]]<-multitests[[r]]
                                    }
                                    self$tab_fit<-alist
                                    
                            } else {
                              self$warnings<-list(topic="tab_fitindices",message=results$warning)
                              self$warnings<-list(topic="tab_fitindices",message=results$error)
                            }
                            
                            
                            # fit indices
                            alist<-list()
                            alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist)+1]]<-c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist)+1]]<-c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist)+1]]<-c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist)+1]]<-c(info="",value="")
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value=round(ff[["logl"]],digits=3) ))
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value=round(ff[["unrestricted.logl"]],digits=3)))
                            alist[[length(alist)+1]]<-c(info="",value="")
                            
                            self$tab_info<-alist

                            if (length(grep("==|<|>",private$.lav_structure$op))) {
                              check<-sapply(self$constraints,function(con) length(grep("<|>",con$value))>0,simplify = T)
                              if (any(check)) {
                                self$warnings<-list(topic="main",message=WARNS[["scoreineq"]])
                              } else {
                                tab<-lavaan::lavTestScore(self$model,
                                                          univariate = self$options$scoretest,
                                                          cumulative = self$options$cumscoretest)
                                
                                if (self$options$scoretest) {
                                  names(tab$uni)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  self$tab_constfit<-tab$uni
                                  self$tab_constfit$type="Univariate"
                                }
                                if (self$options$cumscoretest) {
                                  names(tab$cumulative)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  tab$cumulative$type<-"Cumulative"
                                  self$tab_constfit<-rbind(self$tab_constfit,tab$cumulative)
                                }
                                
                                self$tab_constfit$lhs<-gsub(".","",self$tab_constfit$lhs,fixed = T)
                                self$tab_constfit$rhs<-gsub(".","",self$tab_constfit$rhs,fixed = T)
                                
                                self$tab_fit[[length(self$tab_fit)+1]]<-list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value,..space..=3)
                                
                                
                              }
                            } # end of checking constraints
                            
                            ginfo("Estimation is done...")
                          }, # end of private function estimate
                          
                          multitest=function() {
                            
                            tab<-self$structure
                            gstat<-self$model@test$standard$stat.group
                            ### compute df for each group ###
                            sel<-grep("==|<|>",private$.lav_structure$op)
                            con<-private$.lav_structure[sel,]
                            con$lhs<-gsub(".","",con$lhs,fixed=T)
                            con$rhs<-gsub(".","",con$rhs,fixed=T)
                            fixed<-unique(c(con$lhs,con$rhs))
                            g<-tab[tab$label %in% fixed,"group"]
                            df<-table(g)
                            groups<-1:self$multigroup$nlevels
                            dfs<-sapply(groups, function(g) ifelse(hasName(df,g),df[[as.character(g)]],0)) 
                            lapply(groups, function(g) {
                              df<-dfs[g]
                              if (df>0) {
                                chisq=gstat[g]
                                pvalue=pchisq(gstat[g], df=dfs[g], lower.tail=FALSE)
                              } else {
                                chisq=0
                                pvalue=1
                              }
                              list(
                                  label=paste("Group",g),
                                  chisq=chisq,
                                  df=df,
                                  pvalue=pvalue,
                                  ..space..=2
                              )
                            })
                                                        
                          },
                          
                          computeR2=function(end) {
                            
                            end$var<-end$est/end$std.all
                            upper<-end$ci.upper
                            lower<-end$ci.lower
                            end$ci.upper<-1-(lower/end$var)
                            end$ci.lower<-1-(upper/end$var)
                            end$r2<-1-end$std.all
                             for (i in seq_along(end$r2)) 
                                    if (end$r2[[i]]>1 | end$r2[[i]]<0)  {
                                      end$r2[[i]]<-NA
                                      self$warnings<-list(topic="r2",message="Some R-square index cannot be computed for this model")
                                    }
                            
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
                            self$tab_r2<-end
                            if (!self$options$r2test)
                              return()
                            
                                    end$chisq<-0
                                    end$df<-0
                                    end$pvalue<-0
                                    if (!("group" %in% names(end)))
                                               end$group<-1
                                    
                                    .lav_structure<-private$.lav_structure

                                    sel<-(self$structure$lhs %in% unique(end$lhs) & self$structure$op=="~" & self$structure$group>0)
                                    .structure<-self$structure[sel,]
                                      for (i in seq_len(nrow(end))) {
                                            if (is.na(end$r2[i]))  {
                                              end$chisq[i]<-NaN   
                                              end$df[i]<-NaN
                                              end$pvalue[i]<-NaN
                                              next()
                                            }
                                            
                                            sel<-(.structure$lhs==end$lhs[i] &  .structure$group==end$group[i])
                                           ..structure<-.structure[sel,]
                                            const<-paste(..structure$label,0,sep="==",collapse = " ; ")
                                            results<-try_hard({tests<-lavaan::lavTestWald(self$model,const)})
                                            if (results$error!=FALSE) {
                                                  self$warnings<-list(topic="r2",message="Some inferential tests cannot be computed for this model")
                                                  self$warnings<-list(topic="r2",message=results$error)
                                                  end$chisq[i]<-NaN   
                                                  end$df[i]<-NaN
                                                  end$pvalue[i]<-NaN
                                            
                                            } else {
                                                end$chisq[i]<-tests$stat   
                                                end$df[i]<-tests$df
                                                end$pvalue[i]<-tests$p.value
                                          }
                                    }
                            self$tab_r2<-end
                            
                        } ## end of r2
                        

              ) # end of private
)  # end of class


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
                          tab_mi=NULL,
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
                            
                            # modification indices (diagnostics)
                            if (isTRUE(self$options$modindices)) {
                              mires <- try_hard({ lavaan::modindices(self$model) })
                              if (isFALSE(mires$error)) {
                                mi <- mires$obj
                                if (nrow(mi)==0) {
                                  self$warnings<-list(topic="modindices",message="No fixed parameter available to compute modification indexes.")
                                  mi[1,1]<-"-"
                                  self$tab_mi<-mi
                                  return()
                                }
                                # threshold filter
                                if (is.something(self$options$miMin))
                                  mi <- mi[!is.na(mi$mi) & mi$mi >= self$options$miMin, , drop=FALSE]
                                # add group label if multigroup
                                if (is.something(self$multigroup)) {
                                  mi$lgroup <- self$multigroup$levels[mi$group]
                                } else {
                                  mi$lgroup <- "1"
                                }
                                # decode names
                                mi$lhs <- fromb64(mi$lhs, self$vars)
                                mi$rhs <- fromb64(mi$rhs, self$vars)
                                # keep relevant columns if present
                                keep <- c("lgroup","lhs","op","rhs","mi","epc","sepc.all")
                                cols <- intersect(keep, names(mi))
                                mi <- mi[, cols, drop=FALSE]
                                # order by MI descending
                                if ("mi" %in% names(mi))
                                  mi <- mi[order(-mi$mi), , drop=FALSE]
                                self$tab_mi <- mi
                              } else {
                                self$warnings <- list(topic="modindices", message = mires$warning)
                                self$warnings <- list(topic="modindices", message = mires$error)
                              }
                            }

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

                            # Guard: if no rows, nothing to compute
                            if (is.null(end) || nrow(end) == 0) {
                              self$tab_r2 <- end
                              return()
                            }

                            # Compute variance from lavaan outputs; avoid division by zero/NA
                            end$var <- NA_real_
                            ok_std <- !is.na(end$std.all) & end$std.all != 0
                            end$var[ok_std] <- end$est[ok_std] / end$std.all[ok_std]

                            # Preserve original CI bounds from lavaan for fallback
                            lav_upper <- end$ci.upper
                            lav_lower <- end$ci.lower

                            # Transform variance CI to R2 CI when possible
                            tr_ok <- ok_std & !is.na(lav_upper) & !is.na(lav_lower) & !is.na(end$var)
                            if (any(tr_ok)) {
                              end$ci.upper[tr_ok] <- 1 - (lav_lower[tr_ok] / end$var[tr_ok])
                              end$ci.lower[tr_ok] <- 1 - (lav_upper[tr_ok] / end$var[tr_ok])
                            }

                            # Compute R2 = 1 - std.all; invalid outside [0,1]
                            end$r2 <- 1 - end$std.all
                            bad_r2 <- is.na(end$r2) | end$r2 < 0 | end$r2 > 1
                            if (any(bad_r2)) {
                              end$r2[bad_r2] <- NA_real_
                              self$warnings <- list(topic = "r2", message = "Some R-square index cannot be computed for this model")
                            }

                            # Optional Fisher CI for R2
                            if (self$options$r2ci == "fisher") {
                              # Reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3821705/
                              N <- tryCatch(lavaan::lavInspect(self$model, "ntotal"), error = function(e) NA_integer_)
                              if (!is.na(N) && N > 3) {
                                r <- sqrt(end$r2)
                                # clamp r within (eps, 1-eps) to avoid infinities in atanh
                                eps <- 1e-12
                                r_clamped <- pmin(pmax(r, eps), 1 - eps)
                                # rows where we can compute Fisher CI
                                f_ok <- !is.na(r_clamped)
                                if (any(f_ok)) {
                                  f <- 0.5 * log((1 + r_clamped[f_ok]) / (1 - r_clamped[f_ok]))
                                  zr <- f * sqrt(N - 3)
                                  z0 <- qnorm((1 - self$ciwidth) / 2, lower.tail = FALSE)

                                  lower <- zr - z0
                                  upper <- zr + z0
                                  flower <- lower / sqrt(N - 3)
                                  fupper <- upper / sqrt(N - 3)
                                  rupper <- (exp(2 * fupper) - 1) / (1 + exp(2 * fupper))
                                  rlower <- (exp(2 * flower) - 1) / (1 + exp(2 * flower))
                                  # square to get R2 bounds
                                  end$ci.upper[f_ok] <- rupper^2
                                  end$ci.lower[f_ok] <- rlower^2
                                }
                              } else {
                                # Not enough N to compute Fisher CI; leave previous bounds
                                self$warnings <- list(topic = "r2", message = "Sample size too small for Fisher CI for R-square")
                              }
                            }

                            self$tab_r2 <- end
                            
                        } ## end of r2                        

              ) # end of private
)  # end of class



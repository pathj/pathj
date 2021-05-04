Syntax <- R6::R6Class(
         "Syntax",
          class=FALSE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              endogenous=NULL,
              lav_terms=NULL,
              lav_structure=NULL,
              structure=NULL,
              options=NULL,
              constraints=NULL,
              userestimates=NULL,
              hasInteractions=FALSE,
              interactions=list(),
              factorinfo=NULL,
              contrasts_names=NULL,
              multigroup=NULL,
              initialize=function(options,datamatic) {
                super$initialize(options=options,vars=unlist(c(options$endogenous,options$factors,options$covs)))
                self$contrasts_names<-datamatic$contrasts_names
                factorinfo<-sapply(self$options$factors,function(f) length(datamatic$factors_levels[[f]])-1 )
                self$factorinfo<-factorinfo
                self$lav_terms<-lapply(self$options$endogenousTerms, function(alist) private$.factorlist(alist,factorinfo))
                names(factorinfo)<-tob64(names(factorinfo))
                private$.lav_terms<-lapply(tob64(self$options$endogenousTerms), function(alist) private$.factorlist(alist,factorinfo))
                private$.check_models()
                private$.check_interactions()
                private$.check_constraints()
                private$.check_varcov()

                self$multigroup=datamatic$multigroup
                
                
                lavoptions<-list(
                  model=private$.lavaan_syntax(),
                  int.ov.free = TRUE, 
                  auto.var = TRUE,
                  auto.th = TRUE, 
                  auto.cov.y = self$options$cov_y,
                  fixed.x=!self$options$cov_x,
                  meanstructure = TRUE
                )
                if (is.something(self$multigroup))
                   lavoptions[["ngroups"]]<-self$multigroup$nlevels

                results<-try_hard({
                    do.call(lavaan::lavaanify, lavoptions)
                  })
                private$.lav_structure<-results$obj
                self$warnings<-list(topic="main",message=results$warning)
                self$errors<-results$error
                
                if (is.something(self$errors))
                     stop(paste(self$errors,collapse = "\n"))
                

                 private$.lav_structure$label<-gsub(".","",private$.lav_structure$plabel,fixed=T)
                .lav_structure<-private$.lav_structure
                .lav_structure$user<-ifelse(.lav_structure$exo==1,"Sample","Estim")
                .lav_structure$lhs<-fromb64(.lav_structure$lhs,self$vars)
                .lav_structure$rhs<-fromb64(.lav_structure$rhs,self$vars)
                if (is.something(self$multigroup)) {
                     levs<-c(self$multigroup$levels,"All")
                     .lav_structure$group<-ifelse(.lav_structure$group==0,length(levs)+1,.lav_structure$group)
                    .lav_structure$lgroup<-levs[.lav_structure$group]
                } else
                  .lav_structure$lgroup<-"1"
                 self$structure<-.lav_structure[.lav_structure$op!="==",]
                 ### this is weired, but it works fine with multigroups
                 r2test<-((.lav_structure$op=="~~") & (.lav_structure$lhs %in% self$options$endogenous) & (.lav_structure$lhs==.lav_structure$rhs))
                 self$r2<-.lav_structure[r2test,c("lhs","lgroup")]

                }, # here initialize ends
               models=function() {
                  lapply(seq_along(self$options$endogenousTerms), 
                       function(i) list(info="Model",
                                        value=as.character(jmvcore::constructFormula(dep=self$options$endogenous[i],
                                                                                     self$options$endogenousTerms[[i]])))
                )
              }
              
          ),   # End public
          active=list(
           warnings=function(obj) {
             
             if (missing(obj))
               return(private$.warnings)
             if (is.null(obj))
               return()
             if (is.null(obj$message))
                 return()
             check<-length(grep("fixed.x=FALSE",obj$message,fixed = T)>0) 
             if (check) 
               obj$message<-WARNS[["usercov"]]
               
             super$warnings<-obj
           },
           errors=function(obj) {
             
             if (missing(obj))
               return(private$.errors)
             if (is.null(obj))
               return()
             check<-length(grep("infinite or missing",obj,fixed = T)>0) 
             if (check) 
               obj<-ERRS[["noluck"]]
             
             super$errors<-obj
           }
           
          ),
          private=list(
            .lav_terms=NULL,
            .lav_structure=NULL,
            .constraints=NULL,
            .userestimates=NULL,
            .lav_models=NULL,
            .models=NULL,
            .check_models=function() {
              
              terms<-private$.lav_terms
              endogenous<-tob64(self$options$endogenous)
              
              models<-lapply(seq_along(terms), function(i)
                jmvcore::constructFormula(dep=endogenous[i],terms[[i]]))
              
              private$.models<-lapply(models,function(m) {
                res<-gsub(":",INTERACTION_SYMBOL,m)
                if (res!=m) {
                  self$hasInteractions=TRUE
                  int<-strsplit(res,"+",fixed = T)[[1]]
                  ind<-grep(INTERACTION_SYMBOL,int,fixed = TRUE)
                  for (j in ind)
                    self$interactions[[length(self$interactions)+1]]<-int[j]
                }
                res
              })
            
            },
            
            .lavaan_syntax=function() {
              models<-private$.models
              f<-glue::glue_collapse(unlist(models),sep = " ; ")
              con<-paste(private$.constraints,collapse = " ; ")
              f<-paste(f,con,sep=" ; ")
              est<-paste(private$.userestimates,collapse = " ; ")
              f<-paste(f,est,sep=" ; ")
              f
            },
            
            .check_constraints=function() {
              
              consts<-self$options$constraints
              realconsts<-list()
              realestims<-list()
              for (con in consts) {
                check<-(length(grep("=~",con,fixed=T))>0)
                if (check) {
                      self$errors<-ERRS[["nolatent"]]
                      return()
                }
                check<-(length(grep("==",con,fixed=T))>0) 
                if (check)
                  realconsts[[length(realconsts)+1]]<-con
                else
                  realestims[[length(realestims)+1]]<-con
              }
              j<-0
              realestims<-lapply(seq_along(realestims), function(j) {
                estim<-realestims[[j]]
                estim<-trimws(estim)
                if (estim=="")
                     return("")
                check<-grep(":=|~",estim)
                if (length(check)==0 ) {
                     paste0("dp",j,":=",estim)
                }
                else
                  estim
              })
              
              self$constraints<-lapply(realconsts, function(x) list(info="Constraint",value=x))
              self$userestimates<-lapply(realestims, function(x) list(info="Defined parameter",value=x))

              for (i in seq_along(realconsts)) {
                     
                      for (term in self$interactions) {
                        realconsts[[i]]<-gsub(fromb64(term),term,realconsts[[i]],fixed=TRUE)
                      }
                     
                      realconsts[[i]]<-gsub(":",INTERACTION_SYMBOL,realconsts[[i]],fixed = T)
                
                
                     for (name in names(self$contrasts_names))
                       realconsts[[i]]<-gsub(name,self$contrasts_names[[name]],realconsts[[i]],fixed=TRUE)
              }
              
              for (i in seq_along(realestims)) {
                  estim<-realestims[[i]]
                  estim<-gsub(":="," $ ",estim,fixed = T)
                for (term in self$interactions) {
                  estim<-gsub(fromb64(term),term,estim)
                }
                estim<-gsub(":",INTERACTION_SYMBOL,estim)
                
                for (name in names(self$contrasts_names))
                  estim<-gsub(name,self$contrasts_names[[name]],estim,fixed=TRUE)
                estim<-gsub("$",":=",estim,fixed = T)
                realestims[[i]]<-estim
              }
              
              
              private$.constraints<-tob64(realconsts,self$vars)
              private$.userestimates<-tob64(realestims,self$vars)
 
            },
            .check_interactions=function() {

                            noscale<-unlist(sapply(self$options$scaling , function(a) if (a$type=="none") return(a$var)))
                            res<-lapply(seq_along(self$options$endogenousTerms), function(i) {
                                    sapply(self$options$endogenousTerms[[1]],function(term) {
                                          if (length(term)>1)
                                                return(intersect(term,noscale))
                                          else return(NULL)
                                    })
                            })
                            res<-unique(unlist(res))
                            if (is.something(res)) {
                                    w<-glue::glue(WARNS[["nocenterd"]],vars=paste(res,collapse = ", "))
                                   self$warnings<-list(topic="main",message=w)
                            }
                            

            },
            .check_varcov=function() {
              
              varcov64<-tob64(self$options$varcov)
              factorinfo64<-self$factorinfo
              names(factorinfo64)<-tob64(names(factorinfo64))
              varcov64<-private$.factorlist(varcov64,factorinfo64)
              res<-lapply(varcov64, function(vc) {
                if (!is.null(vc$i1) &  !is.null(vc$i2)) {
                       private$.userestimates[[length(private$.userestimates)+1]]<-paste(vc$i1,vc$i2,sep = "~~")
                }
              })

              
            },
            .factorlist=function(terms,factorslen) {
              .terms<-list()
              for (f in names(factorslen)) {
                for (term in terms) {
                  ind<-which(term==f)
                  for (i in ind) {
                    for (j in seq_len(factorslen[[f]])) {
                      .term<-term
                      .term[[i]]<-paste0(.term[[i]],FACTOR_SYMBOL,j)
                      .terms[[length(.terms)+1]]<-.term
                    }
                  }
                  if (length(ind)==0)
                    .terms[[length(.terms)+1]]<-term
                }
                terms<-.terms
              }

              terms  
            }
            
            
            
          ) # end of private
) # End Rclass

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
                    definedParameters=NULL,
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
                      self$correlations<-self$parameters[self$parameters$op=="~~",]
                      self$correlations$type<-ifelse(self$correlations$endo,"Residuals","Variables")
                      self$definedParameters<-self$parameters[self$parameters$op==":=",]
                      if (nrow(self$definedParameters)==0) self$definedParameters<-NULL
                      tab<-self$correlations
                      end<-tab[tab$lhs %in% self$options$endogenous & tab$lhs==tab$rhs,]
                      end$var<-end$est/end$std.all
                      upper<-end$ci.upper
                      lower<-end$ci.lower
                      end$ci.upper<-1-(lower/end$var)
                      end$ci.lower<-1-(upper/end$var)
                      end$r2<-1-end$std.all
                      self$r2<-end
                      
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
                      alist[[length(alist)+1]]<-c(info="Free parameters",value=self$model@Fit@npar)
                      alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$model@Options$estimator)
                      alist[[length(alist)+1]]<-c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                      alist[[length(alist)+1]]<-c(info="Converged",value=self$model@Fit@converged) 
                      alist[[length(alist)+1]]<-c(info="",value="")
                      try(alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value=ff[["logl"]]) )
                      try(alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value=ff[["unrestricted.logl"]]))
                      self$info<-alist
                      if (is.something(self$constraints)) {
                        tab<-lavaan::lavTestScore(self$model)
                        names(tab$uni)<-c("lhs","op","rhs","chisq","df","pvalue")
                        self$constfit<-tab$uni
                        
                        self$fit[[length(self$fit)+1]]<-list(label="Constraints Score Test",
                                                             chisq=tab$test$X2,
                                                             df=tab$test$df,
                                                             pvalue=tab$test$p.value)

                        
                      }
                    }
                  )
)
                    

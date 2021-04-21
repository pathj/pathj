Syntax <- R6::R6Class(
         "Syntax",
          public=list(
              endogenous=NULL,
              lavterms=NULL,
              lav_structure=NULL,
              structure=NULL,
              options=NULL,
              constraints=NULL,
              userestimates=NULL,
              initialize=function(options,data) {
                self$options=options
                private$.check_constraints(options$constraints)
                factorinfo<-sapply(self$options$factors,function(f) nlevels(data[[f]])-1 )
                self$lavterms<-lapply(self$options$endogenousTerms, function(alist) private$.factorlist(alist,factorinfo))
                withCallingHandlers({
                  self$lav_structure<-lavaan::lavaanify(self$lavaan_syntax(),
                                                        int.ov.free = TRUE, 
                                                        auto.var = TRUE,
                                                        auto.th = TRUE, 
                                                        auto.cov.y = self$options$cov_y,
                                                        fixed.x=!self$options$cov_x
                  )},
                  warning=function(w) self$warnings<-w$message,
                  error=function(w) self$errors<-w$message)
                
                self$lav_structure$label<-gsub(".","",self$lav_structure$plabel,fixed=T)
                self$structure<-self$lav_structure[self$lav_structure$op!="==",]
                
              }, # here initialize ends
              models=function() {
                lapply(seq_along(self$options$endogenousTerms), 
                       function(i) jmvcore::constructFormula(dep=self$options$endogenous[i],self$options$endogenousTerms[[i]]))
              },
              lavaan_models=function() {
                 lapply(seq_along(self$lavterms), function(i) jmvcore::constructFormula(dep=self$options$endogenous[i],self$lavterms[[i]]))
              },
              lavaan_syntax=function() {
                f<-glue::glue_collapse(unlist(self$lavaan_models()),sep = " ; ")
                con<-paste(self$constraints,collapse = " ; ")
                f<-paste(f,con,sep=" ; ")
                est<-paste(self$userestimates,collapse = " ; ")
                f<-paste(f,est,sep=" ; ")
                f
              }
          ),   # End public
          private=list(
            
            .check_constraints=function(consts) {
              realconsts<-list()
              realestims<-list()
              for (con in consts) {
                check<-((length(grep("==",con,fixed=T))>0) | (length(grep("[<>]",con)>0)))
                if (check)
                  realconsts[[length(realconsts)+1]]<-con
                else
                  realestims[[length(realestims)+1]]<-con
              }
              realestims<-lapply(realestims, function(estim) {
                check<-grep(":=|~",estim)
                if (length(check)==0)
                  paste0("`",estim,"`:=",estim)
                else
                  estim
              })
              
              self$constraints<-realconsts
              self$userestimates<-realestims
              
            },
            
            .factorlist=function(terms,factorslen) {
              .terms<-list()
              for (f in names(factorslen)) {
                for (term in terms) {
                  ind<-which(term==f)
                  for (i in ind) {
                    for (j in seq_len(factorslen[[f]])) {
                      .term<-term
                      .term[[i]]<-paste0(.term[[i]],j)
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

Estimate <- R6Class("Estimate",
                  inherit = Syntax,
                  list(
                    warnings=NULL,
                    errors=NULL,
                    model=NULL,
                    ciwidth=NULL,
                    parameters=NULL,
                    coefficients=NULL,
                    correlations=NULL,
                    definedParameters=NULL,
                    fit=NULL,
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
                      
                      withCallingHandlers({
                                 self$model<-lavaan::lavaan(model = self$lav_structure, 
                                                            data = data,
                                                            se=self$options$se,
                                                            bootstrap=self$options$bootN
                                                            )},
                          warning=function(w) self$warnings<-w$message,
                          error=function(w) self$errors<-w$message)
                      
                      if (!is.null(self$errors))
                           return(self$errors)
                      self$parameters<-lavaan::parameterestimates(
                                            self$model,
                                            ci=self$options$ci,
                                            level = self$ciwidth,
                                            standardized = T,
                                            boot.ci.type = self$options$bootci
                      )

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
                      alist<-list( 
                            list(label="User Model",chisq=ff[["chisq"]],df=ff[["df"]],pvalue=ff[["pvalue"]]),
                            list(label="Baseline Model",chisq=ff[["baseline.chisq"]],df=ff[["baseline.df"]],pvalue=ff[["baseline.pvalue"]])
                      )

                      self$fit<-alist
                      
                      # fit indices
                      ff<-sapply(ff, round,3)
                      alist<-list(
                             c(info="Free parameters",value=self$model@Fit@npar),
                             c(info="Estimation Method",value=self$model@Options$estimator),
                             c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")), 
                             c(info="Converged",value=self$model@Fit@converged), 
                             c(info="",value=""),
                             c(info="Loglikelihood user model",value=ff[["logl"]]), 
                             c(info="Loglikelihood unrestricted model",value=ff[["unrestricted.logl"]]),
                             c(info="AIC",value=ff[["aic"]]), 
                             c(info="BIC",value=ff[["bic"]]),
                             c(info="Sample-size adjusted BIC",value=ff[["bic2"]])
                             ) 
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
                    

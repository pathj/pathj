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
              indirect_synt=NULL,
              indirect_names=NULL,
              intercepts=NULL,
              definedParameters=NULL,
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
                ### here we update to build a lavaanify structure
                private$.update()  
                ### here we set up things that require a lavaanify structure
                private$.indirect()
                # we finally update for good
                private$.update()  
                
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
            .update=function() {
                               
              lavoptions<-list(
                model=private$.lavaan_syntax(),
                int.ov.free = self$options$intercepts, 
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
              sel<-grep("==|<|>",.lav_structure$op,invert = T)
              self$structure<-.lav_structure[sel,]
              ### this is weired, but it works fine with multigroups
              r2test<-((.lav_structure$op=="~~") & (.lav_structure$lhs %in% self$options$endogenous) & (.lav_structure$lhs==.lav_structure$rhs))
              self$r2<-.lav_structure[r2test,c("lhs","lgroup")]
              
              self$intercepts<-.lav_structure[.lav_structure$op=="~1",]
              if (nrow(self$intercepts)==0) self$intercepts<-NULL
              
              alist<-list()
              alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$options$estimator)
              alist[[length(alist)+1]]<-c(info="Number of observations",value="") 
              alist[[length(alist)+1]]<-c(info="Free parameters",value=max(.lav_structure$free))
              alist[[length(alist)+1]]<-c(info="Converged","") 
              alist[[length(alist)+1]]<-c(info="",value="")
              alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value="" )
              alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value="")
              alist[[length(alist)+1]]<-c(info="",value="")
              
              self$info<-alist
              
              dp<-.lav_structure[.lav_structure$op==":=",]
              if (nrow(dp)>0) {
                      dp$desc<-""
                      .structure<-self$structure[self$structure$op %in% c("~","~~","~1"),]
                      for (i in seq_along(dp$rhs)) {
                            r<-dp$rhs[i]
                            for (j in 1:nrow(.structure)) {
                            arow<-.structure[j,]
                            group<-arow$group
                            if (is.something(self$multigroup))  groupsub<-SUB[[group]] else groupsub<-""
                            target<-paste0("(",arow$lhs,arow$op,arow$rhs,")",groupsub)
                            reg<-paste0(arow$label,"(?![0-9])")
                            r<-stringr::str_replace(r,reg,target)
                      }
                      dp$desc[i]<-r
                      }
              }
              if (nrow(dp)>0) {
                self$definedParameters=dp
                if (is.something(self$multigroup)) {
                     msg<-paste(1:self$multigroup$nlevels,self$multigroup$levels,sep="= group ",collapse = ", ")
                     msg<-paste("Description subscripts refer to groups, with",msg)
                     self$warnings<-list(topic="defined",message=msg) 
                     
                }
              }

            },
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
              if (is.something(self$indirect_synt)) {
                f<-paste(f,";")
                f<-paste(f,self$indirect_synt,collapse = " ; ")
              }
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
                check<-(length(grep("==|>|<",con,fixed=F))>0) 
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
                      estim<-paste0("dp",j,":=",estim)
                  }
                    else {
                       check<-grep("^IE",estim)
                       if (length(check)>0)
                         self$warnings<-list(topic="defined",message=glue:glue(WARNS[["noreseved"]],var="IE"))
                       check<-grep("^dp",estim)
                       if (length(check)>0)
                         self$warnings<-list(topic="defined",message=glue:glue(WARNS[["noreseved"]],var="dp"))
                       
                    }
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
            },
            .indirect=function() {
              
              if (!self$options$indirect)
                 return()
              tab<-self$structure
              termslist<-list()
              labslist<-list()
              sel<-grep(":",tab$rhs,fixed = T,invert=T) 
              tab<-tab[sel,]
              sel<-tab$op=="~" 
              tab<-tab[sel,]
              tab<-tab[tab$group>0,]
              
              .doit<-function(tab,term,alist=list(),blist=list(),lab=NULL) {
                alist<-c(alist,term)
                blist<-c(blist,lab)
                if (term %in% deps) {
                  final<-unlist(alist)
                  if (length(final)>2) {
                    termslist[[length(termslist)+1]]<<-final
                    labslist[[length(labslist)+1]]<<-unlist(blist)
                  }
                  return()
                }
                a<-tab[tab$rhs==term,]
                if (length(a$lhs)==0)
                  return()
                for (i in 1:nrow(a)) {
                  x<-a$lhs[i]
                  lab<-a$label[i]
                  .doit(tab,x,alist,blist,lab)
                }
                
              }
              
              terms<-unique(tab$rhs)
              deps<-unique(setdiff(tab$lhs,tab$rhs))
              
              
              if (length(deps)==0) {
                self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
                return()
              }
              tabs<-list()
              for (i in tab$group) 
                tabs[[i]]<-tab[tab$group==i,]
              
              results<-list()
              for (i in seq_along(tabs)) {
                .results<-try_hard({
                  for (tt in terms)
                    .doit(tabs[[i]],term=tt)
                })
                if (.results$error)
                    self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
                results[[i]]<-.results
              }
              pars<-sapply(labslist,paste,collapse="*")
              if (!is.something(pars))
                return()
              labs<-sapply(termslist,paste,collapse="->")
              plabs<-paste0("IE",1:length(pars))
              synt<-paste(plabs,pars,sep=":=",collapse = " ; ")
              self$indirect_synt<-synt
              self$indirect_names<-labs

              
            }
            
            
            
          ) # end of private
) # End Rclass


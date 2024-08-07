### This class takes care of passing all information from lavaan tables definitions and estimations  from jamovi input
### to jamovi results tables. It wokrs using Syntax R6 class, Estimate R6 class, and Plotter R6 class.
### Syntax R6 class gets all input options and defines the tables required for showing the results. Estimate R6 class inherit from Syntax
### all properties of the tables and fill them with the actual results estimated with lavaan() function.
### Estimate inherit from Syntax, so only one instance of Estimate is defined. Here is called lav_machine
### Filling the results tables is handle by function in jamovi.R (functions starting with j.)
### Data are handled by a Datamatic R6 class, which does all transformations and checking required.


pathjClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathjClass",
    inherit = pathjBase,
    private = list(
        .factors=NULL,
        .lav_machine=NULL,
        .data_machine=NULL,
        .plot_machine=NULL,
        .model=NULL,
        .ready=NULL,
        .init = function() {
            ginfo("init")
            ### check that we have enough information to run ####
            private$.ready<-readiness(self$options)
            if (!private$.ready$ready) {
                  if(private$.ready$report)
                      self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
                return()
            }
            ### prepare R6 classes that do the work ####
            data_machine<-Datamatic$new(self$options,self$data)
            lav_machine<-Estimate$new(self$options,data_machine)
            plot_machine<-Plotter$new(self$options,data_machine,lav_machine,self$results$pathgroup)
            
            ### fill the info table ###
            j.init_table(self$results$info,lav_machine$tab_info)
            j.init_table_append(self$results$info,lav_machine$models())
           
            j.init_table_append(self$results$info,lav_machine$varcov)
            j.init_table_append(self$results$info,lav_machine$constraints)
            j.init_table_append(self$results$info,lav_machine$defined)
            


            
            #### parameter fit indices tables ####
            j.init_table(self$results$fit$indices,"",ci=T,ciroot="rmsea.",ciformat='RMSEA {}% CI',ciwidth=self$options$ciWidth)
            j.init_table(self$results$fit$indices2,"",ci=F)
            
            ### prepare r2 table
            j.init_table(self$results$models$r2,
                         lav_machine$tab_r2,
                         ci=T,
                         ciwidth=self$options$ciWidth,
                         spaceby="lgroup")
            
            #### parameter estimates table ####
            j.init_table(self$results$models$coefficients,
                         lav_machine$tab_coefficients,
                         ci=T,
                         ciwidth=self$options$ciWidth,
                         spaceby="group")

            ### prepare var cov table ###
            j.init_table(self$results$models$correlations,
                         lav_machine$tab_covariances,
                         ci=T,
                         ciwidth=self$options$ciWidth,
                         spaceby="group")
            

            ### prepare defined params ###
            j.init_table(self$results$models$defined,
                         lav_machine$tab_defined,
                         ci=T,
                         ciwidth=self$options$ciWidth,
                         spaceby="group")

            ### prepare intercepts ###
            if (self$options$showintercepts)
                 j.init_table(self$results$models$intercepts,
                              lav_machine$tab_intercepts,
                              ci=T,
                              ciwidth=self$options$ciWidth,
                              spaceby="group")
            
            # #### contrast tables ####
             if (length(self$options$factors)>0) {
                for (factor in self$options$factors) {
                 clabs<-data_machine$contrasts_labels[[factor]]
                 for (i in seq_along(clabs)) {
                       clab<-clabs[[i]]
                       self$results$models$contrastCodeTable$addRow(paste0(factor,i),list(rname=paste0(factor,i),clab=clab))
                 }
                 self$results$models$contrastCodeTable$setVisible(TRUE)    
             }
             }
            
            if (self$options$constraints_examples) {
                j.init_table(self$results$contraintsnotes,CONT_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,DP_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,SY_EXAMPLES,indent=-1)
                self$results$contraintsnotes$setNote("1",CONT_NOTE)
            }
            
            private$.lav_machine<-lav_machine
            private$.data_machine<-data_machine
            plot_machine$initPlots()
            private$.plot_machine<-plot_machine 
            
            
        },
    
        .run = function() {
            ginfo("run")
            ### check that we have enough information to run ####
            if (!private$.ready$ready)
                return()

            ### clean the data and prepare things ###
            lav_machine<-private$.lav_machine
            data<-private$.data_machine$cleandata(self$data,lav_machine$interactions)

            lav_machine$estimate(data)

            warns<-lav_machine$warnings
            if (is.something(warns[["main"]]))
                for (i in seq_along(warns[["main"]]))
                      self$results$info$setNote(paste0("n",i),warns[["main"]][[i]])

            if (is.something(lav_machine$errors)) {
                    stop(paste(lav_machine$errors,collapse = "; "))
            } 
            ## fit info
             j.fill_table(self$results$info,lav_machine$tab_info)
            
             ## fit indices tables
             self$results$fit$indices$setRow(rowNo=1,lav_machine$tab_fitindices)
             j.add_warnings(self$results$fit$indices,lav_machine,"tab_fitindices")
             
             self$results$fit$indices2$setRow(rowNo=1,lav_machine$tab_fitindices)
             j.add_warnings(self$results$fit$indices2,lav_machine,"tab_fitindices")
             
             ## constraints fit test
             
             j.fill_table(self$results$fit$constraints,lav_machine$tab_constfit,append=T, spaceby="type")


             ## fit test
             j.fill_table(self$results$fit$main,lav_machine$tab_fit,append=T)
             
             
            ### parameters estimates ####
            j.fill_table(self$results$models$coefficients,lav_machine$tab_coefficients)

            j.fill_table(self$results$models$correlations,lav_machine$tab_covariances)
            
            j.fill_table(self$results$models$r2,lav_machine$tab_r2)
            j.add_warnings(self$results$models$r2,lav_machine,"r2")
            
            j.fill_table(self$results$models$defined,lav_machine$tab_defined)
            j.add_warnings(self$results$models$defined,lav_machine,"defined")
            
            if (self$options$showintercepts)
                   j.fill_table(self$results$models$intercepts,lav_machine$tab_intercepts)
            

            ## diagrams
            private$.plot_machine$preparePlots()   
            if (is.something(private$.plot_machine$warnings$diagram)) {
                 for (i in seq_along(private$.plot_machine$warnings$diagram))
                        self$results$pathgroup$notes$addRow(i,list(message=private$.plot_machine$warnings$diagram[[i]]))
                  self$results$pathgroup$notes$setVisible(TRUE)
            }
            
            self$results$.setModel(lav_machine$model)
        },
 
        .showDiagram=function(image,ggtheme, theme, ...) {
            if (self$options$diagram==FALSE) 
                return()
            if (!is.something(image$state$semModel))
                 return()
            options<-private$.plot_machine$semPathsOptions
      #      res<-try_hard({
            sp<-semPlot::semPaths(image$state$semModel,
                              layout =options$layout,
                              residuals = options$residuals,
                              rotation = options$rotation,
                              intercepts = options$intercepts,
                              nodeLabels= options$nodeLabels,
                              whatLabels=options$whatLabels,
                              sizeMan = options$sizeMan,
                              sizeMan2=options$sizeMan2,
                              curve=options$curve,
                              shapeMan=options$shapeMan,
                              edge.label.cex =options$edge.label.cex,
                              doNotPlot=TRUE,
                              style = "ram")
            
            if (self$options$diag_offset_labs)
                sp$graphAttributes$Edges$edge.label.position<-rep(.60,length(sp$graphAttributes$Edges$edge.label.position))
            
            sp$graphAttributes$Edges$lty[sp$Edgelist$bidirectional]<-2
            sp$graphAttributes$Edges$curve[sp$Edgelist$bidirectional]<-.6
            
            plot(sp)
            
       #     }
      #      )
            note<-FALSE
            
       return(TRUE)     
            if (!isFALSE(res$error)) {
                if  (length(grep("Circle layout only supported",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("graph_from_edgelist",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("subscript out of",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["fail"]]
                    note<-TRUE
                }
            }
            
            
            
            if (!isFALSE(res$error)) {
                 self$results$pathgroup$notes$addRow("err",list(message=res$error))
                 note<-TRUE
            }
            if (!isFALSE(res$warning)) {
                self$results$pathgroup$notes$addRow("war",list(message=res$warning))
                note<-TRUE
            }

            if (note)
                self$results$pathgroup$notes$setVisible(TRUE)

            return(TRUE)

        },
        .marshalFormula= function(formula, data, name) {
            endogenous<-list()
            endogenousTerms<-list()
            j<-0
            for (i in seq_along(formula)) {
                if (lgrep("<|>|==|~~",formula[[i]]))
                    warning("Constraints and defined parameters are ignored in `formula`. Please use `constraints` option")
                else {
                    j<-j+1
                    line<-as.formula(formula[[i]])
                    endogenous[[j]]<-as.character(line[[2]])
                    endogenousTerms[[j]]<-jmvcore::decomposeFormula(expand.formula(as.formula(line)))
                }
            }
            exogenous<-setdiff(unique(unlist(endogenousTerms)),endogenous)
            allvars<-unlist(c(endogenous,exogenous))
            if (name=="endogenous")
                return(endogenous)
            if (name=="endogenousTerms")
                return(endogenousTerms)
            if (name=="exogenous")
                return(exogenous)

            data<-data[0,allvars]
            
            if (name=="covs") {
                return(allvars[(!sapply(data, is.factor))])
            }
            if (name=="factors") {
                data<-data[0,allvars]
                return(allvars[(sapply(data, is.factor))])
            }
            
            
            
        },
        
        .formula = function() {
            if (!is.something(private$.lav_machine))
                  return("")
            paste0("list(",paste(sapply(private$.lav_machine$models(),function(m) paste0('"',m$value,'"')),collapse = ","),")")
            
        },
        
        .sourcifyOption = function(option) {
            
            name <- option$name
            value <- option$value
            
            if (!is.something(value))
                return('')
            
            if (option$name %in% c('factors', 'endogenous', 'covs', 'endogenousTerms'))
                return('')
            
            if (name =='scaling') {
                vec<-sourcifyList(option,"none")
                return(vec)
            }
            if (name =='contrasts') {
                vec<-sourcifyList(option,"simple")
                return(vec)
            }
            if (name =='varcov') {
                vec<-lapply(self$options$varcov, function(v) c(v$i1,v$i2))
                vec=paste0("varcov=list(",paste(vec,collapse = ","),")",collapse = "")
                return(vec)
            }
            
            super$.sourcifyOption(option)
        }
        
        
        
        
        
        )
)

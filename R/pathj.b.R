
# This file is a generated template, your changes will not be overwritten

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
            j.init_table_append(self$results$info,lav_machine$constraints)
            j.init_table_append(self$results$info,lav_machine$defined)
            


            
            #### parameter fit indices table ####
            j.init_table(self$results$fit$indices,"",ci=T,ciroot="rmsea.",ciformat='RMSEA {}% CI',ciwidth=self$options$ciWidth)
            ### prepare r2 table
            j.init_table(self$results$models$r2,lav_machine$tab_r2,ci=T,ciwidth=self$options$ciWidth)
            
            #### parameter estimates table ####
            j.init_table(self$results$models$coefficients,lav_machine$tab_coefficients,ci=T,ciwidth=self$options$ciWidth)

            ### prepare var cov table ###
            j.init_table(self$results$models$correlations,lav_machine$tab_covariances,ci=T,ciwidth=self$options$ciWidth)
            

            ### prepare defined params ###
            j.init_table(self$results$models$defined,lav_machine$tab_defined,ci=T,ciwidth=self$options$ciWidth)

            ### prepare intercepts ###
            if (self$options$showintercepts)
                 j.init_table(self$results$models$intercepts,lav_machine$tab_intercepts,ci=T,ciwidth=self$options$ciWidth)
            
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
                self$results$contraintsnotes$setNote(1,CONT_NOTE)
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
                      self$results$info$setNote(i,warns[["main"]][[i]])

            if (is.something(lav_machine$errors)) {
                    stop(paste(lav_machine$errors,collapse = "\n\n"))
            }
            ## fit info
             j.fill_table(self$results$info,lav_machine$tab_info)

             ## fit indices
             self$results$fit$indices$setRow(rowNo=1,lav_machine$tab_fitindices)
             
             ## fit test
             j.fill_table(self$results$fit$main,lav_machine$tab_fit,append=T)

             ## constraints fit test
             
             j.fill_table(self$results$fit$constraints,lav_machine$tab_constfit,append=T, spaceby="type")


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
        },
 
        .showDiagram=function(image,ggtheme, theme, ...) {
            if (self$options$diagram==FALSE) 
                return()
            if (!is.something(image$state$plot))
                 return()
            
            plot(image$state$plot)
            return(image$state$plot)

        }

        
        
        
        )
)

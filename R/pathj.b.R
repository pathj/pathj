
# This file is a generated template, your changes will not be overwritten

pathjClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathjClass",
    inherit = pathjBase,
    private = list(
        .factors=NULL,
        .lav_machine=NULL,
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
            ### clean data and prepare the syntax ####
            data<-private$.cleandata()
            lav_machine<-Estimate$new(self$options,data)

            ### fill the info table ###
            j.init_table(self$results$info,lav_machine$models())
            j.init_table_append(self$results$info,lav_machine$constraints)
            j.init_table_append(self$results$info,lav_machine$userestimates)

            ### get the lavaan structure of the model ###
            tab<-lav_machine$structure

            #### parameter fit indices table ####
            j.init_table(self$results$fit$indices,"",ci=T,ciroot="rmsea.",ciformat='RMSEA {}% CI',ciwidth=self$options$ciWidth)
            #### parameter estimates table ####
            tab1<-tab[tab$op=="~",]
            j.init_table(self$results$models$main,tab1,ci=T,ciwidth=self$options$ciWidth)

            ### prepare var cov table ###
            tab1<-tab[tab$op=="~~",]
            j.init_table(self$results$models$correlations,tab1,ci=T,ciwidth=self$options$ciWidth)
            
            ### prepare r2 table
            tab1<-lapply(self$options$endogenous,function(e) list(lhs=e))
            j.init_table(self$results$models$r2,tab1,ci=T,ciwidth=self$options$ciWidth)

            ### prepare defined params ###
            tab1<-tab[tab$op==":=",]
            j.init_table(self$results$models$defined,tab1,ci=T,ciwidth=self$options$ciWidth)
            

            #### contrast tables ####
            if (length(self$options$factors)>0) {
                
            for (factor in tob64(self$options$factors)) {
                cont<-attr(data[[factor]],"jcontrast")
                clabs<-lf.contrastLabels(cont$levels,cont$type)
                for (i in seq_along(clabs)) {
                      clab<-clabs[[i]]
                      self$results$models$contrastCodeTable$addRow(paste0(factor,i),list(rname=paste0(fromb64(factor),i),clab=clab))
                }
            }
                self$results$models$contrastCodeTable$setVisible(TRUE)    
            }
            
            if (self$options$constraints_examples) {
                j.init_table(self$results$contraintsnotes,CONT_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,DP_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,SY_EXAMPLES,indent=-1)
                self$results$contraintsnotes$setNote(1,CONT_NOTE)
            }

            private$.lav_machine<-lav_machine

        },
    
        .run = function() {
            ginfo("run")
            ### check that we have enough information to run ####
            if (!private$.ready$ready)
                return()

            ### clean the data and prepare things ###
            data<-private$.cleandata()
            lav_machine<-private$.lav_machine
            results<-lav_machine$estimate(data)
            
            if (is.something(lav_machine$warnings))
                for (i in seq_along(lav_machine$warnings))
                      self$results$info$setNote(i,lav_machine$warnings[[i]])

            if (is.something(lav_machine$errors)) {
                    stop(paste(lav_machine$errors,collapse = "\n\n"))
            }
            ## fit info
             nr<-self$results$info$rowCount
             for (i in seq_along(lav_machine$info)) {
                 self$results$info$addRow(rowKey=nr+i+1,lav_machine$info[[i]])
             }
             self$results$info$addFormat(rowNo=nr, col=1,jmvcore::Cell.BEGIN_END_GROUP)
             ## fit indices
             self$results$fit$indices$addRow(1,lav_machine$fitindices)
             
             ## fit test
             for (i in seq_along(lav_machine$fit)) 
                 self$results$fit$main$addRow(rowKey=i,lav_machine$fit[[i]])
             
             
             tab<-lav_machine$constfit
             if (is.something(tab)) {
                 for (i in seq_len(nrow(tab)))
                     self$results$fit$constraints$addRow(rowKey=i,lav_machine$constfit[i,])
                 self$results$fit$constraints$setVisible(TRUE)
             }
             

            ### parameters estimates ####
            tab1<-lav_machine$coefficients
            for (i in seq_len(nrow(tab1))) 
                self$results$models$main$setRow(rowKey=i,tab1[i,])
   
            tab2<-lav_machine$correlations
            for (i in seq_len(nrow(tab2))) {
                if (is.na(tab2$z[i])) {
                    tab2$user[i]="Sample"
                    tab2$z[i]<-""
                    tab2$pvalue[i]<-""
                    tab2$se[i]<-""
                }
                else tab2$user[i]="Estim."
                self$results$models$correlations$setRow(rowKey=i,tab2[i,])
            }
            tab3<-lav_machine$r2
            for (i in seq_len(nrow(tab3))) 
                self$results$models$r2$setRow(rowKey=i,tab3[i,])
            
            tab<-lav_machine$definedParameters
            if (is.something(tab)) {
                 for (i in seq_len(nrow(tab))) 
                    self$results$models$defined$setRow(rowKey=i,tab[i,])
                 self$results$models$defined$setVisible(TRUE)
            }
        },
        
        .cleandata=function() {

            .warning<-list()
            dataRaw <- jmvcore::naOmit(self$data)
            names(dataRaw)<-tob64(names(dataRaw))
            data <- list()
            endogenous<-tob64(self$options$endogenous)
            for (endo in endogenous) {
                if (class(dataRaw[[endo]]) == "factor")
                    .warning<-append(.warning,"Warming: An endogenous variables is defined as factor. Please make sure it is a continuous variable.")
                data[[endo]] <- jmvcore::toNumeric(dataRaw[[endo]])
            }
            
            covs<-tob64(self$options$covs)
            for (cov in covs) {
                data[[cov]] <- jmvcore::toNumeric(dataRaw[[cov]])
            }
            
            .contrasts<-sapply(self$options$contrasts,function(a) a$type)
            .contrastsnames<-sapply(self$options$contrasts,function(a) tob64(a$var))
            names(.contrasts)<-.contrastsnames
            factors<-tob64(self$options$factors)
            for (factor in factors) {
                ### we need this for Rinterface ####
                if (!("factor" %in% class(dataRaw[[factor]]))) {
                    info(paste("Warning, variable",factor," has been coerced to factor"))
                    dataRaw[[factor]]<-factor(dataRaw[[factor]])
                }
                data[[factor]] <- dataRaw[[factor]]
                levels <- base::levels(data[[factor]])
                .cont<-ifelse(factor %in% .contrastsnames,.contrasts[[factor]],"simple")
                stats::contrasts(data[[factor]]) <- lf.createContrasts(levels,.cont)
                attr(data[[factor]],"jcontrast")<-list(type=.cont,levels=levels)
                dummies<-model.matrix(as.formula(paste0("~",factor)),data=data)
                dummies<-dummies[,-1]
                dummies<-data.frame(dummies)
                private$.factors<-c(private$.factors,names(dummies))
                onames<-names(data)
                data<-cbind(data,dummies)
                names(data)<-c(onames,paste0(factor,FACTOR_SYMBOL,1:(length(levels)-1)))
            }
            
             #### here we do thing if cleandata is called by run (not init) ####
             if (is.something(private$.lav_machine)) {
                 ### here we build the interactions variables                 
                 if (private$.lav_machine$hasInteractions) {
                     ints<-private$.lav_machine$interactions
                     for (int in ints) {
                         int<-trimws(int)
                         terms<-strsplit(int,INTERACTION_SYMBOL,fixed = T)[[1]]
                         terms<-paste0("data$",trimws(terms))
                         head<-paste0("data$",int,"<-")
                         op<-paste(terms,collapse = " * ")
                         synt<-paste0(head,op)
                         eval(parse(text=synt))
                     }
                     
                 
             }
                          
                          
                      }
            data<-as.data.frame(data)     
            attr(data,"warning")<-.warning
            return(data)
            
        },
        .showDiagram=function(image, ggtheme, theme, ...) {
          
          if (!private$.ready$ready)
                return()
            
          labs<-self$options$diag_paths
          model<-private$.lav_machine$model
          pt<-lavaan::parTable(model)
          nodeLabels<-unique(pt$lhs)
          nodeLabels<-fromb64(nodeLabels)
          size<-12
          if (self$options$diag_labsize=="small") size<-8
          if (self$options$diag_labsize=="large") size<-18
          nNodes<-length(nodeLabels)
          size<-size*exp(-nNodes/80)+1
          options<-list(object = private$.lav_machine$model,
                                  layout = self$options$diag_type,
                                  residuals = self$options$diag_resid,
                                  rotation = as.numeric(self$options$diag_rotate),
                                  intercepts = F
                                  ,nodeLabels=nodeLabels
                                  ,whatLabels=labs
                                  ,sizeMan = size
                                  ,nCharNodes=10
                                  ,sizeMan2=size/2
                                  , curve=2
                                  , shapeMan=self$options$diag_shape
                                  ,edge.label.cex =1.3)
          res<-try_hard(do.call(semPlot::semPaths,options))
          if (is.something(res$error)) {
             if  (length(grep("Circle layout only supported",res$error,fixed = T))>0) {
                  self$results$pathgroup$notes$addRow(1,list(info="Circle layout requires rotation to be `Exogenous Top` or Exogenous Bottom`"))
                  self$results$pathgroup$notes$setVisible(TRUE)
             }
          }
           return(res$obj)
        }
        
        
        
        )
)

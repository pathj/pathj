
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
            forms<-lav_machine$models()
            
            
            ### fill the info table ###
            for (i in seq_along(forms)) {
                  self$results$info$addRow(rowKey=i,list(info="Model",value=as.character(forms[[i]])))
            }

            for (j in seq_along(lav_machine$constraints)) {
                   self$results$info$addRow(rowKey=i+j,list(info="Constraint",value=lav_machine$constraints[[j]]))
            }

            for (k in seq_along(lav_machine$userestimates)) {
                self$results$info$addRow(rowKey=i+j+k,list(info="Defined Parameters",value=lav_machine$userestimates[[k]]))
            }

            
            
            
            ### get the lavaan structure of the model ###
            tab<-lav_machine$structure
            
            #### parameter fit indices table ####
            aTable<-self$results$fit$indices
            aTable$getColumn('rmsea.ci.lower')$setSuperTitle(jmvcore::format('RMSEA {}% CI', self$options$ciWidth))
            aTable$getColumn('rmsea.ci.upper')$setSuperTitle(jmvcore::format('RMSEA {}% CI', self$options$ciWidth))
            
            
            #### parameter estimates table ####
            aTable<-self$results$models$main
            aTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            aTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            
            
            ### prepare coefficients table ###
            tab1<-tab[tab$op=="~",]
            who<-tab1$lhs[1]
            for (i in seq_len(nrow(tab1))) {
                aTable$addRow(i,list(lhs=tab1[i,"lhs"],rhs=tab1[i,"rhs"]))
                if (tab$lhs[i]!=who)    aTable$addFormat(rowKey = i, col = 1, jmvcore::Cell.BEGIN_GROUP)
                who<-tab1$lhs[i]
            }
            
            ### prepare var cov table ###
            aTable<-self$results$models$correlations
            aTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            aTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            
            tab1<-tab[tab$op=="~~",]
            who<-tab1$lhs[1]
            for (i in seq_len(nrow(tab1))) {
                aTable$addRow(i,list(lhs=tab1[i,"lhs"],rhs=tab1[i,"rhs"]))
                if (tab$lhs[i]!=who)    aTable$addFormat(rowKey = i, col = 1, jmvcore::Cell.BEGIN_GROUP)
                who<-tab1$lhs[i]
            }
            aTable<-self$results$models$r2
            aTable$getColumn('ci.lower')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            aTable$getColumn('ci.upper')$setSuperTitle(jmvcore::format('{}% Confidence Interval', self$options$ciWidth))
            
            for (i in seq_along(self$options$endogenous)) 
                aTable$addRow(rowKey=i,list(lhs=self$options$endogenous[i]))
            
            tab1<-tab[tab$op==":=",]
            for (i in seq_len(nrow(tab1))) 
                self$results$models$defined$addRow(rowKey=i,list(lhs=tab1[i,"lhs"],rhs=tab1[i,"rhs"]))
            

            #### contrast tables ####
            if (length(self$options$factors)>0) {
            for (factor in self$options$factors) {
                type<-attr(data[[factor]],"jcontrast")
                clabs<-lf.contrastLabels(levels(data[[factor]]),type)
                for (i in seq_along(clabs)) {
                      clab<-clabs[[i]]
                      self$results$models$contrastCodeTable$addRow(paste0(factor,i),list(rname=paste0(factor,i),clab=clab))
                }
            }
                self$results$models$contrastCodeTable$setVisible(TRUE)    
            }
            
            if (self$options$constraints_examples) {
                 for (i in seq_along(CONT_EXAMPLES)) {
                       self$results$contraintsnotes$addRow(rowKey=i,CONT_EXAMPLES[[i]])
                       if (i!=1)
                          self$results$contraintsnotes$addFormat(rowKey=i,col=1,jmvcore::Cell.INDENTED)
                 }
                for (j in seq_along(DP_EXAMPLES)) {
                    self$results$contraintsnotes$addRow(rowKey=i+j,DP_EXAMPLES[[j]])
                    if (j!=1)
                        self$results$contraintsnotes$addFormat(rowKey=i+j,col=1,jmvcore::Cell.INDENTED)
                }
                for (k in seq_along(SY_EXAMPLES)) {
                    self$results$contraintsnotes$addRow(rowKey=i+j+k,SY_EXAMPLES[[k]])
                    if (k!=1)
                        self$results$contraintsnotes$addFormat(rowKey=i+j+k,col=1,jmvcore::Cell.INDENTED)
                }
                

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
             mark(class(lav_machine$fitindices))
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
            data <- list()
            for (endo in self$options$endogenous) {
                if (class(dataRaw[[endo]]) == "factor")
                    .warning<-append(.warning,"Warming: An endogenous variables is defined as factor. Please make sure it is a continuous variable.")
                data[[endo]] <- jmvcore::toNumeric(dataRaw[[endo]])
            }
            
            for (cov in self$options$covs) {
                data[[cov]] <- jmvcore::toNumeric(dataRaw[[cov]])
            }
            
            .contrasts<-sapply(self$options$contrasts,function(a) a$type)
            .contrastsnames<-sapply(self$options$contrasts,function(a) a$var)
            names(.contrasts)<-.contrastsnames
            for (factor in self$options$factors) {
                ### we need this for Rinterface ####
                if (!("factor" %in% class(dataRaw[[factor]]))) {
                    info(paste("Warning, variable",factor," has been coerced to factor"))
                    dataRaw[[factor]]<-factor(dataRaw[[factor]])
                }
                data[[factor]] <- dataRaw[[factor]]
                levels <- base::levels(data[[factor]])
                .cont<-ifelse(factor %in% .contrastsnames,.contrasts[[factor]],"simple")
                stats::contrasts(data[[factor]]) <- lf.createContrasts(levels,.cont)
                attr(data[[factor]],"jcontrast")<-.cont
                dummies<-model.matrix(as.formula(paste0("~",factor)),data=data)
                dummies<-dummies[,-1]
                dummies<-data.frame(dummies)
                private$.factors<-c(private$.factors,names(dummies))
                onames<-names(data)
                data<-cbind(data,dummies)
                names(data)<-c(onames,paste0(factor,1:(length(levels)-1)))
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
          nodelabels<-gsub(INTERACTION_SYMBOL,":",nodeLabels)
          
          options<-list(object = private$.lav_machine$model,
                                  layout = "tree2",
                                  residuals = self$options$diag_resid,
                                  rotation = 2,
                                  intercepts = F
                                  ,nodeLabels=nodelabels
                                  ,whatLabels=labs
                                  ,sizeMan = 10
                                  ,nCharNodes=10
                                  ,edge.label.cex =1.3)
          diag<-do.call(semPlot::semPaths,options)
           return(diag)
        }
        
        
        
        )
)

Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      initialize=function(options,datamatic,operator,resultsplots) {
            super$initialize(options=options,vars=unlist(c(options$endogenous,options$factors,options$covs)))
            private$.plotgroup<-resultsplots
            private$.operator<-operator
            private$.datamatic<-datamatic
      },

      initPlots=function() {
           if (!self$options$diagram)
               return()

           if (is.something(private$.datamatic$multigroup))  {
                for (level in private$.datamatic$multigroup$levels) {
                     title <- paste(private$.datamatic$multigroup$var, "=", level)
                     private$.plotgroup$diagrams$addItem(level)
                     private$.plotgroup$diagrams$get(key = level)$setTitle(title)
                }
           } else {
                private$.plotgroup$diagrams$addItem("0")
                private$.plotgroup$diagrams$get(key = "0")$setTitle("")
      }
      
      },
      preparePlots=function(image, ggtheme, theme, ...) {
        
        if (!self$options$diagram)
          return()
        
        labs<-self$options$diag_paths
        model<-private$.operator$model
        nodeLabels<-model@pta$vnames$ov.num[[1]]
        nodeLabels<-fromb64(nodeLabels)
        
        size<-12
        if (self$options$diag_labsize=="small") size<-8
        if (self$options$diag_labsize=="large") size<-18
        nNodes<-length(nodeLabels)
        size<-size*exp(-nNodes/80)+1
        options<-list(object = private$.operator$model,
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
        redo<-FALSE
        if (is.something(res$error)) {
          
          if  (length(grep("Circle layout only supported",res$error,fixed = T))>0) {
            private$.plotgroup$notes$addRow(1,list(info="Rotation set to `Exogenous Top`. Circle layout requires rotation to be `Exogenous Top` or `Exogenous Bottom`"))
            res$error<-NULL
            options[["rotation"]]<-1
            redo<-TRUE
            
          } 
          if  (length(grep("graph_from_edgelist",res$error,fixed = T))>0) {
            private$.plotgroup$notes$addRow(1,list(info="Layout has been set to Circle"))
            options[["layout"]]<-"circle"
            options[["rotation"]]<-1
            res$error<-NULL
            redo<-TRUE
          } 
          if  (length(grep("subscript out of",res$error,fixed = T))>0) {
            res$error<-"The diagram cannot be displayed. Please try a different layout type"
            redo<-FALSE
          } 
          
          
          if (redo) {
            res<-try_hard(do.call(semPlot::semPaths,options))
          }   
          if (!isFALSE(res$warning))
            private$.plotgroup$notes$addRow("war",list(info=res$warning))
          
          if (!isFALSE(res$error))
            private$.plotgroup$notes$addRow("err",list(info=res$error))
          
        }
        
        if (private$.plotgroup$notes$rowCount>0)
               private$.plotgroup$notes$setVisible(TRUE)

        diags<-res$obj

        images<-private$.plotgroup$diagrams
        
        if ("list" %in% class(diags))
          for (i in seq_along(images$itemKeys)) {
            image<-images$get(key = images$itemKeys[[i]])
            image$setState(list(plot = diags[[i]]))
          }
        else {
          image<-images$get(key = "0")
          image$setState(list(plot = diags))
        }

        return()
      }
      
      
  ), # end of public
  private = list(
    .diagrams=NULL,
    .datamatic=FALSE,
    .plotgroup=NULL,
    .operator=NULL

  ) # end of private
) # end of class
    
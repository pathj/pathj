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
        
        model<-private$.operator$model
        ### this is due to the fact that semPaths seems to fail when a inequality constraints is in ###
        ### check for more lavaanian solution ###
#        check<-grep(">|<",model@ParTable$op,invert = T)
#        par<-model@ParTable
#        model@ParTable<-sapply(par, function(x) x[check],simplify = F)
        ### end ###

        labs<-self$options$diag_paths
        nodeLabels<-model@pta$vnames$ov.num[[1]]
        nodeLabels<-fromb64(nodeLabels)
        if (self$options$diag_abbrev!="0")
            nodeLabels<-abbreviate(nodeLabels,minlength = as.numeric(self$options$diag_abbrev),strict = T)
        
        size<-12
        if (self$options$diag_labsize=="small") size<-8
        if (self$options$diag_labsize=="large") size<-18
        if (self$options$diag_labsize=="vlarge") size<-24
        
        nNodes<-length(nodeLabels)
        size<-size*exp(-nNodes/80)+1
        ## for some reason we cannot do a do.call for semPlot::semPaths because
        ## it fails in windows 10. So we call it directly every time
        
        options<-list(object = model,
                      layout = self$options$diag_type,
                      residuals = self$options$diag_resid,
                      rotation = as.numeric(self$options$diag_rotate),
                      intercepts = F
                      ,nodeLabels=nodeLabels
                      ,whatLabels=labs
                      ,sizeMan = size
                      ,sizeMan2=size/2
                      , curve=2
                      , shapeMan=self$options$diag_shape
                      ,edge.label.cex =1.3)

        res<-private$.semPaths(options)
        
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
            res<-private$.semPaths(options)
          }   
          if (!isFALSE(res$warning))
            private$.plotgroup$notes$addRow("war",list(info=res$warning))
          
          if (!isFALSE(res$error)) {
            private$.plotgroup$notes$addRow("err",list(info=res$error))
            private$.plotgroup$notes$setVisible(TRUE)
            private$.plotgroup$diagrams$setVisible(FALSE)
            
            return()
          }
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
    .operator=NULL,
    .semPaths=function(options) {
      ### we need this because semPaths does not work with do.call() in windows
      res<-try_hard({
        graphics.off()
        semPlot::semPaths(object = options$object,
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
                      edge.label.cex =options$edge.label.cex)
      })
      return(res)
    }

  ) # end of private
) # end of class
    
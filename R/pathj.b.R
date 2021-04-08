
# This file is a generated template, your changes will not be overwritten

pathjClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathjClass",
    inherit = pathjBase,
    private = list(
        .init = function() {
            ginfo("init")
            goOn<-readiness(self$options)
            mark(goOn)
            if (!goOn$ready) {
                  if(goOn$report)
                      self$results$info$addRow("info",list(info="Setup",specs=goOn$reason))
            }


            
        },
    
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

        })
)

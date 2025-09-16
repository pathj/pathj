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

            ### model diagnostics: modification indices ###
            # init with current content (likely none at init)
            j.init_table(self$results$diagnostics$modindices,
                         lav_machine$tab_mi,
                         ci=F,
                         spaceby="lgroup")
            
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

            ## init p-graphs images per group (separate panels)
            if (self$options$pgraphs) {
                images <- self$results$pgraphs$pcurves
                if (is.something(data_machine$multigroup))  {
                    for (level in data_machine$multigroup$levels) {
                        images$addItem(level)
                        images$get(key = level)$setTitle(paste(data_machine$multigroup$var, "=", level))
                        images$get(key = level)$setState(list(gkey = level))
                    }
                } else {
                    images$addItem("All")
                    images$get(key = "All")$setTitle("")
                    images$get(key = "All")$setState(list(gkey = NULL))
                }
            }
            
            
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
            if (is.something(warns[["main"]])) {
                notes <- warns[["main"]]
                notes <- unlist(notes, use.names = FALSE)
                notes <- as.character(notes)
                notes[is.na(notes)] <- ""
                notes <- vapply(notes, enc2utf8, FUN.VALUE = character(1))
                notes <- notes[nchar(notes) > 0]
                for (i in seq_along(notes))
                      self$results$info$setNote(paste0("n",i), notes[[i]])
            }

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

             ## diagnostics: modification indices
             j.fill_table(self$results$diagnostics$modindices, lav_machine$tab_mi, append=TRUE)
             j.add_warnings(self$results$diagnostics$modindices, lav_machine, "modindices")

             
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
        .plotPvalues=function(image, ggtheme, theme, ...) {
            if (self$options$pgraphs==FALSE)
                return()

            dm <- private$.data_machine
            lavm <- private$.lav_machine
            # work with a plain data.frame copy of the source data
            alldata <- as.data.frame(self$data)
            mg <- dm$multigroup

            .sizesFor <- function(N) {
                base <- c(50, 100, 200, 500)
                ss <- base[base <= N]
                if (length(ss) == 0) {
                    ss <- unique(round(c(max(5, floor(N*0.33)), max(6, floor(N*0.66)), N)))
                    ss <- ss[ss <= N]
                }
                unique(sort(ss))
            }

            ## New: project p-values from standardized effects without resampling
            {
                # parse sizes from options (comma/space separated), fallback to defaults
                .parseSizes <- function(s) {
                    if (is.null(s) || length(s) == 0) return(c(50,100,200,500))
                    s <- as.character(s)
                    parts <- unlist(strsplit(s, "[,;\t\n\r ]+"))
                    v <- suppressWarnings(as.numeric(parts))
                    v <- unique(sort(v[is.finite(v) & v >= 2]))
                    if (length(v) == 0) v <- c(50,100,200,500)
                    v
                }
                sizes <- .parseSizes(try(self$options$pcurve_sizes, silent=TRUE))
                tab <- lavm$tab_coefficients
                if (is.something(tab)) {
                    tab$z <- suppressWarnings(as.numeric(tab$z))
                    tab <- tab[is.finite(tab$z), , drop=FALSE]
                    # capture standardized beta for labelling
                    if ("std.all" %in% names(tab))
                        tab$beta <- suppressWarnings(as.numeric(tab$std.all))
                    else
                        tab$beta <- NA_real_
                }
                if (!is.something(tab) || nrow(tab)==0) {
                    jmvcore::reject("Model has no regression coefficients to plot")
                    return()
                }
                # group sizes
                nobs <- try(lavaan::lavInspect(lavm$model, "nobs"), silent=TRUE)
                nmap <- list()
                if (inherits(nobs, "try-error")) {
                    nmap[["All"]] <- nrow(as.data.frame(self$data))
                } else if (is.list(nobs)) {
                    for (nm in names(nobs)) nmap[[nm]] <- as.numeric(nobs[[nm]])
                } else if (length(nobs) > 1 && is.something(mg)) {
                    for (i in seq_along(mg$levels)) nmap[[mg$levels[[i]]]] <- as.numeric(nobs[[i]])
                } else if (length(nobs) == 1) {
                    nmap[["All"]] <- as.numeric(nobs)
                }

                # helper to robustly resolve per-group N
                .getNobs <- function(g) {
                    # try mapped by group label
                    v <- suppressWarnings(as.numeric(nmap[[g]]))
                    if (length(v) == 0 || is.na(v) || !is.finite(v) || v <= 1) {
                        # fallback by group index based on mg$levels position
                        if (is.something(mg)) {
                            pos <- which(as.character(mg$levels) %in% as.character(g))
                            if (length(pos) == 1 && pos >= 1) {
                                vv <- suppressWarnings(as.numeric(nobs[[pos]]))
                                if (length(vv) > 0 && is.finite(vv) && vv > 1)
                                    v <- vv
                            }
                        }
                    }
                    v
                }
                rows <- list()
                for (i in seq_len(nrow(tab))) {
                    # derive group label robustly: prefer lgroup, else map numeric group via mg$levels
                    if ("lgroup" %in% names(tab)) {
                        g <- as.character(tab$lgroup[i])
                    } else if ("group" %in% names(tab) && is.something(mg)) {
                        gi <- suppressWarnings(as.integer(tab$group[i]))
                        if (is.finite(gi) && gi >= 1 && gi <= length(mg$levels))
                            g <- as.character(mg$levels[[gi]])
                        else
                            g <- "All"
                    } else {
                        g <- "All"
                    }
                    Nobs <- .getNobs(g)
                    # Guard against missing/invalid group sizes causing NA in if() condition
                    if (length(Nobs) == 0 || is.na(Nobs) || !is.finite(Nobs) || Nobs <= 1) next
                    z0 <- as.numeric(tab$z[i])
                    for (n in sizes) {
                        zn <- z0 * sqrt(n / Nobs)
                        p <- 2 * stats::pnorm(-abs(zn))
                        rows[[length(rows)+1]] <- list(group=g, lhs=tab$lhs[i], rhs=tab$rhs[i], n=n, p=p, beta=tab$beta[i])
                    }
                }
                d <- if (length(rows)>0) do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors=FALSE)) else NULL
                if (is.null(d) || nrow(d)==0) {
                    jmvcore::reject("Could not compute projected p-values")
                    return()
                }
                d$p <- pmin(pmax(as.numeric(d$p), 0), 1)
                d$n <- as.numeric(d$n)
                d <- d[order(d$n), , drop=FALSE]
                # label with beta per path (same within a group)
                fmtb <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
                d$lab <- paste0(d$rhs, " \u2192 ", d$lhs, " (\u03B2=", fmtb(as.numeric(d$beta)), ")")

                baseBreaks <- sizes
                brks <- baseBreaks

                # filter per-image group
                ttl <- "Mean p-values vs Sample Size"
                if (is.something(mg) && !is.null(image$state$gkey)) {
                    d <- d[d$group == image$state$gkey, , drop=FALSE]
                    ttl <- paste0(image$state$gkey, ": Mean p-values vs Sample Size")
                }
                # line style options
                .lt <- try(as.character(self$options$pcurve_linetype), silent=TRUE)
                if (!is.character(.lt) || length(.lt) == 0 || ! .lt[1] %in% c("solid","dashed","dotted")) .lt <- "solid"
                .lw <- try(as.numeric(self$options$pcurve_lwd), silent=TRUE)
                if (!is.finite(.lw) || .lw <= 0) .lw <- 1.2

                p <- ggplot2::ggplot() +
                     ggplot2::scale_x_continuous(breaks = brks) +
                     ggplot2::scale_y_continuous(limits = c(0, 1)) +
                     ggplot2::geom_hline(ggplot2::aes(yintercept = 0.05, linetype = "p = 0.05"), color = "red", show.legend = TRUE) +
                     ggplot2::scale_linetype_manual(values = c("p = 0.05" = "dashed"), name = "") +
                     ggplot2::labs(x = "Sample size (n)", y = "Mean p-value", color = "Predictor", title = ttl) +
                     ggplot2::theme_minimal(base_size = 12)
                # palette option
                .pal <- try(as.character(self$options$pcurve_palette), silent=TRUE)
                if (is.character(.pal) && length(.pal) > 0 && .pal[1] == "okabeito") {
                    okabeito <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                    p <- p + ggplot2::scale_color_manual(values = okabeito)
                }
                p <- p + ggplot2::geom_line(data=d, ggplot2::aes(x = n, y = p, color = lab, group = lab), linetype=.lt, size=.lw) +
                        ggplot2::geom_point(data=d, ggplot2::aes(x = n, y = p, color = lab, group = lab), size = 2)
                print(p)
                return(TRUE)
            }

            out <- list()
            reps <- 5

            if (is.something(mg)) {
                # resolve the grouping column name robustly
                possibles <- unique(c(mg$var, mg$var64, try(jmvcore::toB64(mg$var), silent=TRUE), try(jmvcore::fromB64(mg$var64), silent=TRUE)))
                possibles <- as.character(possibles[!is.na(possibles)])
                gvar <- possibles[possibles %in% names(alldata)][1]
                if (!is.something(gvar)) {
                    # fallback to no-group plot
                    mg <- NULL
                }
            }

            if (is.something(mg)) {
                # robustly determine group levels from the actual data
                gvec <- alldata[[gvar]]
                gfac <- as.factor(gvec)
                tab <- table(gfac)
                levs <- names(tab)             # only levels present in the subset
                if (length(levs) == 0) {
                    # fallback to no-group plot
                    mg <- NULL
                } else {
                    counts <- as.integer(tab)
                    Nmin <- min(counts)
                sizes <- .sizesFor(Nmin)
                sizes <- sizes[sizes >= 2]     # avoid zero or one
                    if (length(sizes) == 0) {
                        # fallback to no-group plot
                        mg <- NULL
                    }
                }
            }

            if (is.something(mg)) {
                for (n in sizes) {
                    for (r in seq_len(reps)) {
                        # Sample n per group and bind
                        subsets <- lapply(levs, function(l) {
                            rows <- (!is.na(gfac)) & (gfac == l)
                            d <- alldata[rows, , drop=FALSE]
                            if (nrow(d) < n) return(NULL)
                            d[sample.int(nrow(d), n), , drop=FALSE]
                        })
                        if (any(vapply(subsets, is.null, TRUE)))
                            next()
                        sub <- do.call(rbind, subsets)
                        csub <- dm$cleandata(sub, lavm$interactions)
                        est <- Estimate$new(self$options, dm)
                        res <- try_hard({ est$estimate(csub) })
                        if (res$error != FALSE)
                            next()
                        tab <- est$tab_coefficients
                        if (!is.something(tab))
                            next()
                        df <- data.frame(group = tab$lgroup, lhs = tab$lhs, rhs = tab$rhs,
                                         p = tab$pvalue, n = n, stringsAsFactors = FALSE)
                        out[[length(out)+1]] <- df
                    }
                }
            } else {
                N <- nrow(alldata)
                sizes <- .sizesFor(N)
                sizes <- sizes[sizes >= 2]
                for (n in sizes) {
                    if (N < n) next
                    for (r in seq_len(reps)) {
                        sub <- alldata[sample.int(N, n), , drop=FALSE]
                        csub <- dm$cleandata(sub, lavm$interactions)
                        est <- Estimate$new(self$options, dm)
                        res <- try_hard({ est$estimate(csub) })
                        if (res$error != FALSE)
                            next()
                        tab <- est$tab_coefficients
                        if (!is.something(tab))
                            next()
                        df <- data.frame(group = "All", lhs = tab$lhs, rhs = tab$rhs,
                                         p = tab$pvalue, n = n, stringsAsFactors = FALSE)
                        out[[length(out)+1]] <- df
                    }
                }
            }

            if (length(out) == 0) {
                # Fallback: use full-sample estimates so the user sees something
                tab <- lavm$tab_coefficients
                if (!is.something(tab)) {
                    jmvcore::reject("Model has no regression coefficients to plot")
                    return()
                }
                if (is.something(mg) && is.something(gvar) && gvar %in% names(alldata)) {
                    gfac <- as.factor(alldata[[gvar]])
                    gtab <- table(gfac)
                    nper <- as.integer(gtab)
                    names(nper) <- names(gtab)
                    # align counts to the labels used in the coefficients table
                    labs <- as.character(tab$lgroup)
                    nval <- nper[match(labs, names(nper))]
                    # replace unresolved with total N (best effort)
                    nval[is.na(nval)] <- sum(!is.na(gfac))
                    df <- data.frame(group = tab$lgroup, lhs = tab$lhs, rhs = tab$rhs,
                                     p = tab$pvalue, n = as.integer(nval), stringsAsFactors = FALSE)
                } else {
                    N <- nrow(alldata)
                    df <- data.frame(group = "All", lhs = tab$lhs, rhs = tab$rhs,
                                     p = tab$pvalue, n = as.integer(N), stringsAsFactors = FALSE)
                }
                out[[1]] <- df
            }

            d_raw <- do.call(rbind, out)
            # guard against zeros/negatives from underflow
            eps <- .Machine$double.xmin
            d_raw$p[!is.finite(d_raw$p) | is.na(d_raw$p)] <- NA
            d_raw <- d_raw[!is.na(d_raw$p) & is.finite(d_raw$p), , drop=FALSE]
            # summarise across replicates to get mean
            if (nrow(d_raw) > 0) {
                key <- d_raw[c("group","lhs","rhs","n")]
                id <- interaction(key, drop=TRUE)
                pmean <- tapply(d_raw$p, id, mean, na.rm=TRUE)
                keyu <- unique(key[match(names(pmean), as.character(id)),])
                d <- data.frame(keyu, p=as.numeric(pmean))
                d <- d[order(d$n), , drop=FALSE]
                # label each line as 'lhs <- rhs'
                d$lab <- paste(d$lhs, "<-", d$rhs)
            } else {
                d <- d_raw
            }
            if (nrow(d) == 0) {
                jmvcore::reject("No valid p-values computed for any subsample")
                return()
            }

            ttl <- "Mean p-values vs Sample Size"

            baseBreaks <- c(50, 100, 200, 500)
            if (nrow(d) > 0) {
                brks <- baseBreaks[baseBreaks >= min(d$n) & baseBreaks <= max(d$n)]
                if (length(brks) == 0) brks <- sort(unique(d$n))
            } else {
                brks <- baseBreaks
            }

            # If this image is keyed to a specific group, filter to it
            if (is.something(mg) && !is.null(image$state$gkey)) {
                d <- d[d$group == image$state$gkey, , drop=FALSE]
                ttl <- paste0(image$state$gkey, ": Mean p-values vs Sample Size")
            }

            # Build a base plot: X = sample size (n), Y = mean p-value
            p <- ggplot2::ggplot() +
                 ggplot2::scale_x_continuous(breaks = brks) +
                 ggplot2::scale_y_continuous(limits = c(0, 1)) +
                 ggplot2::geom_hline(ggplot2::aes(yintercept = 0.05, linetype = "p = 0.05"), color = "red", show.legend = TRUE) +
                 ggplot2::scale_linetype_manual(values = c("p = 0.05" = "dashed"), name = "") +
                 ggplot2::labs(x = "Sample size (n)", y = "Mean p-value", color = "Predictor", title = ttl) +
                 ggplot2::theme_minimal(base_size = 12)

            if (nrow(d) > 0) {
                # draw mean line (no CI bars)
                p <- p + ggplot2::geom_line(data=d, ggplot2::aes(x = n, y = p, color = lab, group = lab)) +
                        ggplot2::geom_point(data=d, ggplot2::aes(x = n, y = p, color = lab, group = lab))
            } else {
                p <- p + ggplot2::annotate("text", x = min(brks, na.rm=TRUE), y = 0.2, label = "No valid p-values to plot", hjust = 0)
            }

            print(p)
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

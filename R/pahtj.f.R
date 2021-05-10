#' Path Analysis
#'
#' Path Analysis
#' @param data the data as a data frame
#' @param endogenous a vector of strings naming the mediators from \code{data}
#' @param factors a vector of strings naming the fixed factors from
#'   \code{data}
#' @param covs a vector of strings naming the covariates from \code{data}
#' @param multigroup factor defining groups for multigroup analysis
#' @param se .
#' @param r2ci Choose the confidence interval type
#' @param r2test .
#' @param bootci Choose the confidence interval type
#' @param ci .
#' @param ciWidth a number between 50 and 99.9 (default: 95) specifying the
#'   confidence interval width for the parameter estimates
#' @param bootN number of bootstrap samples for estimating confidence
#'   intervals
#' @param showintercepts \code{TRUE} or \code{FALSE} (default), show
#'   intercepts
#' @param intercepts \code{TRUE} or \code{FALSE} (default), show intercepts
#' @param indirect \code{TRUE} or \code{FALSE} (default), show intercepts
#' @param contrasts a list of lists specifying the factor and type of contrast
#'   to use, one of \code{'deviation'}, \code{'simple'}, \code{'difference'},
#'   \code{'helmert'}, \code{'repeated'} or \code{'polynomial'}
#' @param showRealNames \code{TRUE} or \code{FALSE} (default), provide raw
#'   names of the contrasts variables
#' @param showContrastCode \code{TRUE} or \code{FALSE} (default), provide
#'   contrast coefficients tables
#' @param scaling a named vector of the form \code{c(var1='type',
#'   var2='type2')} specifying the transformation to apply to covariates, one of
#'   \code{'centered'} to the mean, \code{'standardized'},\code{'log'} or
#'   \code{'none'}. \code{'none'} leaves the variable as it is.
#' @param endogenousTerms a list of lists specifying the models for with the
#'   mediators as dependent variables.
#' @param diagram \code{TRUE} or \code{FALSE} (default), produce a path
#'   diagram
#' @param diag_paths Choose the diagram labels
#' @param diag_resid \code{TRUE} or \code{FALSE} (default), produce a path
#'   diagram
#' @param diag_labsize Choose the diagram labels
#' @param diag_rotate Choose the diagram labels
#' @param diag_type Choose the diagram labels
#' @param diag_shape Choose the diagram labels
#' @param diag_abbrev Choose the diagram labels
#' @param varcov a list of lists specifying the  covariances that need to be
#'   estimated
#' @param cov_y \code{TRUE} or \code{FALSE} (default), produce a path diagram
#' @param cov_x \code{TRUE} or \code{FALSE} (default), produce a path diagram
#' @param constraints a list of lists specifying the models random effects.
#' @param constraints_examples .
#' @param showlabels .
#' @param scoretest .
#' @param cumscoretest .
#' @param estimator Choose the diagram labels
#' @param likelihood Choose the diagram labels
#' @param formula (optional) the formula to use, see the examples
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$model} \tab \tab \tab \tab \tab The underlying \code{lavaan} object \cr
#'   \code{results$info} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$main} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$constraints} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$fit$indices} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$r2} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$coefficients} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$correlations} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$intercepts} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$defined} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$models$contrastCodeTable} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$pathgroup$diagrams} \tab \tab \tab \tab \tab an array of path diagrams \cr
#'   \code{results$pathgroup$notes} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$contraintsnotes} \tab \tab \tab \tab \tab a table \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$info$asDF}
#'
#' \code{as.data.frame(results$info)}
#'
#' @export
pathj <- function(
  data,
  endogenous = NULL,
  factors = NULL,
  covs = NULL,
  multigroup = NULL,
  se = "standard",
  r2ci = "fisher",
  r2test = FALSE,
  bootci = "perc",
  ci = TRUE,
  ciWidth = 95,
  bootN = 1000,
  showintercepts = TRUE,
  intercepts = TRUE,
  indirect = FALSE,
  contrasts = NULL,
  showRealNames = TRUE,
  showContrastCode = FALSE,
  scaling = NULL,
  endogenousTerms = list(
    list()),
  diagram = FALSE,
  diag_paths = "est",
  diag_resid = FALSE,
  diag_labsize = "medium",
  diag_rotate = "2",
  diag_type = "tree2",
  diag_shape = "rectangle",
  diag_abbrev = "0",
  varcov=NULL,
  cov_y = TRUE,
  cov_x = TRUE,
  constraints = list(),
  constraints_examples = FALSE,
  showlabels = FALSE,
  scoretest = TRUE,
  cumscoretest = FALSE,
  estimator = "ML",
  likelihood = "normal",
  formula) {
  
  if ( ! requireNamespace("jmvcore", quietly=TRUE))
    stop("pathj requires jmvcore to be installed (restart may be required)")
  
  if ( ! missing(formula)) {
    if (missing(endogenous))
      endogenous <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="endogenous")
    if (missing(endogenousTerms))
      endogenousTerms <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="endogenousTerms")
    if (missing(factors))
      factors <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="factors")
    if (missing(covs))
      covs <- pathjClass$private_methods$.marshalFormula(
        formula=formula,
        data=`if`( ! missing(data), data, NULL),
        name="covs")
  }
  
  if ( ! missing(endogenous)) endogenous <- jmvcore::resolveQuo(jmvcore::enquo(endogenous))
  if ( ! missing(factors)) factors <- jmvcore::resolveQuo(jmvcore::enquo(factors))
  if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
  if ( ! missing(multigroup)) multigroup <- jmvcore::resolveQuo(jmvcore::enquo(multigroup))
  if (missing(data))
    data <- jmvcore::marshalData(
      parent.frame(),
      `if`( ! missing(endogenous), endogenous, NULL),
      `if`( ! missing(factors), factors, NULL),
      `if`( ! missing(covs), covs, NULL),
      `if`( ! missing(multigroup), multigroup, NULL))
  
  for (v in factors) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  for (v in multigroup) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
  
  varcov<-lapply(varcov,function(v) {
    if (length(v)!=2)
       NULL
    else
      list(i1=v[[1]],i2=v[[2]])
  })

  options <- pathjOptions$new(
    endogenous = endogenous,
    factors = factors,
    covs = covs,
    multigroup = multigroup,
    se = se,
    r2ci = r2ci,
    r2test = r2test,
    bootci = bootci,
    ci = ci,
    ciWidth = ciWidth,
    bootN = bootN,
    showintercepts = showintercepts,
    intercepts = intercepts,
    indirect = indirect,
    contrasts = contrasts,
    showRealNames = showRealNames,
    showContrastCode = showContrastCode,
    scaling = scaling,
    endogenousTerms = endogenousTerms,
    diagram = diagram,
    diag_paths = diag_paths,
    diag_resid = diag_resid,
    diag_labsize = diag_labsize,
    diag_rotate = diag_rotate,
    diag_type = diag_type,
    diag_shape = diag_shape,
    diag_abbrev = diag_abbrev,
    varcov = varcov,
    cov_y = cov_y,
    cov_x = cov_x,
    constraints = constraints,
    constraints_examples = constraints_examples,
    showlabels = showlabels,
    scoretest = scoretest,
    cumscoretest = cumscoretest,
    estimator = estimator,
    likelihood = likelihood)
  
  analysis <- pathjClass$new(
    options = options,
    data = data)
  
  analysis$run()
  
  analysis$results
}


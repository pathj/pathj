build_indirect_estimate <- function(outcome_terms, mediator_terms, data) {
  options <- pathj:::pathjOptions$new(
    endogenous = c("y1", "y2"),
    covs = c("x1", "x2"),
    endogenousTerms = list(outcome_terms, mediator_terms),
    indirect = TRUE,
    scaling = lapply(c("y1", "y2", "x1", "x2"), function(variable) {
      list(var = variable, type = "none")
    })
  )
  datamatic <- pathj:::Datamatic$new(options, data)
  estimate <- pathj:::Estimate$new(options, datamatic)
  list(estimate = estimate, data = datamatic$cleandata(data))
}

testthat::test_that("indirect descriptions follow parameter identity across estimation", {
  data("pathjdata", package = "pathj")
  mediator <- stats::lm(y2 ~ x1 + x2, data = pathjdata)
  outcome <- stats::lm(y1 ~ y2 + x1 + x2, data = pathjdata)
  expected <- stats::coef(mediator)[c("x1", "x2")] * stats::coef(outcome)[["y2"]]

  for (outcome_terms in list(list("y2", "x1", "x2"), list("y2", "x2", "x1"))) {
    for (mediator_terms in list(list("x1", "x2"), list("x2", "x1"))) {
      model <- build_indirect_estimate(outcome_terms, mediator_terms, pathjdata)
      descriptions <- model$estimate$tab_defined
      model$estimate$tab_defined <- descriptions[rev(seq_len(nrow(descriptions))), ]
      model$estimate$estimate(model$data)
      defined <- model$estimate$tab_defined
      testthat::expect_equal(defined$desc, descriptions$desc[match(defined$lhs, descriptions$lhs)])
      for (predictor in names(expected)) {
        selected <- defined$desc == paste(predictor, "⇒ y2 ⇒ y1")
        testthat::expect_equal(sum(selected), 1L)
        testthat::expect_equal(defined$est[selected], unname(expected[[predictor]]), tolerance = 1e-6)
      }
      model$estimate$estimate(model$data)
      testthat::expect_equal(model$estimate$tab_defined$desc, defined$desc)
    }
  }
})

testthat::test_that("refreshing indirect estimates updates reordered descriptions", {
  data("pathjdata", package = "pathj")
  initial <- pathj::pathj(
    formula = list("y1 ~ y2 + x1 + x2", "y2 ~ x1 + x2"),
    data = pathjdata,
    indirect = TRUE
  )
  model <- build_indirect_estimate(list("y2", "x2", "x1"), list("x1", "x2"), pathjdata)
  model$estimate$estimate(model$data)
  pathj:::j.fill_table(initial$models$defined, model$estimate$tab_defined)
  refreshed <- initial$models$defined$asDF
  testthat::expect_equal(refreshed$desc, model$estimate$tab_defined$desc)
  testthat::expect_equal(refreshed$rhs, model$estimate$tab_defined$rhs)
  testthat::expect_equal(refreshed$est, model$estimate$tab_defined$est)
})

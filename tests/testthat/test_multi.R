context("fit")
tol<-.01
data("pathjdata")
pathjdata$groups_a<-factor(pathjdata$groups_a)
pathjdata$groups_b<-factor(pathjdata$groups_b)

forms<-list("y1~y2+x2+x1","y2~x1+ x2")
mod<-pathj::pathj(formula=forms,
                  data=pathjdata,
                  indirect = T,
                  multigroup = "groups_b",
                  diagram=T
                  
)

mod
testthat::test_that("indiect effects",{
  testthat::expect_equal(as.character(obj$lhs[3]),"IE3")        
  testthat::expect_equal(obj$est[2],.0743,tolerance=tol)        
  testthat::expect_equal(obj$z[4],-.661,tolerance=tol)        
  testthat::expect_equal(obj$ci.lower[3],-.0243,tolerance=tol)        
  
}
)

obj<-mod$models$coefficients$asDF

testthat::test_that("regressions",{
  testthat::expect_equal(as.character(obj$lhs[3]),"y1")        
  testthat::expect_equal(obj$est[2],.713,tolerance=tol)        
  testthat::expect_equal(obj$z[4],1.29,tolerance=tol)        
  testthat::expect_equal(obj$ci.lower[3],.9749,tolerance=tol)        
}
)



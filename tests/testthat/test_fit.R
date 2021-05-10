context("fit")
tol<-.01
data("pathjdata")
pathjdata$groups_a<-factor(pathjdata$groups_a)
pathjdata$groups_b<-factor(pathjdata$groups_b)

forms<-list("y1~y2+x2+x1","y2~x1+ x2+groups_a")
mod<-pathj::pathj(formula=forms,
             data=pathjdata,
             varcov=list("x1~~x2")
             )
mod
fits<-mod$fit$indices$asDF
  
testthat::test_that("fit table",{
  testthat::expect_equal(fits$aic,593.60,tolerance = tol)        
  testthat::expect_equal(fits$bic2,587.51,tolerance = tol)        
  testthat::expect_equal(fits$rmsea,.12003,tolerance = tol)        
}
)

obj<-mod$fit$main$asDF
testthat::test_that("fit table",{
  testthat::expect_equal(obj$chisq[1],4.8817,tolerance = tol)        
  testthat::expect_equal(obj$df[2],9,tolerance = tol)        
  testthat::expect_equal(obj$pvalue[2],0,tolerance = tol)        
}
)

forms<-list("y1~y2+x2+x1","y2~x1+ x2+groups_a")
mod<-pathj::pathj(formula=forms,
                  data=pathjdata,
                  varcov = NULL,
                  showlabels = T,
                  constraints = list("c1:=p1*p2","c1==0"),
                  r2test = T
)

obj<-mod$models$r2$asDF
anote<-mod$models$r2$notes[[1]]$note
enote<-"Some inferential tests cannot be computed for this model"

testthat::test_that("r2 table not working",{
  testthat::expect_equal(as.character(obj$lhs[2]),"y2")        
  testthat::expect_equal(obj$r2[1],.8472,tolerance = tol)        
  testthat::expect_true(is.na(obj$chisq[1]))        
  testthat::expect_equal(anote,enote)        
}
)

obj<-mod$fit$constraints$asDF

testthat::test_that("r2 table",{
  testthat::expect_equal(obj$chisq[1],4.1727,tolerance=tol)        
  testthat::expect_equal(as.character(obj$op),"==")        
  testthat::expect_equal(obj$df,1)        
}
)


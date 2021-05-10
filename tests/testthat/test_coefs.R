context("fit")
tol<-.01
data("pathjdata")
pathjdata$groups_a<-factor(pathjdata$groups_a)
pathjdata$groups_b<-factor(pathjdata$groups_b)

forms<-list("y1~y2+x2+x1","y2~x1+ x2+groups_a")
mod<-pathj::pathj(formula=forms,
                  data=pathjdata,
                  indirect = T
)
mod

forms<-list("y1~x2+x1")
mod<-pathj::pathj(formula=forms,
                  data=pathjdata,
                  varcov = list(c("x1","x2")),
                  showlabels = T,
                  r2test = T
)

obj<-mod$models$correlations$asDF
obj2<-grep("variance",mod$info$notes[[1]]$note)
testthat::test_that("random exo",{
  testthat::expect_equal(as.character(obj$user[1]),"Estim")        
  testthat::expect_equal(obj$std.all[3],1,tolerance=tol)        
  testthat::expect_equal(obj$ci.upper[1],1.1173,tolerance=tol)        
  testthat::expect_equal(obj2,1,tolerance=tol)        
  
}
)

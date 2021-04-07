install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))


jmvtools::version()
jmvtools::check()
library(jamm)
installme<-function() {
  s<-sessionInfo()
  pv<-s$otherPkgs$jamm$Version
  zf<-yaml::read_yaml("jamovi/0000.yaml")
  zv<-zf$version
  h<-git2r::repository_head()
  gv<-gsub("Version.","",h$name,fixed = T)
  gv<-gsub("version.","",gv,fixed = T)
  cat("yaml version:",zv,"\n")
  cat("pack version:",pv,"\n")
  cat("git version:",gv,"\n")
  
  if (all(c(pv,zv)==gv))
    jmvtools::install(home = "flatpak")
  else
    warning("versions mismatch")
  
}
installme()

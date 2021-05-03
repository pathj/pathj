install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

jmvtools::version()
jmvtools::check()

source("local/functions.R")
installme("pathj")


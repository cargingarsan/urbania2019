
############
##########
#
library(parallelly)
availableCores() 


slow_sqrt <- function(x) {
  Sys.sleep(1)
  sqrt(x)
}



library(foreach)

tic()

z <- foreach(i=1:10) %do% {
  slow_sqrt(i)
}

toc()

##############
#############
library(tictoc)

x<- 1:10

tic()

result <- lapply(x, slow_sqrt)

toc()

#usando future.apply

library(future.apply)
plan(multisession, workers = 6)

tic()

result <- future_lapply(x, slow_sqrt)

toc()


##############

#paralel for loops

z <- list()
for (i in 1:10){
  z[i] <- slow_sqrt(i)
}


#Usando foreach() para el mismo loop

z <- foreach(i = 1:10) %do% {
  slow_sqrt(i)
}

###############
#version sequencial

library(foreach)

tic()

z <- foreach(i = 1:10) %do% {
  slow_sqrt(i)
}
toc()




library(doFuture)

tic()

z <- foreach (i = 1:10) %dofuture% {
  slow_sqrt(i)
}

toc()

#cerrar plan multisession
plan(sequential)


################
#################
################

#reemplazar por codigo c++











#################################################################################


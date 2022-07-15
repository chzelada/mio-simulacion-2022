## Paradoja del cumpleañero

### exploracion
N <- 40
grupo <- sample(1:364,N,replace=TRUE)

vgrupo <- as.vector(table(grupo))
sum(vgrupo>1)


#### funcion que devuelve Verdader si hay elementos repetidos

hay_repetidos <- function(vec){
  vgrupo <- as.vector(table(vec))
  x<-sum(vgrupo>1)
  return(x>0)
}


sim_pdc <- function(id,N=20){
  grupo <- sample(1:364,N,replace=TRUE)
  return(hay_repetidos(grupo))
}

prob_pdc <- function(nsim=100,N=20){
  casos <- sum(sapply(1:nsim, sim_pdc,N=N))
  return(casos/nsim)
}

prob_pdc(nsim=10000, N=30)











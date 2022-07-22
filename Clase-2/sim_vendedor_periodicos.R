### Vendedor de periodicos
### Hay tres tipos de días de noticia, “Excelente”, “Bueno” y “Malo”, 
### con probabilidades de 0.35, 0.45 y 0.20 respectivamente. 

set.seed(123, kind = 'Mersenne-Twister', normal.kind = 'Inversion')

tipo_de_dia <- c('Excelente', 'Bueno', 'Malo')
pdia <- c(0.35,0.45,0.2)

demanda <- function(tdia){
  dist_dia_excelente <- c(0.03,0.05,0.15,0.20,0.35,0.15,0.07)
  dist_dia_bueno<-c(0.10,0.18,0.40,0.20,0.08,0.04,0.00)
  dist_dia_malo <- c(0.44,0.22,0.16,0.12,0.06,0.00,0.00)
  demanda <- (4:10)*10
  if(tdia=='Excelente'){
    return(sample(demanda,1,prob = dist_dia_excelente))
  } else if (tdia=='Bueno'){
    return(sample(demanda,1,prob = dist_dia_bueno))
  }
  return(sample(demanda,1,prob = dist_dia_malo))
}

ganancia <- function(demanda,oferta){
  if(demanda<oferta){
    reciclaje <- -(oferta-demanda)*0.28
    ganancia <- reciclaje+demanda* 0.17
    return(ganancia)
  } else if(demanda>oferta){
    ganancia =oferta*0.17-(demanda-oferta)*0.17
    return(ganancia)
  } else{
    return(demanda*0.17)
  }
}



simular_mes <- function(x,oferta){
  tipo_de_dia <- c('Excelente', 'Bueno', 'Malo')
  pdia <- c(0.35,0.45,0.2)
  mes <- sample(tipo_de_dia,20,replace = TRUE,prob = pdia)
  demanda_por_dia <- sapply(mes,'demanda')
  tganancia <- sum(sapply(demanda_por_dia, 'ganancia', oferta=oferta))
  return(tganancia)
  }

n_meses <-
mean(n_meses)

n_meses <- function(oferta,nsim){
  sim <- sapply(1:nsim,'simular_mes',oferta=oferta)
  return(mean(sim))
}

ganacia_promedia <- sapply((4:10)*10, n_meses, nsim=5000)

plot((4:10)*10,ganacia_promedia)

View(data.frame(oferta=(4:10)*10, ganacia_promedia))


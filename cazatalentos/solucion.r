#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function()
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total,
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

# Calculo parámetros de los jugadores seleccionados de todo el grupo
# y excluyendo al lider
planilla_parametros  <- function( planilla_jugadores, ids )
{
  leader_pos <- planilla_jugadores[ ids, which.max(promedio_general) ]
  leader <- planilla_jugadores[leader_pos, id]
  the_rest <- ids[ids != leader]

  mean <- planilla_jugadores[ids, mean(promedio_general)]
  sd <- planilla_jugadores[ids, sd(promedio_general)]
  max <- planilla_jugadores[ids, max(promedio_general)]
  min <- planilla_jugadores[ids, min(promedio_general)]

  mean_rest <- planilla_jugadores[the_rest, mean(promedio_general)]
  sd_rest <- planilla_jugadores[the_rest, sd(promedio_general)]
  max_rest <- planilla_jugadores[the_rest, max(promedio_general, max)]
  min_rest <- planilla_jugadores[the_rest, min(promedio_general, min)]

  return(
         list("mean"=mean,
              "sd"=sd,
              "max"=max,
              "min"=min,
              "mean_rest"=mean_rest,
              "sd_rest"=sd_rest
         )
  )
}

Estrategia_D <- function( verbose = FALSE )
{
  #Estrategia
  #Se juegan varias rondas.
  # Se dividen en grupos de a 2 aleatoriamente.
  # Luego en cada ronda siguiente se eliminan grupos con varianza muy chica en tiros totales.
  # El grupo con mayor varianza será con alta probabilidad el que contenga al crack.

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  #Ronda 1  -------- PRECALENTAMIENTO (COLD BOOT) ---------------------
  #tiran los 100 jugadores es decir 1:100  XX  tiros libres cada uno
  # Grupos de a 2
  ids_juegan_coldboot  <- 1:100   #los jugadores que participan en la ronda,

  coldboot_rounds <- 60
  planilla_cazatalentos[ ids_juegan_coldboot, tiros_coldboot := coldboot_rounds ]  #registro en la planilla que tiran 70 tiros
  planilla_cazatalentos[ ids_juegan_coldboot, tiros_total := coldboot_rounds ]

  resultado_coldboot  <- gimnasio_tirar( ids_juegan_coldboot, coldboot_rounds)

  planilla_cazatalentos[ ids_juegan_coldboot, aciertos_coldboot := resultado_coldboot ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_coldboot, aciertos_total := resultado_coldboot ] # registro los aciertos totales
  ids_last_round <- ids_juegan_coldboot


  # Juego más rondas, en cada una descarto el grupo que tiene menor puntuación total
  rounds_step <- 40
  num_rounds <- 15
  for ( iter in 1:num_rounds )
  {
    planilla_cazatalentos[ids_last_round, promedio_general := aciertos_total / tiros_total]

    params <- planilla_parametros(planilla_cazatalentos, ids_last_round)
    num_sd = 3.75
    if (iter >= 2)
    {
      num_sd = 2
    }

    promedio_corte <- params$max - num_sd*params$sd_rest

    if (verbose) {
      cat("Iteración ", iter, "Promedio general", promedio_corte, "\n")
      cat("Iteración ", iter, "Promedio máximo", planilla_cazatalentos[ids_last_round, max(promedio_general)], "\n")
    }

    ids_next_round <- planilla_cazatalentos[ ids_last_round ][ promedio_general >= promedio_corte, id]

    if (length(ids_next_round) == 0)
    {
      if (verbose) {
        cat("No more players on iteration",iter, "!\n")
      }
      #break
      ids_next_round <- ids_last_round
    } # Corto si me quedo sin jugadores. Uso los anteriores ?

    # Si me voy a pasar, corto el experimento
    if (GLOBAL_tiros_total + length(ids <- ids_next_round) * rounds_step > 15000)
    {
      if (verbose) {
        cat("Me paso de la cantidad en iteración ",iter, ", corto el experimento!\n")
      }
      break
    }

    if (verbose) {
        cat("Total jugadores en ronda ", iter, " = ", length(ids_next_round),
            ", promedio máx = ", planilla_cazatalentos[ids_next_round, max(promedio_general)],
            ", tiros = ", planilla_cazatalentos[ids_next_round, max(tiros_total)] + rounds_step, "\n")
    }

    cumulativo_last_round <- planilla_cazatalentos[ ids_next_round, aciertos_total ]

    resultado_ronda <- gimnasio_tirar(ids_next_round, rounds_step)
    cumulativo_ronda <- rowSums(cbind(cumulativo_last_round, resultado_ronda))


    planilla_cazatalentos [ ids_next_round, aciertos_total := cumulativo_ronda ]
    planilla_cazatalentos [ ids_next_round, tiros_total := tiros_total + rounds_step ]
    ids_last_round <- ids_next_round
  }


  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en total
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos_total) ]
  #cat(" Posición Mejor = ", pos_mejor, "\n")
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )

  return( veredicto )

}

#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia D

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 100 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_D()

  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

cat("\n")
cat(tiros_total)
cat("\n")
cat(tasa_eleccion_correcta)
cat("\n")

#Es una sábana corta ...



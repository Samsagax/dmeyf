#Incorpora el  max_bin  a la Bayesian Optimization
#Atencion,  si se quiere un max_bin mayor a 255, se debe DUPLICAR la cantidad de memoria RAM

#Este script necesita
# 64 GB de memoria RAM
# 256 GB de espacio en disco
# 8 vCPU
# demora 22 horas en correr

#Optimización bayesiana

#Dataset
#Campos originales mas  lag1 y delta1

#Entreno en [202001, 202009]  con undersampling al 10% de los CONTINUA
#Valido en la mitad de 202011
#Testeo en la otra mitad de 202011

#Algoritmo
#Corro lightGBM con los mismos parametros cambiaando   kcantidad_semillas=10    semillas
#y promedio esos modelos
#NO hago el promedio de las probabilidades, sino que promedio la posicion en el ranking de ese registro

#Optimización bayesiana
#En lugar de leaf_size y  num_leaves que son ABSOLUTOS,  optimizo  gleaf_size y gnum_leaves que seran RELATIVOS


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("primes")
require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )


kexperimento  <- NA #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript       <- "1421_lgbm_maxbin"

karchivo_dataset   <-  "./datasets/semillerio_dataset_lag1.csv.gz"

kfecha_cutoff  <- 202001
ktrain_desde   <- 202001
ktrain_hasta   <- 202009


kBO_iter    <-  120   #cantidad de iteraciones de la Optimizacion Bayesiana

kcantidad_semillas  <- 10


#Aqui se cargan los hiperparametros
#ATENCION  se juega con  gleaf_size y gnum_leaves
hs <- makeParamSet( 
         makeIntegerParam("max_bin",          lower=  31      , upper=   32),
         makeNumericParam("learning_rate",    lower=   0.02   , upper=    0.2),
         makeNumericParam("feature_fraction", lower=   0.1    , upper=    1.0),
         makeNumericParam("gleaf_size",       lower=  20.0    , upper=  100.0),
         makeNumericParam("gnum_leaves",      lower=   0.01   , upper=    1.0)
        )


ksemilla_azar  <- 102191  #Aqui poner la propia semilla

#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
# Particiona un dataset en forma estratificada

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------


fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #solo sumo 48750 si vpesos > 1, hackeo 
  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 48750, -1250 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]

  gan  <-  tbl[ , max(gan_acum) ]

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Conversion de los hiperparametros "g"  a los hiperparametros que acepta lightGBM
#la explicacion sera vista en clase01

trafo_hiperparametros  <- function( x, registros_cantidad )
{
  vmin_data_in_leaf  <- pmax( 4 , as.integer( round( registros_cantidad /(1+ exp(x$gleaf_size/10.0) ) ) ) )
  max_leaves         <- as.integer( 1 + registros_cantidad/ vmin_data_in_leaf )
  vnum_leaves        <- pmin(  pmax(  2,  as.integer( round(x$gnum_leaves*max_leaves)) ), 100000 )

  y  <- x

  y$gleaf_size  <- NULL
  y$gnum_leaves <- NULL
  y$min_data_in_leaf  <- vmin_data_in_leaf
  y$num_leaves  <-  vnum_leaves

  return( y )
}
#------------------------------------------------------------------------------


EstimarGanancia_lightgbm  <- function( x )
{
  gc()
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #validacion es una mitad de 202011
  dvalid  <- lgb.Dataset( data=    data.matrix(  dapply[ fold==1, campos_buenos, with=FALSE]),
                          label=   dapply[ fold==1, clase01],
                          weight=  dapply[ fold==1, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                          free_raw_data= FALSE
                        )

  #genero el dataset de training con el formato que necesita LightGBM
  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ train==1 , campos_buenos, with=FALSE]),
                          label=   dataset[ train==1, clase01],
                          weight=  dataset[ train==1, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                          free_raw_data= FALSE
                        )


  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          early_stopping_rounds= 200,
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  param_basicos$early_stopping_rounds  <-  as.integer(200 + 4/x$learning_rate )


  #armo los parametros para training
  param_trans_train     <- trafo_hiperparametros( x,  nrow(dtrain) )
  param_completo_train  <- c( param_basicos, param_trans_train )


  tb_predtest  <- as.data.table(  list(  "rank_acum" = rep( 0, nrow(dapply) ),
                                         "gan"=  ifelse( dapply$clase_ternaria=="BAJA+2", 48750, -1250 ),
                                         "fold"=  dapply$fold
                                       ) )


  tb_resultados  <- data.table( "semilla" = integer(),
                                "isemilla" = integer(),
                                "num_iterations" =  integer(),
                                "pos_actual"     =  integer(),
                                "pos_ratio_actual" = numeric(),
                                "gan_validate" = numeric(),
                                "gan_test"     = numeric(),
                                "pos_acum"     = numeric(),
                                "pos_ratio_acum"= numeric(),
                                "gan_validate_acum" = numeric(),
                                "gan_test_acum"     = numeric()
                              )


  isemilla  <- 0
  for( semilla in ksemillas )   #calculo para las semillas
  {
    gc()
    isemilla  <- isemilla + 1
    param_completo_train$seed  <- semilla  #cambio la semilla donde entreno

    set.seed( semilla )
    modelo_train  <- lgb.train( data= dtrain,
                                valids= list( valid= dvalid ),
                                eval= fganancia_lgbm_meseta,
                                param= param_completo_train,
                                verbose= -100 ) 

    prediccion  <- frank( predict( modelo_train, data.matrix( dapply[ , campos_buenos, with=FALSE]) ) , ties.method="random")

    tb_predtest[ , rank_actual :=  prediccion ]          #el ultimo modelo
    tb_predtest[ , rank_acum := rank_acum + prediccion ]   #acumulo el ranking

    #El actual sobre  validation
    tb_meseta  <- copy(tb_predtest[ fold==1 ])
    setorder( tb_meseta, -rank_actual )
    tb_meseta[ , gan_acum := cumsum(gan) ]
    pos_actual  <- which.max( tb_meseta$gan_acum )      #el punto de corte optimo en validation
    pos_ratio_actual  <-  pos_actual / nrow(tb_meseta)
    ganancia_validate_actual  <- tb_meseta[ 1:pos_actual, sum(gan) ]

    #El acumulado sobre  training
    setorder( tb_meseta, -rank_acum )
    tb_meseta[ , gan_acum := cumsum(gan) ]
    pos_acum  <- which.max( tb_meseta$gan_acum )
    pos_ratio_acum  <- pos_acum / nrow(tb_meseta)
    ganancia_validate_acum  <- tb_meseta[ 1:pos_acum, sum(gan) ]

    tb_meseta  <- copy(tb_predtest[ fold==2 ])
    setorder( tb_meseta, -rank_actual )
    ganancia_test_actual  <- tb_meseta[ 1:pos_actual, sum(gan) ]

    setorder( tb_meseta, -rank_acum )
    ganancia_test_acum <-  tb_meseta[ 1:pos_acum, sum(gan) ]


    tb_resultados  <- rbind( tb_resultados, 
                             list( semilla,
                                   isemilla,
                                   modelo_train$best_iter,
                                   pos_actual,
                                   pos_ratio_actual,
                                   ganancia_validate_actual,
                                   ganancia_test_actual,
                                   pos_acum,
                                   pos_ratio_acum,
                                   ganancia_validate_acum,
                                   ganancia_test_acum ) )

    #logueo 
    xx  <- param_completo_train
    xx$gleaf_size   <- x$gleaf_size
    xx$gnum_leaves  <- x$gnum_leaves
    xx <-  c( xx, tb_resultados[ nrow(tb_resultados) ]  )
    xx$iteracion_bayesiana  <- GLOBAL_iteracion
    loguear( xx,  arch= klog_semillas )

  }


  #logueo final
  xx  <- param_completo_train
  xx$seed  <- NULL
  xx$gleaf_size   <- x$gleaf_size
  xx$gnum_leaves  <- x$gnum_leaves
  
  xx$semilla   <- NULL
  xx$isemilla  <- NULL
  
  xx$acumulaciones      <- nrow( tb_resultados )
  xx$num_iterations     <- tb_resultados[ , mean(num_iterations) ]
  xx$pos_ratio_actual   <- tb_resultados[ , mean(pos_ratio_actual) ]
  xx$gan_validate       <- tb_resultados[ , mean(gan_validate) ]
  xx$gan_test           <- tb_resultados[ , mean(gan_test) ]


  xx$pos_ratio_acum     <- tb_resultados[  nrow(tb_resultados) , pos_ratio_acum ]
  xx$gan_validate_acum  <- tb_resultados[  nrow(tb_resultados) , gan_validate_acum ]
  xx$gan_test_acum      <- tb_resultados[  nrow(tb_resultados) , gan_test_acum ]

  xx$iteracion_bayesiana  <- GLOBAL_iteracion
    
  loguear( xx,  arch= klog )

  return( tb_resultados[  nrow(tb_resultados) , gan_test_acum ] )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work

kbayesiana     <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".RDATA" )
klog           <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_BOlog.txt" )
klog_semillas  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_BOsemillas.txt" )

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog)
  GLOBAL_iteracion  <- nrow( tabla_log ) -1
} else {
  GLOBAL_iteracion  <- 0
}


set.seed( 102191 )   #dejo fija esta semilla

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:kcantidad_semillas ]   #me quedo con kcantidad_semillas primos al azar

#cargo el dataset que tiene los 36 meses
dataset  <- fread(karchivo_dataset)
dataset  <- dataset[ foto_mes >= kfecha_cutoff ]
gc()
setorder( dataset,  foto_mes, numero_de_cliente )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
dataset[ , train:=0L]


dapply  <- copy( dataset[  foto_mes %in% c(202011) ] )
particionar( dapply,  c(1,1), agrupa="clase_ternaria", seed=17 )
  
particionar( dataset,  c(1,9), agrupa=c("foto_mes","clase_ternaria"), campo="subsampling", seed=17 )

#reduzco el tamaño del dataset y libero memoria
dataset <-  dataset[  (clase01==1  | subsampling==1) ]
gc()

campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "fold", "train", "subsampling" ) )


#undersampling para training
dataset[  , train := 0L ]
dataset[  foto_mes>=ktrain_desde & foto_mes<=ktrain_hasta &
          (clase01==1  | subsampling==1),
          train := 1L ]


#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


#-------------------------------------------------------

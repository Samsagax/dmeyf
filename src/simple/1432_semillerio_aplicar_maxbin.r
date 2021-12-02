#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("primes")

require("lightgbm")


setwd("~/buckets/b1/")

karch_dataset  <- "./datasets/semillerio_dataset_lag1.csv.gz"
ksalida  <- "semillerio" 

kexperimento  <- NA
kscript         <- "s1432"


kcantidad_semillas  <- 200

#ATENCION
#aqui van unos hiperparametros muy distintos a los ganadores
#en este caso, la iteracion bayesiana 81,  la octava con mejor ganancia
x  <- list()
x$gleaf_size   <-  78.7820840103
x$gnum_leaves  <-  0.0807201476
x$learning_rate <-  0.1600471786
x$feature_fraction <-  0.6166306179
x$max_bin  <-  31
x$num_iterations  <-   165
x$pos_ratio  <- 0.0405295708


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

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  

  data[ ,  (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
            by= agrupa ]
}
#------------------------------------------------------------------------------


setwd("~/buckets/b1/")

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work
dir.create( paste0( "./kaggle/E",  kexperimento, "/" ) )   #creo carpeta del experimento dentro de kaggle
dir.create( paste0( "./kaggle/E",  kexperimento, "/meseta/" ) )   #creo carpeta del experimento dentro de kaggle

kkaggle       <- paste0("./kaggle/E",kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kkagglemeseta <- paste0("./kaggle/E",kexperimento, "/meseta/E",  kexperimento, "_", kscript, "_" )


set.seed( 102191 )   #dejo fija esta semilla
#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:kcantidad_semillas ]   #me quedo con CANTIDAD_SEMILLAS primos al azar

#cargo el dataset donde voy a entrenar
dataset  <- fread(karch_dataset)

dataset  <- dataset[ foto_mes >= 202001 ]
gc()


setorder( dataset,  foto_mes, numero_de_cliente )

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


dataset[  , generacion := 0L ]
dataset[  foto_mes>=202001 & foto_mes<=202011, generacion := 1L ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "generacion") )


dfuturo  <- copy( dataset[ foto_mes==202101 ] )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ generacion==1, campos_buenos, with=FALSE]),
                        label= dataset[ generacion==1, clase01] )



#Hago la transformacion de los hiperparametros
x$min_data_in_leaf  <- pmax( 4 , as.integer( round( nrow(dtrain) /(1+ exp(x$gleaf_size/10.0) ) ) ) )
max_leaves          <- as.integer( 1 + nrow(dtrain) / x$min_data_in_leaf )
x$num_leaves        <- pmin(  pmax(  2,  as.integer( round(x$gnum_leaves*max_leaves)) ), 100000 )


#Aqui se deben cargar los parametros
param_buenos  <- list( objective= "binary",
                       metric= "custom",
                       first_metric_only= TRUE,
                       boost_from_average= TRUE,
                       feature_pre_filter= FALSE,
                       verbosity= -100,
                       seed= 484201,
                       max_depth=  -1,
                       max_bin= x$max_bin,
                       min_gain_to_split= 0.0,
                       lambda_l1= 0.0,
                       lambda_l2= 0.0, 
                       num_iterations= x$num_iterations,
                       learning_rate=  x$learning_rate,
                       feature_fraction= x$feature_fraction,
                       min_data_in_leaf=  x$min_data_in_leaf,
                       num_leaves= x$num_leaves
                     )


#inicializo donde voy a guardar los resultados
tb_predicciones  <- as.data.table( list( predicciones_acumuladas = rep( 0, nrow(dfuturo) ) ) )


isemilla  <- 0

for( semilla in  ksemillas)
{
  gc()

  isemilla  <- isemilla + 1
  cat( isemilla, " " )  #imprimo para saber por que semilla va, ya que es leeentooooo

  param_buenos$seed  <- semilla   #aqui utilizo la semilla
  #genero el modelo
  set.seed( semilla )
  modelo  <- lgb.train( data= dtrain,
                        param= param_buenos )

  #aplico el modelo a los datos nuevos
  prediccion  <- frank(  predict( modelo, 
                                  data.matrix( dfuturo[ , campos_buenos, with=FALSE ]) ) )

  tb_predicciones[  , predicciones_acumuladas :=  predicciones_acumuladas +  prediccion ]  #acumulo las predicciones
  tb_predicciones[  , paste0( "pred_", isemilla ) :=  prediccion ]  #guardo el resultado de esta prediccion


  if(  isemilla %% 10 == 0 )  #imprimo cada 10 semillas
  {
    #Genero la entrega para Kaggle
    entrega  <- as.data.table( list( "numero_de_cliente"= dfuturo[  , numero_de_cliente],
                                     "prob"= tb_predicciones$predicciones_acumuladas ) ) #genero la salida

    setorder( entrega, -prob )

    #genero la salida oficial, sin mesetas
    entrega[ ,  Predicted := 0L ]
    cantidad_estimulos  <-  as.integer( nrow(dfuturo)*x$pos_ratio )
    entrega[ 1:cantidad_estimulos,  Predicted := 1L ]  #me quedo con los primeros

    #genero el archivo para Kaggle
    fwrite( entrega[ , c("numero_de_cliente","Predicted"), with=FALSE], 
            file=  paste0(  kkaggle, isemilla, ".csv" ),  
            sep= "," )



    for(  corte  in seq( 11000, 14000, 1000) ) #imprimo cortes en 10000, 11000, 12000, 13000, 14000 y 15000
    {
      entrega[ ,  Predicted := 0L ]
      entrega[ 1:corte,  Predicted := 1L ]  #me quedo con los primeros

      #genero el archivo para Kaggle
      fwrite( entrega[ , c("numero_de_cliente","Predicted"), with=FALSE], 
              file=  paste0(  kkagglemeseta, isemilla, "_",corte, ".csv" ),  
              sep= "," )
    }
  }


}


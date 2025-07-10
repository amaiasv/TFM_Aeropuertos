################################################################################
##                                                                            ##
##  Proyecto: Análisis avanzado y predicción de series temporales aplicados   ##
##                          al estudio de retrasos en aeropuertos             ##
##                                                                            ##
##  Autor: Amaia Sainz Villa                                                  ##
##  Fecha: Curso 2024/2025                                                    ##
##                                                                            ##
################################################################################



########----------------------------------------------------------------########
########                             Librerías                          ########
########----------------------------------------------------------------########


# Carga de librerías necesarias para realizar el trabajo

library(readxl) # Cargar datos
library(e1071) # Calcular algunos estadísticos
library(imputeTS) # Interpolación
library(cluster) # Requerida por TSclust
library(pdc) # Requerida por TSclust
library(TSclust) # Calcular similitudes entre series temporales
library(MASS) # Escalado Multidimensional
library(forecast) # Predicción con los modelos SARIMA y el método TBATS
library(tsfknn) # Predicción con el método de los k vecinos más próximos
library(rnn) # Predicción con LSTM
library(ForecastComb) # Predicción con combinaciones de predicciones
library(xts) # Para aplicar antes de representar
library(dygraphs) # Representación de series temporales
library(tidyverse) # Sirve para el proceso general del script
library(fpp2) # Sirve para realizar predicciones
library(zoo) # Sirve para tratamiento de las series temporales
library(factoextra) # Sirve para graficar el método Silhouette
install.packages("gplots") # Sin este no se instala GDM
install.packages("https://cran.r-project.org/src/contrib/Archive/GMD/GMD_0.3.3.tar.gz", 
                 repos = NULL, type = "source") 
# Instalación de una versión específica del paquete GMD desde archivo fuente
library(GMDH) # Modelado de redes GMDH 
library(ggplot2) # Visualización de datos elegante y personalizable basada en 
# la gramática de gráficos
library(tsoutliers) # Detección de valores atípicos en series temporales
library(tfruns) # Gestión de ejecuciones de modelos con TensorFlow
library(tibbletime) # Extensión de tibbles para análisis de datos temporales
library(ggpubr) # Crear gráficos más pulidos para publicaciones
library(gridExtra) # Organización de múltiples gráficos en una misma página
library(grid) # Sistema de gráficos base para control avanzado sobre el layout 
library(openxlsx) # Lectura y escritura de archivos Excel sin necesidad de Java
library(reticulate) # Interfaz para ejecutar código Python desde R
library(tensorflow) # Acceso a la API de TensorFlow para deep learning desde R
library(keras) # Construcción y entrenamiento de redes neuronales profundas



########----------------------------------------------------------------########
########                       Ficheros de entrada                      ########
########----------------------------------------------------------------########


# Se parte de los datos de Excel

# Hay un fichero para los retrasos y otro para el tráfico
# Se emplea la ruta donde estén los ficheros
series <- read_excel("D:/MASTER/TFM/datos_Delay_2017_09.xlsx")
series_traf <- read_excel("D:/MASTER/TFM/datos_traf_2017_09.xlsx")

# El resultado se guarda en el objeto series, siendo este un dataframe
tidy.name.vector <- make.names(colnames(series), unique=TRUE)
colnames(series) <- tidy.name.vector # para que no haya espacios en los nombres

tidy.name.vector_traf <- make.names(colnames(series_traf), unique=TRUE)
colnames(series_traf) <- tidy.name.vector_traf # para que no haya espacios en los nombres



########----------------------------------------------------------------########
########                          Visualización                         ########
########----------------------------------------------------------------########


# Se visualizan primero de una en una las series iniciales de los aeropuertos

# Nombres de las columnas
colnames(series)
columnas <- colnames(series)[-1] # Quita la columna de fecha

# Representación
for (col in columnas) {
  print(ggplot(series, aes(x = Date, y = .data[[col]])) + geom_line())
}


# Se visualizan ahora todas las paradas a la vez
paradas <- as.matrix(series[,-c(1,1)]) # Quita las columnas de fechas

# Transforma los datos para que pueda trabajarse sobre ellos fácilmente
data <- data.frame(Date = rep(series$Date,ncol(paradas)), Delays = as.vector(paradas), 
                   AIRPORT = rep(colnames(paradas), each = length(series$Date)))

# Representación de todos a la vez
p <- ggplot(data=data, aes(x=Date, y=Delays, group=AIRPORT, colour=AIRPORT)) + 
  geom_line(size=0.5) + scale_colour_viridis_d() +
  geom_point(aes(x=Date, y=Delays, group=AIRPORT, colour=AIRPORT), size=1) + 
  theme_minimal() +
  theme(legend.position = "none", axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_datetime(date_breaks="1 days", date_labels = "%b %d")
p



########----------------------------------------------------------------########
########              Transformaciones y comprobaciones                 ########
########----------------------------------------------------------------########


# Se verifica que no hay series constantes 

# Se comprueba que la desviación estándar es diferente a cero
series_todo <- series
series <- series[,-c(1,1)] # Matriz de datos, sin nombres

# Cálculo de la desviación estándar para cada serie
desviacion <- apply(series, MARGIN = 2, FUN = sd) 
cond <- desviacion != 0 # Devuelve TRUE o FALSE si es distinta de cero
cond

# Busca si hay alguna variación
cond2 <- which(cond == T) # Indica qué series tienen desviación distinta de cero
length(cond2) # 50 -> No hay series planas 

# Se eliminan variables para no ocupar memoria
rm(desviacion, cond, cond2)


# Se transforman a formato de serie temporal

# La frecuencia era de 24 horas en un día. 
# Se tienen datos de 7 a 23 = 17 horas 
# Mes de septiembre = 30 dias x 17 horas = 510 observaciones (filas)
# 50 columnas = 50 aeropuertos
series <- ts(series, frequency = 17)

# Visualización de las series con el nuevo formato
View(series)



########----------------------------------------------------------------########
########                           Clustering                           ########
########----------------------------------------------------------------########


# Estandarización de las series ((observación - media) / desviación)
series_st <- scale(series)

# Trasposición, aeropuertos por filas 
series_tp <- t(series_st) # Permite clusters con distintas medidas


# Se calcula la distancia de autocorrelación
inicio <- Sys.time()
IP.dis <- diss(series_tp, "ACF")
fin <- Sys.time()
temp_cor <- fin - inicio; temp_cor # 2.573129 secs
IP.dis


# Se visualiza el número óptimo de clusters a partir de la matriz de distancias

# Método del codo
p1 <- fviz_nbclust(series_tp, FUNcluster = hcut, method = "wss", k.max = 10, 
                   diss = IP.dis) + 
  geom_vline(xintercept = 3, linetype = 2,)+
  labs(subtitle = "Elbow method")  + 
  geom_line(aes(group = 1), color = "steelblue", size = 1.5) + 
  geom_point(group = 1, size = 5, color = "steelblue")+ 
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"), 
    title = element_text(size = 16, face = "bold")
  )
p1

# Método de la silueta
p2 <- fviz_nbclust(series_tp, FUNcluster = hcut, method = "silhouette", k.max = 20, 
                   dmatrix = IP.dis)+
  labs(subtitle = "Silhouette method")+ 
  geom_line(aes(group = 1), color = "steelblue", size = 1.5) + 
  geom_point(group = 1, size = 5, color = "steelblue")+ 
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"), 
    title = element_text(size = 16, face = "bold")
  )
p2

# Para visualizar las dos a la vez
grid.arrange(p1, p2, nrow = 1)

# Estadístico GAP
p3 <- fviz_nbclust(x = series_tp, FUNcluster = hcut, method = "gap_stat", k.max = 15, 
                   diss = IP.dis)
p3 


# Dendograma

# Número de clusters elegido
k <- 3 

# Aplica clustering jerárquico usando la matriz de distancias
hc <- hcut(series_tp, k=k, dmatrix = IP.dis)
fviz_dend(x = hc, k = k, cex = 0.4)

# Dibuja los clusters
fviz_cluster(object = list(data=series_tp, cluster=cutree(hc, k=k)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = T,
             labelsize = 8, show_label=FALSE, geom = "point")  +
  labs(title = "Hierarchical clustering")+
  theme_bw() +
  theme(legend.position = "bottom")

# Guarda cada series agrupada
IP.hclus <- hc$cluster
series_cl <- vector("list", k)

for (i in 1:k) {
  series_cl[[i]] <- series_tp[IP.hclus == i,]
}


# Clusters
cluster1 <- rownames(series_cl[[1]]); cluster1 # 25
cluster2 <- rownames(series_cl[[2]]); cluster2 # 12
cluster3 <- rownames(series_cl[[3]]); cluster3 # 13


# Se obtienen las series sin normalizar
IP.hclus  
series_cl      <- vector("list", k)
series_filas <- t(series)

for (i in 1:k) {
  series_cl[[i]] <- series_filas [IP.hclus == i,]
}


# Se buscan los representantes con mayor varianza
k <- 3 
representatives <- numeric(k)
repres.serie    <- matrix(nrow=nrow(series), ncol=k)

colnames(repres.serie) <- rep("Airport",k)

# Se ven los aeropuertos de cada cluster
for(i in 1:k){
  print(paste("Cluster", i))
  cl <- series_cl[[i]] 
  entradas  <- apply(cl, 1, var, na.rm=T)
  rpr.ind <- which.max(entradas)
  print(names(sort(entradas, decreasing = T)[1:15]))
  representatives[i] <- names(rpr.ind)
  repres.serie[,i]  <- cl[rpr.ind,]
  colnames(repres.serie)[i] <- representatives[i]
}

# Según visualización, se obtienen los representantes y se meten en un vector
representatives <- c("EHAM","LFML","UKBB")

# Se almacenan las series representativas
repres.series_cl <- matrix(nrow=ncol(series_filas), ncol=k)

for(i in 1:k){
  idx <- which(rownames(series_cl[[i]])== representatives[i])
  repres.series_cl[,i] <- series_cl[[i]][idx,]
}

colnames(repres.series_cl) <- representatives

# Se guardan las series en un Excel
for(i in 1:k){
  cl <- series_cl[[i]]
  write.xlsx(t(cl), paste0("Cluster", i, ".xlsx"), colNames = TRUE, rowNames = FALSE)
}


# Se organizan los representantes
repres.df <- data.frame(Entries = as.vector(repres.series_cl), 
                        Rep = rep(representatives, each=nrow(repres.series_cl)), 
                        Date=rep(series_todo$Date[1:length(repres.series_cl[,i])],1))

ggplot(repres.df, aes(x=Date, y=Entries, group=Rep, colour=Rep)) + geom_line()

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Se añaden los gráficos de cada cluster
i <- 1
p1 <- ggplot(subset(repres.df, Rep==representatives[i]), 
             aes(x=Date, y=Entries, group=Rep, colour=Rep)) + 
  geom_line(size=1.2) + scale_colour_manual(values=cbbPalette[i]) + 
  ggtitle(paste("Cluster", i)) + 
  theme(axis.line = element_line(size = 0.2,
          linetype = "solid"), panel.background = element_rect(fill = "white"))

i <- 2
p2 <- ggplot(subset(repres.df, Rep==representatives[i]), 
             aes(x=Date, y=Entries, group=Rep, colour=Rep)) + 
  geom_line(size=1.2) + scale_colour_manual(values=cbbPalette[i]) + 
  ggtitle(paste("Cluster", i)) + 
  theme(axis.line = element_line(size = 0.2,
          linetype = "solid"), panel.background = element_rect(fill = "white"))

i <- 3
p3 <- ggplot(subset(repres.df, Rep==representatives[i]), 
             aes(x=Date, y=Entries, group=Rep, colour=Rep)) + 
  geom_line(size=1.2) + scale_colour_manual(values=cbbPalette[i]) + 
  ggtitle(paste("Cluster", i)) + 
  theme(axis.line = element_line(size = 0.2,
          linetype = "solid"), panel.background = element_rect(fill = "white"))

# Se dibujan los tres a la vez
ggarrange(p1, p2, p3,  ncol = 3, nrow = 1) 



########----------------------------------------------------------------########
########                           Predicción                           ########
########----------------------------------------------------------------########


# Se emplean código externos con los modelos ARNN y LSTM

# Ruta del directorio que contiene los archivos
# Nombre del archivo que contiene la librería
# Ruta completa al archivo que contiene la librería
# Cargar la librería usando la función source()

ruta_directorio <- "D:/MASTER/TFM/aeropuertos (1)/"
nombre_archivo <- "ARNN.R"
ruta_completa <- paste(ruta_directorio, nombre_archivo, sep = "")
source(ruta_completa)

ruta_completa <- paste(ruta_directorio, "LSTM.R", sep = "")
source(ruta_completa)


# Para LSTM, se accede a Python desde R de la siguiente forma:

# Se instala Miniconda desde cero
reticulate::install_miniconda(force = TRUE)

# Se crea un entorno limpio con Python 3.10
reticulate::conda_create(envname = "r-tensorflow", packages = "python=3.10")

# Se instala TensorFlow en ese entorno
tensorflow::install_tensorflow(envname = "r-tensorflow")

# Se selecciona y se activa el entorno en R
reticulate::use_condaenv("r-tensorflow", required = TRUE)

# Se verifica la configuración actual de Python y paquetes
reticulate::py_config()

# Se revisa que las versiones son las correctas
cat("TensorFlow version:", tf$`__version__`, "\n")
cat("Keras version:", tf$keras$`__version__`, "\n")


# Se definen los parámetros para la predicción

# Número de clusters
k <- 3 

# Días de histórico (7) y de predicción (1)
dias.historico <- 7
dias.predic <- 1
nhist <- 30-dias.predic-dias.historico+1


# Se graba el inicio de ejecución para obtener después el tiempo de ejecución
# Se crean los vectores para ir rellenando información después
start_time <- Sys.time()
mse_ALL <- vector("list", k)
mae_ALL <- vector("list", k)
grafs   <- vector("list", k)

# Bucle de ejecución de modelos, con la definición de cada uno en R
for (j in 1:k){ # Repetir para cada uno de los k clusters
  print(paste("Cluster", j))
  cluster_series <- t(series_cl[[j]])
  grafs[[j]] <- list()
  
  modelos <- c("ARIMA", "HOLT", "KNN", "ARNN", "SVM", "TBATS", "LSTM")
  mse_cl <- matrix(nrow=nhist, ncol=length(modelos)) 
  rownames(mse_cl) <- paste("Hist?rico", 1:nhist)
  colnames(mse_cl) <- modelos
  
  mae_cl <- matrix(nrow=nhist, ncol=length(modelos)) 
  rownames(mae_cl) <- paste("Histórico", 1:nhist)
  colnames(mae_cl) <- modelos
  
  print(paste("Representative", representatives[j]))  
  repre <- repres.series_cl[,j]
  
  # Se escoge el primer histórico, 17 horas cada día 
  a <- 1 
  b <- dias.historico*17
  
  for(h in 1:nhist){ 
    print(paste("Historico",h))
    historico    <-  repre[a:b] 
    historico_ts <- ts(historico, frequency = 17)
    
    i <- 1 # predicciones a i día 
    values <- repre[(b+1):(b+17*i)] # Test 
    
    # Datos
    df <- data.frame(Entries= c(historico, values), 
                     Index = 1:(length(historico)+17*i), 
                     Method = c(rep("Historic", length(historico)), 
                                rep("Values", length(values))), 
                     Time = series_todo$Date[a:(b+17*i)])
    
    df.original  <- df # Guardar el de partida por si hace falta volver
    
    # ARIMA
    print("ARIMA")
    modeloARiMA <- auto.arima(historico_ts, stationary=FALSE, seasonal = TRUE) 
    preds   <- forecast(modeloARiMA, h = 17*i) 
    preds  <- round(pmax(preds$mean, 0)) # Redondeo y negativos a 0
    error      <- as.numeric(preds - values)
    mse_cl[h,"ARIMA"] <- mean(error^2, na.rm=TRUE)
    mae_cl[h,"ARIMA"] <- mean(abs(error), na.rm=TRUE)
    
    df.arima <- data.frame(Entries= preds, 
                           Index = (length(historico)+1):(length(historico)+17*i), 
                           Method = rep("Arima", length(preds)), 
                           Time = series_todo$Date[(b+1):(b+17*i)])
    
    df <- rbind(df, df.arima)
    
    # HOLT 
    print("HOLT")
    modeloH <- HoltWinters(historico_ts, seasonal = "additive")
    predH   <- forecast(modeloH, h=17*i)
    predH  <- round(pmax(predH$mean, 0)) # Redondeo y negativos a 0
    errorH <- as.numeric(predH - values)
    mse_cl[h,"HOLT"] <- mean(errorH^2, na.rm=TRUE)
    mae_cl[h,"HOLT"] <- mean(abs(errorH), na.rm=TRUE)
    
    df.holt <- data.frame(Entries= predH, 
                          Index = (length(historico)+1):(length(historico)+17*i), 
                          Method = rep("Holt-Winters", length(predH)), 
                          Time = series_todo$Date[(b+1):(b+17*i)])
    
    df <- rbind(df, df.holt)
    
    # KNN 
    print("KNN")
    predK <-  knn_forecasting(historico_ts, 
                              h = i*17, # Número de valores a predecir 
                              lags = 1:17, # Lags   
                              k = 2, # Distancia
                              msas = "MIMO") # Estrategia MIMO  
    predK  <- round(pmax(predK$prediction, 0)) # Redondeo y negativos a 0
    error <-  predK- values
    mse_cl[h,"KNN"] <- mean(error^2, na.rm=TRUE)
    mae_cl[h,"KNN"] <- mean(abs(error), na.rm=TRUE)
    
    df.knn <- data.frame(Entries= predK, 
                         Index = (length(historico)+1):(length(historico)+17*i), 
                         Method = rep("Knn", length(predK)), 
                         Time = series_todo$Date[(b+1):(b+17*i)])
    
    df <- rbind(df, df.knn)
    
    # ARNN 
    print("ARNN")
    modeloARNN <- NULL
    while(class(modeloARNN) != "arnn"){
      print("ARNN try")
      modeloARNN <- try(arnn(x = historico_ts, lags = 1:17, H = 3, isMLP = FALSE, 
                             restarts = 2))
    } # H son las neuronas
    predA <- forecast(modeloARNN, h=17*i, level = 95)
    predA  <- round(pmax(predA$mean, 0)) # Redondeo y negativos a 0
    error  <- predA - values
    mse_cl[h,"ARNN"] <- mean(error^2, na.rm=TRUE)
    mae_cl[h,"ARNN"] <- mean(abs(error), na.rm=TRUE)
    
    df.arnn <- data.frame(Entries= predA, 
                          Index = (length(historico)+1):(length(historico)+17*i), 
                          Method = rep("Arnn", length(predA)), 
                          Time = series_todo$Date[(b+1):(b+17*i)])
    
    df <- rbind(df, df.arnn)
    ggplot(df, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) + geom_line()
    
    # SVM
    print("SVM")
    df.historico <- subset(df.original, Method == "Historic")
    df.test      <- subset(df.original, Method == "Values")
    
    #Datos del histórico
    df.historico$Weekday    <- weekdays(as.Date(df.historico$Time)) 
    df.historico$Hour    <- format(df.historico$Time,'%H')
    
    # Variables de día de la semana 
    dias <- unique(weekdays(as.Date(df$Time))) 
    for(w in 1:length(dias)){
      idx1 <- which(df.historico$Weekday == dias[w])
      idx0 <- which(df.historico$Weekday != dias[w])
      aux  <- numeric(nrow(df.historico))
      aux[idx1] <- 1
      aux[idx0] <- 0
      df.historico[, dias[w]] <- aux 
    }
    
    # Variables de hora 
    horas <- unique(df.historico$Hour)
    for(t in 1:length(horas)){
      idx1 <- which(df.historico$Hour == horas[t])
      idx0 <- which(df.historico$Hour != horas[t])
      aux  <- numeric(nrow(df.historico))
      aux[idx1] <- 1
      aux[idx0] <- 0
      df.historico[, horas[t]] <- aux 
    }
    
    svm.data  <- df.historico[, -(3:6)] # Sin Data, Time y Weekday
    
    # Moving Average 
    svm.data$ma <-  rollmean(svm.data$Entries, k = 3, fill = NA)
    
    # Se mete un lag
    lag      <- c(NA, svm.data[-nrow(svm.data),1])
    svm.data <- cbind(svm.data, lag )
    svm.data <- na.omit(svm.data)
    
    # Se preparan los datos para predecir igual
    df.test$Weekday <- weekdays(as.Date(df.test$Time))
    df.test$Hour    <- format(df.test$Time,'%H')
    # Variables de día de semana 
    dias <- unique(weekdays(as.Date(df$Time))) 
    for(w in 1:length(dias)){
      idx1 <- which(df.test$Weekday == dias[w])
      idx0 <- which(df.test$Weekday != dias[w])
      aux  <- numeric(nrow(df.test))
      aux[idx1] <- 1
      aux[idx0] <- 0
      df.test[, dias[w]] <- aux 
    }
    #Variables de hora 
    horas <- unique(df.test$Hour)
    for(t in 1:length(horas)){
      idx1 <- which(df.test$Hour == horas[t])
      idx0 <- which(df.test$Hour != horas[t])
      aux  <- numeric(nrow(df.test))
      aux[idx1] <- 1
      aux[idx0] <- 0
      df.test[, horas[t]] <- aux 
    }
    
    nd <-  df.test[, -(3:6)] # Se quitan las variables
    
    # Moving Average 
    nd$ma <-  rollmean(nd$Entries, k = 3, fill = NA)
    
    # Se mete un lag
    lag <- c(NA, nd[-nrow(nd),1])
    nd  <- cbind(nd, lag )
    
    nd <- na.omit(nd)
    
    svmodel <- svm(Entries ~ .,data= svm.data, type="eps-regression",
                   kernel="polynomial",cost=5)
    predSVM <- predict(svmodel, newdata=nd[,-1])
    predSVM <- round(pmax(predSVM, 0)) # Redondeo y negativos a 0
    error   <- predSVM - values[-c(1, length(values))]
    mse_cl[h,"SVM"] <- mean(error^2, na.rm=TRUE)
    mae_cl[h,"SVM"] <- mean(abs(error), na.rm=TRUE)
    
    df.svm <- data.frame(Entries= predSVM , 
                         Index = (length(historico)+2):(length(historico)+16*i), 
                         Method = rep("SVM", length(predSVM)), 
                         Time = series_todo$Date[(b+2):(b+16*i)])
    
    df <- rbind(df, df.svm)
    ggplot(df, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) + geom_line()
    
    # TBATS
    print("TBATS")
    tbats_model <- tbats(historico_ts)
    predTBATS   <- forecast(tbats_model, h=17*i)
    predTBATS   <- round(pmax(predTBATS$mean, 0)) # Redondeo y negativos a 0
    error       <-  predTBATS - values
    mse_cl[h,"TBATS"] <- mean(error^2, na.rm=TRUE)
    mae_cl[h,"TBATS"] <- mean(abs(error), na.rm=TRUE)
    
    df.tbats <- data.frame(Entries= predTBATS, 
                           Index = (length(historico)+1):(length(historico)+17*i), 
                           Method = rep("TBATS", length(predTBATS)), 
                           Time = series_todo$Date[(b+1):(b+17*i)])
    
    df <- rbind(df, df.tbats)
    grafs[[j]][[h]] <- df
    
    # LSTM 
    print("LSTM")
    
    # Parámetros
    n_forecast <- as.integer(17 * i)   
    
    # Comprobación de que historico_ts no es vacío o NA antes de normalizar
    if(length(historico) < 2 || any(is.na(historico)) || all(historico == historico[1])) 
    {
      print("Advertencia: Datos insuficientes o constantes para LSTM en este 
            histórico. Saltando LSTM.")
      # Llenar con NA o 0 las métricas de error si no se puede ejecutar LSTM
      mse_cl[h, "LSTM"] <- NA
      mae_cl[h, "LSTM"] <- NA
      df.lstm <- data.frame(
        Entries = rep(NA, n_forecast), # O rep(0, n_forecast)
        Index = (length(historico) + 1):(length(historico) + n_forecast),
        Method = rep("LSTM", n_forecast),
        Time = series_todo$Date[(b + 1):(b + n_forecast)]
      )
      df <- rbind(df, df.lstm)
      grafs[[j]][[h]] <- df
      next # Saltar a la siguiente iteración del bucle
    }
    
    # Normalizar
    min_hist <- min(historico, na.rm = TRUE)
    max_hist <- max(historico, na.rm = TRUE)
    
    # Evitar división por cero si max == min
    if (max_hist - min_hist == 0) {
      series_norm <- rep(0, length(historico)) 
      # Todos los valores son iguales, normalizar a 0
    } else {
      series_norm <- (historico - min_hist) / (max_hist - min_hist)
    }
    
    # Asegurarse de que x e y tengan al menos un elemento
    if (length(series_norm) < 2) {
      print("Advertencia: Serie normalizada demasiado corta para preparar datos 
            LSTM (necesita al menos 2 elementos). Saltando LSTM.")
      mse_cl[h, "LSTM"] <- NA
      mae_cl[h, "LSTM"] <- NA
      df.lstm <- data.frame(
        Entries = rep(NA, n_forecast),
        Index = (length(historico) + 1):(length(historico) + n_forecast),
        Method = rep("LSTM", n_forecast),
        Time = series_todo$Date[(b + 1):(b + n_forecast)]
      )
      df <- rbind(df, df.lstm)
      grafs[[j]][[h]] <- df
      next
    }
    
    x <- series_norm[1:(as.integer(length(series_norm)) - 1)]
    y <- series_norm[2:as.integer(length(series_norm))]
    
    # Formatear para LSTM, asegurarse que las dimensiones son enteras
    x_array <- array(x, dim = c(as.integer(length(x)), 1, 1))
    y_array <- array(y, dim = c(as.integer(length(y)), 1))
    
    # Verificar si los arrays están vacíos
    if (prod(dim(x_array)) == 0 || prod(dim(y_array)) == 0) {
      print("Advertencia: Arrays de entrada/salida para LSTM vacíos. Saltando LSTM.")
      mse_cl[h, "LSTM"] <- NA
      mae_cl[h, "LSTM"] <- NA
      df.lstm <- data.frame(
        Entries = rep(NA, n_forecast),
        Index = (length(historico) + 1):(length(historico) + n_forecast),
        Method = rep("LSTM", n_forecast),
        Time = series_todo$Date[(b + 1):(b + n_forecast)]
      )
      df <- rbind(df, df.lstm)
      grafs[[j]][[h]] <- df
      next
    }
    
    # Modelo compatible con Keras 3.x
    model <- keras_model_sequential()
    model$add(layer_lstm(units = as.integer(10), input_shape = c(1, 1))) 
    model$add(layer_dense(units = as.integer(1)))
    
    model$compile(
      loss = 'mean_squared_error',
      optimizer = keras$optimizers$Adam(learning_rate = 0.01)
    )
    
    # Entrenamiento
    model$fit(
      x = x_array,
      y = y_array,
      epochs = as.integer(50),
      batch_size = as.integer(1),
      verbose = 0
    )
    
    # Predicción
    preds <- numeric(n_forecast)
    last_input <- tail(series_norm, 1)
    
    for (k_pred in 1:n_forecast) { 
      input_array <- array(last_input, dim = c(as.integer(1), 1, 1)) 
      # Asegurar dimensiones enteras
      next_pred <- model$predict(input_array, verbose = 0) 
      # Añadir verbose = 0 para evitar salida excesiva
      preds[k_pred] <- next_pred
      last_input <- next_pred
    }
    
    # Asegurarse de que denormalize use los min/max originales
    if (max_hist - min_hist == 0) {
      predLSTM <- round(pmax(min_hist + preds * 0, 0)) # Todos min_hist
    } else {
      predLSTM <- round(pmax(min_hist + preds * (max_hist - min_hist), 0))
    }
    
    # Errores
    # Asegurarse de que 'values' y 'predLSTM' tengan la misma longitud.
    if (length(values) != n_forecast) {
      print(paste("Advertencia: Longitud de 'values' (", length(values), ") 
                  no coincide con 'n_forecast' (", n_forecast, "). 
                  Ajustando 'values' o 'preds'."))
      predLSTM_adjusted <- predLSTM[1:min(length(predLSTM), length(values))]
      values_adjusted <- values[1:min(length(predLSTM), length(values))]
      error <- predLSTM_adjusted - values_adjusted
    } else {
      error <- predLSTM - values
    }
    
    mse_cl[h, "LSTM"] <- mean(error^2, na.rm = TRUE)
    mae_cl[h, "LSTM"] <- mean(abs(error), na.rm = TRUE)
    
    # Guardar resultados
    df.lstm <- data.frame(
      Entries = predLSTM,
      Index = (length(historico) + 1):(length(historico) + n_forecast),
      Method = rep("LSTM", n_forecast),
      Time = series_todo$Date[(b + 1):(b + n_forecast)]
    )
    
    df <- rbind(df, df.lstm)
    grafs[[j]][[h]] <- df
    
    print("LSTM listo")
    
    # Avanzar
    a <- a + 17
    b <- b + 17
    
  }  
  
  # mse_cluster tiene una columna para cada serie del cluster
  mse_ALL[[j]] <- mse_cl
  mae_ALL[[j]] <- mae_cl
}

# Se calcula el tiempo de ejecución final
end_time <- Sys.time()
time <- end_time - start_time;time



########----------------------------------------------------------------########
########                           Evaluación                           ########
########----------------------------------------------------------------########


# Se da nombre a las columnas con los 3 clusters
names(mse_ALL) <-  paste("Cluster", 1:3) 
names(mae_ALL) <-  paste("Cluster", 1:3)

# Se obtienen los MAEs
colMeans(mae_ALL[[1]])
colMeans(mae_ALL[[2]])
colMeans(mae_ALL[[3]])

# Se obtienen los MSEs
colMeans(mse_ALL[[1]])
colMeans(mse_ALL[[2]])
colMeans(mse_ALL[[3]])


# Se grafican los diagramas de caja de los errores para cada cluster
# Se pone 23 como parámetro, al ser 23 históricos
# Después se crea el dataframe

j <- 1
boxplot(mae_ALL[[j]], main=paste("Cluster",j, "MAE"),outline=FALSE)
boxplot(mse_ALL[[j]], main=paste("Cluster",j, "MSE"),outline=FALSE)
df.mae <- data.frame(MAE=c(mae_ALL[[j]]), Method=rep(colnames(mae_ALL[[j]]), each=23))

j <- 2
boxplot(mae_ALL[[j]], main=paste("Cluster",j, "MAE"),outline=FALSE)
boxplot(mse_ALL[[j]], main=paste("Cluster",j, "MSE"),outline=FALSE)
df.mae <- data.frame(MAE=c(mae_ALL[[j]]), Method=rep(colnames(mae_ALL[[j]]), each=23))

j <- 3
boxplot(mae_ALL[[j]], main=paste("Cluster",j, "MAE"),outline=FALSE)
boxplot(mse_ALL[[j]], main=paste("Cluster",j, "MSE"),outline=FALSE)
df.mae <- data.frame(MAE=c(mae_ALL[[j]]), Method=rep(colnames(mae_ALL[[j]]), each=23))

# Se muestra el dataframe
df.mae


# Se analiza la longitud y se construye el dataframe final con todos
length(mae_ALL[[j]])

df.mae <- data.frame(MAE=c(mae_ALL[[1]], mae_ALL[[2]],mae_ALL[[3]]),
                     Cluster = c(rep("Cluster 1",42), rep("Cluster 2",42), rep("Cluster 3",42)),
                     Method=rep(rep(colnames(mae_ALL[[j]])),3))

df.mae



########----------------------------------------------------------------########
########                  Comparaciones por cluster                     ########
########----------------------------------------------------------------########


# CLUSTER 1

# Se selecciona el cluster 1
i   <- 1
cl1 <- grafs[[i]]
cl1

# Se selecciona el histórico 5, de viernes a jueves y predice el viernes siguiente
j <- 5
df <- cl1[[j]]
round(mae_ALL[[i]][j,],2)
round(mse_ALL[[i]][j,],2)


# Dibujo del histórico y de los valores
df.or <- subset(df, Method == "Historic" | Method == "Values")
ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) +
  geom_line() +   labs(title=paste("Cluster", i))

# Predicción del modelo por horas
df.models <- subset(df, Method != "Historic" )
ggplot(df.models, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) + 
  geom_line(linewidth=1) + labs(title=paste("Cluster", i))

# Otra opción de gráfica
df.models <- subset(df, Method != "Historic" & Method != "Values" )
df.values <- subset(df, Method== "Values")

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", 
                "#FF0000", "#000000")
p12 <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + geom_point(size=3) + 
  geom_line(size=1.5) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), linewidth=1) +
  scale_colour_manual(values=cbbPalette) +scale_shape_manual(values=c(15, 16,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p12 <- p12 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p12

p12.aux <- p12 + theme(legend.position = "none") +labs(title = NULL)

# Se obtiene ahora una gráfica con los valores históricos y la predicción 
# sobre muestra
aux <- df.values[1,]

aux$Method <- "Historic"
df.or2 <- rbind(df.or[86:136,],aux )
cbbPaletteH <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                 "#CC79A7", "#FF0000", "#000000")
p21 <- ggplot(df.or2, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteH) + scale_shape_manual(values=c(15, 16, NA,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p21 <- p21 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p21 


# Teniendo en cuenta el siguiente orden:
# ARIMA, ARNN, HISTORIC, HOLT WINTERS, KNN, SVM, TBATS, LSTM, VALUES
shapess <- c(15, 16, NA, 17, 18, 19, 25, 8, 4)

# Se cogen los mejores modelos según visualización
models.cl <- c("SVM", "LSTM", "Arnn")
shapes.cl <- shapess[c(6,8,2,9)]
cols.cl   <- cbbPaletteH[c(6,8,2,9)]

# Se obtiene la gráfica de los mejores para partir de ellos
df.models   <- subset(df, Method %in% models.cl)
df.models  <- subset(df.models, Index != 120 & Index!= 136)
df.or  <- subset(df, Method == "Historic" | Method == "Values")

ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.25) +
  scale_colour_manual(values=cbbPaletteH[c(4,3,5,6,8)]) +
  labs(title=paste("Cluster", i))

pmod <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.5) +
  scale_colour_manual(values=cols.cl) +scale_shape_manual(values=shapes.cl)+
  labs(title=paste("Cluster", i))
pmod <- pmod + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pmod


# Combinación de modelos
Holt.Winters <- subset(df.models, Method=="Holt-Winters")$Entries
Arima  <- subset(df.models, Method=="Arima")$Entries
Arnn  <- subset(df.models, Method=="Arnn")$Entries
SVM    <- subset(df.models, Method=="SVM")$Entries
TBATS  <- subset(df.models, Method=="TBATS")$Entries
Knn  <- subset(df.models, Method=="Knn")$Entries
LSTM  <- subset(df.models, Method=="LSTM")$Entries
Values <- df.values$Entries[-c(1,17)]

# Datos en formato necesario para el paquete ForecastComb 
pred.data <- foreccomb(Values, cbind(SVM, LSTM, Arnn)) 


# Media aritmética
Arith.Mean <- round(comb_SA(pred.data)$Fitted) # Redondeo para enteros

# Combinación media ponderada BG (varianza-covarianza)
Bates.Granger <- round(comb_BG(pred.data)$Fitted)

# Combinación media ponderada CLS (regresión) 
CLS <- round(comb_CLS(pred.data)$Fitted)

# Gráfica de combinaciones
df.combs <- data.frame(Index   = rep(unique(df.models$Index),4), 
                       Entries = c(Arith.Mean, Bates.Granger, CLS, Values), 
                       Method  = c(rep("Arithmetic Mean", length(Arith.Mean)), 
                                   rep("Bates and Granger", length(Bates.Granger)), 
                                   rep("Constrained Least Squares", length(CLS)),
                                   rep("Values", length(Values))),
                       Time    = rep(unique(df.models$Time),4) )

cbbPalette <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                "#CC79A7", "#FF0000", "#000000")

cbbPaletteC <- c("#E69F00", "#56B4E9", "#5b2570","#009E73", "#D55E00", "#CC79A7", 
                 "#FF0000", "#000000")
ggplot(df.combs, aes(x=Index, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Index, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Index, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteC) +
  labs(title=paste("Cluster", i))

# MAE
MAE.comb <- matrix(ncol=3, nrow = 1)
colnames(MAE.comb) <- c("AM", "BG", "CLS")
MAE.comb[,"AM"] <- mean(abs(subset(df.combs, Method == "Arithmetic Mean")$Entries - Values))
MAE.comb[,"BG"] <- mean(abs(subset(df.combs, Method == "Bates and Granger")$Entries - Values))
MAE.comb[,"CLS"] <- mean(abs(subset(df.combs, Method == "Constrained Least Squares")$Entries - Values))
MAE.comb

# MSE
MSE.comb <- matrix(ncol=3, nrow = 1)
colnames(MSE.comb) <- c("AM", "BG", "CLS")
MSE.comb[,"AM"] <- mean((subset(df.combs, Method == "Arithmetic Mean")$Entries - Values)^2)
MSE.comb[,"BG"] <- mean((subset(df.combs, Method == "Bates and Granger")$Entries - Values)^2)
MSE.comb[,"CLS"] <- mean((subset(df.combs, Method == "Constrained Least Squares")$Entries - Values)^2)
MSE.comb

# Gráfica final según visualización, los valores reales son el 8
model.final <- c("SVM", "Constrained Least Squares", "Values")
cbbPaletteF <- cbbPaletteC[c(6,4,8)]

# Gráfica con las dos mejores opciones
df.final   <- subset(df.combs, Method %in% model.final)

shapp      <- c(15, 16, NA,17,18, 19, 25,8,4)
shap       <- c(8, shapp[c(5,8)])
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  geom_point(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap)+
  labs(title=paste("Cluster", i))

pp + theme(axis.title = element_text(size = 15),
           axis.text = element_text(size = 12),
           plot.title = element_text(size = 15),
           legend.text = element_text(size = 13),
           legend.title = element_text(size = 13),
           panel.background = element_rect(fill = "white"),
           legend.key = element_rect(size = 1.1),
           legend.background = element_rect(size = 1.5),
           legend.position = "top", legend.direction = "horizontal")

# Gráfica con la mejor predicción, CLS
cbbPaletteF <- cbbPaletteC[c(4,8)]
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=4) + 
  geom_line(size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap) +
  labs(title=" ")

pp.all <- pp + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pp.all


# Se realiza ahora la predicción fuera de muestra

# Seleccionar el cluster 1
i   <- 1
cl1 <- grafs[[i]]

# Seleccionar histórico j
j <- 23
df <- cl1[[j]]

# Crear predicción combinada (CLS)
Values <- subset(df, Method == "Values")$Entries
SVM    <- subset(df, Method == "SVM")$Entries
LSTM    <- subset(df, Method == "LSTM")$Entries
Arnn   <- subset(df, Method == "Arima")$Entries

# Crear objeto foreccomb y aplicar comb_CLS
pred.data <- foreccomb(Values, cbind(SVM, LSTM, Arnn))
CLS <- round(comb_CLS(pred.data)$Fitted)

# Crear fechas para la predicción (1 de octubre desde las 07:00)
fechas_pred <- seq(from = as.POSIXct("2017-10-01 07:00:00"), by = "hour", 
                   length.out = 17)

# Crear data.frame con predicción CLS
df_cls_pred <- data.frame(
  Time = fechas_pred,
  Entries = CLS,
  Method = "CLS",
  Index = max(df$Index, na.rm = TRUE) + 1
)

# Añadir predicción al dataframe original
df <- rbind(df, df_cls_pred)

# Guardar en grafs
cl1[[j]] <- df
grafs[[i]] <- cl1

# Filtrar solo datos CLS del 1 de octubre
df_oct1 <- subset(df, Method == "CLS" &
                    Time >= as.POSIXct("2017-10-01 07:00:00") &
                    Time <  as.POSIXct("2017-10-01 23:00:00"))

# Gráfico solo con CLS
ggplot(df_oct1, aes(x = Time, y = Entries)) +
  geom_line(color = "black", size = 2, linetype = "dashed") +
  geom_point(color = "black", size = 4, shape = 17) +
  labs(title = paste("Cluster", i, "- Predicción CLS (1 Oct)"),
       x = "Hora", y = "Entries") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  ) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")


# CLUSTER 2

# Se selecciona el cluster 2
i   <- 2
cl1 <- grafs[[i]]
cl1

# Se selecciona el histórico 5, de viernes a jueves y predice el viernes siguiente
j <- 5
df <- cl1[[j]]
round(mae_ALL[[i]][j,],2)
round(mse_ALL[[i]][j,],2)


# Dibujo del histórico y de los valores
df.or <- subset(df, Method == "Historic" | Method == "Values")
ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) +
  geom_line() +   labs(title=paste("Cluster", i))

# Predicción del modelo por horas
df.models <- subset(df, Method != "Historic" )
ggplot(df.models, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) + 
  geom_line(linewidth=1) + labs(title=paste("Cluster", i))

# Otra opción de gráfica
df.models <- subset(df, Method != "Historic" & Method != "Values" )
df.values <- subset(df, Method== "Values")

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", 
                "#FF0000", "#000000")
p12 <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + geom_point(size=3) + 
  geom_line(size=1.5) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), linewidth=1) +
  scale_colour_manual(values=cbbPalette) +scale_shape_manual(values=c(15, 16,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p12 <- p12 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p12

p12.aux <- p12 + theme(legend.position = "none") +labs(title = NULL)

# Se obtiene ahora una gráfica con los valores históricos y la predicción 
# sobre muestra
aux <- df.values[1,]

aux$Method <- "Historic"
df.or2 <- rbind(df.or[86:136,],aux )
cbbPaletteH <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                 "#CC79A7", "#FF0000", "#000000")
p21 <- ggplot(df.or2, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteH) + scale_shape_manual(values=c(15, 16, NA,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p21 <- p21 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p21 


# Teniendo en cuenta el siguiente orden:
# ARIMA, ARNN, HISTORIC, HOLT WINTERS, KNN, SVM, TBATS, LSTM, VALUES
shapess <- c(15, 16, NA, 17, 18, 19, 25, 8, 4)

# Se cogen los mejores modelos según visualización
models.cl <- c("SVM", "Knn", "Arnn")
shapes.cl <- shapess[c(6,5,2,9)]
cols.cl   <- cbbPaletteH[c(6,5,2,9)]

# Se obtiene la gráfica de los mejores para partir de ellos
df.models   <- subset(df, Method %in% models.cl)
df.models  <- subset(df.models, Index != 120 & Index!= 136)
df.or  <- subset(df, Method == "Historic" | Method == "Values")

ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.25) +
  scale_colour_manual(values=cbbPaletteH[c(4,3,5,6,8)]) +
  labs(title=paste("Cluster", i))

pmod <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.5) +
  scale_colour_manual(values=cols.cl) +scale_shape_manual(values=shapes.cl)+
  labs(title=paste("Cluster", i))
pmod <- pmod + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pmod


# Combinación de modelos
Holt.Winters <- subset(df.models, Method=="Holt-Winters")$Entries
Arima  <- subset(df.models, Method=="Arima")$Entries
Arnn  <- subset(df.models, Method=="Arnn")$Entries
SVM    <- subset(df.models, Method=="SVM")$Entries
TBATS  <- subset(df.models, Method=="TBATS")$Entries
Knn  <- subset(df.models, Method=="Knn")$Entries
LSTM  <- subset(df.models, Method=="LSTM")$Entries
Values <- df.values$Entries[-c(1,17)]

# Datos en formato necesario para el paquete ForecastComb 
pred.data <- foreccomb(Values, cbind(SVM, Knn, Arnn)) 


# Media aritmética
Arith.Mean <- round(comb_SA(pred.data)$Fitted) # Redondeo para enteros

# Combinación media ponderada BG (varianza-covarianza)
Bates.Granger <- round(comb_BG(pred.data)$Fitted)

# Combinación media ponderada CLS (regresión) 
CLS <- round(comb_CLS(pred.data)$Fitted)

# Gráfica de combinaciones
df.combs <- data.frame(Index   = rep(unique(df.models$Index),4), 
                       Entries = c(Arith.Mean, Bates.Granger, CLS, Values), 
                       Method  = c(rep("Arithmetic Mean", length(Arith.Mean)), 
                                   rep("Bates and Granger", length(Bates.Granger)), 
                                   rep("Constrained Least Squares", length(CLS)),
                                   rep("Values", length(Values))),
                       Time    = rep(unique(df.models$Time),4) )

cbbPalette <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                "#CC79A7", "#FF0000", "#000000")

cbbPaletteC <- c("#E69F00", "#56B4E9", "#5b2570","#009E73", "#D55E00", "#CC79A7", 
                 "#FF0000", "#000000")
ggplot(df.combs, aes(x=Index, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Index, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Index, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteC) +
  labs(title=paste("Cluster", i))

# MAE
MAE.comb <- matrix(ncol=3, nrow = 1)
colnames(MAE.comb) <- c("AM", "BG", "CLS")
MAE.comb[,"AM"] <- mean(abs(subset(df.combs, Method == "Arithmetic Mean")$Entries - Values))
MAE.comb[,"BG"] <- mean(abs(subset(df.combs, Method == "Bates and Granger")$Entries - Values))
MAE.comb[,"CLS"] <- mean(abs(subset(df.combs, Method == "Constrained Least Squares")$Entries - Values))
MAE.comb

# MSE
MSE.comb <- matrix(ncol=3, nrow = 1)
colnames(MSE.comb) <- c("AM", "BG", "CLS")
MSE.comb[,"AM"] <- mean((subset(df.combs, Method == "Arithmetic Mean")$Entries - Values)^2)
MSE.comb[,"BG"] <- mean((subset(df.combs, Method == "Bates and Granger")$Entries - Values)^2)
MSE.comb[,"CLS"] <- mean((subset(df.combs, Method == "Constrained Least Squares")$Entries - Values)^2)
MSE.comb

# Gráfica final según visualización, los valores reales son el 8
model.final <- c("SVM", "Constrained Least Squares", "Values")
cbbPaletteF <- cbbPaletteC[c(6,4,8)]

# Gráfica con las dos mejores opciones
df.final   <- subset(df.combs, Method %in% model.final)

shapp      <- c(15, 16, NA,17,18, 19, 25,8,4)
shap       <- c(8, shapp[c(5,8)])
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  geom_point(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap)+
  labs(title=paste("Cluster", i))

pp + theme(axis.title = element_text(size = 15),
           axis.text = element_text(size = 12),
           plot.title = element_text(size = 15),
           legend.text = element_text(size = 13),
           legend.title = element_text(size = 13),
           panel.background = element_rect(fill = "white"),
           legend.key = element_rect(size = 1.1),
           legend.background = element_rect(size = 1.5),
           legend.position = "top", legend.direction = "horizontal")

# Gráfica con la mejor predicción, CLS
cbbPaletteF <- cbbPaletteC[c(6,8)]
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=4) + 
  geom_line(size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap) +
  labs(title=" ")

pp.all <- pp + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pp.all


# Se realiza ahora la predicción fuera de muestra

# Seleccionar el cluster 2
i   <- 2
cl1 <- grafs[[i]]

# Seleccionar histórico j
j <- 23
df <- cl1[[j]]

# Crear predicción combinada (CLS)
Values <- subset(df, Method == "Values")$Entries
SVM    <- subset(df, Method == "SVM")$Entries
Knn    <- subset(df, Method == "Knn")$Entries
Arnn   <- subset(df, Method == "Arnn")$Entries

# Crear objeto foreccomb y aplicar comb_CLS
pred.data <- foreccomb(Values, cbind(SVM, Knn, Arnn))
CLS <- round(comb_CLS(pred.data)$Fitted)

# Crear fechas para la predicción (1 de octubre desde las 07:00)
fechas_pred <- seq(from = as.POSIXct("2017-10-01 07:00:00"), by = "hour", length.out = 17)

# Crear data.frame con predicción CLS
df_cls_pred <- data.frame(
  Time = fechas_pred,
  Entries = CLS,
  Method = "CLS",
  Index = max(df$Index, na.rm = TRUE) + 1
)

# Añadir predicción al dataframe original
df <- rbind(df, df_cls_pred)

# Guardar en grafs
cl1[[j]] <- df
grafs[[i]] <- cl1

# Filtrar solo datos CLS del 1 de octubre
df_oct1 <- subset(df, Method == "CLS" &
                    Time >= as.POSIXct("2017-10-01 07:00:00") &
                    Time <  as.POSIXct("2017-10-01 23:00:00"))

# Gráfico solo con CLS

ggplot(df_oct1, aes(x = Time, y = Entries)) +
  geom_line(color = "black", size = 2, linetype = "dashed") +
  geom_point(color = "black", size = 4, shape = 17) +
  labs(title = paste("Cluster", i, "- Predicción CLS (1 Oct)"),
       x = "Hora", y = "Entries") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  ) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours")


# CLUSTER 3

# Se selecciona el cluster 3
i   <- 3
cl1 <- grafs[[i]]
cl1

# Se selecciona el histórico 5, de viernes a jueves y predice el viernes siguiente
j <- 5
df <- cl1[[j]]
round(mae_ALL[[i]][j,],2)
round(mse_ALL[[i]][j,],2)


# Dibujo del histórico y de los valores
df.or <- subset(df, Method == "Historic" | Method == "Values")
ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) +
  geom_line() +   labs(title=paste("Cluster", i))

# Predicción del modelo por horas
df.models <- subset(df, Method != "Historic" )
ggplot(df.models, aes(x=Time, y=Entries, colour = Method)) + geom_point(size=2) + 
  geom_line(linewidth=1) + labs(title=paste("Cluster", i))

# Otra opción de gráfica
df.models <- subset(df, Method != "Historic" & Method != "Values" )
df.values <- subset(df, Method== "Values")

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", 
                "#FF0000", "#000000")
p12 <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + geom_point(size=3) + 
  geom_line(size=1.5) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), linewidth=1) +
  scale_colour_manual(values=cbbPalette) +scale_shape_manual(values=c(15, 16,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p12 <- p12 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p12

p12.aux <- p12 + theme(legend.position = "none") +labs(title = NULL)

# Se obtiene ahora una gráfica con los valores históricos y la predicción 
# sobre muestra
aux <- df.values[1,]

aux$Method <- "Historic"
df.or2 <- rbind(df.or[86:136,],aux )
cbbPaletteH <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                 "#CC79A7", "#FF0000", "#000000")
p21 <- ggplot(df.or2, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteH) + scale_shape_manual(values=c(15, 16, NA,17,18, 19, 25,8,4))+
  labs(title=paste("Cluster", i))
p21 <- p21 + theme(axis.title = element_text(size = 15),
                   axis.text = element_text(size = 12),
                   plot.title = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.title = element_text(size = 13),
                   panel.background = element_rect(fill = "white"),
                   legend.key = element_rect(size = 1.1),
                   legend.background = element_rect(size = 1.5),
                   legend.position = "top", legend.direction = "horizontal")
p21 


# Teniendo en cuenta el siguiente orden:
# ARIMA, ARNN, HISTORIC, HOLT WINTERS, KNN, SVM, TBATS, LSTM, VALUES
shapess <- c(15, 16, NA, 17, 18, 19, 25, 8, 4)

# Se cogen los mejores modelos según visualización
models.cl <- c("TBATS", "SVM", "Arima")
shapes.cl <- shapess[c(7,6,1,9)]
cols.cl   <- cbbPaletteH[c(7,6,1,9)]

# Se obtiene la gráfica de los mejores para partir de ellos
df.models   <- subset(df, Method %in% models.cl)
df.models  <- subset(df.models, Index != 120 & Index!= 136)
df.or  <- subset(df, Method == "Historic" | Method == "Values")

ggplot(df.or, aes(x=Time, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.25) +
  scale_colour_manual(values=cbbPaletteH[c(4,3,5,6,8)]) +
  labs(title=paste("Cluster", i))

pmod <- ggplot(df.values, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=df.models, aes(x=Time, y=Entries, colour = Method), size=1.5) +
  scale_colour_manual(values=cols.cl) +scale_shape_manual(values=shapes.cl)+
  labs(title=paste("Cluster", i))
pmod <- pmod + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pmod


# Combinación de modelos
Holt.Winters <- subset(df.models, Method=="Holt-Winters")$Entries
Arima  <- subset(df.models, Method=="Arima")$Entries
Arnn  <- subset(df.models, Method=="Arnn")$Entries
SVM    <- subset(df.models, Method=="SVM")$Entries
TBATS  <- subset(df.models, Method=="TBATS")$Entries
Knn  <- subset(df.models, Method=="Knn")$Entries
LSTM  <- subset(df.models, Method=="LSTM")$Entries
Values <- df.values$Entries[-c(1,17)]

# Datos en formato necesario para el paquete ForecastComb 
pred.data <- foreccomb(Values, cbind(TBATS, SVM, Arima)) 


# Media aritmética
Arith.Mean <- round(comb_SA(pred.data)$Fitted) # Redondeo para enteros

# Combinación media ponderada BG (varianza-covarianza)
Bates.Granger <- round(comb_BG(pred.data)$Fitted)

# Combinación media ponderada CLS (regresión) 
CLS <- round(comb_CLS(pred.data)$Fitted)

# Gráfica de combinaciones
df.combs <- data.frame(Index   = rep(unique(df.models$Index),4), 
                       Entries = c(Arith.Mean, Bates.Granger, CLS, Values), 
                       Method  = c(rep("Arithmetic Mean", length(Arith.Mean)), 
                                   rep("Bates and Granger", length(Bates.Granger)), 
                                   rep("Constrained Least Squares", length(CLS)),
                                   rep("Values", length(Values))),
                       Time    = rep(unique(df.models$Time),4) )

cbbPalette <- c("#E69F00", "#56B4E9", "#5b2570", "#009E73", "#0072B2", "#D55E00", 
                "#CC79A7", "#FF0000", "#000000")

cbbPaletteC <- c("#E69F00", "#56B4E9", "#5b2570","#009E73", "#D55E00", "#CC79A7", 
                 "#FF0000", "#000000")
ggplot(df.combs, aes(x=Index, y=Entries, colour = Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) + 
  geom_point(data=df.models, aes(x=Index, y=Entries, colour = Method), size=2) + 
  geom_line(data=df.models, aes(x=Index, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteC) +
  labs(title=paste("Cluster", i))

# MAE
MAE.comb <- matrix(ncol=3, nrow = 1)
colnames(MAE.comb) <- c("AM", "BG", "CLS")
MAE.comb[,"AM"] <- mean(abs(subset(df.combs, Method == "Arithmetic Mean")$Entries - Values))
MAE.comb[,"BG"] <- mean(abs(subset(df.combs, Method == "Bates and Granger")$Entries - Values))
MAE.comb[,"CLS"] <- mean(abs(subset(df.combs, Method == "Constrained Least Squares")$Entries - Values))
MAE.comb

## MSE 
MSE.comb <- matrix(ncol=3, nrow = 1)
colnames(MSE.comb) <- c("AM", "BG", "CLS")
MSE.comb[,"AM"] <- mean((subset(df.combs, Method == "Arithmetic Mean")$Entries - Values)^2)
MSE.comb[,"BG"] <- mean((subset(df.combs, Method == "Bates and Granger")$Entries - Values)^2)
MSE.comb[,"CLS"] <- mean((subset(df.combs, Method == "Constrained Least Squares")$Entries - Values)^2)
MSE.comb

# Gráfica final según visualización, los valores reales son el 8
model.final <- c("TBATS", "Constrained Least Squares", "Values")
cbbPaletteF <- cbbPaletteC[c(6,4,8)]

# Gráfica con las dos mejores opciones
df.final   <- subset(df.combs, Method %in% model.final)

shapp      <- c(15, 16, NA,17,18, 19, 25,8,4)
shap       <- c(8, shapp[c(6,8)])
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=2) + 
  geom_line(size=1) +
  geom_point(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=4) + 
  geom_line(data=subset(df.models, Method=="SVM"), aes(x=Time, y=Entries, colour = Method), size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap)+
  labs(title=paste("Cluster", i))

pp + theme(axis.title = element_text(size = 15),
           axis.text = element_text(size = 12),
           plot.title = element_text(size = 15),
           legend.text = element_text(size = 13),
           legend.title = element_text(size = 13),
           panel.background = element_rect(fill = "white"),
           legend.key = element_rect(size = 1.1),
           legend.background = element_rect(size = 1.5),
           legend.position = "top", legend.direction = "horizontal")

# Gráfica con la mejor predicción, TBATS
cbbPaletteF <- cbbPaletteC[c(6,8)]
pp <- ggplot(df.final, aes(x=Time, y=Entries, colour = Method, shape=Method)) + 
  geom_point(size=4) + 
  geom_line(size=1) +
  scale_colour_manual(values=cbbPaletteF) + scale_shape_manual(values=shap) +
  labs(title=" ")

pp.all <- pp + theme(axis.title = element_text(size = 15),
                     axis.text = element_text(size = 12),
                     plot.title = element_text(size = 15),
                     legend.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     panel.background = element_rect(fill = "white"),
                     legend.key = element_rect(size = 1.1),
                     legend.background = element_rect(size = 1.5),
                     legend.position = "top", legend.direction = "horizontal")
pp.all


# Se realiza ahora la predicción fuera de muestra

# Seleccionar el cluster 3
i   <- 3
cl1 <- grafs[[i]]
cl1

# Histórico 5, de viernes a jueves y predice el viernes siguiente
j <- 23
df <- cl1[[j]]
round(mae_ALL[[i]][j,],2)
round(mse_ALL[[i]][j,],2)

# 1. Ver rango de fechas disponibles
print(range(df$Time))

# 2. Ver métodos disponibles
print(unique(df$Method))

# 3. Ajustar fechas a las que tengas disponibles, por ejemplo:
historic_df <- subset(df, Method == "Historic" & 
                        Time >= as.POSIXct("2017-09-22 00:00:00") & 
                        Time <  as.POSIXct("2017-10-01 00:00:00"))

print(nrow(historic_df))  # debe ser > 0

# 4. Si hay datos, continuar con creación de serie
serie_tbats <- msts(historic_df$Entries, seasonal.periods = c(17, 119))
modelo_tbats <- tbats(serie_tbats)
pred_tbats <- forecast(modelo_tbats, h = 17)
pred_vals <- round(pmax(pred_tbats$mean, 0))

fechas_pred <- seq(from = as.POSIXct("2017-10-01 07:00:00"), by = "hour", length.out = 17)
df_tbats_pred <- data.frame(
  Time = fechas_pred,
  Entries = pred_vals,
  Method = "TBATS",
  Index = max(df$Index, na.rm = TRUE) + 1
)
df <- rbind(df, df_tbats_pred)
cl1[[j]] <- df
grafs[[i]] <- cl1


# Subset para históricos y valores reales
df_hist_val <- subset(df, Method %in% c("Historic", "Values"))

# Subset para las predicciones TBATS
df_pred <- subset(df, Method == "TBATS")

# Gráfico combinado
ggplot() +
  geom_line(data = df_hist_val, aes(x = Time, y = Entries, color = Method), size = 1) +
  geom_point(data = df_hist_val, aes(x = Time, y = Entries, color = Method), size = 2) +
  geom_line(data = df_pred, aes(x = Time, y = Entries, color = Method), size = 1, linetype = "dashed") +
  geom_point(data = df_pred, aes(x = Time, y = Entries, color = Method), size = 3, shape = 17) +
  scale_colour_manual(values=cbbPaletteF) +
  labs(title = paste("Cluster", i, "- Predicción TBATS"), x = "Fecha y Hora", y = "Entries") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())



# Subconjunto solo con datos reales y predicción (sin Historic)
df_hist_val <- subset(df, Method == "Values")
df_pred <- subset(df, Method == "TBATS")

# Paleta de colores
cols.cl <- c("Values" = "black", "TBATS" = "red")


# Filtrar valores y predicciones SOLO para el 1 de octubre
df_oct1 <- subset(df, Method %in% c("TBATS") &
                    Time >= as.POSIXct("2017-10-01 07:00:00") &
                    Time <  as.POSIXct("2017-10-01 23:00:00"))

# Paleta de colores (puedes ajustar los colores como quieras)
cols.cl <- c("TBATS" = "black")

# Gráfico
ggplot(df_oct1, aes(x = Time, y = Entries, color = Method, shape = Method)) +
  geom_line(size = 1, aes(linetype = Method)) +
  geom_point(size = 3) +
  scale_colour_manual(values = cols.cl) +
  scale_shape_manual(values = c("Values" = 16, "TBATS" = 17)) +
  scale_linetype_manual(values = c("Values" = "solid", "TBATS" = "dashed")) +
  labs(title = paste("Cluster", i, "- Predicción TBATS (1 Oct)"),
       x = "Hora", y = "Entries") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    legend.text = element_text(size = 12)
  ) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") 


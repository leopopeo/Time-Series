#'Durbin Levionson Prediction
#'
#'\code{DL_prediction} benutzt den Durbin-Levinson_Algorithmus (\code{DLA}) um eine Vorhersage für die Zeitreihe zu treffen. Mit dem \code{len} Parameter kann die Anzahl der zu vorhersagenden Werten getroffen werden. Für mehr Informationen rufe die Vignette "Time-Series" auf.
#'
#'@param X Eingabevektor der die beobachteten Daten enthält
#'@param len Anzahl der vorherzusagenden Werten
#'@param all logical-Wert der angibt ob nur die vorhergesagten Werte oder die alte Zeitreihe plus die vorhergesagten Werte zurückgegeben wird. Der Default-Wert ist Ersteres.
#'@return Vektor mit den vorhgesagten Werten
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(phi=0.3,sd=1,I=1000)
#'
#'  DL_prediction(x,len=10)
#'@export
DL_prediction <- function(X,len=1,all=FALSE){

  prediction <- numeric(length(len))

  zwischen <- X
  #Vorhersage
  for(i in 1:len){
    pred <- DLA(zwischen)
    prediction[i] <- sum(rev(zwischen)*pred)
    zwischen <- c(zwischen,prediction[i])
  }
  if(all==F){
    return(prediction)
  }
  else{
    return(zwischen)
  }
}

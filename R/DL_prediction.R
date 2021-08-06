#'Durbin Levionson Prediction
#'
#'\code{DL_prediction} benutzt den Durbin-Levinson_Algorithmus (\code{DLA}) um eine Vorhersage fuer die Zeitreihe zu treffen. Mit dem \code{len} Parameter kann die Anzahl der zu vorhersagenden Werten getroffen werden. Für mehr Informationen rufe die Vignette "Time-Series" auf.
#'
#'@param x Eingabevektor der die beobachteten Daten enthält
#'@param len Anzahl der vorherzusagenden Werten
#'@param all logical-Wert der angibt ob nur die vorhergesagten Werte oder die alte Zeitreihe plus die vorhergesagten Werte zurueckgegeben wird. Der Default-Wert ist Ersteres.
#'@return Vektor mit den vorhgesagten Werten
#'@examples
#'  #Erstelle eine Zeitreihe
#'  X = arma_sim(phi=0.3,sd=1,I=1000)
#'
#'  DL_prediction(x,len=10)
#'@export
DL_prediction <- function(x,len=1,all=FALSE){

  laenge <- length(x)
  #Eingabewerte überprüfen
  stopifnot("Eingabe ist nicht numerisch!" = is.numeric(x))
  stopifnot("Die Länge des Vektors muss größer als 1 sei!" = length(x) > 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!"  = (is.null(laenge) |
                                                              is.numeric(laenge)))

  stopifnot("len muss >= 1 sein!" =  len >= 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = length(len) == 1)
  stopifnot("len muss NULL oder ein Integer Wert sein!" = len %% 1 == 0)

  stopifnot("all muss ein logical-Wert sein"= is.logical(all)==T)


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

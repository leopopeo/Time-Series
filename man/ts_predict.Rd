% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prediction_Innovation.R
\name{ts_predict}
\alias{ts_predict}
\title{ts_predict}
\usage{
ts_predict(X, steps = 1, all = FALSE)
}
\arguments{
\item{X}{Eine stationaere Zeitreihe.}

\item{steps}{Anzahl N der Schritte der Prognose.}

\item{all}{Wenn FALSE wird nur der Vorhersagevektor der Laenge steps ausgegeben. Andernfalls auch alle bis dahin berechneten X_hat.}
}
\value{
Numerischer Vektor der Vorhersage. Wenn all = FALSE, werden nur die vorhergesagten N Werte zurueckgegeben, andernfalls der ganze Vektor X_hat.
}
\description{
N-Schritt Prognose mithilfe des Innovation Algorithmus.
}
\examples{
X = arma_sim(phi = c(0.5, 0.1), theta = c(-0.2), sd = 0.01, I = 100)
ts_predict(X, 4)

}

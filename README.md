# Time-Series
Uni HD; R-Kurs SS2021, Projekt: Time Series
Letztes Update: 18.06.2021

Dieses Projekt ist Teil der Vorlesung "Die Programmiersprache R und ihre Anwendungen in der Stochastik" der Universität Heidelberg im Sommersemester 2021.

_Description of the project:_
The theory of time series is often applied in economic processes, signal processing and simple denoizingsituations. The two processes we want to consider are the moving average (MA) process and the autoregressive (AR) process. The goal of this project is to implement an R-package that provides its users withfunctions to generate simulations of these processes and to estimate their characteristcs. 

The _main references_ is the book Brockwell et al. (2002).

_Tasks:_

•Implement a generator forAR(p)andMA(q)processes (Definition 3.1.1, p 74).

•Implement the sample autocovariance function (Definition 1.4.4, p. 16).

•Implement the Durbin Levinson Algorithm using the sample autocovariance function (p. 60-61).

•Implement the Innovation Algorithm using the sample autocovariance function (p. 62).

•Implement the periodogram as an estimator of the spectral density (Definition 4.2.1, p.107).

•Implement functions that create nice plots from the time series and their spectral densities and theestimate of the spectral densities.

•Illustration

#' Optimierung des 1. Koutsoyiannis-Parameters
#' @description
#' Optimierung des 1. Koutsoyiannis-Parameters zur Skalierung der Intensitaeten je nach Dauer entsprechend der robusten Methode (basierend auf der Kruskal-Wallis-Statistik) wie in Koutsoyiannis et al. 1998
#' @param Inten.Daten der extrahierten jaehrlichen Serien, nicht als maximale Niederschlagsvolume, sondern als maximale Niederschlagsintensitaet in mm/h, angegeben fuer jede Dauer (D) und jedes Jahr.// Format data.frame(ncol = Dauer, nrow=Jahre)
#' @param Theta Der erste Koutsoyiannis-Parameter
#' @param Dauern Dauern (h), die fuer die Berechnung der jaehrlichen Serien verwendet sind, in Stunden!
#' @param nD Anzahl der Jahre oder Anzahl der Extremewerte fuer jede Dauer
#' @param Partition die Anzahl der Extremwerte pro Dauer, die in die Berechnung der Gesamtintensitaet einbezogen werden sollen (1/3 oder 1/2 aller Daten)
#' @param m Hoechster Rang fuer die Intensitaeten
#' @details
#' Die Optimierung der Koutsoyiannis-Parameter durch Minimierung der Kruskal-Wallis-Statistik (KW):
#' \deqn{KW =  \frac{12}{m(m+1)} \sum_{D=1}^{k} n_D ( \bar{r}_D - \frac{m+1}{2} )^2}
#' wo:
#' \itemize{
#'   \item \eqn{m} Hoechster Rang fuer die Intensitaeten: Anzahl aller Beobachtungen fuer die gesamten Dauern
#'   \item \eqn{k} Anzahl aller Dauern, die fuer die Berechnung der jaehrlichen Serien verwendet sind
#'   \item \eqn{n_D} Anzahl der Jahre oder Anzahl der Extremewerte fuer jede Dauer \eqn{D} .
#'   \item \eqn{\bar{R}_D} Mittlerer Rang fuer die Stichprobe jede Dauer \eqn{D}
#' }
#' \deqn{KW\left( \theta,\eta \right) \rightarrow Min}
#' wo:
#' \itemize{
#'   \item \eqn{\theta} Der 1. Koutsoyiannis-Parameter
#'   \item \eqn{\eta} Der 2. Koutsoyiannis-Parameter
#'   }
#' @return KW Kruskal-Wallis Teststatistik
kw_koupar1 = function(Theta, Inten.Daten=Inten.Daten, Dauern=Dauern, Partition=Partition, nD=nD, m=m){
  Eta = stats::optimize(kw_koupar2, lower=0, upper=1, Theta=Theta, Dauern=Dauern, Inten.Daten = Inten.Daten, Partition=Partition,nD=nD,m=m, maximum=FALSE)$minimum
  bD = (Dauern+Theta)^Eta
  alle.Inten = do.call(c, lapply(1:length(Dauern), function(i) Inten.Daten[1:Partition,i]*bD[i]))
  if(length(which(is.na(alle.Inten)==T))>0) alle.Inten = alle.Inten[-which(is.na(alle.Inten)==T)]
  alle.sortiert = sort(alle.Inten, decreasing=T)
  rank.all    = 1:length(alle.sortiert)
  mittlere.rD= do.call(c, lapply(1:length(Dauern), function(i){
    Inten.jeDauer   = Inten.Daten[1:Partition,i]
    if(length(which(is.na(Inten.jeDauer)==T))>0) Inten.jeDauer = Inten.jeDauer[-which(is.na(Inten.jeDauer)==T)]
    Inten.jeDauer   = Inten.jeDauer*bD[i]
    mittlere.Rang  = mean(unlist(sapply(Inten.jeDauer, function(k) mean(which(alle.sortiert==k)))))
    return(round(mittlere.Rang,0))
  }))
  zweiterTeil = sapply(1:length(Dauern), function(i) nD[i]*((mittlere.rD[i]-((m+1)/2))^2))
  KW = (12/(m*(m+1)))* sum(zweiterTeil)
  return(KW)

}

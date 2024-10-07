#' Beruecksichtigung der Intervalllaenge
#' @description
#' Korrektur der Regenhoehen fuer Dauern bis zum 4-mal die Intervalllaenge. Fuer weitere Informationen siehe Kapitel 5.3 und Tabelle 1 des Merkblatts DWA-A 531.
#'
#' @param Serie Jaehrliche Maximum Serie als Tabelle, wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen.
#' @param Intervall das Zeitintervall der Niederschlagsmessungen (entweder in Minuten oder Stunden).  Standardwert ist 5min. Soll in den gleichen Einheiten wie die Regendauern in der data.frame Serie
#' @examples
#' #Korrektur der Goerlitzer jaehrlichen Serie bezueglich der Intervalllaenge:
#' korrigierteSerie = Intervallkorrektur(Serie=Goerlitz_iN)
#' print(korrigierteSerie)
#' @return Die Regenhoehe von Regendauern, die kuerzer oder gleich dem 4-maliger Intervalllaenge der Regendaten sind, werden mit den Korrekturfaktoren nach Tabelle 1 in DWA-A-531 multipliziert.
Intervallkorrektur <- function(Serie, Intervall = 5){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if(missing(Serie)) stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Maximum Serie an (in mm/h oder mm), wobei die Zeilen die Jahren und die Spalten die Regendauer entsprechen.")
  else if(class(Serie)!="data.frame") stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Maximum Serie an (in mm/h oder mm), wobei die Zeilen die Jahren und die Spalten die Regendauer entsprechen.")
  else if(any(apply(Serie,1,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlenden Zeilen.")
  else if(any(apply(Serie,2,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")

  # Bedingung 2: Das Input Intervall sollte nur ein Element vom Typ numerisch und nicht Null oder negativ sein!
  if(class(Intervall)!="numeric") stop("Das Input Intervall sollte vom numerischen Typ sein!")
  else if(length(Intervall)!=1) stop("Das Input Intervall sollte nur 1 Element haben!")
  else if(Intervall<=0) stop("Das Input Intervall kann nicht 0 oder negativ sein!")


  Anzahl_der_Intervalle = as.numeric(colnames(Serie))/Intervall
  if(any(Anzahl_der_Intervalle==1)==T) Serie[, which(Anzahl_der_Intervalle==1)] =  Serie[, which(Anzahl_der_Intervalle==1)] *1.14
  if(any(Anzahl_der_Intervalle==2)==T) Serie[, which(Anzahl_der_Intervalle==2)] =  Serie[, which(Anzahl_der_Intervalle==2)] *1.07
  if(any(Anzahl_der_Intervalle==3)==T) Serie[, which(Anzahl_der_Intervalle==3)] =  Serie[, which(Anzahl_der_Intervalle==3)] *1.04
  if(any(Anzahl_der_Intervalle==4)==T) Serie[, which(Anzahl_der_Intervalle==4)] =  Serie[, which(Anzahl_der_Intervalle==4)] *1.03
  return(Serie)
}

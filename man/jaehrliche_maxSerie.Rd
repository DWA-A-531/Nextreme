% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jaehrliche_maxSerie.R
\name{jaehrliche_maxSerie}
\alias{jaehrliche_maxSerie}
\title{Berechnung der jaehrlichen Maximum Serie}
\usage{
jaehrliche_maxSerie(
  Regendaten,
  Dauern = c(5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320, 10080),
  Intervall = 5,
  DSDmin = 240,
  SerieTyp = "INT",
  report = ""
)
}
\arguments{
\item{Regendaten}{gemessene Regenzeitreihen in festen Intervallen (vorzugsweise in 5 Minuten als pro Intervall gemessene Volumen). Als data.frame-Format mit Datum als erster Spalte (Datum als as.POSIXct-Typ) und Regenhoehe als zweiter Spalte (RH).Fehlende Werte sollten als NA angegeben werden!}

\item{Dauern}{Dauern, die fuer die Berechnung der jaehrlichen Serien verwendet sind. Die gleiche Einheit (entweder Minuten oder Stunden) wie das Intervall. Standartwerte sind: 5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320 und 10080min.}

\item{Intervall}{das Zeitintervall der Niederschlagsmessungen (entweder in Minuten oder Stunden).  Standardwert ist 5min.}

\item{DSDmin}{Mindestdauer der Trockenperiode, die fuer die Unabhaengigkeit der Extremwerte erforderlich ist, angegeben in der gleichen Einheit wie das Intervall. Standardwert ist 240 min (4 Stunden).}

\item{SerieTyp}{Typ der ausgegebenen jaehrlichen Serien entweder als Volumen in mm pro Dauer (VOL) oder Intensitaeten in mm/Stunde (INT). Standardwert ist INT.}

\item{report}{falls gewuenscht, einen Ordnerpfad, in dem die Informationen ueber die jaehrlichen Extremwerte gespeichert werden sollen}
}
\value{
Jaehrliche Maximum Serie (als Regenhoehe in mm/Dauer oder Regenintensitaet in mm/h) als data.frame, wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen.
}
\description{
Berechnung der jaehrlichen Maximum Serie (basierend auf dem Kalenderjahr) aus einer Niederschlagszeitreihe und gegebenen Dauern.
}
\details{
Funktion zur Ermittlung der jaehrlichen Serie (auf der Grundlage des Kalenderjahres) aus einer Regenzeitreihe und vorgegebenen Dauern. Eine Mindestdauer der Trockenperiode wird verwendet, um unabhaengige Regenereignisse am Anfange/Ende eines Jahres zu identifizieren. Das maximale Volumen / Intensitaet fuer jedes Jahr wird zurueckgegeben.

Fuer weitere Hinweise siehe Kapitel 5.2 des Merkblattes DWA-A 531.
}
\examples{
# Anwendung Beispiel
head(Regendaten_01684)
jaehrlicheSerie_VOL = jaehrliche_maxSerie(Regendaten_01684, SerieTyp="VOL")
jaehrlicheSerie_INT = jaehrliche_maxSerie(Regendaten_01684, SerieTyp="INT")
}

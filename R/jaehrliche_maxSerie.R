#' Berechnung der jaehrlichen Maximum Serie
#' @description
#' Berechnung der jaehrlichen Maximum Serie (basierend auf dem Kalenderjahr) aus einer Niederschlagszeitreihe und gegebenen Dauern.
#' @param Regendaten gemessene Regenzeitreihen in festen Intervallen (vorzugsweise in 5 Minuten als pro Intervall gemessene Volumen). Als data.frame-Format mit Datum als erster Spalte (Datum als as.POSIXct-Typ) und Regenhoehe als zweiter Spalte (RH).Fehlende Werte sollten als NA angegeben werden!
#' @param Dauern Dauern, die fuer die Berechnung der jaehrlichen Serien verwendet sind. Die gleiche Einheit (entweder Minuten oder Stunden) wie das Intervall. Standartwerte sind: 5, 10, 15,30,60,120,360,720,1440, 2880, 4320 und 10080min.
#' @param Intervall das Zeitintervall der Niederschlagsmessungen (entweder in Minuten oder Stunden).  Standardwert ist 5min.
#' @param DSDmin Mindestdauer der Trockenperiode, die fuer die Unabhaengigkeit der Extremwerte erforderlich ist, angegeben in der gleichen Einheit wie das Intervall. Standardwert ist 240 min (4 Stunden).
#' @param SerieTyp Typ der ausgegebenen jaehrlichen Serien entweder als Volumen in mm pro Dauer (VOL) oder Intensitaeten in mm/Stunde (INT). Standardwert ist INT.
#' @details
#' Funktion zur Ermittlung der jaehrlichen Serie (auf der Grundlage des Kalenderjahres) aus einer Regenzeitreihe und vorgegebenen Dauern.
#' 1. Alle fehlenden Werte werden als Null zugewiesen.
#' 2. Eine Mindestdauer der Trockenperiode wird verwendet, um unabhaengige Regenereignisse innerhalb eines Jahres zu identifizieren. Ist die Dauer laenger als die Trockenheitsdauer, wird die Dauer selbst fuer die Trennung unabhaengiger Regenereignisse verwendet.
#' 3. Innerhalb jedes unabhaengigen Regenereignisses in einem Jahr wird ein gleitendes Fenster mit der Dauer verwendet, um das maximale Volumen / Intensitaet zu finden.
#' 4. Das maximale Volumen / Intensitaet fuer jedes Jahr wird zurueckgegeben.
#'
#' Fuer weitere Hinweise siehe Kapitel 5.2 des Merkblattes DWA-A 531.
#'
#' @return
#' Jaehrliche Maximum Serie (als Regenhoehe in mm/Dauer oder Regenintensitaet in mm/h) als Tabelle, wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen.
#' @examples
#' # Anwendung Beispiel
#' head(Regendaten_01684)
#' jaehrlicheSerie_VOL = jaehrliche_maxSerie(Regendaten_01684, SerieTyp="VOL")
#' jaehrlicheSerie_INT = jaehrliche_maxSerie(Regendaten_01684, SerieTyp="INT")
jaehrliche_maxSerie = function(Regendaten,
                          Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                          Intervall = 5,
                          DSDmin=240,
                          SerieTyp="INT"){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Regendaten sollte existieren, vom Typ data.frame sein.
  if(missing(Regendaten)) stop("Das Regendaten Input ist nicht vorhanden! Bitte geben Sie einen data.frame der Regenzeitreihe mit 2 Spalten: Datum - Date() Typ und RH - numeric() Typ.")
  else if(class(Regendaten)!="data.frame") stop("Das Regendaten Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der Regenzeitreihe mit 2 Spalten: Datum - Date() Typ und RH - numeric() Typ.")
  else if(dim(Regendaten)[1]<5) stop("Das Regendaten Input enthaelt weniger als 5 Zeitschritte! Um Fehler zu vermeiden, sind mindestens 5 Zeitschritte erforderlich.")
  else if(dim(Regendaten)[2]!=2) stop("Das Regendaten Input soll zwei Spalten mit Namen Datum und RH enthalten! Die Spalte 'Datum' soll die Datumsangaben zur Messung enthalten (im Format Date oder POSIXct). Die Spalte 'RH' soll die Regenhoehe mm/Intervalllaenge enthalten (im Format numeric).")
  else if(any(c("Datum","RH")%in%colnames(Regendaten)==F)==T)  stop("Das Regendaten Input soll zwei Spalten mit Namen Datum und RH enthalten! Die Spalte 'Datum' sollte die Datumsangaben zur Messung enthalten (im Format Date oder POSIXct). Die Spalte 'RH' sollte die Regenhoehe mm/Intervalllaenge enthalten (im Format numeric)")
  else if(any(class(Regendaten$Datum)%in%c("Date","POSIXct","POSIXt")==F)==T) stop("Die Spalte 'Datum' der Regendaten ist keines der akzeptierten Formate: Date, POSIXct oder POSIXt!")
  else if(class(Regendaten$RH)!="numeric") stop("Die Spalte 'RH' der Regendaten ist nicht numerisch!")
  else if(any(Regendaten$RH<0, na.rm = T)==T) stop("Die Spalte 'RH' der Regendaten enthaelt negative Werte!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==1) stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")

  # Bedingung 3: Das Input Intervall sollte nur ein Element vom Typ numerisch und nicht Null oder negativ sein!
  if(class(Intervall)!="numeric") stop("Das Input Intervall sollte vom numerischen Typ sein!")
  else if(length(Intervall)!=1) stop("Das Input Intervall sollte nur 1 Element haben!")
  else if(Intervall<=0) stop("Das Input Intervall kann nicht 0 oder negativ sein!")

  # Bedingung 4: Das Input DSDmin sollte nur ein Element vom Typ numerisch und nicht Null oder negativ sein!
  if(class(DSDmin)!="numeric") stop("Das Input DSDmin sollte vom numerischen Typ sein!")
  else if(length(DSDmin)!=1) stop("Das Input DSDmin sollte nur 1 Element haben!")
  else if(DSDmin<Intervall) stop("Das Input DSDmin kann nicht kleiner als der Intervalllaenge sein!")

  # Bedingung 5: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(SerieTyp)!="character") stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  else if(length(SerieTyp)!=1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  require(lubridate)
  Jahre_mit_Daten          = unique(year(Regendaten$Datum))
  RI_je_Jahr       = lapply(Jahre_mit_Daten , function(Jahr){
    RI_Jahr = Regendaten[which(year(Regendaten$Datum)==Jahr),]
    # assigning missing values to zero
    if(length(which(is.na(RI_Jahr$RH)==T))>0) RI_Jahr$RH[which(is.na(RI_Jahr$RH)==T)] = 0
    return(RI_Jahr)
  })
  extremserie_jeDauer = lapply(RI_je_Jahr, function(entry){
    pcp                   = entry$RH
    maxVol_jeDauer_jeJahr = do.call(c, lapply(Dauern, function(dur){
      step=dur/Intervall
      #Calculating the precipitation sum (as a vector) for the moving window defined by the steps
      summ <-c(sum(pcp[1:step]), diff(cumsum(pcp),step))
      #Removing the dependent extreme based on a dry separation time of 4hours.
      extremes <- list()
      if(step<=(DSDmin/Intervall)){
        y <- diff(cumsum(summ),((DSDmin/Intervall)-step+1))
        x <- summ[1:length(y)]
      }else{
        y <- diff(cumsum(summ),1)
        x <- summ[1:length(y)]
      }
      data <- as.data.frame(cbind(x,y))
      indexes <- as.numeric(row.names(data[which(data$x!=0 & data$y==0),]))
      if(length(indexes)>1){
        extremes <- append(extremes, max(summ[1:(indexes[1])]))
        loop <- 2: length(indexes)
        for (i in loop) extremes <- append(extremes, max(summ[(indexes[(i-1)]+1):(indexes[i])]))
        if(tail(indexes,1)< (length(summ)-1))  extremes <- append(extremes, max(summ[(tail(indexes,1)+1):length(summ)]))
        # sorting the extremes
        extremes <- max(as.numeric(extremes), na.rm=T)

      } else extremes <- NA
      #print(dur)
      return(extremes)
    }))
    output                = round(maxVol_jeDauer_jeJahr,3)
    return(output)
  })
  TabelleExtreme               = as.data.frame(do.call(rbind, extremserie_jeDauer))
  colnames(TabelleExtreme)     = Dauern
  rownames(TabelleExtreme)     = Jahre_mit_Daten

  if(SerieTyp=="INT"){
    Endtabelle             = lapply(1:length(Dauern), function(i) round(TabelleExtreme[,i]/(Dauern[i]/60),3))
    Endtabelle             = as.data.frame(do.call(cbind, Endtabelle))
    colnames(Endtabelle)   = Dauern
    rownames(Endtabelle)   = Jahre_mit_Daten
  }else if (SerieTyp=="VOL") Endtabelle = TabelleExtreme
  else stop(paste0("Gewuenschter Output [", SerieTyp, "] ist nicht gueltig! Bitte geben Sie [VOL] fuer die Hoehe der jaehrlichen Extremwerte oder [INT] fuer die Intensitaet der jaehrlichen Extremwerte an."))
  return(Endtabelle)
}

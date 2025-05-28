#' Berechnung der jaehrlichen Maximum Serie
#' @description
#' Berechnung der jaehrlichen Maximum Serie (basierend auf dem Kalenderjahr) aus einer Niederschlagszeitreihe und gegebenen Dauern.
#' @param Regendaten gemessene Regenzeitreihen in festen Intervallen (vorzugsweise in 5 Minuten als pro Intervall gemessene Volumen). Als data.frame-Format mit Datum als erster Spalte (Datum als as.POSIXct-Typ) und Regenhoehe als zweiter Spalte (RH).Fehlende Werte sollten als NA angegeben werden!
#' @param Dauern Dauern, die fuer die Berechnung der jaehrlichen Serien verwendet sind. Die gleiche Einheit (entweder Minuten oder Stunden) wie das Intervall. Standartwerte sind: 5, 10, 15,30,60,120,360,720,1440, 2880, 4320 und 10080min.
#' @param Intervall das Zeitintervall der Niederschlagsmessungen (entweder in Minuten oder Stunden).  Standardwert ist 5min.
#' @param DSDmin Mindestdauer der Trockenperiode, die fuer die Unabhaengigkeit der Extremwerte erforderlich ist, angegeben in der gleichen Einheit wie das Intervall. Standardwert ist 240 min (4 Stunden).
#' @param SerieTyp Typ der ausgegebenen jaehrlichen Serien entweder als Volumen in mm pro Dauer (VOL) oder Intensitaeten in mm/Stunde (INT). Standardwert ist INT.
#' @param report falls gewuenscht, einen Ordnerpfad, in dem die Informationen ueber die jaehrlichen Extremwerte gespeichert werden sollen
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
                               SerieTyp="INT", report = ""){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Regendaten sollte existieren, vom Typ data.frame sein.
  if(missing(Regendaten)) stop("Das Regendaten Input ist nicht vorhanden! Bitte geben Sie einen data.frame der Regenzeitreihe mit 2 Spalten: Datum - Date() Typ und RH - numeric() Typ.")
  else if(class(Regendaten)!="data.frame") stop("Das Regendaten Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der Regenzeitreihe mit 2 Spalten: Datum - Date() Typ und RH - numeric() Typ.")
  else if(dim(Regendaten)[1]<5) stop("Das Regendaten Input enthaelt weniger als 5 Zeitschritte! Um Fehler zu vermeiden, sind mindestens 5 Zeitschritte erforderlich.")#AP comment: this condition might not be meaningful, as results with only 5 measurements are likely not reliable.

  else if(dim(Regendaten)[2]!=2) stop("Das Regendaten Input soll zwei Spalten mit Namen Datum und RH enthalten! Die Spalte 'Datum' soll die Datumsangaben zur Messung enthalten (im Format Date oder POSIXct). Die Spalte 'RH' soll die Regenhoehe mm/Intervalllaenge enthalten (im Format numeric).")
  else if(any(c("Datum","RH")%in%colnames(Regendaten)==F)==T)  stop("Das Regendaten Input soll zwei Spalten mit Namen Datum und RH enthalten! Die Spalte 'Datum' sollte die Datumsangaben zur Messung enthalten (im Format Date oder POSIXct). Die Spalte 'RH' sollte die Regenhoehe mm/Intervalllaenge enthalten (im Format numeric)")
  else if(any(class(Regendaten$Datum)%in%c("Date","POSIXct","POSIXt")==F)==T) stop("Die Spalte 'Datum' der Regendaten ist keines der akzeptierten Formate: Date, POSIXct oder POSIXt!")
  else if(class(Regendaten$RH)!="numeric") stop("Die Spalte 'RH' der Regendaten ist nicht numerisch!")
  else if(any(Regendaten$RH<0, na.rm = T)==T) stop("Die Spalte 'RH' der Regendaten enthaelt negative Werte!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==1) stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.") #AP comment: is it reasonable to define only one duration? Consider how this choice might influence the estimation of GEV parameters, particularly eta and theta.

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

  Regendaten$YYYY   = lubridate::year(Regendaten$Datum)
  Regendaten$MM     = lubridate::month(Regendaten$Datum)

  # Bedingung 6: fuer jedes Jahr sollte ein Minimum an verfuegbaren Beobachtungen vorliegen, um sie fuer die Niederschlagsstatistik zu beruecksichtigen
  # Zwischen Maerz und Oktober mindestens 172 Tage mit vollstaendigen Beobachtungen oder zwischen April und September mindestens 128 Tage mit vollstaendigen Beobachtungen
  daten_verfuegbar_pro_jahr_c1 = sapply(unique(Regendaten$YYYY), function(Jahr) length(which(is.na(Regendaten[which(Regendaten$YYYY==Jahr & Regendaten$MM>=3 & Regendaten$MM <=10), 2])==F)))
  daten_verfuegbar_pro_jahr_c2 = sapply(unique(Regendaten$YYYY), function(Jahr) length(which(is.na(Regendaten[which(Regendaten$YYYY==Jahr & Regendaten$MM>=4 & Regendaten$MM <=9), 2])==F)))

  PP_Jahressumme_c1  = sapply(unique(Regendaten$YYYY), function(Jahr) sum(Regendaten[which(Regendaten$YYYY==Jahr & Regendaten$MM>=3 & Regendaten$MM <=10), 2], na.rm=T))
  PP_Jahressumme_c2  = sapply(unique(Regendaten$YYYY), function(Jahr) sum(Regendaten[which(Regendaten$YYYY==Jahr & Regendaten$MM>=4 & Regendaten$MM <=9), 2], na.rm=T))

  Jahre_mit_Daten = unique(Regendaten$YYYY)[which((daten_verfuegbar_pro_jahr_c1>=172*288 & PP_Jahressumme_c1>0) |(daten_verfuegbar_pro_jahr_c2>=128*288 & PP_Jahressumme_c2>0) )]

  if(length(Jahre_mit_Daten)==0) stop("Die angegebene Station hat nicht genuegend nicht fehlende Beobachtungen pro Jahr, daher ist das jaehrliche Maximum nicht repraesentativ.")

  # alle fehlenden Daten werden auf Null gesetzt
  Regendaten$RH[which(is.na(Regendaten$RH)==T)] = 0
  DSDmin.step = DSDmin/Intervall
  extremserie_jeDauer = lapply(Dauern, function(dur){
    step=dur/Intervall
    Regendaten_Dur = Regendaten
    extremserie_jeJahr = lapply(Jahre_mit_Daten, function(Jahr){
      if(dur  > DSDmin){
        entry.vorJahr    = Regendaten_Dur[which(Regendaten_Dur$YYYY==Jahr-1),]
        entry.jahr       = Regendaten_Dur[which(Regendaten_Dur$YYYY==Jahr),]
        entry.nexteJahr  = Regendaten_Dur[which(Regendaten_Dur$YYYY==Jahr+1),]

        # die letzte Trockenperiode dieses Jahres zu finden
        summ_DSD.min_Jahr <- c(sum(entry.jahr$RH[1:DSDmin.step]), diff(cumsum(entry.jahr$RH),DSDmin.step))
        index.DSDmin.Jahr <- utils::tail(which(summ_DSD.min_Jahr==0),1)

        # die letzte Trockenperiode des vergangenen Jahres finden
        if(dim(entry.vorJahr)[1]>0){
          summ_DSD.min_vorJahr <- c(sum(entry.vorJahr$RH[1:DSDmin.step]), diff(cumsum(entry.vorJahr$RH),DSDmin.step))
          index.DSDmin_vorJahr <- utils::tail(which(summ_DSD.min_vorJahr==0),1)
          # der Fall, dass die Vorjahresbeobachtungen kuerzer als DSDmin sind (deshalb hat der Index die Laenge Null)
          if(length(index.DSDmin_vorJahr)==0) index.DSDmin_vorJahr = 1
          entry.vorJahr   <- entry.vorJahr[index.DSDmin_vorJahr:dim(entry.vorJahr)[1],]
        }

        # die erste Trockenperiode im naechsten Jahr finden
        if(dim(entry.nexteJahr)[1]>0){
          summ_DSD.min_nJahr <- c(sum(entry.nexteJahr$RH[1:DSDmin.step]), diff(cumsum(entry.nexteJahr$RH),DSDmin.step))
          index.DSDmin_nJahr <- which(summ_DSD.min_nJahr==0)[1]
          # der Fall, dass die Beobachtungen des naechsten Jahres kuerzer sind als DSDmin (deshalb hat der Index die Laenge Null)
          if(length(index.DSDmin_nJahr)==0) index.DSDmin_nJahr = length(entry.nexteJahr$RH)
          entry.nexteJahr   <- entry.nexteJahr[1:index.DSDmin_nJahr,]
        }
        entry                = rbind(entry.vorJahr, entry.jahr, entry.nexteJahr)
      }else entry           = Regendaten_Dur[which(Regendaten_Dur$YYYY==Jahr),]
      # Berechnung der Niederschlagssumme (als Vektor) fuer das gleitende Fenster (step)
      summ <-c(sum(entry$RH[1:step]), diff(cumsum(entry$RH),step))
      # Ermittlung der maximalen Niederschlagsmenge und der entsprechenden End- und Startzeit
      max.RH = max(summ, na.rm=T)
      start.max = entry$Datum[which.max(summ)[1]]
      end.max   = entry$Datum[which.max(summ)[1]+step-1]
      # Wenn das Maximum am Ende des Jahres oder am Anfang des naechstes Jahres liegt, koennte es Probleme mit der Ereignisunabhaengigkeit geben. Daher wird dieses Ereignis, das den hier verwendeten Hoechstwert enthaelt, auf Null gesetzt.
      if((lubridate::month(start.max)==12 & lubridate::year(start.max)==Jahr-1) |(lubridate::month(start.max)==12 & lubridate::year(start.max)==Jahr) | (lubridate::month(end.max)==1 & lubridate::year(end.max)==(Jahr+1))){
        vorRegen = entry$RH[which(entry$Datum< start.max)]
        if(length(vorRegen)>=DSDmin.step){
          vorDSD = c(sum(vorRegen[1:DSDmin.step]), diff(cumsum(vorRegen),DSDmin.step))
          vorStart = entry$Datum[utils::tail(which(vorDSD==0),1)]
        }else vorStart = start.max
        nachRegen = entry$RH[which(entry$Datum> end.max)]
        if(length(nachRegen)>=DSDmin.step){
          nachDSD = c(sum(nachRegen[1:DSDmin.step]), diff(cumsum(nachRegen),DSDmin.step))
          nachEnde = end.max + utils::head(which(nachRegen==0),1)*Intervall*60
        }else nachEnde = end.max
        Regendaten_Dur$RH[which(Regendaten_Dur$Datum%in%seq(vorStart, nachEnde, Intervall*60))] <<- 0
      }
      output = data.frame(Vol = max.RH, Zeit = start.max, Dauer = dur, Jahr= Jahr)
      return(output)
    })
    Output = do.call(rbind, extremserie_jeJahr)
    return(Output)
  })

  Extra_Info                   = as.data.frame(do.call(rbind, extremserie_jeDauer))
  if(dir.exists(report)==T)    utils::write.table(Extra_Info, file=paste0(report, "Info_Jaehrliche_maxSerie.txt"), sep=";", row.names=F, col.names = T, quote=F)
  TabelleExtreme               = as.data.frame(do.call(cbind, lapply(extremserie_jeDauer, function(i) i$Vol )))
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

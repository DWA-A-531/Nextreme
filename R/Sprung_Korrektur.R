#' Spruengkorrektur bei kleinen Dauerstufen (D<= 30min)
#' @description
#' Die jaehrlichen Serien von kurzen Dauern werden vom Sprung-Instationaritaet korrigiert, der durch den Wechsel der Messsensoren von analoger zu digitaler Technologie verursacht werden koennte.
#' Analogsensoren: Regenschreiber, Unbekannt oder unbekannt
#' Digitalsensoren: H3, Tropfengeber, Wippengeber, Pluvio oder PLUVIO
#' 1. Zuerst wird ein Instationaritaet Test fuer die Jahresreihen mit einer Dauer von 30 Minuten oder weniger durchgefuehrt.
#' 2. Wenn die Instationaritaet vom Typ "Sprung" ist, dann wird eine Sprungkorrektur angewendet.
#' @param Serie Jaehrliche Reihen als Tabelle, wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen (in Minuten!).
#' @param wechselDatum Der Zeitpunkt, zu dem der Sensor von einem analogen auf einen digitalen Sensor umgestellt wurde. Angegeben als as.POSIXct-Format.
#' @details
#' Wenn es einen Sprung gibt - Typ Instationaritaet auf der jaehrlichen Serie von kurzen Dauern, wird der Sprung eliminiert und eine korrigierte jaehrliche Serientabelle zurueckgegeben.
#' @return
#' Die korrigierte jaehrlichen Serien als data.frame wird zurueckgegeben (das gleiche Format wie die Eingabe).
#' @examples
#' wechselDatum = as.Date("1992-12-31", format=c("%Y-%m-%d"))
#' korrigierte_maxSerie = Sprung_Korrektur(Goerlitz_maxSerie, wechselDatum)
#' print(korrigierte_maxSerie)
Sprung_Korrektur  <- function(Serie, wechselDatum){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if(missing(Serie)) stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und Spalte die Dauer entsprechen.")
  else if(class(Serie)!="data.frame") stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und Spalte die Dauer entsprechen.")
  else if(dim(Serie)[1]<5) stop("Das Serie Input enthaelt weniger als 5 Jahren! Um Fehler zu vermeiden, sind mindestens 5 Jahren erforderlich.")
  else if(dim(Serie)[2]==1) stop("Das Serie Input enthaelt nur ein Dauer! Um Fehler zu vermeiden, sind mindestens 2 Dauern erforderlich.")
  else if(any(apply(Serie,1,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlende Zeile.")
  else if(any(apply(Serie,2,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")
  else if(length(which(as.numeric(colnames(Serie))<=30))<1) stop("Das Serie Input sollte mindestens eine Spalte mit einer Dauer kleiner oder gleich 30 Minuten enthalten! Die Funktion wird nur auf die Spalten angewendet, deren Namen kuerzer oder gleich 30 Minuten sind.")
  else if(length(which(as.numeric(rownames(Serie))>1800))<1) stop("Das Serie Input die Jahre als Zeilennamen enthalten. Achten Sie darauf, dass die Jahreszahlen vierstellig sind (z.B. groesser als 1800).")

  # Bedingung 2: Das Input wechselDatum sollte nur ein Element vom Typ Date() und nicht Nullsein!
  if(class(wechselDatum)!="Date") stop("Das Input wechselDatum sollte vom Date() Typ sein!")
  else if(length(wechselDatum)!=1) stop("Das Input wechselDatum sollte nur 1 Element haben!")

  Wechseljahr = lubridate::year(wechselDatum)
  alleJahren  = as.numeric(rownames(Serie))
  Dauern  = as.numeric(colnames(Serie))
  suppressWarnings({
  if(Wechseljahr>alleJahren[1] & Wechseljahr< alleJahren[length(alleJahren)]){
    if( Wechseljahr%in%alleJahren==F) Wechseljahr = alleJahren[which(alleJahren>=Wechseljahr)[1]]
    sensor_vec = rep("analog", length(Serie[,1]))
    sensor_vec[which(rownames(Serie)==Wechseljahr):length(sensor_vec)] = "digital"

    # Pruefung auf „Sprung“ in Spalten mit Dauern <= 30 min und wenn mindestens ein „Sprung“ entdeckt wird, korrigiere alle AMS fuer Dauern <= 30 min
    Sprung_detected = any(apply(Serie[, which(Dauern <= 30)], 2, function(x) {
      SIG.TEST = Trend_vs_Sprung(Zeit = 1:length(x[which(is.na(x) == F)]),Serienwerte = x[which(is.na(x) == F)],Sensor = sensor_vec[which(is.na(x) == F)])
      return(SIG.TEST$AicRes == "Sprung")
    }))

    if (Sprung_detected == TRUE) {
      Serie[, which(Dauern <= 30)] = apply(Serie[, which(Dauern <= 30)], 2, function(x) {
        x_out = x
        x_out[which(is.na(x) == F)] = Sprung_Elimination(x[which(is.na(x) == F)],sensor_vec[which(is.na(x) == F)], ZielSensor = sensor_vec[length(sensor_vec)])$SerieNeu
        return(round(x_out, 3))
      })
    }
  }
  })
  return(Serie)
}

#' Schaetzung der Parameter der extremen Niederschlagsreihen
#' @description
#' Berechnung der GEV-Parameter und der Koutsoyiannis-Parameter fuer die gegebenen jaehrlichen Serien mit unterschiedlichen Dauern.
#' 1. Die Koutsoyiannis-Parameter, die die Intensitaeten ueber alle Dauern normalisieren, werden auf der Grundlage der Kruskal-Wallis-Statistik geschaetzt.
#' 2. Die GEV-Parameter werden nach der Methode der L-Momente geschaetzt (mit Ausnahme des Formparameters, der im Voraus auf einen bestimmten Wert festgelegt werden kann
#' @param Serie jaehrliche Maximum Serie als Tabelle, wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen. Die Werte der Tabelle sollten entweder als Regenintensitaet mm/h oder in Regenhoehe mm/Dauer angegeben werden. Bitte geben Sie die Einheiten entsprechend ueber die Variable SerieTyp an.
#' @param SerieTyp Information ueber die Einheiten der Eingabetabelle (Serie). Die Optionen sind: "VOL" fuer Regenhoehe in mm/Dauer, und "INT" fuer Regenintensitaet in mm/h.
#' @param Dauern Dauern, die fuer die Berechnung der jaehrlichen Maximum Serien verwendet sind. Die gleiche Einheit (entweder Minuten oder Stunden) wie das Intervall. Standartwerte sind: 5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320 und 10080min.
#' @param methGEV  Typ der Generalisierten Extremwertverteilung (GEV), die an die jaehrlichen Maximum Serien angepasst werden soll. Optionen sind: "GEV" fuer Typ 2 oder Typ 3 (Formparameter ist nicht gleich Null) und "GUM" fuer Typ 1 (Formparameter ist gleich Null – Gumbel Verteilung.)
#' @param formTyp kontrolliert, wie der Formparameter der Generalisierten Extremwertverteilung (nur bei methGEV=„GEV“) geschaetzt werden soll. Die Option „CON“ berechnet die Formparameter auf der Basis der L-Momente, und die Option „FIX“ erzwingt einen bestimmten Wert fuer den Formparameter (zum Beispiel -0,1).
#' @param Gamma den vorbestimmten Wert des GEV-Form-Parameters angeben. Nur wichtig fuer die Variante von methGEV=„GEV“ und formTyp=„FIX“.
#' @details
#' Funktion zur Berechnung der Parameter, die die Niederschlagsextremwerte an einer einzelnen Station auf der Grundlage der extrahierten jaehrlichen Maximum Serien (als Regenintensitaet in mm/h) verschiedener Dauern beschreiben.
#'
#' 1. Die Koutsoyiannis-Parameter normalisieren die Intensitaeten ueber alle Dauern und werden auf der Grundlage der Kruskal-Wallis-Statistik geschaetzt.
#'
#' 2. Die GEV-Parameter werden nach der Methode der L-Momente geschaetzt (mit Ausnahme des Formparameters, der im Voraus auf einen bestimmten Wert festgelegt werden kann).
#' @return GEV- und Koutsoyiannis-Parameter fuer die angegebene Serie als einzeiliger data.frame. Die Namen der Variablen im data.frame sind Mu / Sigma / Gamma - jeweils fuer die GEV- Lokations- / Skalen- / Formparameter, und Theta / Eta fuer die 1./ 2. Koustoyiannis-Parameter.
#' @examples
#' # Berechnung der dauerstufenübergreifenden Verteilungsparameter fuer die Station Goerlitz im Zeitraum 1991-2020
#' # ohne Intervall-oder Sprungkorrektur
#' # Fall 1: ueber alle Dauern mit der GEV-Verteilung und dem Formparameter von -0,1
#' Dauern = c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
#' extremParameter = Parameter_Schaetzung(Goerlitz_maxIntSerie,Dauern, methGEV="GEV", formTyp="FIX", Gamma=-0.1)
#' print(extremParameter)
#' # Fall 2: ueber alle Dauern mit der Gumbel-Verteilung
#' extremParameter = Parameter_Schaetzung(Goerlitz_maxIntSerie,Dauern, methGEV="GUM")
#' print(extremParameter)
Parameter_Schaetzung = function(Serie, Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                               methGEV="GEV", formTyp="FIX", Gamma=-0.1, SerieTyp="INT"){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if(missing(Serie)) stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  else if(class(Serie)!="data.frame") stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  else if(dim(Serie)[1]<5) stop("Das Serie Input enthaelt weniger als 5 Jahren! Um Fehler zu vermeiden, sind mindestens 5 Jahren erforderlich.")
  else if(dim(Serie)[2]==1) stop("Das Serie Input enthaelt nur ein Dauer! Um Fehler zu vermeiden, sind mindestens 2 Dauern erforderlich.")
  else if(any(apply(Serie,1,function(i) all(is.na(i)==T)==T))){
    warning("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Die fehlenden Zeilen werden aus dem Datenframe entfernt.")
    Serie = Serie[which(apply(Serie,1,function(i) all(is.na(i)==T)==T)==F),]
  }else if(any(apply(Serie,2,function(i) all(is.na(i)==T)==T))){
    warning("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten! Die fehlenden Spalten werden aus dem Datenframe entfernt.")
    Serie = Serie[,which(apply(Serie,2,function(i) all(is.na(i)==T)==T)==F)]
    Dauern = Dauern[which(apply(Serie,2,function(i) all(is.na(i)==T)==T)==F)]
  }else if(any(Serie<=0)) stop("Das Serie Input enthaelt negative oder Null Werte!")
  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==1) stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")

  # Bedingung 3: Laenge und Namen der Dauer sollte mit der Anzahl und Namen der Spalten in der jaehrlichenSerie uebereinstimmen
  if(dim(Serie)[2]!=length(Dauern)) stop("Die Anzahl der Spalten der jaehrlichenSerie sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")
  else if(identical(as.numeric(colnames(Serie)), Dauern)==F) stop("Die Spaltennamen der jaehrlichenSerie sollten mit den bei Vektor Dauern angegebenen Dauern identisch sein.")

  # Bedingung 4: Das Input Gamma sollte nur ein Element vom Typ numerisch und nicht Null sein!
  if(class(Gamma)!="numeric") stop("Formparameter Gamma sollte vom numerischen Typ sein!")
  else if(length(Gamma)!=1) stop("Formparameter Gamma sollte nur 1 Element haben!")
  else if(Gamma==0) stop("Der Formparameter Gamma kann nicht 0 sein! Bitte verwenden Sie methGEV als GUM, wenn Sie den Formparameter auf 0 setzen wollen.")

  # Bedingung 5: methGEV sollte nur ein Element vom Typ Charakter sein!
  if(class(methGEV)!="character") stop("Das methGEV Input sollte vom Charaktertyp sein!")
  else if(length(methGEV)!=1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 6: formTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(formTyp)!="character") stop("Das formTyp Input sollte vom Charaktertyp sein!")
  else if(length(formTyp)!=1) stop("Das formTyp Input sollte nur 1 Element haben!")

  # Bedingung 7: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(SerieTyp)!="character") stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  else if(length(SerieTyp)!=1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  Dauern_inStunden = round(Dauern/60,3)
  j = 1:length(Dauern_inStunden)
  nD = apply(Serie[,1:(length(Dauern_inStunden))],2, function(i){
    if(length(which(is.na(i)==T))>0) out = length(i[-which(is.na(i)==T)])
    else out = length(i)
    return(out)
  })
  ##### Converting to Intensities
  if(SerieTyp=="INT") Serie = Serie
  else if(SerieTyp =="VOL") Serie = round(Serie/as.data.frame(t(replicate(dim(Serie)[1],Dauern_inStunden))),3)
  else stop(paste0("Die gegebene Charakter fuer den SerieTyp [", SerieTyp, "] existiert nicht! Bitte INT fuer die Regenintensitaet (mm/h) or VOL fuer die Regenhoehe (mm/h) eingeben."))

  ##### Intensitaeten sollten mm/Stunde sein!
  Inten.Daten  = do.call(cbind, lapply(1:length(Dauern_inStunden), function(i) sort(round(Serie[,i],3),na.last=TRUE, decreasing=T)))
  Partition = round(as.numeric(min(nD[1], na.rm=T)),0)
  # SCHRITT 1 Berechnung der Koutsoyiannis Parameter
  m = Partition*length(Dauern_inStunden)
  Theta.Werte = stats::optimize(kw_koupar1, lower=0, upper=1,  Inten.Daten = Inten.Daten, Dauern=Dauern_inStunden, Partition = Partition, nD=nD, m=m, maximum=FALSE)
  Eta.Werte = stats::optimize(kw_koupar2, lower=0, upper=1, Dauern = Dauern_inStunden, Theta=Theta.Werte$minimum, Inten.Daten = Inten.Daten,Partition=Partition,nD=nD,m=m, maximum=FALSE)
  output = data.frame(Theta=Theta.Werte$minimum, Eta= Eta.Werte$minimum, KW = Eta.Werte$objective)
  bD = (Dauern_inStunden+output$Theta)^output$Eta
  alle.Inten = do.call(c, lapply(1:length(Dauern_inStunden), function(i) Inten.Daten[,i]*bD[i]))
  # falls vorhanden, fehlende Werte entfernen
  if(length(which(is.na(alle.Inten)==T))>0) alle.Inten = alle.Inten[-which(is.na(alle.Inten)==T)]

  # SCHRITT 2 Berechnung der GEV Parameter
  lmoments = lmomco::lmom.ub(unlist(alle.Inten))
  if(methGEV=="GEV"){
    if(formTyp=="FIX") gev.par  = pargev2(lmoments, kappa=Gamma)
    else if (formTyp=="CON") gev.par  = lmomco::pargev(lmoments)
    else stop(paste0("Die gegebene Chararakter fuer den Formtyp [", formTyp, "] existiert nicht! Bitte FIX oder CON eingeben."))
    extrem.Parameter  = data.frame(Mu = gev.par$para[1], Sigma=gev.par$para[2],Gamma=gev.par$para[3], Theta=output$Theta, Eta=output$Eta,
                                   KW = output$KW)
  }else if (methGEV=="GUM"){
    gum.par  = lmomco::pargum(lmoments)
    extrem.Parameter  = data.frame(Mu = gum.par$para[1], Sigma=gum.par$para[2], Gamma=0, Theta=output$Theta, Eta=output$Eta,
                                   KW = output$KW)
  }else stop(paste0("Die gegebene Chararakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))
  rownames(extrem.Parameter) = NULL
  return(extrem.Parameter)
}

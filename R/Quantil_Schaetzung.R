#' Berechnung Starkniederschlaege fuer bestimmte Dauern und Wiederkehrintervalle
#' @description
#' Berechnung der Starkniederschlaege (entweder in Volumen mm oder Intensitaet mm/h) fuer bestimmte Dauern (z.B. 5, 10, 60 und 120min) und Wiederkehrintervalle (z.B. 5, 10 und 100 Jahren), wenn die Parameter, die die Extremwerte beschreiben, bereits bekannt sind.
#' @param extrem.Parameter GEV- und Koutsoyiannis-Parameter fuer die angegebene Serie als einzeiliger data.frame. Die Namen der Variablen im data.frame sind Mu / Sigma / Gamma - jeweils fuer die GEV- Lokations- / Skalen- / Formparameter, und Theta / Eta fuer die 1./ 2.Koustoyiannis-Parameter.
#' @param Dauern die Dauer, fuer die die Regenhoehe berechnet werden soll. Die Dauer sollte in Minuten angegeben werden!
#' @param Tn die Wiederkehrintervalle, fuer die die Regenhoehe berechnet werden soll. Die Wiederkehrintervalle sollte in Jahren angegeben werden!
#' @param methGEV den Typ der Generalized Extreme Value-Verteilung, die an die jaehrlichen Serien angepasst wurde. Die Optionen sind: "GEV" fuer Typ 2 oder Typ 3 (Form-Parameter ist nicht gleich Null) und "GUM" fuer Typ 1 (Form-Parameter ist gleich Null - also Gumberl Verteilung.)
#' @param SerieTyp Kontrolle ueber die Einheiten der Ausgabetabelle. Die Optionen sind: "VOL" fuer Regenhoehe in mm/Dauer, und "INT" fuer Regenintensitaet in mm/h.
#' @details
#' R-Funktion zur Ableitung der Regenhoehe (hN) oder Regenintesitaet (rN) fuer die gegebenen Extremwertparameter (sowohl GEV- als auch Koutsoyiannis-Parameter), Dauern und Wiederkehrintervalle, nur fuer eine einzelne Station.
#' @return Eine Tabelle im Dataframe-Format, die entweder die Regenhoehe-Dauer-Wiederkehrintervall hN(D,Tn) oder Regenintensitaet-Dauer-Wiederkehrintervall iN(D,Tn) enthaelt. Die Spalten geben die Dauer (D) an und die Zeilen die Wiederkehrintervalle (Tn).
#' @examples
#' # Berechnung der Starkregenparameter fuer die Station Goerlitz im Zeitraum 1991-2020
#' # ohne Intervall-oder Sprungkorrektur
#' # ueber alle Dauern mit der GEV-Verteilung und dem Formparameter von -0,1
#' Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
#' extremParameter = Parameter_Schaetzung(Goerlitz_iN,Dauern, methGEV="GEV", formTyp="FIX", Gamma=-0.1)
#' print(extremParameter)
#' # Berechnung der Regenintensitaet-Dauer-Wiederkehrintervall Tabelle
#' # fuer 6 Wiederkehrintervalle und 12 Dauern von der berechneten Parameter
#' Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
#' Tn=c(1,5,10,20,50,100)
#' IDF_Tabelle = Quantil_Schaetzung(extremParameter, Dauern, Tn, methGEV="GEV", SerieTyp="INT")
#' # Berechnung der Regenhoehe-Dauer-Wiederkehrintervall Tabelle
#'# fuer 6 Wiederkehrintervalle und 8 Dauern von der berechneten Parameter.
#' Dauern=c(60,120,360,720,1440, 2880, 4320, 10080)
#' Tn=c(1,5,10,20,50,100)
#' DDF_Tabelle = Quantil_Schaetzung(extremParameter, Dauern, Tn, methGEV="GEV", SerieTyp="VOL")
Quantil_Schaetzung = function(extrem.Parameter,
                              Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                              Tn=c(1,5,10,20,50,100),
                              methGEV="GEV",
                              SerieTyp = "VOL"){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input extrem.Parameter sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.

  if(missing(extrem.Parameter)) stop("Das extrem.Parameter Input ist nicht vorhanden! Bitte geben Sie die GEV- und Koutsoyiannis-Parametern als einzeiliger data.frame.")
  else if(class(extrem.Parameter)!="data.frame") stop("Das extrem.Parameter Input sollte als data.frame sein! Bitte geben Sie die GEV- und Koutsoyiannis-Parameter als einzeiliger data.frame.")
  else if(dim(extrem.Parameter)[1]!=1) stop("Das extrem.Parameter Input soll nur eine Zeile enthalten!")
  else if(dim(extrem.Parameter)[2]<5) stop("Das extrem.Parameter Input soll mindestens 5 Spalten enthalten (bzw. fuer Mu, Sigma, Gamma, Theta und Eta)!")
  else if(any(is.na(extrem.Parameter[1,])==T)) stop("Das extrem.Parameter Input enthaelt mindestens einen fehlenden Werte! Entfernen Sie die fehlenden Werte.")
  else if(any(c("Mu","Sigma", "Gamma","Theta","Eta")%in%names(extrem.Parameter)==F)) stop("Das extrem.Parameter Input sollte alle der folgenden Spaltennamen enthalten: Mu, Sigma, Gamma, Theta, Eta!")
  else if(extrem.Parameter$Mu <= 0) stop("Der Lokationsparameter Mu ist kleiner oder gleich Null!")
  else if(extrem.Parameter$Sigma <= 0) stop("Der Skalenparameter Sigma ist kleiner oder gleich Null!")
  else if(extrem.Parameter$Theta < 0 ) stop("Der erste Koutsoyiannis Parameter Theta ist kleiner als Null!")
  else if(extrem.Parameter$Eta < 0 & extrem.Parameter$Eta >1 ) stop("Der zweite Koutsoyiannis Parameter Eta ist kleiner als Null oder grosser als 1!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==1) stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt Negative oder Null Werte!")

  # Bedingung 3: Das Input Tn sollte als Zahlenvektor angegeben werden und mehr als 0 Element haben.
  if(class(Tn)!="numeric") stop("Das Tn Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben.")
  else if(length(Tn)==0) stop("Das Tn Input ist leer!")
  else if(any(is.na(Tn)==T)) stop("Das Tn Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Tn<=0)) stop("Das Tn Input enthaelt Negative oder Null Werte!")

  # Bedingung 4: methGEV sollte nur ein Element vom Typ Charakter sein!
  if(class(methGEV)!="character") stop("Das methGEV Input sollte vom Charaktertyp sein!")
  else if(length(methGEV)!=1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 5: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(SerieTyp)!="character") stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  else if(length(SerieTyp)!=1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  Dauern_inStunden        = Dauern/60
  bD         = (Dauern_inStunden+extrem.Parameter$Theta)^extrem.Parameter$Eta
  Tn_Input   = Tn
  Tn = sapply(Tn, function(Tn) exp(1/Tn)/(exp(1/Tn)-1))
  Tn = round(Tn,2)
  Tn[which(Tn>10)] = round(Tn[which(Tn>10)],0)
  Quantile = (1-(1/Tn))


  if(methGEV=="GEV"){
    pars           = lmomco::pargev(lmomco::lmom.ub(1:10))
    pars$para[1]   = extrem.Parameter$Mu
    pars$para[2]   = extrem.Parameter$Sigma
    pars$para[3]   = extrem.Parameter$Gamma
    quan           = lmomco::quagev(Quantile, pars)
  }else if (methGEV=="GUM"){
    pars           = lmomco::pargum(lmomco::lmom.ub(1:10))
    pars$para[1]   = extrem.Parameter$Mu
    pars$para[2]   = extrem.Parameter$Sigma
    quan           = lmomco::quagum(Quantile, pars)
  }else stop(paste0("Die gegebene Charakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))

  IDF              = do.call(rbind, lapply(quan, function(qq) do.call(c, lapply(bD, function(beta) qq/beta))))
  IDF              = round(as.data.frame(IDF),2)
  rownames(IDF)    = Tn_Input
  colnames(IDF)    = Dauern

  DDF              = do.call(cbind, lapply(1:length(Dauern), function(dur) IDF[,dur]*Dauern_inStunden[dur]))
  DDF              = round(as.data.frame(DDF),2)
  rownames(DDF)    = Tn_Input
  colnames(DDF)    = Dauern

  if(SerieTyp=="INT") OUTPUT = IDF
  else if(SerieTyp =="VOL") OUTPUT = DDF
  else stop(paste0("Die gegebene Charakter fuer den SerieTyp [", SerieTyp, "] existiert nicht! Bitte INT fuer die Regenintensitaet-Dauer-Jaehrlichkeit Tabelle or VOL fuer die Regenhoehe-Dauer-Jaehrlichkeit Tabelle eingeben."))

  return(OUTPUT)
}

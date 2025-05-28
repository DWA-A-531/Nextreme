#' Berechnung der Wiederkehrintervalle fuer bestimmte Starkniederschlaege und Dauern.
#' @description
#' Berechnung der Wiederkehrintervalle (in Jahren) fuer bestimmte fuer bestimmte Regenhoehe (in mm) und -dauern (z.B. 5, 10, 60 und 120min), wenn die Parameter, die die Extremwerte beschreiben, bereits bekannt sind.
#' @param extrem.Parameter GEV- und Koutsoyiannis-Parameter fuer die angegebene Serie als einzeiliger data.frame. Die Namen der Variablen im data.frame sind Mu / Sigma / Gamma - jeweils fuer die GEV- Lokations- / Skalen- / Formparameter, und Theta / Eta fuer die 1./ 2.Koustoyiannis-Parameter.
#' @param Dauern die Dauer, fuer die die Regenhoehe berechnet werden soll. Die Dauer sollte in Minuten angegeben werden!
#' @param hN Regenhoehen (in mm) fuer jede der Dauern, fuer die die Wiederkehrintervalle geschaetzt werden sollte.
#' @param methGEV den Typ der Generalized Extreme Value-Verteilung, die an die jaehrlichen Serien angepasst wurde. Die Optionen sind: "GEV" fuer Typ 2 oder Typ 3 (Form-Parameter ist nicht gleich Null) und "GUM" fuer Typ 1 (Form-Parameter ist gleich Null – Gumbel Verteilung).
#' @details
#' R-Funktion zur Berechnung der Wiederkehrintervalle (in Jahren) bestimmter Regenhoehen (in mm) bei verschiedenen Dauern (in Minuten).
#' @examples
#' # Berechnung der Starkregenparameter fuer die Station Goerlitz im Zeitraum 1991-2020,
#' # ohne Intervall-oder Sprungkorrektur ueber alle Dauern
#' # mit der GEV-Verteilung und dem Formparameter von -0,1
#' Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
#' extremParameter = Parameter_Schaetzung(Goerlitz_iN,Dauern, methGEV="GEV", formTyp="FIX", Gamma=-0.1)
#' print(extremParameter)
#' # Am 18 Juli 2010 wurden an der Station Goerlitz 58,6 mm in 6 Stunden gemessen.
#' # Basierend auf den geschaetzten Parametern betraegt die berechnete Wiederkehrperiode:
#' Ta_Ereignis = Tn_Schaetzung(extremParameter, Dauern = 360, hN= 58.6, methGEV="GEV")
#' # Auf der Grundlage dieser Parameter wird die entsprechende Wiederkehrintervalle fuer
#' # die Niederschlagsmenge hN=40 mm und Dauern 60, 120 und 240 Minuten:
#' Ta_Ereignis = Tn_Schaetzung(extremParameter, Dauern = c(60,120,360), hN= c(40,40,40), methGEV="GEV")
#' # Auf der Grundlage dieser Parameter wird die entsprechende Wiederkehrintervalle fuer
#' # die Niederschlagsmenge hN=c(50,90,95) mm und Dauern 240, 720 und 1440 Minuten:
#' Ta_Ereignis = Tn_Schaetzung(extremParameter, Dauern = c(240,720,1440), hN= c(50,90,95), methGEV="GEV")
#' @return Eine Tabelle im data.frame-Format, die die Wiederkehrintervalle fuer die gegebene Regenhoehe und -dauer enthaelt. Die Spalten geben die Regenhoehe (hN), die Dauer (D) und die Jaehrlichkeit (Tn) an.
Tn_Schaetzung = function(extrem.Parameter,
                         Dauern= c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                         hN   = c(5, 10, 12,20,30, 40, 60, 70, 100, 100,   120,   150),
                         methGEV="GEV"){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input extrem.Parameter sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.

  if(missing(extrem.Parameter)) stop("Das extrem.Parameter Input ist nicht vorhanden! Bitte geben Sie die GEV- und Koutsoyiannis-Parametern als einzeiliger data.frame.")
  else if(class(extrem.Parameter)!="data.frame") stop("Das extrem.Parameter Input sollte als data.frame sein! Bitte geben Sie die GEV- und Koutsoyiannis-Parameter als einzeiliger data.frame.")
  else if(dim(extrem.Parameter)[1]!=1) stop("Das extrem.Parameter Input soll nur eine Zeile enthalten!")
  else if(dim(extrem.Parameter)[2]<5) stop("Das extrem.Parameter Input soll mindestens 5 Spalten enthalten (bzw. fuer Mu, Sigma, Gamma, Theta und Eta)!")
  else if(any(is.na(extrem.Parameter[1,])==T)) stop("Das extrem.Parameter Input enthaelt mindestens einen fehlenden Wert! Entfernen Sie die fehlenden Werte")
  else if(any(c("Mu","Sigma", "Gamma","Theta","Eta")%in%names(extrem.Parameter)==F)) stop("Das extrem.Parameter Input sollte alle der folgenden Spaltennamen enthalten: Mu, Sigma, Gamma, Theta, Eta!")
  else if(extrem.Parameter$Mu <= 0) stop("Der Lokationsparameter Mu ist kleiner oder gleich Null!")
  else if(extrem.Parameter$Sigma <= 0) stop("Der Skalenparameter Sigma ist kleiner oder gleich Null!")
  else if(extrem.Parameter$Theta < 0 ) stop("Der erste Koutsoyiannis Parameter Theta ist kleiner als Null!")
  else if(extrem.Parameter$Eta < 0 & extrem.Parameter$Eta >1 ) stop("Der zweite Koutsoyiannis Parameter Eta ist kleiner als Null oder grosser als 1!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==0) stop("Das Dauern Input ist leer")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt negative oder Null Werte!")

  # Bedingung 3: Das Input hN sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(hN)!="numeric") stop("Das hN Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(hN)==0) stop("Das hN Input ist leer!")
  else if(any(is.na(hN)==T)) stop("Das hN Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(hN<=0)) stop("Das hN Input enthaelt negative oder Null Werte!")

  # Bedingung 4: Laenge der Dauer sollte mit der Laenge der hN uebereinstimmen
  if(length(hN)!=length(Dauern)) stop("Die Laenge des hN Inputs sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")


  # Bedingung 5: methGEV sollte nur ein Element vom Typ Charakter sein!
  if(class(methGEV)!="character") stop("Das methGEV Input sollte vom Charaktertyp sein!")
  else if(length(methGEV)!=1) stop("Das methGEV Input sollte nur 1 Element haben!")

  Dauern_inStunden        = Dauern/60
  bD         = (Dauern_inStunden+extrem.Parameter$Theta)^extrem.Parameter$Eta

  alle.Inten      = (hN/Dauern*60)*bD

  if(methGEV=="GEV"){
    pars           = lmomco::pargev(lmomco::lmom.ub(1:10))
    pars$para[1]   = extrem.Parameter$Mu
    pars$para[2]   = extrem.Parameter$Sigma
    pars$para[3]   = extrem.Parameter$Gamma
    probs          = lmomco::cdfgev(alle.Inten, pars)
  }else if (methGEV=="GUM"){
    pars           = lmomco::pargum(lmomco::lmom.ub(1:10))
    pars$para[1]   = extrem.Parameter$Mu
    pars$para[2]   = extrem.Parameter$Sigma
    probs          = lmomco::cdfgev(alle.Inten, pars)
  }else stop(paste0("Die gegebene Chararakter fuer den methGEV [", methGEV, "] existiert nicht! Bitte GEV fuer Generalized Extreme Value oder GUM fuer Gumbell Verteilung eingeben"))

  returnPeriod1     = (1/(1-probs))

  returnPeriod = sapply(returnPeriod1, function(Tn) 1/(log(Tn)-log(Tn-1)))
  returnPeriod = round(returnPeriod,2)
  returnPeriod[which(returnPeriod>10)] = floor(returnPeriod[which(returnPeriod>10)])

  Output        = data.frame(hN = hN, D = Dauern, Tn = returnPeriod)

  return(Output)
}

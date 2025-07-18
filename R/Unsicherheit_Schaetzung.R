#' Schaetzung der Stichprobenunsicherheit durch Bootstrapping.
#' @description
#' Die Stichprobenunsicherheit der extremen Niederschlagsparameter und der erforderlichen Quantile wird auf der Grundlage des Bootstrapping-Algorithmus berechnet, wie in Kapitel 6.3 des DWA-A 531 Merkblattes beschrieben.
#' 1. die Jahre der jaehrlichen Maximum Serien (als Regenintensitaet in mm/h) werden nBoots mal mit Ersetzung neu gesampelt. Die neu gesampelten Jahre werden fuer jede Dauer selektiert und bilden so nBoots neue jaehrliche Serien.
#' 2. fuer jede aehrlichen Maximum Serien (als Regenintensitaet in mm/h) werden die Parameter berechnet. Die Konfidenzgrenzen fuer jeden Parameter werden aus nBoots berechnet.
#' 3. fuer jeden Parametersatz wird die Regenhoehe/-intensitaet fuer die gewuenschten Dauern und Wiederkehrintervalle berechnet, wobei die Konfidenzgrenzen fuer jeden Wert aus nBoots errechnet werden.
#' @param Serie Jaehrliche Maximum Serien (als Regenintensitaet in mm/h) werden als Tabelle (data.frame Format), wo die Anzahl der Zeilen die Jahre mit verfuegbaren Daten und die Anzahl der Spalten die ausgewaehlten Dauern bezeichnen.
#' @param Tn die Wiederkehrintervalle, fuer die die Regenhoehe/Intensitaet berechnet werden sollen. Die Wiederkehrintervalle sollten in Jahren angegeben werden!
#' @param Dauern Dauern, die fuer die Berechnung der Jaehrliche Maximum Serien verwendet sind. Die gleiche Einheit (entweder Minuten oder Stunden) wie das Intervall. Standartwerte sind: 5, 10, 15, 30, 60, 120, 360, 720, 1440, 2880, 4320 und 10080min.
#' @param methGEV  den Typ der Generalized Extreme Value-Verteilung, die an die jaehrlichen Serien angepasst werden soll. Optionen sind: "GEV" fuer Typ 2 oder Typ 3 (Formparameter ist nicht gleich Null) und "GUM" fuer Typ 1 (Formparameter ist gleich Null – Gumbel Verteilung)
#' @param formTyp kontrolliert, wie der Formparameter der Generalized Extreme Value Distribution (nur bei methGEV=„GEV“) geschaetzt werden soll. Die Option „CON“ berechnet die Formparameter auf der Basis der L-Momente, und die Option „FIX“ erzwingt einen bestimmten Wert fuer den Formparameter (zum Beispiel -0,1).
#' @param Gamma den vorbestimmten Wert des GEV-Formparameters angeben. Nur wichtig fuer die Variante von methGEV=„GEV“ und formTyp=„FIX“.
#' @param nBoots die Anzahl der zufaelligen Realisierungen, die aus den jaehrlichen Serien zu ziehen sind.
#' @param rSeed Random Seed fuer das Bootstrapping und die Realisationen, um die gleiche Ausgabe fuer jede gleiche Eingabe zu garantieren.
#' @param SerieTyp Kontrolle ueber die Einheiten der Ausgabetabelle. Die Optionen sind: "VOL" fuer Regenhoehe in mm/Dauer, und "INT" fuer Regenintensitaet in mm/h.
#' @param Konfidenzgrenzen  Perzentile der nBoots-Realisierungen, die die Funktion zurueckbringen soll. Das Format sollte Vektor sein, wobei der erste Wert fuer die untere Konfidenzgrenze und der zweite Wert fuer die obere Konfidenzgrenze steht.
#' @details
#' Die Unsicherheit wird auf der Basis der Breite der Konfidenzgrenzen geschaetzt, die aus nBoots-Realisierungen fuer einen bestimmten Wert (entweder Parameter oder Quantil) erhalten sind.
#' Die folgende Formel kann verwendet werden:
#' @return Eine Liste, die die erhaltenen nBoots-Realisierungen fuer die Quantile (erster Eintrag ~  QUA_INFO) und fuer die Parameter (zweiter Eintrag ~ PAR_INFO) enthaelt.
#' @examples
#' # Beispiel 1
#' # Berechnung der Stichprobenunsicherheit durch 50 Realisierungen
#' # fuer die jaehrlichen Serien in Goerlitz von 1991 bis 2020:
#' Unsicherheit  = Unsicherheit_Schaetzung(Goerlitz_maxIntSerie,Tn=100, nBoots =50, rSeed=15, SerieTyp="VOL" )
#' # aus der Unsicherheit nur die Quantile Information extrahieren
#' HN_KI  = Unsicherheit$QUA_INFO
#' dauern = c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080)
#' # das geschaetzte Konfidenzintervall fuer Tn=100 und die gegebenen Dauern darstellen:
#' library(scales)
#' plot(dauern, HN_KI$Mittelwert["100",], type="l", lwd=2, lty=1, log="xy",
#'  ylim=range(HN_KI$`95%`["100",], HN_KI$`5%`["100",]), col="royalblue",
#'  ylab="Hn [mm]", xlab="Dauer [min]", main = "Station Goerlitz")
#' polygon(c(dauern, rev(dauern)),
#'  c(HN_KI$`5%`["100",], rev(HN_KI$`95%`["100",])),
#'  col=alpha("royalblue",0.5), border=NA)
#' legend("topleft", c("95%KI", "Mittelwert"),
#'  col=c(alpha("royalblue",0.5), "royalblue"), lty=c(1, 1),
#'  lwd=c(10,2), title = "Legende", bty="n")
#' # Relative Unsicherheit fuer T =100 und die gegebenen Dauern darstellen:
#' barplot(unlist(HN_KI$rel.Unsicherheit["100",]),
#'  ylab=expression('100 x(K'[o]~-~K[u]~')/K'),xlab="Dauern [min]",
#'  main="Tn=100Jahre", col = hcl.colors(12, palette = "viridis"))
#'
#' #  Beispiel 2
#' # Berechnung der Stichprobenunsicherheit durch 100 Realisierungen
#' # fuer die jaehrlichen Serien in Goerlitz von 1991 bis 2020.
#' # Wiederkehrintervalle von 20, 50 und 100 Jahren betrachten.
#' Unsicherheit  = Unsicherheit_Schaetzung(Goerlitz_maxIntSerie,Tn=c(20,50,100),
#'  nBoots =100, rSeed=15, SerieTyp="VOL")
#' # aus der Unsicherheit nur die Parameterformation extrahieren
#' PAR_KI  = Unsicherheit$PAR_INFO
#' print(PAR_KI)
#' # aus der Unsicherheit nur die Quantils Information extrahieren
#' hN_KI   = Unsicherheit$QUA_INFO
#' # Relative Unsicherheit fuer die gegebene Wiederkehrintervalle und Dauern:
#' barplot(as.matrix(hN_KI$rel.Unsicherheit), beside=TRUE,
#'  ylab=expression('100 x(K'[o]~-~K[u]~')/K'),ylim=c(0,50), xaxt='n',
#'  xlab="Dauern [min]",main="Station Goerlitz",
#'  col=c("royalblue1","royalblue3", "royalblue4"))
#' legend("top",legend=rownames(hN_KI$rel.Unsicherheit),
#'  fill = c("royalblue1","royalblue3", "royalblue4"), bty="n", title="Ta")
#' # Alternativ Darstellung
#' barplot(as.matrix(t(hN_KI$rel.Unsicherheit)), beside=TRUE,
#'   ylab=expression('100 x(K'[o]~-~K[u]~')/K'),ylim=c(0,50),
#'   xaxt='n',main="Station Goerlitz", col=hcl.colors(12, palette = "viridis"))
#' axis(1, at = c(7, 20, 34), rownames(hN_KI$rel.Unsicherheit))
#' legend_order <- matrix(1:12,ncol=6,byrow = TRUE)
#' legend("top",legend=dauern[legend_order],
#'  fill=hcl.colors(12, palette = "viridis")[legend_order],
#'  bty="n",title="Daurn [min]", cex=0.6, ncol=6)
Unsicherheit_Schaetzung = function(Serie,
                                   Tn=c(2,5,10,20,50,100),
                                   Dauern=c(5, 10, 15,30,60,120,360,720,1440, 2880, 4320, 10080),
                                   methGEV="GEV",
                                   formTyp="FIX",
                                   Gamma=-0.1,
                                   nBoots = 100,
                                   rSeed = 1232,
                                   SerieTyp = "VOL",
                                   Konfidenzgrenzen = c(0.05,0.95)){
  # ueberpruefung der Bedingungen, die erfuellt sein muessen, damit die Funktion ohne Probleme laufen kann
  # Bedingung 1: Das Input Serie sollte existieren, vom Typ data.frame sein und Jahre als Zeilennamen und Dauer als Spaltennamen haben. Es sollte mehr als 5 Jahre und mehr als 1 Dauer enthalten.
  if(missing(Serie)) stop("Das Serie Input ist nicht vorhanden! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  else if(class(Serie)!="data.frame") stop("Das Serie Input sollte als data.frame sein! Bitte geben Sie einen data.frame() der jaehrlichen Intensitaetsserie an (in mm/h), wobei die Zeile die Jahre und die Spalte die Dauer entsprechen.")
  else if(dim(Serie)[1]<5) stop("Das Serie Input enthaelt weniger als 5 Jahren! Um Fehler zu vermeiden, sind mindestens 5 Jahren erforderlich.")
  else if(dim(Serie)[2]==1) stop("Das Serie Input enthaelt nur ein Dauer! Um Fehler zu vermeiden, sind mindestens 2 Dauern erforderlich.")
  else if(any(apply(Serie,1,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Zeile mit nur fehlenden Werten! Entfernen Sie die fehlende Zeile.")
  else if(any(apply(Serie,2,function(i) all(is.na(i)==T)==T))) stop("Das Serie Input enthaelt mindestens eine Spalte mit nur fehlenden Werten. Entfernen Sie die fehlenden Spalte.")
  else if(any(Serie<=0)) stop("Das Serie Input enthaelt negative oder Null Werte!")

  # Bedingung 2: Das Input Dauern sollte als Zahlenvektor angegeben werden und mehr als 1 Element haben.
  if(class(Dauern)!="numeric") stop("Das Dauern Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Dauern)==1) stop("Das Dauern Input hat nur ein Element! Bitte geben Sie mehr als eine Dauer an.")
  else if(any(is.na(Dauern)==T)) stop("Das Dauern Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Dauern<=0)) stop("Das Dauern Input enthaelt Negative oder Null Werte!")

  # Bedingung 3: Laenge und Namen der Dauer sollte mit der Anzahl und Namen der Spalten in der jaehrlichenSerie uebereinstimmen
  if(dim(Serie)[2]!=length(Dauern)) stop("Die Anzahl der Spalten der jaehrlichenSerie sollte gleich der Anzahl der Dauern sein, die im Vektor Dauern angegeben sind.")
  else if(identical(as.numeric(colnames(Serie)), Dauern)==F) stop("Die Spaltennamen der jaehrlichenSerie sollten mit den bei Vektor Dauern angegebenen Dauern identisch sein.")

  # Bedingung 4: Das Tn Dauern sollte als Zahlenvektor angegeben werden und mehr als 0 Element haben.
  if(class(Tn)!="numeric") stop("Das Tn Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben.")
  else if(length(Tn)==0) stop("Das Tn Input ist leer!")
  else if(any(is.na(Tn)==T)) stop("Das Tn Input hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Tn<=0)) stop("Das Tn Input enthaelt negative oder Null Werte!")

  # Bedingung 5: Das Input Gamma sollte nur ein Element vom Typ numerisch und nicht Null sein!
  if(class(Gamma)!="numeric") stop("Formparameter Gamma sollte vom numerischen Typ sein!")
  else if(length(Gamma)!=1) stop("Formparameter Gamma sollte nur 1 Element haben!")
  else if(Gamma==0) stop("Der Formparameter Gamma kann nicht 0 sein! Bitte verwenden Sie methGEV als GUM, wenn Sie den Formparameter auf 0 setzen wollen.")

  # Bedingung 6: methGEV sollte nur ein Element vom Typ Charakter sein!
  if(class(methGEV)!="character") stop("Das methGEV Input sollte vom Charaktertyp sein!")
  else if(length(methGEV)!=1) stop("Das methGEV Input sollte nur 1 Element haben!")

  # Bedingung 7: SerieTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(SerieTyp)!="character") stop("Das SerieTyp Input sollte vom Charaktertyp sein!")
  else if(length(SerieTyp)!=1) stop("Das SerieTyp Input sollte nur 1 Element haben!")

  # Bedingung 8: formTyp sollte nur ein Element vom Typ Charakter sein!
  if(class(formTyp)!="character") stop("Das formTyp Input sollte vom Charaktertyp sein!")
  else if(length(formTyp)!=1) stop("Das formTyp Input sollte nur 1 Element haben!")

  # Bedingung 9: Das Input Konfidenzgrenzen sollte als Zahlenvektor angegeben werden und nur 2 Elemente haben.
  if(class(Konfidenzgrenzen)!="numeric") stop("Das Konfidenzgrenzen Input sollte als numeric sein! Bitte einen 1d-Vektor der numerischen Dauer angeben. Die Dauern sollten in Minuten sein.")
  else if(length(Konfidenzgrenzen)!=2) stop("Das Konfidenzgrenzen Input hat nicht 2 Elemente! Bitte geben Sie nur 2 Elemente.")
  else if(any(is.na(Konfidenzgrenzen)==T)) stop("Das Dauern Konfidenzgrenzen hat fehlende Werte! Bitte entfernen Sie diese.")
  else if(any(Konfidenzgrenzen<=0)) stop("Das Dauern Konfidenzgrenzen enthaelt Negative oder Null Werte!")
  else if(any(Konfidenzgrenzen>=1)) stop("Das Dauern Konfidenzgrenzen enthaelt Werte groesser als oder gleich mit 1!")

  # Bedingung 10: Das Input rSeed sollte nur ein Element vom Typ numerisch und nicht fehlende sein!
  if(class(rSeed)!="numeric") stop("Das Input rSeed sollte vom numerischen Typ sein!")
  else if(length(rSeed)!=1) stop("Das Input rSeed sollte nur 1 Element haben!")

  # Bedingung 11: Das Input nBoots sollte nur ein Element vom Typ numerisch und nicht kleiner als 1 sein!
  if(class(nBoots)!="numeric") stop("Das Input nBoots sollte vom numerischen Typ sein!")
  else if(length(nBoots)!=1) stop("Das Input nBoots sollte nur 1 Element haben!")
  else if(nBoots<=1) stop("Das Input nBoots darf nicht kleiner als 1 sein!")

  All_Boots = lapply(1:nBoots, function(Boot){
    set.seed(rSeed+Boot)
    newAMS   = Serie[sample(1:dim(Serie)[1], size = dim(Serie)[1], replace=T),]
    newStats = Parameter_Schaetzung(newAMS, Dauern = Dauern, methGEV = methGEV, Gamma=Gamma, formTyp = formTyp)
    newQuans = Quantil_Schaetzung(newStats, Dauern=Dauern, Tn= Tn, methGEV=methGEV, SerieTyp = SerieTyp)
    return(list(Pars = unlist(newStats), Quas = unlist(newQuans)))
  })

  PAR_Boots = do.call(rbind, lapply(All_Boots, function(i) i$Pars))
  PAR_Boots = as.data.frame(PAR_Boots[, which(colnames(PAR_Boots)%in%c("Mu", "Sigma", "Gamma", "Theta", "Eta"))])
  QUA_Boots = do.call(rbind, lapply(All_Boots, function(i) i$Quas))

  QUA_INFO   = lapply(Konfidenzgrenzen, function(x) {
    data = as.data.frame(matrix(apply(QUA_Boots, 2, function(sim) stats::quantile(sim, prob=x, na.rm=T)), nrow=length(Tn), ncol=length(Dauern), byrow=F))
    rownames(data) = Tn
    colnames(data) = Dauern
    return(data)
  })
  names(QUA_INFO)   = paste0(Konfidenzgrenzen*100, "%")
  QUA_INFO$Mittelwert = as.data.frame(matrix(apply(QUA_Boots, 2, mean, na.rm=T), nrow=length(Tn), ncol=length(Dauern), byrow=F))
  rownames(QUA_INFO$Mittelwert) = Tn
  colnames(QUA_INFO$Mittelwert) = Dauern
  QUA_INFO$rel.Unsicherheit = 100*(QUA_INFO[[2]] - QUA_INFO[[1]])/QUA_INFO$Mittelwert

  PAR_INFO   = lapply(Konfidenzgrenzen, function(x) {
    data = as.data.frame(matrix(apply(PAR_Boots, 2, function(sim) stats::quantile(as.numeric(sim), prob=x, na.rm=T)), nrow=1, byrow=F))
    colnames(data) = c("Mu", "Sigma", "Gamma", "Theta", "Eta")
    return(data)
  })
  names(PAR_INFO)   = paste0(Konfidenzgrenzen*100, "%")
  PAR_INFO$Mittelwert = as.data.frame(matrix(apply(PAR_Boots, 2, function(i) mean(as.numeric(i), na.rm=T)), nrow=1,  byrow=F))
  colnames(PAR_INFO$Mittelwert) = c("Mu", "Sigma", "Gamma", "Theta", "Eta")
  PAR_INFO$rel.Unsicherheit = 100*(PAR_INFO[[2]] - PAR_INFO[[1]])/PAR_INFO$Mittelwert
  PAR_INFO = do.call(rbind, PAR_INFO)

  CI_INFO = list(QUA_INFO = QUA_INFO, PAR_INFO = PAR_INFO)
  return(CI_INFO)
}


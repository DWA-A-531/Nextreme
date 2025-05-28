#' TrendVsSprung
#' @param Zeit numeric vector, Zeitvektor der Serienwerte einer gegebenen Dauerstufe.
#' @param Serienwerte numeric vector, Vektor der Serienwerte einer gegebenen Dauerstufe.
#' @param Sensor character vector, Vektor der Sensorangaben mit identischer Laenge wie Serie. Wenn Sensor=NULL, dann wird nur Trend gegen Stationaer getestet.
#' @param ifTS logical, TRUE, wenn auf gleichzeitiges Auftreten von Trend und Sprung getestet werden soll.Default ist False.
#' @param ifAnova logical, TRUE, wenn die Trendstrukturpruefung zusaetzlich auch durch Devianzanalyse vorgenommen werden soll.
#' @param skaliereZeit logical, TRUE, wenn der Zeitvektor in der Form (Zeit - min(Zeit))/length(Zeit)-0.5 in den Wertebereich zwischen -0.5 und 0.5 transformiert werden soll. Der berechnete Trend-Parameter bezieht sich dann auf die transformierte Zeit. Fuer den Optimierungsalgorithmus ist es in der Regel einfacher, das globale Optimum zu finden, wenn die Eingangsdaten des Zeitvektors einen kleinen Werteraum umspannen.
#' @details
#' Die Generalisierte Extremwertverteilung GEV wird mittels der Maximum Likelihood Methode an die Serienwerte angepasst, wobei bezogen auf den Lokationsparameter
#' die vier Modellformen "Stat" (d.h. stationaer), "Trend", "Sprung" oder "TrendSprung" (d.h. Trend und Sprung) angepasst werden. Fuer alle vier Modelle werden die
#' Informationskriterien AIC und BIC ermittelt und anhand des minimalen IC-Wertes wird ausgewertet, welches der vier Modelle die Daten am besten beschreibt.
#' Optional kann zusaetzlich auch ein partieller Devianztest (ifAnova=TRUE) durchgefuehrt werden, wobei dieser das Nullmodell moeglicherweise zu haeufig zugunsten des kompexeren verwirft.
#' @return AicRes character, "Stat|Trend|Sprung|TrendSprung", Ergebnis auf der Grundlage des AIC-Kriteriums
#' @return BicRes character, "Stat|Trend|Sprung|TrendSprung", Ergebnis auf der Grundlage des BIC-Kriteriums
#' @return Aic.<Modell> numeric vector, AIC-Wert fuer die einzelnen Modelle Stat|Trend|Sprung|TrendSprung
#' @return Bic.<Modell> numeric vector, BIC-Wert fuer die einzelnen Modelle Stat|Trend|Sprung|TrendSprung
#' @return AnovaRes character, "Stat|Trend|Sprung|TrendSprung", Ergebnis auf Grundlage des Devianztests
#' @return Anova.<Modell> numeric vector, pValues fuer Uebergang von Nullmodell Stat zu einem der drei anderen Modelle Trend|Sprung|TrendSprung
#' @export
#' @examples
#' library(evd)
#' n=100
#' set.seed(1234)
#' # synthetische GEV-verteilte Daten, stationaer:
#' xStat   = rgev(n,10,2,0.1)
#' # synthetische GEV-verteilte Daten, mit Trend:
#' xTrend  = rgev(n,10,2,0.1) + 1:n*0.05
#' # synthetische GEV-verteilte Daten, mit Sprung:
#' xSprung = c(rgev(n/2,10,1,0.1),rgev(n/2,20,2,0.1))
#' # synthetische GEV-verteilte Daten, mit Trend und Sprung:
#' xTrendSprung = c(rgev(n/2,10,2,0.1),rgev(n/2,20,2,0.1)) + (1:n)*0.05
#' Sensor = factor(c(rep("analog",n/2),rep("digital",n/2)))
#' X = data.frame(Jahr=1:n,Sensor=Sensor,xStat,xTrend,xSprung,xTrendSprung)
#' Trend_vs_Sprung(Zeit=X[,"Jahr"],Serienwerte=X[,"xStat"],Sensor=X[,"Sensor"])
#' Trend_vs_Sprung(Zeit=X[,"Jahr"],Serienwerte=X[,"xTrend"],Sensor=X[,"Sensor"])
#' Trend_vs_Sprung(Zeit=X[,"Jahr"],Serienwerte=X[,"xSprung"],Sensor=X[,"Sensor"])
#' Trend_vs_Sprung(Zeit=X[,"Jahr"],Serienwerte=X[,"xTrendSprung"],Sensor=X[,"Sensor"])
Trend_vs_Sprung = function(Zeit,Serienwerte,Sensor=NULL,ifTS=F,ifAnova=F,skaliereZeit = T){
  Fgev=function(...){ res=list(mo="",Aic=as.numeric(NA),Bic=as.numeric(NA));  mo=try(evd::fgev(...,std.err=F),sil=T); if(!is.character(mo)){mo$Aic=stats::AIC(mo);mo$Bic=stats::AIC(mo,k=length(mo$est))};  mo }
  Anova=function(...){res=try(stats::anova(...)[2,5]);if(is.character(res))res=as.numeric(NA);res}

  #if(!is.null(PrintLine))print(PrintLine)

  #data
  DTR = data.frame(Tr=Zeit)
  if(skaliereZeit)DTR = data.frame(Tr=(Zeit-min(Zeit))/length(Zeit)-0.5)
  DSP=DTRSP=""
  if(!is.null(Sensor)){
    DSP   = try(data.frame(stats::model.matrix(~factor(Sensor),data.frame(Sensor)))[,-1],sil=T) # ohne intercept
    DTRSP = try(data.frame(DTR,DSP),sil=T)
  }
  #model
  mo=mTr=mSp=mTrSp=list(mo="",Aic=as.numeric(NA),Bic=as.numeric(NA))
  m0    = Fgev(Serienwerte)
  mTr   = Fgev(Serienwerte,nsloc=DTR)
  if(!is.character(DSP)) mSp = Fgev(Serienwerte,nsloc=DSP)
  if(ifTS & !is.character(DTRSP)) mTrSp = Fgev(Serienwerte,nsloc=DTRSP)

  #IC
  AicVec = c(Stat=m0$Aic,Trend=mTr$Aic,Sprung=mSp$Aic,TrendSprung=mTrSp$Aic)
  BicVec = c(Stat=m0$Bic,Trend=mTr$Bic,Sprung=mSp$Bic,TrendSprung=mTrSp$Bic)
  AicRes = names(which.min(AicVec))
  BicRes = names(which.min(BicVec))

  res=data.frame(AicRes,BicRes,Aic=t(AicVec),Bic=t(BicVec))
  #anova
  if(ifAnova){
    AnovaVec=c(Trend=Anova(mTr,m0),Sprung=Anova(mSp,m0))
    if(ifTS)AnovaVec=c(AnovaVec,TrendSprung=Anova(mTrSp,m0))
    AnovaRes=names(which.min(AnovaVec))
    res=cbind(res,data.frame(AnovaRes, Anova=t(AnovaVec)))
  }
  #
  #out
  res
}

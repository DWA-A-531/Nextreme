#' Berechnung der GEV-Parameter mit einer festen Form (Kappa) Parameter
#' @param lmom die L-Momente, die zuvor mit dem Paket lmomco berechnet wurden
#' @param kappa der feste Wert des Kappa-Parameters (Formparameter). Wenn kappa=NULL ist, wird der Formparameter aus den L-Momenten berechnet.
#' @param checklmom logische Variable, die entweder TRUE oder FALSE sein kann. Gibt an, ob die L-Momente geprueft werden sollen oder nicht.
#' @details
#' R-Funktion zur Berechnung der GEV-Parameter, wenn der Formparameter (kappa) auf einen bestimmten Wert eingestellt ist.
#' @return  Die GEV-Parameter
#' @examples
#' ShapeFix = 0.1 #(nach Koutsoyiannis 2008)
#' kappaFix = ShapeFix * -1 # vorzeichenwechsel aufgrund unterschiedl. Formeln beachten!
#' pargev2(lmomco::lmoms(1:10),kappa= kappaFix )$para
pargev2 = function (lmom, checklmom = TRUE, kappa=NULL) { # aus lmomco, erweitert um kappa = NULL, wenn kappa != NULL, dann wird mit fixiertem kappa geschuetzt
  para <- rep(NA, 3)
  names(para) <- c("xi", "alpha", "kappa")
  SMALL <- 1e-05
  EPS <- 1e-06
  MAXIT <- 20
  EU <- 0.57721566
  DL2 <- 0.69314718
  DL3 <- 1.0986123
  A0 <- 0.2837753
  A1 <- -1.21096399
  A2 <- -2.50728214
  A3 <- -1.13455566
  A4 <- -0.07138022
  B1 <- 2.06189696
  B2 <- 1.31912239
  B3 <- 0.25077104
  C1 <- 1.59921491
  C2 <- -0.48832213
  C3 <- 0.01573152
  D1 <- -0.64363929
  D2 <- 0.08985247
  if (length(lmom$L1) == 0) {
    lmom <- lmomco::lmorph(lmom)
  }
  if (checklmom & !lmomco::are.lmom.valid(lmom)) {
    warning("L-moments are invalid")
    return()
  }
  T3 <- lmom$TAU3
  if (T3 > 0) {
    Z <- 1 - T3
    G <- (-1 + Z * (C1 + Z * (C2 + Z * C3)))/(1 + Z * (D1 +
                                                         Z * D2))
    if (abs(G) < SMALL) {
      para[3] <- 0
      para[2] <- lmom$L2/DL2
      para[1] <- lmom$L1 - EU * para[2]
      return(list(type = "gev", para = para))
    }
  }
  else {
    G <- (A0 + T3 * (A1 + T3 * (A2 + T3 * (A3 + T3 * A4))))/(1 +
                                                               T3 * (B1 + T3 * (B2 + T3 * B3)))
    if (T3 >= -0.8) {
    }
    else {
      if (T3 <= -0.97)
        G <- 1 - log(1 + T3)/DL2
      T0 <- (T3 + 3) * 0.5
      CONVERGE <- FALSE
      for (it in seq(1, MAXIT)) {
        X2 <- 2^-G
        X3 <- 3^-G
        XX2 <- 1 - X2
        XX3 <- 1 - X3
        T <- XX3/XX2
        DERIV <- (XX2 * X3 * DL3 - XX3 * X2 * DL2)/(XX2 *
                                                      XX2)
        GOLD <- G
        G <- G - (T - T0)/DERIV
        if (abs(G - GOLD) <= EPS * G)
          CONVERGE <- TRUE
      }
      if (CONVERGE == FALSE) {
        warning("Noconvergence---results might be unreliable")
      }
    }
  }
  if(!is.null(kappa))G = kappa
  para[3] <- G
  GAM <- exp(lgamma(1 + G))
  para[2] <- lmom$L2 * G/(GAM * (1 - 2^(-G)))
  para[1] <- lmom$L1 - para[2] * (1 - GAM)/G
  return(list(type = "gev", para = para, source = "pargev"))
}

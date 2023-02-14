newdataCAshiny <- dta
  lignesupCAshiny <- NULL
  axe1CAshiny <- 1
  axe2CAshiny <- 2
  InvisibleCAshiny <- NULL
  selec1CAshiny <- gettext("No selection",domain="R-Factoshiny")
  selec2CAshiny <- gettext("No selection",domain="R-Factoshiny")
  valueselec1CAshiny <- 0
  valueselec2CAshiny <- 0
  sizeCAshiny <- 1
  title1CAshiny <- gettext("Carte des rédactions et mots",domain="R-Factoshiny")
  sizeCAshiny <- 1
  color_pointInit <- gettext("row/column",domain="R-Factoshiny")
  col1CAshiny <- "blue"
  col2CAshiny <- "red"
  col3CAshiny <- "#0C2B94"
  col4CAshiny <- "darkred"
  col5CAshiny  <- "magenta"
  ellipsesCAshiny <- NULL
  nbdimclustCAshiny <- 5
  hcpcparaCAshiny <- FALSE
  listeChoixColourPoint<- list(gettext("row/column",domain="R-Factoshiny"),"cos2"="cos2","contribution"="contribution")

nomDataCAshiny <- "dta"
nomCAshiny <- rownames(dta)
if (global==FALSE){
  VariableChoicesCAshiny <- colnames(theme)
  colonnesupCAshiny <- colnames(theme)
} else {
  VariableChoicesCAshiny <- colnames(dta)
  colonnesupCAshiny <- colnames(dta)
}
QualiChoiceCAshiny <- colnames(dta)[which(!(sapply(dta,is.numeric)))]

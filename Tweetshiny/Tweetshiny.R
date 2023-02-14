Tweetshiny <- function(nbfois=10, nbredac=2, global = TRUE, liste_theme="all"){
  library(FactoMineR)
  G <- .GlobalEnv
  assign("objTweetshiny",ls(all.names=TRUE, envir=G),envir=G)

PathAdress <- "C:/Users/husson/AOBox/Travail/Bureau/journalist-main/data"
MotJournaux <- read.table(paste0(PathAdress,"/count-lemme-journal.csv"),header = TRUE,sep=",",encoding="UTF-8",row.names=1)
MotJournaux <- read.table(paste0(PathAdress,"/count-mot-journal.csv"),header = TRUE,sep=",",encoding="UTF-8",row.names=1)
rownames(MotJournaux) <- MotJournaux[,1]
MotJournaux <- t(MotJournaux[,-1])
# media <- c("J","TV","TV","TV","J","J","Radio","J","Radio","J","J","J","J","J","J","J","J","J","J","J","J","J","J","J")
# national <- rep("y",nrow(MotJournaux))
# national[c(5,6,11,14,19,20,22,24)] <- "n"

mot10 <- MotJournaux[,which(apply(MotJournaux,2,sum)>=nbfois)] ## mot cités plus de nbfois
# selectRedac <- which(national=="y")
# selectRedac <- setdiff(selectRedac,which(rownames(mot10)%in%c("Valeur","Lacroix"))) ## ces 2 journaux sont très particuliers
# matMot <- mot10[selectRedac,]

# matMot <- matMot[,which(apply(matMot,2,sum)>0)] ## supprime mots non utilisés
# selectMots <- which(apply(matMot>0,2,sum)>=nbredac)
# matMot <- matMot[,selectMots] ## mots cités par au moins nbredac rédactions

# resCA <- CA(matMot,graph=FALSE)
# plot(resCA, invisible="col",cex=.5)


theme = read.table("../data/themes.csv",header=TRUE,sep=";",quote="£",encoding="UTF-8")
theme <- sapply(theme,tolower)

if (liste_theme=="all") liste_theme <- (1:ncol(theme))

if (global==TRUE){
  dta <- mot10[,liste_theme,drop=FALSE] ## initialisation
  for (i in liste_theme) dta[,i] <- apply(mot10[,which(colnames(mot10)%in%theme[,i])],1,sum)
  colnames(dta) <- colnames(theme)[liste_theme]
} else dta <- mot10

  assign("dta", dta, envir=G)
  assign("theme", theme, envir=G)
  assign("global", global, envir=G)
# liste_mots <- unique(unlist(theme))
#resCA <- CA(cbind.data.frame(dta,mot10[,which(colnames(mot10)%in%liste_mots)]),graph=FALSE,col.sup=(ncol(dta)+1):(ncol(dta)+ncol(mot10[,which(colnames(mot10)%in%liste_mots)])))
#plot(resCA,invisible="col.sup",cex=.8)

   outShiny <- shiny::runApp()
  assign("myListOfThingsCAshiny",setdiff(ls(all.names=TRUE,envir=G),c("outShiny",objTweetshiny)),envir=G)  
  rm(list=myListOfThingsCAshiny, envir=G)
  rm(list=c("myListOfThingsCAshiny"),envir=G)
  return(invisible(outShiny))
}




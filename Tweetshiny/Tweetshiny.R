Tweetshiny <- function(nbfois=10, nbredac=2, global = TRUE, liste_theme="all",select_redac="all"){
# select_redac peut prendre "all", "Media=='Radio'","Media=='TV'","Media==Ecrit", "Audience==National", "Audience==Regional" ou des mixtes

  library(FactoMineR)
  G <- .GlobalEnv
  assign("objTweetshiny",ls(all.names=TRUE, envir=G),envir=G)

PathAdress <- "C:/Users/husson/AOBox/Travail/Bureau/journalist-main/data"
MotJournaux <- read.table(paste0(PathAdress,"/count-lemme-journal.csv"),header = TRUE,sep=",",encoding="UTF-8",row.names=1)
MotJournaux <- read.table(paste0(PathAdress,"/count-mot-journal.csv"),header = TRUE,sep=",",encoding="UTF-8",row.names=1)


rownames(MotJournaux) <- MotJournaux[,1]
MotJournaux <- t(MotJournaux[,-1])

mot10 <- MotJournaux[,which(apply(MotJournaux,2,sum)>=nbfois)] ## mot cités plus de nbfois

 desc_redaction = read.table("../data/desc_redaction.csv",header=TRUE,sep=";",quote="£",encoding="UTF-8",row.names=1)
if (select_redac!="all"){
   mot10 <- mot10[rownames(subset(desc_redaction,eval(parse(text=select_redac)))),]
 }

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




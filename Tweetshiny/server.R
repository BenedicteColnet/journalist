# server scipt for CA2
  function(input, output) {
  
  output$NB1 <- renderUI({
       return(textInput("nb1", label = NULL, axe1CAshiny,width='41px'))
  })

  output$NB2 <- renderUI({
       return(textInput("nb2", label = NULL, axe2CAshiny,width='41px'))
  })

  output$NbDimForClustering <- renderUI({
    if(input$hcpcparam==TRUE){
        return(tags$div( 
            div(gettext("Number of dimensions kept for clustering",domain="R-Factoshiny"), style="display: inline-block; padding: 0px 0px 0px 0px"),
		    div(numericInput(inputId = "nbDimClustering", label = NULL,value=if(is.null(nbdimclustCAshiny)){5} else {nbdimclustCAshiny},min=1), style="display: inline-block;width: 70px; padding: 0px 0px 0px 10px"))
		)
    }
  })
    
  values <- reactive({
	 if (length(input$nb1)>0){
	   if (max(input$nb1,input$nb2)>5) return(isolate(valeur()))
	 }
	 if (length(input$nbDimClustering)>0){
	   if (input$nbDimClustering >5) return(isolate(valeur()))
	 }
     if (length(input$caparam)==0){
	   return(valeur())
	 } else {
        if (input$submit>=0) isolate(valeur())
     }
 })

  valeur <- function(){
	  
    if (global==FALSE) liste_mots <- unique(unlist(theme[,input$supvar]))
	else liste_mots <- input$supvar
    ColSup <- NULL
	# QualiSup <- which(NomCol%in%input$supquali)
	QualiSup <- NULL
    RowSup <- which(nomCAshiny%in%input$rowsupl)
	if (length(ColSup)==0) ColSup <- NULL
	if (length(QualiSup)==0) QualiSup <- NULL
    if(length(input$rowsupl)==0) RowSup <- NULL
	codeCA <- NULL
	nomTabDon <- paste0(nomDataCAshiny,"[,which(colnames(",nomDataCAshiny,")%in%liste_mots)]")
	# else nomTabDon <- paste0(nomDataCAshiny,"[,",colnames(nomDataCAshiny)[input$supvar],"]")

    codeCA <- paste0(codeCA,"res.CA<-CA(",nomTabDon)
	codeCA <- paste0(codeCA,if(max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))!=5) paste0(",ncp=",max(5*as.integer(!input$hcpcparam),as.numeric(input$nb1),as.numeric(input$nb2),as.numeric(input$nbDimClustering))),if(!is.null(QualiSup)) paste0(",quali.sup=c(",paste(QualiSup,collapse=","),")"),if(!is.null(ColSup)) paste0(",col.sup=c(",paste(ColSup,collapse=","),")"),if(!is.null(RowSup)) paste0(",row.sup=c(",paste(RowSup,collapse=","),")"),",graph=FALSE)")
    list(res.CA=eval(parse(text=codeCA)), codeCA=codeCA)
    }
    
  output$choixinvis <- renderUI({
     listechoix <- c(gettext("Rows",domain="R-Factoshiny"),gettext("Columns",domain="R-Factoshiny"))
	 if (!is.null(input$rowsupl)) listechoix <- c(listechoix,gettext("Supplementary rows",domain="R-Factoshiny"))
	 if (!is.null(input$supvar)) listechoix <- c(listechoix,gettext("Supplementary columns",domain="R-Factoshiny"))
	 if (!is.null(input$supquali)) listechoix <- c(listechoix,gettext("Supplementary qualitative variables",domain="R-Factoshiny"))
     return(selectInput("invis",gettext("Invisible elements",domain="R-Factoshiny"),choices=as.list(listechoix),multiple=TRUE,selected=InvisibleCAshiny))
  })

  output$col1CAshiny=renderUI({
    if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colrow", label=NULL, if(!is.null(input$colrow)){if (input$colrow!="blue") input$colrow} else{col1CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
        div(gettext("active rows",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
 	  )
    }
  })
  output$col2CAshiny=renderUI({
    if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colcol", label=NULL, if(!is.null(input$colcol)){if (input$colcol!="red") input$colcol} else{col2CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("active columns",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
  output$col3CAshiny=renderUI({
    if(!is.null(values()$res.CA$row.sup) & sum(gettext("Supplementary rows",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colrowsup", label=NULL, if(!is.null(input$colrowsup)){if (input$colrowsup!="darkblue") input$colrowsup} else{col3CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary rows",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
  output$col4CAshiny=renderUI({
    if(!is.null(values()$res.CA$col.sup) & sum(gettext("Supplementary columns",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colcolsup", label=NULL, if(!is.null(input$colcolsup)){if (input$colcolsup!="darkred") input$colcolsup} else{col4CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary columns",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
    
  output$col5CAshiny=renderUI({
    if(!is.null(values()$res.CA$quali.sup) & sum(gettext("Supplementary categories",domain="R-Factoshiny")==input$invis)==0){
      return(tags$div( 
        div(colourpicker::colourInput("colqualisup", label=NULL, if(!is.null(input$colqualisup)){if (input$colqualisup!="magenta") input$colqualisup} else{col5CAshiny} ,allowTransparent=TRUE), style="display: inline-block; width: 15px; padding: 0px 0px 0px 0px"),
	    div(gettext("supplementary categories",domain="R-Factoshiny"), style="display: inline-block;padding: 0px 0px 0px 10px"))
	  )
    }
  })
    
    output$contribcol=renderUI({
      maxx=nrow(values()$res.CA$col$coord)
      if(selec1CAshiny=="contrib"){
        return(sliderInput("contrib1",gettext("Number of the most contributive active columns",domain="R-Factoshiny"),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      } else{
        return(sliderInput("contrib1",gettext("Number of the most contributive active columns",domain="R-Factoshiny"),min=1,max=maxx,value=maxx,step=1))
      }
      
    })
    
    output$contribrow=renderUI({
      maxx=nrow(values()$res.CA$row$coord)
      if(selec2CAshiny=="contrib"){
        return(sliderInput("contrib2",gettext("Number of the most contributive active rows",domain="R-Factoshiny"),min=1,max=maxx,value=valueselec2CAshiny,step=1))
      }
      else{
        return(sliderInput("contrib2",gettext("Number of the most contributive active rows",domain="R-Factoshiny"),min=1,max=maxx,value=maxx,step=1))
      }
    })
    
  codeGraph <- reactive({
    validate(
      need(input$nb1 != input$nb2, gettext("Please select two different dimensions",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$row$coord)>2 ,gettext("Please select at least three active rows",domain="R-Factoshiny")),
      need(nrow(values()$res.CA$col$coord)>2 ,gettext("Please select at least three active columns",domain="R-Factoshiny"))
    )
    if(length(input$invis)==0){
      invisiText <- NULL
    } else {
      invisi <- NULL
      if(sum(gettext("Rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row")
      if(sum(gettext("Columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col")
      if(sum(gettext("Supplementary rows",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"row.sup")
      if(sum(gettext("Supplementary columns",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"col.sup")
      if(sum(gettext("Supplementary qualitative variables",domain="R-Factoshiny")==input$invis)>0) invisi<-c(invisi,"quali.sup")
      invisiText <- paste0("c(",paste(paste("'",invisi,"'",sep=""),collapse = ","),")")
    }
    sel=NULL
    if(input$seleccol=="cos2"){
      if(input$slider3!=1){
        sel=paste0("'cos2 ",input$slider3,"'")
      } else{
        sel="'cos2 0.9999999'"
      }
    }
    if(input$seleccol=="contrib") sel=paste0("'contrib ",input$contrib1,"'")
    sel2=NULL
    if(input$selecrow=="cos2"){
      if(input$slider4!=1){
        sel2=paste0("'cos2 ",input$slider4,"'")
      } else{
        sel2="'cos2 0.9999999'"
      }
    }
    if(input$selecrow=="contrib") sel2=paste0("'contrib ",input$contrib2,"'")
    values2=c()
    if(!is.null(input$ellip)){
      if(gettext("Columns",domain="R-Factoshiny")%in%input$ellip) values2=c(values2,"col")
      if(gettext("Rows",domain="R-Factoshiny")%in%input$ellip) values2=c(values2,"row")
    }

    hab <- "none"
    if(input$color_point == "cos2") hab <- "'cos2'"
    if(input$color_point == "contribution") hab <- "'contrib'"
    if(input$color_point==gettext("qualitative variable",domain="R-Factoshiny")) hab <- paste0("'",input$habiller,"'")
      if(is.null(input$ellip)||length(input$ellip)==0){
	    myellip=NULL
      }else{
        vect=c()
        if(gettext("Columns",domain="R-Factoshiny")%in%input$ellip) vect=c(vect,"col")
        if(gettext("Rows",domain="R-Factoshiny")%in%input$ellip) vect=c(vect,"row")
        myellip=paste(paste("'",vect,"'",sep=""),collapse=",")
      }
      Code <- paste0(if(is.null(myellip)){"plot.CA"}else{"ellipseCA"},"(res.CA",if(!is.null(myellip)) paste0(",ellipse=c(",myellip,")"),if (input$nb1!=1 | input$nb2!=2) paste0(",axes=c(",input$nb1,",",input$nb2,")"),if (!is.null(sel)) paste0(",selectCol=",sel),if (!is.null(sel2)) paste0(",selectRow=",sel2),if (!is.null(sel2) | !is.null(sel)) paste0(',unselect=0'),if (input$cex!=1) paste0(',cex=',input$cex,',cex.main=',input$cex,',cex.axis=',input$cex),if(input$title1CAshiny!="CA factor map")paste0(',title="',input$title1CAshiny,'"'),if (hab!="none" & hab!="''"){paste0(",habillage=",hab)},if (!is.null(input$colrow)) {if (input$colrow!="#0000FF") paste0(",col.row='",input$colrow,"'")},if (!is.null(input$colcol)){ if (input$colcol!="#FF0000") paste0(",col.col='",input$colcol,"'")},if (!is.null(input$colrowsup)){if (input$colrowsup!="#0C2B94") paste0(",col.row.sup='",input$colrowsup,"'")},if (!is.null(input$colcolsup)){ if (input$colcolsup!="#8B0000") paste0(",col.col.sup='",input$colcolsup,"'")},if (!is.null(input$colqualisup)) paste0(",col.quali.sup='",input$colqualisup,"'"),if (!is.null(invisiText)) paste0(',invisible=',invisiText),')')
	  res.CA <- values()$res.CA
	  Plot <- eval(parse(text=Code))
      return(list(Code=Code, Plot=Plot))
    })
    
    output$map <- renderPlot({
        p <- print(codeGraph()$Plot)
    })
    
    output$habillage2=renderUI({
      if (input$color_point == gettext("qualitative variable",domain="R-Factoshiny")){
        return(selectizeInput("habiller",gettext("select the variable",domain="R-Factoshiny"), choices=QualiChoiceCAshiny, multiple=FALSE, selected=QualiChoiceCAshiny))
      }
    }) 
    

    output$out22=renderUI({
      choix=list(gettext("Summary of outputs",domain="R-Factoshiny"),gettext("Eigenvalues",domain="R-Factoshiny"),gettext("Results for the columns",domain="R-Factoshiny"),gettext("Results for the rows",domain="R-Factoshiny"))
      if(length(input$rowsupl)!=0){
        choix=c(choix,gettext("Results for the supplementary rows",domain="R-Factoshiny"))
      }
      if(!is.null(values()$res.CA$col.sup)){
        choix=c(choix,gettext("Results for the supplementary columns",domain="R-Factoshiny"))
      }
      if(!is.null(values()$res.CA$quali.sup)){
        choix=c(choix,gettext("Results for the categorical variables",domain="R-Factoshiny"))
      }
      radioButtons("out",gettext("Which outputs do you want?",domain="R-Factoshiny"),
                   choices=choix,selected=gettext("Summary of outputs",domain="R-Factoshiny"),inline=TRUE)
    })
    
    output$map3=renderPlot({
    print(ggplot2::ggplot(cbind.data.frame(x=1:nrow(values()$res.CA$eig),y=values()$res.CA$eig[,2])) + ggplot2::aes(x=x, y=y)+ ggplot2::geom_col(fill="blue") + ggplot2::xlab("Dimension") + ggplot2::ylab(gettext("Percentage of variance",domain="R-Factoshiny")) + ggplot2::ggtitle(gettext("Decomposition of the total inertia",domain="R-Factoshiny")) + ggplot2::theme_light() + ggplot2::theme(plot.title = ggplot2::element_text(hjust =0.5))  + ggplot2::scale_x_continuous(breaks=1:nrow(values()$res.CA$eig)))
    })
            
        
    observe({
      if(input$Quit!=0){
        isolate({
          res <- list()
          res$codeCA=values()$codeCA
          res$codeGraph=codeGraph()$Code
          res$anafact=values()$res.CA
          class(res) <- "CAshiny"
          stopApp(returnValue=res)
        })
      }
    })
  }
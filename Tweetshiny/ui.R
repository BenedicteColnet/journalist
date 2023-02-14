# ui script for CA2

fluidPage(
  titlePanel(div("Analyse textuelle des tweets",style="color:#2A0A29",align="center"),windowTitle="CAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #F0E6E6; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
		tags$style(type="text/css", "#loadmessage { padding: 5px 0px 5px 0px; text-align: center; font-weight: bold; font-size: 100%; color: #000000; background-color: #ff8533; z-index: 105; }")
      ),
      wellPanel(
        div(align="center",checkboxInput("caparam",gettext("Choix du thème",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.caparam==true",
            selectizeInput("supvar",label=gettext("Choisir les thèmes",domain="R-Factoshiny"), choices=VariableChoicesCAshiny, selected=colonnesupCAshiny,multiple=TRUE),
            selectizeInput("rowsupl",gettext("Rédaction en supplémentaire",domain="R-Factoshiny"),choices=nomCAshiny, multiple=TRUE,selected=lignesupCAshiny),
            actionButton("submit", label = gettext("Submit",domain="R-Factoshiny"))
        ),
      style = "padding: 3px;background-color: #ffdbdb;"),
      wellPanel(
        div(align="center",checkboxInput("graph",gettext("Graphical options",domain="R-Factoshiny"),FALSE)),
        conditionalPanel(
          condition="input.graph==true",
          div(gettext("Axes:",domain="R-Factoshiny"), style="display: inline-block;padding: 5px"),
          div(uiOutput("NB1"), style="display: inline-block;"),
          div(uiOutput("NB2"), style="display: inline-block;"),
          textInput("title1CAshiny",gettext("Title of the graph:",domain="R-Factoshiny"),title1CAshiny),
		  uiOutput("Titre2"),
          sliderInput("cex",gettext("Size of labels",domain="R-Factoshiny"),min=0.5,max=3.5,value=sizeCAshiny,step=0.05),
          uiOutput("choixinvis"),
          selectInput("color_point",label=gettext("Colour points according to:",domain="R-Factoshiny"),
                                          choices=listeChoixColourPoint,selected=color_pointInit),
            conditionalPanel(
              condition=paste0("input.color_point=='",gettext("row/column",domain="R-Factoshiny"),"'"),
                uiOutput("col1CAshiny"),
                uiOutput("col2CAshiny"),
                uiOutput("col3CAshiny"),
                uiOutput("col4CAshiny"),
                uiOutput("col5CAshiny")
		    ),
        uiOutput("habillage2"),
        selectInput("selecrow",gettext("Labels for rows selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"Cos2"="cos2","Contribution"="contrib"),selected=selec2CAshiny),
        conditionalPanel(
          condition="input.selecrow=='cos2'",
          if(selec2CAshiny=="cos2"){sliderInput("slider4",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                                         min=0,max=1,value=valueselec2CAshiny,step=0.05)}
          else{sliderInput("slider4",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                           min=0,max=1,value=0,step=0.05)}),
        conditionalPanel(
          condition="input.selecrow=='contrib'",
          uiOutput("contribrow")),
          selectInput("seleccol",gettext("Labels for columns selected by:",domain="R-Factoshiny"), choices=list(gettext("No selection",domain="R-Factoshiny"),"Cos2"="cos2","Contribution"="contrib"),selected=selec1CAshiny),
          conditionalPanel(
            condition="input.seleccol=='cos2'",
            if(selec1CAshiny=="cos2"){
              sliderInput("slider3",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                          min=0,max=1,value=valueselec1CAshiny,step=0.05)
            }
            else{
              sliderInput("slider3",label=gettext("Labels for cos2 greater than",domain="R-Factoshiny"),
                          min=0,max=1,value=0,step=0.05) 
          }),
        conditionalPanel(
          condition="input.seleccol=='contrib'",
          uiOutput("contribcol"))
		  ),
      style = "padding: 3px;background-color: #fcefba"),
      wellPanel(
        div(align="center",checkboxInput("hcpcparam",gettext("Perform clustering after leaving CA app?",domain="R-Factoshiny"),hcpcparaCAshiny)),
        conditionalPanel(
          condition="input.hcpcparam==true",
          uiOutput("NbDimForClustering")
        ),
        align="center", style = "padding: 3px;background-color: #ecffdb"
      ),
       # wellPanel(
         # div(align="center",checkboxInput("CAcode",gettext("Get the CA code",domain="R-Factoshiny"),FALSE)),style='padding:5px; background-color: yellow;text-align:center;white-space: normal;'
	   # ),
      div(align="center",actionButton("Quit", gettext("Quit the app",domain="R-Factoshiny"),style='padding:5px; background-color: #fcac44;text-align:center;white-space: normal;'))
      ,width=3,style="background-color: #9b9b9b;padding: 4px"),
      mainPanel(
        tags$style(type = "text/css", "a{color: #2F0B3A;}"),
        tabsetPanel(id = "graph_sort",
                    tabPanel(gettext("Graphique",domain="R-Factoshiny"),
                             div(verbatimTextOutput("warn")),
                             div(verbatimTextOutput("CodePrinted")),
          fluidRow(
                 br(),
                 column(width = 10,shinyjqui::jqui_resizable(plotOutput("map",height=700)),
                             br(),
                             p(gettext("Download as",domain="R-Factoshiny"),downloadButton("downloadData1",gettext("jpg",domain="R-Factoshiny")),downloadButton("downloadData",gettext("png",domain="R-Factoshiny")),downloadButton("downloadData2",gettext("pdf",domain="R-Factoshiny")),align="center"),
                             br()),
                 uiOutput("map22"),
							 br(),align="center"))
        )
      ,width=9)
    )
)

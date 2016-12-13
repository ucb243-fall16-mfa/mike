# ui2.R AFM


shinyUI(fluidPage(
  titlePanel(div(paste(gettext("MFA on the dataset "),nomData),style="color:#6E6E6E",align="center"),windowTitle="MFAshiny"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style("body {background-color: #E1D3FB; }"),
        tags$style(type='text/css', "#nb1 { height:50px; }"),
        tags$style(type='text/css', "#nb2 { height:50px; }"),
        tags$style(type='text/css', "#title1 { height: 25px; }"),
        tags$style(type='text/css', "#title2 { height: 25px; }"),
        tags$style(type='text/css', "#title3 { height: 25px; }"),
        tags$style(type='text/css', "#title4 { height: 25px; }"),
        tags$style(type='text/css', "#title5 { height: 25px; }")
      ),
            br(),
      div(align="center",actionButton("Quit", gettext("Quit the app")))
      ,width=3),
    
    mainPanel(
      tabsetPanel(id = "graph_sort",
                  tabPanel(gettext("Creation of groups"),
                           br(),
                           checkboxInput("activemodif",gettext("Create the groups"),TRUE),
                           conditionalPanel(
                             condition="input.activemodif==true",
                             checkboxGroupInput("insets","sets to compute",
                                                c("Assessor 1"=1,
                                                "Assessor 2"=2,
                                                "Assessor 3"=3,
                                                "Assessor 4"=4,
                                                "Assessor 5"=5,
                                    "Assessor 6"=6,
                                     "Assessor 7"=7,
                                     "Assessor 8"=8,
                  "Assessor 9"=9,
                  "Assessor10"=10))),
                           textInput("inncomp",h6(gettext("number of comps ")),NULL),
                           checkboxInput("incenter",gettext("need center?"),TRUE),
                           checkboxInput("inscale",gettext("need scale?"),TRUE)
                           ),
                  tabPanel(gettext("Graphs"),
                           radioButtons("pType","Choose plot type:",
                                        list("eigenvalues","common factor scores","partial factor scores","loadings")),
                           plotOutput('plot')),
                  
                  tabPanel(gettext("Values"),
                           br(),
                           radioButtons("out",gettext("Which outputs do you want?"),
                                        choices=list(gettext("Summary of outputs"),gettext("Compromise factor scores"),gettext("Partial factor scores"),
                                                     gettext("Contributions")),inline=TRUE),
                           conditionalPanel(
                             #                               condition="input.out=='MFA'",
                             condition=paste("input.out=='",gettext("Summary of outputs"),"'",sep=''),
                             verbatimTextOutput("summaryMFA")
                           ),
                           conditionalPanel(
                             #                               condition="input.out=='eig'",
                             condition=paste("input.out=='",gettext("Compromise factor scores"),"'",sep=''),
                             verbatimTextOutput("compromise")),
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Partial factor scores"),"'",sep=''),
                             #                               condition="input.out=='ind'",
                             verbatimTextOutput("partial")),
                             
                           conditionalPanel(
                             condition=paste("input.out=='",gettext("Contributions"),"'",sep=''),
                             #                               condition="input.out=='quantvar'",
                             radioButtons("out3","What type of results?",choices=list(gettext("Observation"),gettext("Variables"),gettext("Table")),selected=gettext("Observation"),inline=TRUE),
                             conditionalPanel(
                               condition=paste("input.out3=='",gettext("Observation"),"'",sep=''),
                               #                                 condition="input.out3=='coord'",
                               div(align="center",tableOutput("sorties11"))),
                             conditionalPanel(
                               condition=paste("input.out3=='",gettext("Variables"),"'",sep=''),
                               div(align="center",tableOutput("sorties22"))),
                             conditionalPanel(
                               condition=paste("input.out3=='",gettext("Table"),"'",sep=''),
                               div(align="center",tableOutput("sorties44")))
                           )

                  ),
                  tabPanel(gettext("Summary of dataset"),
                           br(),
                           verbatimTextOutput("summary"),
                           selectInput("bam",gettext("Graphs for"),choices=list(IdChoices=VariableChoices),multiple=FALSE),
                           plotOutput("histo")),
                  
                  tabPanel(gettext("Data"),
                           br(),
                           dataTableOutput("JDD")
                  )
      )
      ,width=9)
  )
))

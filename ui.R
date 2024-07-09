library(shinydashboard)
library(shiny)
library(shinyBS)
library(plotly)
library(shinyjs)
#options(rgl.useNULL=TRUE)
#library(rglwidget)
library(reshape2)
library(visNetwork)
library(shinyBS)
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize=600*1024^2) 
ui <- dashboardPage(
  dashboardHeader(title = "Ligand Receptor Interactions",titleWidth = 350,dropdownMenuOutput("userloggedin")),
  dashboardSidebar(width = 350,
                   div(style="overflow-y: scroll"),
                   tags$head(tags$style(HTML(".sidebar { height: 250vh; overflow-y: auto; }
                                             .shiny-notification{position: fixed;top: 33%;left: 45%;right: 30%;}
                                             " )
                   )),
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                    #menuItem('Help page', tabName = 'help', icon = icon('hand-o-right'))
                     
                   )#end of sidebar menu
  ),#end dashboardSidebar
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    useShinyjs(),
    tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
    tabItems(
      tabItem(tabName = "dashboard",
              box(
                width = 12, status = "primary",solidHeader = TRUE,
                title = "Ligand receptor interaction",
                # fluidRow(
                #   column(6,selectInput("org1","Select organism",c('Human' = "human",'Mouse' = "mouse"))), 
                #   column(6,selectInput("org2","Select organism",c('Human' = "human",'Mouse' = "mouse"))) 
                # ),
                fluidRow(
                  column(6,selectInput('org1', 'Organism',choices=c("human","mouse"))), 
                  column(6,selectInput('org2', 'Organism',choices=c("human","mouse"))) 
                ),
                fluidRow(
                   column(6,fileInput('lrlist1', 'Upload list1')), 
                   column(6,fileInput('lrlist2', 'Upload list2')) 
                ),
                plotOutput("venndiagram", height = 500),
                DT::dataTableOutput("datasetTable"),
                fluidRow(
                  column(6,downloadButton('downloadvenn', 'Download Venn diagram')),
                  column(6,downloadButton('downloadtable', 'Download table'))
              )
      ))
    )#end of tabitems
  )#end of dashboard body
)#end of dashboard page


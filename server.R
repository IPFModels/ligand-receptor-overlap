library(shiny)
library(shinyBS)
library(RColorBrewer)
library(biomaRt)
library(Biobase)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(scExtras)
library(cowplot)
library(data.table)
library(tibble)
library("ggvenn")
server <- function(input, output,session) {
  
  ####### Display project list and load data  #######
  #Read the parameter file
  readfiles = reactive({
    validate(
      need(input$lrlist1, "Please Upload Ligand receptor list 1")
    )
    validate(
      need(input$lrlist2, "Please Upload Ligand receptor list 2")
    )
    file1=input$lrlist1
    file2=input$lrlist2
    lrlist1= fread(file1$datapath,header = FALSE)
    lrlist2=fread(file2$datapath,header = FALSE)
    
    #Check if org entered matches the list uploaded
    mousehuman= read.csv("data/mouse_human.csv")
    if (input$org1=="human"){
      genelist= mousehuman$human_name
      newdf= lrlist1 %>% separate(V1,c("Ligand","Receptor"))
      validate(
        need(newdf$Ligand %in% genelist, "Please check and enter correct organism")
      )
    }
    else if (input$org1=="mouse"){
      genelist= mousehuman$mouse_name
      newdf= lrlist1 %>% separate(V1,c("Ligand","Receptor"))
      validate(
        need(newdf$Ligand %in% genelist, "Please check and enter correct organism")
      )
    }
    if (input$org2=="human"){
      genelist= mousehuman$human_name
      newdf= lrlist2 %>% separate(V1,c("Ligand","Receptor"))
      validate(
        need(newdf$Ligand %in% genelist, "Please check and enter correct organism")
      )
    }
    else if (input$org2=="mouse"){
      genelist= mousehuman$mouse_name
      newdf= lrlist2 %>% separate(V1,c("Ligand","Receptor"))
      validate(
        need(newdf$Ligand %in% genelist, "Please check and enter correct organism")
      )
    }
    
    
    #Find homologs if necessary
    if(input$org1 == input$org2){
      lrlist1=lrlist1
      lrlist2=lrlist2
    }
     else{
      lr1= lrlist1 %>% separate(V1,c("Ligand","Receptor"))
      lr2= lrlist2 %>% separate(V1,c("Ligand","Receptor"))
      if(input$org1=="human" && input$org2=="mouse"){
      lr2= left_join(lr2,mousehuman,by=c("Ligand"="mouse_name")) %>% dplyr::select(-Ligand,-human_id,-mouse_id) %>% rename("Ligand"="human_name")
      lr2= left_join(lr2,mousehuman,by=c("Receptor"="mouse_name")) %>% dplyr::select(-Receptor,-human_id,-mouse_id) %>% rename("Receptor"="human_name")
      }
      else if(input$org1=="mouse" && input$org2=="human"){
        lr2= left_join(lr2,mousehuman,by=c("Ligand"="human_name")) %>% dplyr::select(-Ligand,-human_id,-mouse_id) %>% rename("Ligand"="mouse_name")
        lr2= left_join(lr2,mousehuman,by=c("Receptor"="human_name")) %>% dplyr::select(-Receptor,-human_id,-mouse_id) %>% rename("Receptor"="mouse_name")
      }
      lrlist1$V1=paste0(lr1$Ligand,"_",lr1$Receptor,sep="")
      lrlist2$V1=paste0(lr2$Ligand,"_",lr2$Receptor,sep="")
    }
    
    x <- list(LR1 = lrlist1$V1, LR2 = lrlist2$V1)
    return(x)
  })
  
  #make Venn Diagram
  #Plot the PCA/Viz plot for the number of dimensions and number of genes chosen
  vennplot= reactive({
    file = readfiles()
    ggvenn(file,columns = c("LR1","LR2"))
  })
  
  #Render the vizplot
  output$venndiagram = renderPlot({
    withProgress(session = session, message = 'Generating...',detail = 'Please Wait...',{
      vennplot()
    })
  })
  
  #Generate list
  datasetTable = reactive({
    file = readfiles()
    lrlist1= as.data.frame(file$LR1)
    lrlist2=as.data.frame(file$LR2)
    colnames(lrlist1)=colnames(lrlist2)="V1"
    list=as.data.frame(intersect(lrlist1,lrlist2))
    colnames(list)="Ligand Receptor pairs"
    return(list)
  })
  
  output$datasetTable = DT::renderDataTable({
    withProgress(session = session, message = 'Loading...',detail = 'Please Wait...',{
      DT::datatable(datasetTable(),
                    extensions = c('Buttons','Scroller'),
                    options = list(dom = 'Bfrtip',
                                   searchHighlight = TRUE,
                                   pageLength = 20,
                                   lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                   scrollX = TRUE,
                                   buttons = c('copy', 'print')
                    ),rownames=FALSE,selection = list(mode = 'single', selected =1),escape = F)
    })
  })
 
####Download #####
  output$downloadtable <- downloadHandler(
    filename = function() { paste('LRpair.csv', sep='') },
    content = function(file) {
      write.csv(datasetTable(), file)
    })
  
  #Download function for the heatmap
  output$downloadvenn <- downloadHandler(
    filename = function() {
      paste0("Venndiagram.pdf")
    },
    content = function(file){
      pdf(file, width = 13, height = 8,useDingbats=FALSE)
      vennplot()
      dev.off()
    })
  
}#end of server
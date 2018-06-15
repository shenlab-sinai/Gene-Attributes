source("https://bioconductor.org/biocLite.R")
library(biomaRt)
library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
      title = "Obtain Attributes for Genes",
      titleWidth = 380
  ),
  dashboardSidebar(
      width = 380,
      radioButtons("radio", label=h3("Select a species"),
                   choices=list("Mouse" = 1,
                                "Human" = 2,
                                "Rat" = 3),
                   selected = 1),
      fileInput("file2", h3("Upload a list of Ensembl gene IDs/gene names"), 
                accept = c("text/csv", "text/comma-separated-values, 
                           text/plain", ".csv")),

      hr(),
      h4("Export table to a text file -"),
      downloadButton("downloadData", "Download", icon("paper-plane"),
                     style="color: #fff; background-color: maroon; 
                    border-color: black")

  ),
  dashboardBody(
      tags$head(
          tags$link(rel="stylesheet", type = "text/css", href = "custom.css")
      ),#background = "light-blue",height = 330, width = 100, 
      box(collapsible=T,
      tags$h2("About -"),
      tags$h4("This application is for obtaining more details about the genes in 
              your gene list."),
      tags$h2("Instructions -"),
      tags$h4("1. Select a species"),
      tags$h4("2. Upload a genelist"),
      tags$h4("3. Wait ..."),
      tags$h4("4. Done!"),
      tags$h4("If you wish, you can export the table by clicking on the button 'Download'")),
      
      fluidPage(
          tableOutput("output_geneids")
      )
  ),
  skin="red"
  )


server <- function(input, output) {
  datasetInput <- reactive({
    file2 <- input$file2
    if (is.null(file2)) {
      return(NULL)
    }
    
    # read input genes
    data1 <- data.frame(read.table(file2$datapath, stringsAsFactors = F))
    
    # determine species
    if (input$radio == 1) {
      dataset_name <- "mmusculus_gene_ensembl"
    } else if (input$radio == 2) {
      dataset_name <- "hsapiens_gene_ensembl"
      data1 <- data.frame(lapply(data1, function(v) {
        if (is.character(v)) return(toupper(v))
        else return(v)
      }))
    } else if (input$radio == 3) {
      dataset_name <- "rnorvegicus_gene_ensembl"
    }
    
    if(length(data1[grepl("^ENS", data1$V1),]) > length(data1[!grepl("^ENS", data1$V1),])){
      common_col = "ensembl_gene_id"
    } else {
      common_col = "external_gene_name"
    }
    
    colnames(data1) <- c(common_col) 
    
    ensembl <- useMart("ensembl", host="http://aug2017.archive.ensembl.org", 
                       dataset = dataset_name)
    mapping <- getBM(attributes = c('ensembl_gene_id', 'external_gene_name', 
                                    'chromosome_name', 'start_position', 
                                    'end_position', 'gene_biotype', 'description'), 
                     values = data1, mart = ensembl)
    
    fileText <- left_join(x=data1, y=mapping, 
                          by=common_col)
    
  })
  
  output$output_geneids <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {"output.txt"},
    content = function(file){
      write.table(datasetInput(), file, row.names = F, quote = F, sep = "\t")
    }
  )
  
}

shinyApp(ui = ui, server = server)

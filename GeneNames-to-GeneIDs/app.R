source("https://bioconductor.org/biocLite.R")
library(biomaRt)
library(shiny)
library(shinythemes)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("cosmo"), 
  titlePanel("Obtain attributes for genes", 
             windowTitle = "Gene IDs and gene names"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label=h3("Choose a species"),
                   choices=list("Mouse" = 1,
                                "Human" = 2,
                                "Rat" = 3),
                   selected = 1),
      fileInput("file2", h3("Upload a list of Ensembl gene IDs/gene names"), 
                accept = c("text/csv", "text/comma-separated-values, 
                           text/plain", ".csv")),
      submitButton("Submit"),
      hr(),
      downloadButton("downloadData", "Export table")
                ),
    
    mainPanel(
      tableOutput("output_geneids")
    )
  )
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

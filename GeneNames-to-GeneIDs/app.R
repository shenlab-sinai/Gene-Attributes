source("https://bioconductor.org/biocLite.R")
library(biomaRt)
library(shiny)
library(shinythemes)
library(dplyr)

ui <- fluidPage(
  theme = shinytheme("readable"), 
  titlePanel("ENSEMBL Gene IDs and Gene Names Converter", 
             windowTitle = "Gene IDs and gene names"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", label=h3("Choose a species"),
                   choices=list("Mouse" = 1,
                                "Human" = 2),
                   selected = 1),
      radioButtons("radio_function", label=h3("What would you like to do?"),
                   choices=list("Convert gene ID to gene name" = 1,
                                "Convert gene name to gene ID" = 2
                                ),
                   selected = 1),
      fileInput("file2", h3("Please upload a gene list"), 
                accept = c("text/csv", "text/comma-separated-values, 
                           text/plain", ".csv")),
      submitButton("Submit"),
      hr(),
      downloadButton("downloadData", "Download")
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
    
    data2 <- read.table(file2$datapath, stringsAsFactors = F)
    user_input <- data2$V1
    
    if (input$radio == 1) {
      dataset_name <- "mmusculus_gene_ensembl"
    } else if (input$radio == 2) {
      dataset_name <- "hsapiens_gene_ensembl"
      user_input <- toupper(user_input)
    }
    ensembl <- useMart("ensembl", host="http://aug2017.archive.ensembl.org", 
                       dataset = dataset_name)
    
    mapping <- getBM(attributes = c('ensembl_gene_id', 'external_gene_name'), 
                     mart = ensembl)
    
    if (input$radio_function == 1) {
      fileText <- mapping[mapping$ensembl_gene_id %in% user_input, ]
      fileText <- fileText %>% slice(match(user_input, ensembl_gene_id))


    } else if (input$radio_function == 2) {
      fileText <- mapping[mapping$external_gene_name %in% user_input, ]
      fileText <- fileText %>% slice(match(user_input, external_gene_name))

    }
  
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

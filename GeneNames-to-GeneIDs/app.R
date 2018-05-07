source("https://bioconductor.org/biocLite.R")
library(biomaRt)
library(shiny)

ui <- fluidPage(
  theme = "theme.css", 
    titlePanel("Obtain gene IDs for gene names", 
               windowTitle = "Gene IDs for gene names"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file2", h3("Please upload a gene list"), 
                  accept = c("text/csv", "text/comma-separated-values, 
                             text/plain", ".csv")),
        radioButtons("radio", label=h3("Choose a species"),
                     choices=list("Mouse" = 1,
                                  "Human" = 2),
                     selected = 1),
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
    mapping <- getBM(attributes = c('external_gene_name', 'ensembl_gene_id'), 
                     mart = ensembl)
    
    fileText <- mapping[mapping$external_gene_name %in% user_input, ]
    
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


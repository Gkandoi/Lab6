library(shiny)
library(ggplot2)
library(plotly)
library(crosstalk)
library(readr)
library(ggvis)

# Define UI for application
ui <- fluidPage(    
# Application title
  titlePanel("GOexplore: Gene Ontology Explorer!"),
# Sidebar with a radio button to select the Organism
  sidebarPanel(
    radioButtons("radio", label = h3("Choose organism"),
                 choices = list("SGN" = 1, "HS-rna" = 2), 
                 selected = NULL),
    
    hr(),
    fluidRow(column(10, verbatimTextOutput("value"))),
    uiOutput("data"),
    uiOutput("data2")
  ),
  
# Define the tab panels
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("view")),
      tabPanel("Plot", plotlyOutput("aspect")),
      tabPanel("Bar Plot of Genes",plotlyOutput("bars")),
      tabPanel("Bar Plot of Go terms",plotlyOutput("bars2"))
    )
  )
)

# Define server logic
# Download the current annotations from the GO server.
server <- function(input, output) {
  datasetInput <- reactive({
    txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/gene_association.sgn.gz")))
    SGN_data <- read.table(textConnection(txt),skip = 24, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")
    txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/goa_human_rna.gaf.gz")))
    HS_rna_data <- read.table(textConnection(txt),skip = 30, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")
    
    if (input$radio == 1) {
      SGN_data
    }  else {
      HS_rna_data
    }
  })

# Display the summary of data
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
# Show the first "10" observations
  output$view <- renderTable({
    head(datasetInput())
  })
# Plot the number of annotations for every evidence code, colored by the aspect of annotations.
  output$aspect <- renderPlotly({
    gg <-  ggplot(data = datasetInput(), aes(x = V7, fill = V9)) +
      geom_bar(stat = "count")
    ggplotly(gg)
  })
# Extract the list of genes and provide to the user a drop-down menu.
  output$data <- renderUI({
    data <- datasetInput()
    selectInput("genes",label="gene",choices=sort(unique(data$V3)),selected=data$V3[1])
  })
  
# Plot the number of annotations for every gene based on the aspect of annotations and colored by the evidence code.
  output$bars <- renderPlotly({
    gg <-  datasetInput() %>% filter(V3==input$genes) %>% ggplot(aes(x=V9, fill = V7))+
      geom_bar(stat = "count")
    ggplotly(gg)
  })

# Extract the list of GO terms and provide to the user a drop-down menu.
  output$data2 <- renderUI({
    data <- datasetInput()
    selectInput("goterms",label="go terms",choices=sort(unique(data$V5)),selected=data$V5[1])
  })
  
# Plot the number of annotations for every GO term based on the aspect of annotations and colored by the evidence code.  
  output$bars2 <- renderPlotly({
    gg <-  datasetInput() %>% filter(V5==input$goterms) %>% ggplot(aes(x=V9, fill = V7))+
      geom_bar(stat = "count")
    ggplotly(gg)
  })
  
}

# Bind ui and server together
shinyApp(ui, server)

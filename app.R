library(shiny)
library(ggplot2)
library(plotly)
library(crosstalk)
library(readr)
library(ggvis)

txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/gene_association.sgn.gz")))
SGN_data <- read.table(textConnection(txt),skip = 24, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")
txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/goa_human_rna.gaf.gz")))
HS_rna_data <- read.table(textConnection(txt),skip = 30, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")

# Define UI for application that draws a histogram
ui <- fluidPage(    
  # Application title
  titlePanel("Hello Shiny World!"),
  # Sidebar with a slider input for the number of bins
  sidebarPanel(
    radioButtons("radio", label = h3("Choose organism"),
                 choices = list("SGN" = 1, "HS-rna" = 2), 
                 selected = NULL),
    
    hr(),
    fluidRow(column(10, verbatimTextOutput("value"))),
    uiOutput("data"),
    uiOutput("data2")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Table", tableOutput("view")),
      tabPanel("Plot", plotlyOutput("aspect")),
      tabPanel("Bar Plot of Genes",plotOutput("bars")),
      tabPanel("Bar Plot of Go terms",plotOutput("bars2"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    datasetInput <- reactive({
      txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/gene_association.sgn.gz")))
      SGN_data <- read.table(textConnection(txt),skip = 24, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")
      txt <- readLines(gzcon(url("http://geneontology.org/gene-associations/goa_human_rna.gaf.gz")))
      HS_rna_data <- read.table(textConnection(txt),skip = 30, header = F, sep = "\t", strip.white = F, stringsAsFactors = T, skipNul = T, quote = "",comment.char = "")
      
      if (input$radio == 1) {
        current_data <- SGN_data
      }  else {
        current_data <- HS_rna_data
      }
    current_data
    })
    
  output$data <- renderUI({
    data <- datasetInput()
    selectInput("genes",label="gene",choices=sort(unique(data$V3)),selected=data$V3[1])
  })
  
  output$data2 <- renderUI({
    data <- datasetInput()
    selectInput("goterms",label="go terms",choices=sort(unique(data$V5)),selected=data$V5[1])
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "10" observations
  output$view <- renderTable({
    head(datasetInput())
  })
  
  output$aspect <- renderPlotly({
    gg <-  ggplot(data = datasetInput(), aes(x = V7, fill = V9)) +
      geom_bar(stat = "count")
    ggplotly(gg)
  })
  
  output$bars <- renderPlot({
    gg <-  datasetInput() %>% filter(V3==input$genes) %>% ggplot(aes(x=V9))+
      geom_bar(stat = "count")
    gg
  })
  
  output$bars2 <- renderPlot({
    gg <-  datasetInput() %>% filter(V5==input$goterms) %>% ggplot(aes(x=V9))+
      geom_bar(stat = "count")
    gg
  })

}

# Bind ui and server together
shinyApp(ui, server)


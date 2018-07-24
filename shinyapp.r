


ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput('tier1', 'Upload tier 1 data',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        fileInput('site_coordination', 'Upload Site Coordination Data',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )
)




server <- function(input, output) {
  
  data_frames <- reactiveValues({
    
    tier1 = input$tier1
    site_coordination = input$site_coordination
    
    
    if (is.null(input$tier1))
      return(NULL)
    
    observe()
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    
  })
  
  
  output$contents <- renderTable(
    
    getData()
    
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(getData(), file)
      
    })
  
}


shinyApp(ui, server)

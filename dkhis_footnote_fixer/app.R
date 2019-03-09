#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(XML)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("danmarkshistorien.dk's fodnote fixer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("file", "Vælg htmlfil"),
         textInput("missingurl", "Indtast den manglende del af URL"),
         downloadButton("downloadData", "Download")
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
   )
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  htmlcode <- eventReactive(input$file, {
    read_file(input$file$datapath)
    new_url = paste0("<a href=\"http://danmarkshistorien.dk/", input$missingurl, "#_")
    
    new_url = paste0("<a href=\"http://danmarkshistorien.dk/", input$missingurl, "#_")
    
    htmlcode <- str_replace_all(htmlcode, pattern = "(<a href=\"http://danmarkshistorien.dk/)(#_)", new_url)
    
    data_frame(lines= read_lines(input$file)) %>% 
      extract(lines, into = c("id", "notes"), regex = "name=\"(_ftn\\d+)\" rtekeep=\"1\">\\[\\d+\\]</a>(.+)</p>") %>% 
      na.omit() %>% 
      mutate(notes = str_replace_all(notes, "<.*?>", "")) %>% 
      mutate(re_from = paste0("(#", id, "\") (name)")) %>% 
      mutate(re_to   = paste0("\\1 title=\"", notes, "\" \\2")) -> footnotes
    
    
    for (row in 1:nrow(footnotes)) {
      
      re_from <- footnotes[[row, "re_from"]]
      re_to <- footnotes[[row, "re_to"]]
      
      htmlcode <- str_replace(htmlcode, pattern = re_from, re_to)
    }
  })
  

  
  output$downloadData <-  downloadHandler(
    filename = function() {
      paste("fixed_", input$missingurl, ".txt")
    },
    content = function(file) {
      write.table(htmlcode, file)
    }
  )
   
}

# Run the application 
shinyApp(ui = ui, server = server)


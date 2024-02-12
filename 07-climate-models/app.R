
# load relevant libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(scrollytell)

source("helper-text-plots.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # application title
    title = "Climate models: How accurate are they?",
    
    # set the background colours
    setBackgroundColor("#ede4d7"),
    
    # suppress warning messages while data is loading on-screen
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    # article title & name
    fluidRow(
      HTML("<center>
            <h1>Climate models: How accurate are they?</h1>
            <p style='font-size:26px'><a href='https://haganjam.github.io' target='_blank'>James G. Hagan</a></p>
                </center>")),
    
    br(),
    
    fluidRow(column(1),
             
             column(
               10, 
               # intro text
               fluidRow(column(1),
                        column(
                          10, 
                          br(),
                          text0a,
                          br()
                        ),
                        column(1))
               
             ),
             
             column(1)),
    
    fluidRow(column(1),
             column(10, align = "center",
                    h5(
                      class = "instructions",
                      "Observed temperature from 1880 until 2019 from five data products.",
                      br(),
                      "Each line is a different observational data product."
                    ),
                    plotOutput("intro_plot"),
                    br()),
             column(1)),
    
    fluidRow(column(1),
             column(10, 
                    fluidRow(column(1),
                             column(10,
                                    br(),
                                    text0b),
                             column(1)),
             column(1))),
    
    br(),
    br(),
    br(),
    br(),
    
    # scrollytelling plot
    scrolly_container(outputId = "scr",
                      
                      scrolly_graph( imageOutput("gif_n"), width = "62.5%" ),
                      
                      scrolly_sections(
                        scrolly_section(id = "buffer", NULL),
                        scrolly_section(id = 1, render_text(1)),
                        scrolly_section(id = 2, render_text(2)),
                        scrolly_section(id = 3, render_text(3)),
                        scrolly_section(id = 4, render_text(4)),
                        scrolly_section(id = 5, render_text(5)),
                        scrolly_section(id = 6, render_text(6)),
                        scrolly_section(id = 7, render_text(7)),
                        scrolly_section(id = 8, render_text(8)),
                        scrolly_section(id = 9, render_text(9)),
                        scrolly_section(id = 10, render_text(10)),
                        scrolly_section(id = "buffer", br()),
                        width = "35%")
                      ),
    
    # concluding text
    div(fluidRow(
      id = 'text',
      column(2),
      column(8,
             concludingtext,
             br()),
      column(2)
    ), style = 'margin-top: -300px;'),
    
    br(),
    br(),
    br(),
    hr(),
    
    fluidRow(column(1),
             column(10,
                    technicalnotes),
             column(1)),
    br(),
    br(),
    column(1)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$intro_plot <- renderPlot({
    intro_plot
  }, width = 700, height = 400, units = "px", res = 100)
  
  output$gif_n <- renderImage({
    
    list(src = paste0("www/plot_", input$scr, ".gif"),
         contentType = 'video/gif',
         width = "889px",
         height = "350px")
    
  }, deleteFile = FALSE)
  
  output$scr <- renderScrollytell({
    scrollytell()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

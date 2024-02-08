
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
    setBackgroundColor("#f0e9df"),
    
    # article title & name
    fluidRow(
      HTML("<center>
            <h1>Climate models: How accurate are they?</h1>
            <p style='font-size:22px'> by <a href='https://haganjam.github.io' target='_blank'>James G. Hagan</a></p>
            </center>")),
    
    br(),
    
    fluidRow(column(1),
             
             column(
               10, 
               # intro text
               fluidRow(id = 'text',
                        column(1),
                        column(
                          10, 
                          br(),
                          text0,
                          br()
                        ),
                        column(1))
               
             ),
             
             column(1)),
    
    fluidRow(column(1),
             column(10, align = "center",
                    h6(
                      class = "instructions",
                      "Observed temperature from 1880 until 2019 from five data products.",
                      br(),
                      "Each line is a different observational data product."
                    ),
                    plotOutput("intro_plot")),
             column(1)),
    
    # scrollytelling plot
    scrolly_container(outputId = "scr",
                      
                      scrolly_graph(
                        
                        img(src = "plot01.gif", height = '300px', width = '600px')
                        
                      ),
                      
                      scrolly_sections(
                        ## each of these sections corresponds to an update
                        ## the number after id = corresponds to the `scr` update
                        ## the render_text() function will be discussed later
                        scrolly_section(id = 0, render_text(0)),
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
                        # add a scrolly_section with nothing in it;
                        # this buffer prevents the plot from disappearing while reading last section
                        scrolly_section(id = "buffer", br())
                      )
                      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$intro_plot <- renderPlot({
    intro_plot
  }, width = 600, height = 400, res = 100)

}

# Run the application 
shinyApp(ui = ui, server = server)

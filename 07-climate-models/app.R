
# load relevant libraries
library(shiny)
library(shinyjs)
library(scrollytell)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Climate models: How accurate are they?"),
    
    # scrollytelling plot
    scrolly_container(outputId = "scr",
                      
                      scrolly_graph(
                        
                        img(src = "plot01.gif", height = '250px', width = '500px')
                        
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
                        #scrolly_section(id = 9, render_text(9)),
                        #scrolly_section(id = 10, render_text(10)),
                        # add a scrolly_section with nothing in it;
                        # this buffer prevents the plot from disappearing while reading last section
                        scrolly_section(id = "buffer", br())
                      )
                      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

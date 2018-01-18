library(shiny)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

#load the dataset
arrests <- read.csv(file = "dataset/USArrests.csv",header=TRUE, sep = ",")

arr <- arrests %>% add_rownames("region") %>% mutate(region=tolower(Region))

usa <- map_data("usa")
states <- map_data("state")

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("United States Arrests"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Create european maps with 
               general information of the countries."),
        selectInput("var", 
                    label = "Choose a variable to display",
                    choices = names(arrests)[2:5]),
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("selected_var"),
         p(),
         plotOutput("distPlot"),
         plotOutput("distPlot2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$selected_var <- renderText({ 
      paste("You have selected", input$var)
    })
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$distPlot2<-renderPlot({
     gg <- ggplot()
     gg <- gg + geom_map(data=states, map=states,
                         aes(x=long, y=lat, map_id=region),
                         fill="#ffffff", color="#ffffff", size=0.15)
     gg <- gg + geom_map(data=arr, map=states,
                         aes(fill= arrests[input$var], map_id=region),
                         color="#ffffff", size=0.15)
     gg <- gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                      guide='colorbar')
     gg <- gg + labs(x=NULL, y=NULL)
     gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
     gg <- gg + theme(panel.border = element_blank())
     gg <- gg + theme(panel.background = element_blank())
     gg <- gg + theme(axis.ticks = element_blank())
     gg <- gg + theme(axis.text = element_blank())
     gg
   })

}

# Run the application 
shinyApp(ui = ui, server = server)


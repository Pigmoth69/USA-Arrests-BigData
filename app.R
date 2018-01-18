library(shiny)
library(plotly)
library(openintro)

#load the dataset
df <- read.csv("dataset/USArrests.csv")
df["Code"] <- state2abbr(df$Region)

#define hoover
df$hover <- with(df, paste(Region, '<br>', "Murder", Murder, "Assault", Assault, "<br>","Urban Population",UrbanPop,"<br>","Rape",Rape))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# Specify map projection and options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)



# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("United States Arrests"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Create european maps with 
               general information of the countries."),
        #selectInput("var", 
        #            label = "Choose a variable to display",
        #            choices = names(arrests)[2:5]),
        sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #textOutput("selected_var"),
         #p(),
         #plotOutput("distPlot")
         plotlyOutput("distPlot2")
        #plot_geo(df, locationmode = 'USA-states')
        
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
   
   output$distPlot2<-renderPlotly({
       plot_geo(df, locationmode = 'USA-states') %>%
       add_trace(
         z = ~Murder, text = ~hover, locations = ~Code,
         color = ~Murder, colors = 'Purples'
       ) %>%
       colorbar(title = "Millions USD") %>%
       layout(
         title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
         geo = g
       )
     
   })
   
   output$plot <- renderPlotly({
     plot_ly(mtcars, x = ~mpg, y = ~wt)
   })

}

# Run the application 
shinyApp(ui = ui, server = server)


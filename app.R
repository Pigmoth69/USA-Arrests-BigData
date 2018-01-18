library(shiny)
library(plotly)
library(openintro)

#load the dataset
df <- read.csv("dataset/USArrests.csv",header=TRUE, sep = ",")
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
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = colnames(df)[2:5]),
      helpText("Select the values for the VS evaluation"),
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
      plotlyOutput("distPlot2"),
      plotlyOutput("distPlot3"),
      plotlyOutput("distPlot4")
      #plot_geo(df, locationmode = 'USA-states')
      
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
        z = ~get(input$var), text = ~hover, locations = ~Code,
        color = ~get(input$var), colors = 'Purples'
      ) %>%
      colorbar(title = "Millions USD") %>%
      layout(
        title = '1973 US Violent Crime Rates State<br>(Hover for breakdown)',
        geo = g
      )
    
  })
  
  output$distPlot3 <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  output$distPlot4<-renderPlotly({
    plot_ly(df, x = ~Region, y = ~Murder)
    
  })
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
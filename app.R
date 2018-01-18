library(shiny)
library(plotly) #used to plot all the charts and graphics
library(openintro) #used to convert state names to state codes
library(DT) #used to display the dataset in the table with the search option

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
        conditionalPanel(
          'input.graphical_data === "Dataset"',
          img(src="usa.png", width="100%"),
          p(),
          p("The used data was Violent Crime Rates by US State (1973). Here we can se the complete dataset."),
          p(),
          p("Possible Actions:"),
          tags$ul(
            tags$li("Click on the Columns to order the dataset"), p(),
            tags$li("Change the total number of entries in the page"), p(),
            tags$li("Search for a specific word or value")
          )
        ),
        conditionalPanel(
          'input.graphical_data === "Choropleth Map"',
          helpText("Choropleth Map representing the US states. Below the user can choose what variable to analise"),
          p(),
          selectInput("var_cloropleth", 
                      label = "Choose a variable to display",
                      choices = colnames(df)[2:5])
        ),
        conditionalPanel(
          'input.graphical_data === "Region Chart"',
          helpText("In this Histrogram graphic, you can choose what variable you want to plot and order them for a more precise analises"),
          p(),
          selectInput("var_bar_chart", 
                      label = "Choose a variable to display",
                      choices = colnames(df)[2:5])
        ),
        conditionalPanel(
          'input.graphical_data === "Pie Chart"',
          helpText("In this Pie chart..."),
          p()
        )
        #conditionalPanel()
      #helpText("Create european maps with 
      #         general information of the countries."),
      
      #helpText("Select the values for the VS evaluation"),
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = 'graphical_data',
        
        tabPanel("Dataset",DT::dataTableOutput("mytable2"),icon = icon("table")),
        
        tabPanel("Choropleth Map", icon = icon("map-o"),
                 p(),
                 plotlyOutput("dispChoropleth")
        ),
        tabPanel("Region Chart", icon= icon("bar-chart-o"),
                 p(),
                 plotlyOutput("regionChart")
        ),
        tabPanel("Scatter Plot",
                 h5(textOutput("yet another text output")),
                 htmlOutput("yetAnotherHTMLElement")
        ),
        tabPanel("Pie Chart", icon = icon("pie-chart"),
                 p(),
                 plotlyOutput("pieChart")
        )
      )
      #textOutput("selected_var"),
      #p(),
      #plotOutput("distPlot")
      #plotlyOutput("distPlot2"),
      #plotlyOutput("distPlot3"),
      #plotlyOutput("distPlot4")
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
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(df[1:5], options = list(orderClasses = TRUE))
  })
  
  output$dispChoropleth<-renderPlotly({
    plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(
        z = ~get(input$var_cloropleth), text = ~hover, locations = ~Code,
        color = ~get(input$var_cloropleth), colors = 'Reds'
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
  
  output$regionChart<-renderPlotly({
    x <- list(
      title = "Region",
      autotick=TRUE
    )
    y <- list(
      title = input$var_bar_chart
    )
    
    plot_ly(df, x = ~Region, y = ~get(input$var_bar_chart),type="bar") %>%
      layout(xaxis = x, yaxis = y)
    
  })
  
  #Pie chart
  output$pieChart <- renderPlotly({
    names <- colnames(df[c(2,3,5)])
    value <- c(sum(df[2]),sum(df[3]),sum(df[5]))
    colors <- c('rgb(146, 168, 232)','rgb(1, 36, 140)','rgb(79, 101, 165)')
    
    pieFrame <- data.frame("Categorie"= names,value)
    
    plot_ly(pieFrame, labels = ~Categorie, values = ~value, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste('Total:', value, ' people'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = FALSE) %>%
      layout(title = 'Violent Crime Rates by US State (1973)',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
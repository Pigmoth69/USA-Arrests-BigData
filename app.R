library(shiny)
library(plotly) #used to plot all the charts and graphics
library(openintro) #used to convert state names to state codes
library(DT) #used to display the dataset in the table with the search option
library(dplyr)

#load the dataset
df <- read.csv("dataset/USArrests.csv",header=TRUE, sep = ",")
df["Code"] <- state2abbr(df$Region)

#define hoover
#df$hover <- with(df, paste('State: ',Region, '<br>', "Murder", Murder, "Assault", Assault, "<br>","Urban Population",UrbanPop,"<br>","Rape",Rape))

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
          'input.graphical_data === "Stacked Bar Chart"',
          helpText("Stacked bar chart"),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Region Chart"',
          helpText("In this Histrogram graphic, you can choose what variable you want to plot and order them for a more precise analises"),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Choropleth Map"',
          helpText("In this Cloropleth map cenaws cenas ceaass"),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Region Chart" || input.graphical_data === "Choropleth Map"',
          selectInput("var_bar_chart", 
                      label = "Choose a variable to display",
                      choices = colnames(df)[2:5]),
          p(),
          sliderInput("slider_input_bar", h3("Values range:"),
                      min = 0, max = 100, value = c(0, 100))
        ),
        conditionalPanel(
          'input.graphical_data === "Pie Chart"',
          helpText("In this Pie chart..."),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Scatter Plot"',
          helpText("Scatter plot"),
          p(),
          selectInput("var_scatter_plot_1", 
                      label = "Choose a variable to display on axis X",
                      choices = colnames(df)[2:5]),
          p(),
          selectInput("var_scatter_plot_2", 
                      label = "Choose a variable to display on axis Y",
                      choices = colnames(df)[2:5],
                      selected = colnames(df)[3])
          #selectizeInput(
          # 'e5', '5. Max number of items to select', choices = state.name,
          #  multiple = TRUE, options = list(maxItems = 2)
          #),
          #sliderInput("slider_input_scatter", h3("Sliders"),
          #            min = 0, max = 100, value = 50)
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
        tabPanel("Scatter Plot", icon = icon("bar-chart-o"),
                 p(),
                 plotlyOutput("scatterPlot")
        ),
        tabPanel("Stacked Bar Chart", icon = icon("bar-chart-o"),
                 p(),
                 plotlyOutput("stackedBarChart")
        ),
        tabPanel("Pie Chart", icon = icon("pie-chart"),
                 p(),
                 plotlyOutput("pieChart")
        )
      )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(df[1:5], options = list(orderClasses = TRUE))
  })
  
  output$dispChoropleth<-renderPlotly({
    
    newData <-df %>% 
      filter(get(input$var_bar_chart) >= input$slider_input_bar[1]) %>%
      filter(get(input$var_bar_chart) <= input$slider_input_bar[2])
    
    plot_geo(newData, locationmode = 'USA-states')%>%
      add_trace(
        z = ~get(input$var_bar_chart),
        text = ~paste('State: ', Region,
                      '<br>',input$var_bar_chart,': ',get(input$var_bar_chart)),
        locations = ~Code,
        color = ~get(input$var_bar_chart), colors = 'Reds'
      ) %>%
      colorbar(title = "Millions USD") %>%
      layout(
        title = '1973 US Violent Crime Rates State<br>(Hover for breakdown)',
        geo = g
      )
  })
  
  output$regionChart<-renderPlotly({

    plot_ly(df, x = ~Region, y = ~get(input$var_bar_chart), type="bar",
            transforms = list(
              list(
                type = 'filter',
                target = 'y',
                operation = '<=',
                value = input$slider_input_bar[2]
              ),
              list(
                type = 'filter',
                target = 'y',
                operation = '>=',
                value = input$slider_input_bar[1]
              )
            )) %>%
      layout(xaxis = list(title = "Region"),
             yaxis = list(title = input$var_bar_chart))
    
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
  
  output$scatterPlot <- renderPlotly({
    plot_ly(df, x = ~get(input$var_scatter_plot_1), y = ~get(input$var_scatter_plot_2),
            hoverinfo = 'text',
            text = ~paste('State: ', Region,
                          '<br>',input$var_scatter_plot_2,get(input$var_scatter_plot_2),
                          '<br>',input$var_scatter_plot_1,get(input$var_scatter_plot_1)),
                 marker = list(size = 10,
                               color = 'rgba(255, 182, 193, .9)',
                               line = list(color = 'rgba(152, 0, 0, .8)',
                                           width = 2))) %>%
      layout(title = 'Styled Scatter',
             yaxis = list(zeroline = FALSE, title=input$var_scatter_plot_2),
             xaxis = list(zeroline = FALSE, title=input$var_scatter_plot_1))
  })
  
  output$stackedBarChart <- renderPlotly({
    p <- plot_ly(df, x = ~Region, y = ~Assault, type = 'bar', name = 'Assault') %>%
      add_trace(y = ~Murder, name = 'Murder') %>%
      add_trace(y = ~Rape, name = 'Rape') %>%
      layout(yaxis = list(title = 'Total'), barmode = 'stack')
  })
  
  #observe
  observe({
    column <- input$var_bar_chart
    tmp_min <- min(df[column])
    tmp_max <- max(df[column])
    step_tmp <- 0.01
    updateSliderInput(session, "slider_input_bar", value= c(tmp_min,tmp_max), min = tmp_min, max = tmp_max,step=step_tmp)
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
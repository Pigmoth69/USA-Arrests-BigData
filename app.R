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

#Global text
percentage<- 'Arrest (in %)'
perX<- 'Arrest (per 100k residents)'


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
          h4("The used data was Violent Crime Rates by US State (1973). Here we can se the complete dataset."),
          p(),
          h5(tags$b("Possible Actions:")),
          tags$ul(
            tags$li("Click on the Columns to order the dataset"), p(),
            tags$li("Change the total number of entries in the page"), p(),
            tags$li("Search for a specific word or value")
          ),
          p(),
          h5(tags$b("The variables are in the following units:")),
          tags$ul(
            tags$li(tags$b("Murder"),'=',perX),
            tags$li(tags$b("Assault"),"=",perX),
            tags$li(tags$b("UrbanPop"),"=",percentage),
            tags$li(tags$b("Rape"),"=",perX)
          )
        ),
        conditionalPanel(
          'input.graphical_data === "Stacked Bar Chart"',
          helpText("Stacked bar charts are designed to help you simultaneously compare totals and notice sharp changes at the item level that are likely to have the most influence on movements in category totals."),
          helpText("In the following stacked bar chart, we can easily compare the variations of the data and compare it with other plotting values..")
          ),
        conditionalPanel(
          'input.graphical_data === "Region Chart"',
          helpText("A bar chart or bar graph is a chart or graph that presents categorical data with rectangular bars with heights or lengths proportional to the values that they represent. The bars can be plotted vertically or horizontally. A vertical bar chart is sometimes called a line graph."),
          helpText("In this case, we can choose and vary between the variables \"Murder\", \"Assault\", \"Rape\", \"Urban Population\" in the dropdown menu and use the slider to select a range of values according to the preference."),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Choropleth Map"',
          helpText("A choropleth map is a thematic map in which areas are shaded or patterned in proportion to the measurement of the statistical variable being displayed on the map, such as population density or per-capita income. Choropleth maps provide an easy way to visualize how a measurement varies across a geographic area or show the level of variability within a region."),
          helpText("In this case, we can choose and vary between the variables \"Murder\", \"Assault\", \"Rape\", \"Urban Population\" in the dropdown menu and use the slider to select a range of values according to the preference."),
          p()
        ),
        conditionalPanel(
          'input.graphical_data === "Region Chart" || input.graphical_data === "Choropleth Map"',
          h3("Select variable to analyze"),
          selectInput("var_bar_chart", 
                      label = "",
                      choices = colnames(df)[2:5]),
          p(),
          sliderInput("slider_input_bar", h3("Choose values range:"),
                      min = 0, max = 100, value = c(0, 100))
        ),
        conditionalPanel(
          'input.graphical_data === "Pie Chart"',
          helpText("A pie chart is a circular statistical graphic which is divided into slices to illustrate numerical proportion. In a pie chart, the arc length of each slice (and consequently its central angle and area), is proportional to the quantity it represents."),
          helpText(" In the following pie chart we can clearly compare the values and check what's the major arrest in the US that corresponts to the Assault felony.")
        ),
        conditionalPanel(
          'input.graphical_data === "Scatter Plot"',
          helpText("A scatter plot is a type of plot or mathematical diagram using Cartesian coordinates to display values for typically two variables for a set of data. The data is displayed as a collection of points, each having the value of one variable determining the position on the horizontal axis and the value of the other variable determining the position on the vertical axis."),
          helpText("In this case, we can choose and vary between the variables \"Murder\", \"Assault\", \"Rape\", \"Urban Population\" in the dropdown menus. We can combine this variables in order to better visualize and analyze the information."),
          p(),
          selectInput("var_scatter_plot_1", 
                      label = "Choose a variable to display on axis X",
                      choices = colnames(df)[2:5]),
          p(),
          selectInput("var_scatter_plot_2", 
                      label = "Choose a variable to display on axis Y",
                      choices = colnames(df)[2:5],
                      selected = colnames(df)[3])
        )
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
      colorbar(title = buildText(input$var_bar_chart)) %>%
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
             yaxis = list(title = buildText(input$var_bar_chart)))
    
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
      layout(title = paste('Violent Crime Rates by US State (1973',buildText("")),
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
             yaxis = list(zeroline = FALSE, title=buildText(input$var_scatter_plot_2)),
             xaxis = list(zeroline = FALSE, title=buildText(input$var_scatter_plot_1)))
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
  
  buildText <- function(variable){
    if(variable == 'UrbanPop')
      return(paste(variable,percentage))
    else if(variable == 'Murder' || variable == 'Rape' || variable == 'Assault')
      return(paste(variable,perX))
    else
      return(paste(variable,perX))
    
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/



library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)



# reading from the dataset
data_source =read.csv("health_condition_dataset.csv")

# UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dashboard", icon = icon("table")),
      menuItem("Data Visualization", tabName = "analytics", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Medical Conditon and Age Relationship"),
             
               #displaying the data in a table
              DTOutput("table")
             
      ),
      # Second tab content
      tabItem(tabName = "analytics",
              h2("Data Visualization Section"),
              sidebarLayout(
                sidebarPanel (
                  selectInput("condition", "Select Medical Condition:",
                              choices =  unique(data_source$Medical.Condition),
                              selected = unique(data_source$Medical.Condition)[1] )
                ),
                 mainPanel()
              ),
             
               #Analytic explanation
               p("Please each Histogram displays the number of patiences with a specific medical condition a specific Age."),
              plotOutput("my_plot")
              
      )
    )
  )
)

# Server (Backend Logic)
server <- function(input, output) {
  #table rendering
  output$table <- renderDT(data_source, options = list(scrollX = TRUE))
  
  
  
  #cleaning data for selected medical condition
  filtered_data <- reactive({
    subset(data_source, Medical.Condition == input$condition)
  })
  
  #visualization rendering in a histogram form
  output$my_plot <- renderPlot({
    ggplot(filtered_data(), aes(x=Age )) +
      geom_histogram(binwidth=4, fill="black", color="gold", position ="identity") +
      scale_x_continuous(limits = c(10, 80))+
     #graph labels
       labs (
        title =paste("Data Analysis for", input$condition, "Medical Condition"),
        x = "Age",
        y = "Count"
      )+
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)


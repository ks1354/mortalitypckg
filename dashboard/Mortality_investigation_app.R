#install.packages()
library(shiny)
library(shinydashboard)
library(Mortality)

MAT <- mortality_model("SA_mortality.csv")
MAT_Orig <- read.csv("SA_mortality.csv")
MAT_Orig <- reactiveValues(data = MAT_Orig)
MAT <- reactiveValues(data = MAT)

ui <- dashboardPage(
  dashboardHeader(
    title = "Mortality Experience Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview",icon = icon("th")),
      menuItem("Reporting", tabName = "Reporting", icon = icon("th")),
      menuItem("Contact Us", tabName = "Contactus", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview",
              fluidRow(h2("Introduction"), textOutput("Intro")),
              fluidRow(h2("Preview of the Original Mortality dataset"),column(width = 12, dataTableOutput("mortality_table"),
                              style = "overflow-x: scroll;"))),
      tabItem(tabName = "Reporting",
              fluidRow(h2("Visualising Results"), textOutput("results")),
              fluidRow(column(4, selectInput("Age", h3("Select Age"), choices = c(20:65)))),
              fluidRow(h3("Mortality Rates"),
                  infoBoxOutput("Crude_Mortality_Rate"),
                  infoBoxOutput("Actual_Mortality_Rate"),
                  infoBoxOutput("Predicted_Mortality_Rate")
                ),
              fluidRow(h3("Mortality Trend Graph",plotOutput("plot1")))
      ),
      tabItem(tabName = "Contactus",
              fluidRow(h2("Contact Information"), textOutput("contact"))
    ))
)
)


server <- function(input, output, session) {
  observeEvent(input$Age,{
    output$Crude_Mortality_Rate <- renderInfoBox({
      infoBox(title = paste0("Crude Mortality Rate"), value =paste(
        MAT$data[MAT$data$Age == input$Age, "crude_mortality"]),
        color = "red", fill = TRUE)})
    output$Actual_Mortality_Rate <- renderInfoBox({
      infoBox(title = paste0("Actual Mortality Rate"), value =paste(
        MAT$data[MAT$data$Age == input$Age, "price_rate"]),
        color = "blue", fill = TRUE)})
    output$Predicted_Mortality_Rate <- renderInfoBox({
      infoBox(title = paste0("Predicted Mortality Rate"), value =paste(
        MAT$data[MAT$data$Age == input$Age, "predicted"]),
        color = "green", fill = TRUE)})
  }
  )
  output$plot1 <- renderPlot({
    plot(x = MAT$data$Age, y = MAT$data$crude_mortality*1000, xlab = "Age", ylab = "Mortality Rate per Mille",
        main = "Plot of Age against Mortality - \n Basic Poisson Regression", pch = 16, col = "black", xlim = c(20, 70))

    lines(x = MAT$data$Age, y = MAT$data$predicted*1000, col = "royalblue2", lwd = 2, lty = 1)
    lines(x = MAT$data$Age, y = MAT$data$price_rate*1000, col = "springgreen", lwd = 2, lty = 1)

    legend("topleft", c("Crude Mortality", "Poisson Regression", "Actual Mortality Rate"),
          col = c("black", "royalblue2", "springgreen"), lty = c(1, 1), lwd = 2)
  })
  output$Intro <- renderText("This is an examplory mortality experience analysis conducted on an anonymised South African lives mortality dataset.")
  output$mortality_table <- renderDataTable(MAT_Orig$data)
  output$results <- renderText("This page sumarises below the results from the mortality investigation conducted.")
  output$contact <- renderText("If you have any questions or would like to learn more, please get in contact with us at [info@actuartech.com].
                                For actuarial data science training needs, please visit https://training.actuartech.com/")
}



# Run the application
shinyApp(ui = ui, server = server)


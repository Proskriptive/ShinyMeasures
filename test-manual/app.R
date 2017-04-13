library(shiny)
library(shinydashboard)
library(dygraphs)
library(Widgets)


body <- dashboardBody(
  fluidRow(box(
             createdygraph(headertext = "Deaths from Lung Diseases(England)",
               enableDyrange = TRUE, enableDyhighlight = TRUE),width = 8))
          )

controlbar <- dashboardControlbar(
  createNavTabs(
    tabID = "home-tab",
    icon = icon("home")),
  createNavTabs(
    tabID = "settings-tab",
    icon = icon("gears")),
  paneldivs = {
    div( class = "tab-content" ,
         div(class = "tab-pane",id = "control-sidebar-settings-tab",
             createSettingsTabPanel(
               createFormPanel("Report Panel Usage","some Information about this general setting option"),
               createFormPanel("Allow mail redirect","Other sets of options are available"),
               createFormPanel("Expose author name in posts","Allow the user to show his name in blog posts"),
               panelHeading =  "General Settings"
             ),
             createSettingsTabPanel(
               createFormPanel("Show me as Online",""),
               createFormPanel("Turn off Notifications",""),
               createFormPanel("Delete Chat History","",icon = icon("trash")),
               panelHeading =  "Chat settings")),
         createHomeTabPanel(
           createListItems(Header = "Custom Template Design",
                           ProgressValue = 70,
                           ProgressBarClass = "danger")
           ,createListItems(Header = "Update Resume",
                            ProgressValue = 95,
                            ProgressBarClass = "success")
           ,createListItems(Header = "Laravel Integration",
                            ProgressValue = 50,
                            ProgressBarClass = "warning")
           ,panelHeading = "Tasks Progress")
    )
  }
)
ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(),
                    body, controlbar = controlbar)

server <- function(input,output) {
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui = ui, server = server)




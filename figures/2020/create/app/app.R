#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' in R-studio.
#

library(shiny)
library(markdown)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Results"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table: Narrow input warp", icon = icon("columns"), tabName = "table1")
    ),
    sidebarMenu(
      menuItem("Table: Wide input warp", icon = icon("columns"), tabName = "table2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table1",
              DT::dataTableOutput("t1")
      ),
      tabItem(tabName = "table2",
              DT::dataTableOutput("t2")
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Function for rounding only numeric columns of data frame
  round_df <- function(df, digits) {
    is.num      <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,is.num] <- round(df[,is.num], digits = digits)
    return(df)
  }
  
  # Data tables
  icols <- c(1:10)
  output$t1 <- DT::renderDataTable({
    fn <- normalizePath(file.path('../res/results_wide.rds'))
    df <- readRDS(fn)
    df <- round_df(df, digits = 3)
    DT::datatable(df[,icols])
  })
  output$t2 <- DT::renderDataTable({
    fn <- normalizePath(file.path('../res/results_narrow.rds'))
    df <- readRDS(fn)
    df <- round_df(df, digits = 3)
    DT::datatable(df[,icols])
  })
  
}

shinyApp(ui, server)




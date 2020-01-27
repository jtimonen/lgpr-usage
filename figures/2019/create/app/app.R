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
      menuItem("Table: homogeneous", icon = icon("columns"), tabName = "table1"),
      menuItem("Table: heterogeneous", icon = icon("columns"), tabName = "table2"),
      menuItem("Table: compare", icon = icon("columns"), tabName = "table3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table1",
              DT::dataTableOutput("t1")
      ),
      tabItem(tabName = "table2",
              DT::dataTableOutput("t2")
      ),
      tabItem(tabName = "table3",
              DT::dataTableOutput("t3")
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
  output$t1 <- DT::renderDataTable({
    fn <- normalizePath(file.path('../tables/res_basic.rds'))
    df <- readRDS(fn)
    df <- round_df(df, digits = 3)
    DT::datatable(df)
  })
  
  output$t2 <- DT::renderDataTable({
    fn <- normalizePath(file.path('../tables/res_heter.rds'))
    df <- readRDS(fn)
    df <- round_df(df, digits = 3)
    DT::datatable(df)
  })
  
  output$t3 <- DT::renderDataTable({
    fn <- normalizePath(file.path('../tables/res_combined.rds'))
    df <- readRDS(fn)
    df <- round_df(df, digits = 3)
    DT::datatable(df)
  })
  
}

shinyApp(ui, server)




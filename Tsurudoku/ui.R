# Tsurudoku -- a Shiny implementation of a sudoku puzzle generator / solver

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tsurudoku"),
  sidebarLayout(
    sidebarPanel(actionButton("lock", "Lock all"),
                 actionButton("unlock", "Unlock all"),
                 actionButton("clear", "Clear selected"),
                 actionButton("reset", "Reset all")
    ),
    mainPanel(
      plotOutput("numberPlot", click = "num_click", height=80, width=400),
      plotOutput("sudokuPlot", hover = "grid_hover", click = "grid_click", height=400, width=400)
    )
  )
))

# Tsurudoku -- a Shiny implementation of a sudoku puzzle generator / solver

library(shiny);

shinyUI(
  fluidPage(
  
    # Application title
    titlePanel("Tsurudoku"),
    sidebarLayout(
      sidebarPanel(
        actionButton("lock", "Lock all"),
        actionButton("unlock", "Unlock all"),
        actionButton("reset", "Reset all"),
        actionButton("clear", "Clear selected"),
        actionButton("undo", "Undo"),
        downloadButton("sudokugrid.txt", "Save puzzle"),
        downloadButton("sudokugrid.pdf", label="Make PDF"),
        fileInput("sudoku_input.txt", label=NULL, buttonLabel="Load puzzle", 
                  accept = c("text/plain",".txt")),
        checkboxGroupInput("solveLevels", label = "Solve Levels",
                           choices = list("line/line elimination",
                                          "single candidate")),
        actionButton("solve", "Solve"),
        actionButton("makePuzle", "Make Puzzle")
          
      ),
      mainPanel(
        plotOutput("sudokuPlot", hover = hoverOpts("grid_hover", delay=100), 
                   click = "grid_click", height=400, width=400),
        plotOutput("numberPlot", click = "num_click", height=80, width=400)
      )
    ),
    tags$script('
    pressedKeyCount = 0;
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("pressedKey", pressedKeyCount++);
       Shiny.onInputChange("pressedKeyId", e.which);
    });'),
    tags$script('
    pressedKeyCount = 0;
    $(document).on("keyup", function (e) {
       Shiny.onInputChange("releasedKey", pressedKeyCount++);
       Shiny.onInputChange("releasedKeyId", e.which);
    });')
  )
)
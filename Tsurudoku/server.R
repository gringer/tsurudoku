#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  values <- reactiveValues();
  values$click <- c(-1,-1);
  values$numClick <- -1;
  values$numFade <- 0;
  values$hover <- c(-1,-1);
  values$maxPos <- c(9,9);
  values$puzzle <- array(FALSE, c(9,9,9));
  values$locked <- array(FALSE, c(9,9));
  
  ## Number display at top of screen
  output$numberPlot <- renderPlot({
    par(mar=c(0,0,0,0), lwd=3);
    plot(NA, xlim=c(0.5,10.5), ylim=c(0.5,2.5), ann=FALSE, axes=FALSE);
    segments(x0=(1:10), y0=1, y1=2);
    segments(x0=1, x1=10, y0=c(1,2));
    text(x=1:9+0.5, y=1.5, labels=1:9, cex=2);
  });
  
  ## Main sudoku grid
  output$sudokuPlot <- renderPlot({
    
    par(mar=c(0,0,0,0));
    plot(NA, xlim=c(0.5,10.5), ylim=c(0.5,10.5), ann=FALSE, axes=FALSE);
    segments(x0=1, x1=10, y0=(1:10));
    segments(x0=(1:10), y0=1, y1=10);
    segments(x0=1, x1=10, y0=seq(1, 10, length.out=4), lwd=3);
    segments(x0=seq(1, 10, length.out=4), y0=1, y1=10, lwd=3);
    
    ## choices
    boxPos = seq(0.2,0.8,length.out=3);
    for(yi in 1:9){
      for(xi in 1:9){
        if(length(which(values$puzzle[yi,xi,])) > 1){
          rect(xleft = rep(boxPos,3)-0.1+xi, xright=rep(boxPos,3)+0.1+xi,
               ytop = rep(rev(boxPos),each=3)-0.1+yi, ybottom=rep(rev(boxPos),each=3)+0.1+yi,
               border = NA, col = c(NA,"#0000FF")[values$puzzle[yi,xi,]+1]);
        }
      }
    }
    
    textValues <- apply(values$puzzle,c(1,2), 
                        function(x){ifelse(length(which(x)) == 1, which(x), "")});
    text(x=rep(1:9+0.5, each=9), y=rep(1:9, 9)+0.5, textValues, 
         cex=c(1.5,1.75)[values$locked+1], 
         col=c("#404040","#000000")[values$locked+1]);

    if(all(values$hover >= 0)){
      rect(xleft=values$hover[1], xright=values$hover[1]+1,
           ytop=values$hover[2], ybottom=values$hover[2]+1,
           lwd = 3, col = "#80808060", border=NA);
    }
    if(all(values$click >= 0)){
      rect(xleft=values$click[1], xright=values$click[1]+1,
           ytop=values$click[2], ybottom=values$click[2]+1,
           lwd = 5, border = "#0000FF60");
    }
    
  });
  
  observeEvent(!is.null(input$grid_hover$x) && floor(c(input$grid_hover$x, input$grid_hover$y)), {
    if(is.null(input$grid_hover$x)){
    } else {
      values$hover <- floor(c(input$grid_hover$x, input$grid_hover$y));
      if(any((values$hover > values$maxPos) | (values$hover < 1))){
        values$hover <- c(-1,-1);
      }
    }
  });

  observeEvent(!is.null(input$grid_click$x) && floor(c(input$grid_click$x, input$grid_click$y)), {
    if(is.null(input$grid_click$x)){
    } else {
      values$click <- floor(c(input$grid_click$x, input$grid_click$y));
      if(any((values$click > values$maxPos) | (values$click < 1))){
        values$click <- c(-1,-1);
      }
      values$hover <- values$click;
    }
  });
  
  observeEvent(!is.null(input$num_click$x) && floor(input$num_click$x), {
    if(is.null(input$num_click$x)){
    } else {
      values$numClick <- floor(input$num_click$x);
      values$numFade <- 10;
      if(any((values$numClick > values$maxPos) | (values$numClick < 1))){
        values$numClick <- -1;
        values$numFade <- 0;
      } else {
        if(!any(values$click == c(-1,-1))){
          values$puzzle[values$click[2],values$click[1],values$numClick] <-
            xor(values$puzzle[values$click[2],values$click[1],values$numClick],TRUE);
        }
      }
    }
  });
  
  observeEvent(input$lock, {
    for(yi in 1:9){
      for(xi in 1:9){
        values$locked[yi,xi] <- (length(which(values$puzzle[yi,xi,])) == 1);
      }
    }
  });

  observeEvent(input$unlock, {
    for(yi in 1:9){
      for(xi in 1:9){
        values$locked[yi,xi] <- FALSE;
      }
    }
  });

  observeEvent(input$reset, {
    for(yi in 1:9){
      for(xi in 1:9){
        if(!values$locked[yi,xi]){
          values$puzzle[yi,xi,] = rep(FALSE,9);
        }
      }
    }
  });
    
})

# Tsurudoku -- a Shiny implementation of a sudoku puzzle generator / solver

library(shiny);

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  values <- reactiveValues();
  values$click <- c(-1,-1);
  values$numClick <- -1;
  values$numFade <- 0;
  values$hover <- c(-1,-1);
  values$hoverCell <- c(-1,-1);
  values$maxPos <- c(9,9);
  values$puzzle <- array(FALSE, c(9,9,9));
  values$locked <- array(FALSE, c(9,9));
  values$changes <- FALSE;
  values$puzzleHeader <- "## tsurudoku file format v1.0";
  values$puzzleBuffer <- c("## tsurudoku file format v1.0", "## Additional changes");

  checkValid <- function(checkPuzzle){
    validPuzzle <- TRUE;
    singleChoices <- apply(checkPuzzle,c(1,2),
                        function(x){ifelse(length(which(x)) == 1, which(x), NA)});
    if(any(apply(singleChoices, 1, function(x){suppressWarnings(max(table(x)) > 1)}))){
      validPuzzle <- FALSE;
    }
    if(any(apply(singleChoices, 2, function(x){suppressWarnings(max(table(x)) > 1)}))){
      validPuzzle <- FALSE;
    }
    if(any(sapply(0:8, function(x){
      suppressWarnings(max(table(singleChoices[(floor(x/3)*3):(floor(x/3)*3+2)+1, 
                                               ((x %% 3)*3):((x %% 3)*3 + 2)+1])) > 1);
    }))){
      validPuzzle <- FALSE;
    }
    return(validPuzzle);
  }
  
  clearBoard <- function(appendChanges = TRUE){
    values$puzzleBuffer <- values$puzzleHeader;
    if(appendChanges){
      values$puzzleBuffer <- c(values$puzzleBuffer, "## Additional changes");
    }
    values$puzzle <- array(FALSE, c(9,9,9));
    values$locked <- array(FALSE, c(9,9));
  }
  
  flipNumber <- function(px, py, numToFlip){ # inverts the logic for a single number
    if(!values$locked[py,px]){ # make sure the position is unlocked
      puzzleString <- sprintf("%d,%d %d",
                              px, py, numToFlip);
      if(tail(values$puzzleBuffer, 1) == puzzleString){
        values$puzzleBuffer <- head(values$puzzleBuffer, -1);
      } else {
        values$puzzleBuffer <- c(values$puzzleBuffer, puzzleString);
      }
      values$puzzle[py,px,numToFlip] <- xor(values$puzzle[py,px,numToFlip],TRUE);
      values$changed <- TRUE;
    }
  }

  flipAll <- function(px, py){ # inverts the logic for *all* numbers at one location
    if(!values$locked[py,px]){ # make sure the position is unlocked
      puzzleString <- sprintf("%d,%d 0", px, py);
      if(tail(values$puzzleBuffer, 1) == puzzleString){
        values$puzzleBuffer <- head(values$puzzleBuffer, -1);
      } else {
        values$puzzleBuffer <- c(values$puzzleBuffer, puzzleString);
      }
      values$puzzle[py,px,] <- xor(values$puzzle[py,px,],TRUE);
      values$changed <- TRUE;
    }
  }
  
  undoMove <- function(){
    lastLine <- tail(values$puzzleBuffer, 1);
    if(!grepl("^##",lastLine)){
      posData <- as.numeric(unlist(strsplit(lastLine, "[,; ]")));
      if(length(posData) != 3){
        showModal(modalDialog(sprintf(
          "Invalid position information, expecting 'x,y <num>', got '%s'", lastLine)));
      } else {
        if(posData[3] == 0){
          flipAll(posData[1], posData[2]);
        } else {
          flipNumber(posData[1], posData[2], posData[3]);
        }
        values$click <- posData[1:2];
      }
    } else if(lastLine == "## END solve"){ # requested to undo a solve operation
      print(grep("## START solve", values$puzzleBuffer));
      changeBuffer <- 
        head(values$puzzleBuffer, tail(grep("## START solve", values$puzzleBuffer), 1)-1);
      loadBoard(changeBuffer);
    }
  }
  
  clearSelected <- function(){
    if(!any(values$click == c(-1,-1)) && # make sure the selected position is valid 
       !values$locked[values$click[2],values$click[1]]){ # make sure the position is unlocked
      for(ni in which(values$puzzle[values$click[2],values$click[1],])){
        flipNumber(values$click[1], values$click[2], ni);
      }
    }
  }
  
  setHover <- function(posX, posY){
    values$clickTimer <- 0;
    values$hover <- floor(c(posX, posY));
    hoverDelta <- (c(posX, posY) - values$hover) * 3;
    values$hoverCell <- floor(hoverDelta) + 1;
    if(any((values$hover > values$maxPos) | (values$hover < 1))){
      values$hover <- c(-1,-1);
      values$hoverCell <- c(-1,-1);
    } else {
      values$hoverCell[2] <- values$hoverCell[2];
    }
  }
  
  lockBoard <- function(){
    ## TODO: Ideally this should warn (with confirmation required) that the 
    ## undo buffer will be reset
    values$puzzleBuffer <- values$puzzleHeader;
    ## create an image of the board using ASCII art
    lineString <- "";
    values$puzzleBuffer <- c(values$puzzleBuffer, "## Locked positions");
    for(yi in 1:9){
      if((yi-1) %% 3 == 0){
        values$puzzleBuffer <- c(values$puzzleBuffer, 
                                 sprintf("# +%s", paste(rep("---+",3),collapse="")));
      }
      lineString <- "# |";
      for(xi in 1:9){
        val <- which(values$puzzle[yi,xi,]);
        values$locked[yi,xi] <- (length(val) == 1);
        lineString <- 
          sprintf("%s%s%s",lineString,
                  ifelse(length(val) == 1, as.character(val), " "),
                  ifelse(xi %% 3 == 0, "|", ""));
      }
      values$puzzleBuffer <- c(values$puzzleBuffer, lineString);
    }
    values$puzzleBuffer <- c(values$puzzleBuffer, 
                             sprintf("# +%s", paste(rep("---+",3),collapse="")));
    values$puzzleBuffer <- c(values$puzzleBuffer, "## Additional changes");
    for(yi in 1:9){
      for(xi in 1:9){
        val <- which(values$puzzle[yi,xi,]);
        if(length(val) > 1){
          values$puzzleBuffer <- c(values$puzzleBuffer,
                                   sprintf("%d,%d %d", xi, yi, val));
        }
      }
    }
  }
  
  unlockBoard <- function(){
    ## Unlock board, preserving the order of existing unlocked changes
    changeBuffer <- values$puzzleBuffer[-(1:grep("^## Additional changes", values$puzzleBuffer))];
    values$puzzleBuffer <- c(values$puzzleHeader, "## Additional changes");
    for(yi in 1:9){
      for(xi in 1:9){
        if(values$locked[yi, xi]){
          puzzleString <- sprintf("%d,%d %d",
                                  xi, yi, which(values$puzzle[yi,xi,]));
          values$puzzleBuffer <- c(values$puzzleBuffer, puzzleString);
        }
      }
    }
    values$puzzleBuffer <- c(values$puzzleBuffer, changeBuffer);
    values$locked <- array(FALSE, c(9,9));
  }
  
  loadBoard <- function(resultData){
    versionHeader <- resultData[1];
    resultData <- tail(resultData, -1);
    version <- "";
    if(!grep("^## tsurudoku", versionHeader)){
      showModal(modalDialog(
        "Invalid file format, expecting version header line '## tsurudoku ...'"));
      return(FALSE);
    } else {
      version <- sub("^## tsurudoku file format ","",versionHeader);
      if(version != "v1.0"){
        showModal(modalDialog(
          sprintf("Unknown tsurudoku file version '%s'", version)));
      }
    }
    chunkPoss <- c(grep("^##",resultData),length(resultData)+1);
    if(length(chunkPoss) < 1){
      showModal(modalDialog("Invalid file format, expecting metadata lines starting with '##'"));
      return(FALSE);
    }
    ## check passed, can clear the board now
    clearBoard(appendChanges = FALSE);
    chunkPoss <- cbind(head(chunkPoss, -1), tail(chunkPoss, -1)-1);
    chunks <- lapply(1:nrow(chunkPoss), function(x){
      rlines <- tail(resultData[chunkPoss[x,1]:chunkPoss[x,2]], -1);
      rlines;
      });
    names(chunks) <- sub("^## ","",resultData[chunkPoss[,1]]);
    ## Locked positions get special treatment
    if("Locked positions" %in% names(chunks)){
      ## remove graphical elements to leave only grid positions
      lockLines <- sub("^# ","",chunks$`Locked positions`);
      lockLines <- lockLines[grep("[0-9 ]",lockLines)];
      lockLines <- gsub("[^0-9 ]","",lockLines);
      clearBoard();
      for(yi in 1:length(lockLines)){
        lockLine <- as.numeric(unlist(strsplit(lockLines[yi],"")));
        for(xi in 1:length(lockLine)){
          if(!is.na(lockLine[xi])){
            flipNumber(xi, yi, lockLine[xi]);
          }
        }
      }
      lockBoard();
    }
    chunks$`Locked positions` <- NULL;
    ## Process additional chunks as if they were simple placements
    ## This allows for things like labelled checkpoints for backtracking
    for(placeChunk in chunks){
      for(instruction in placeChunk){
        posData <- as.numeric(unlist(strsplit(instruction, "[,; ]")));
        if(length(posData) != 3){
          showModal(modalDialog(sprintf(
            "Invalid position information, expecting 'x,y <num>', got '%s'", instruction)));
          return(FALSE);
        }
        if(posData[3] == 0){
          flipAll(posData[1], posData[2]);
        } else {
          flipNumber(posData[1], posData[2], posData[3]);
        }
      }
    }
    return(TRUE);
  }
  
  resetBoard <- function(){
    changeBuffer <- values$puzzleBuffer[1:grep("^## Additional changes", values$puzzleBuffer)];
    loadBoard(changeBuffer);
  }
  
  singleCandidateEliminationSolver <- function(){
    singleValues <- apply(values$puzzle,c(1,2),
                          function(x){ifelse(length(which(x)) == 1, which(x), 0)});
    singlePoss <- which(singleValues != 0, arr.ind = TRUE);
    for(spi in seq_len(nrow(singlePoss))){
      spx <- singlePoss[spi,2];
      spy <- singlePoss[spi,1];
      pn <- singleValues[spy, spx];
      for(px in (1:9)[-spx]){
        if(all(!values$puzzle[spy,px,])){ # special treatment for blank cells
          flipAll(px, spy);
        }
        if(values$puzzle[spy,px,pn]){
          flipNumber(px, spy, pn);
        }
      }
      for(py in (1:9)[-spy]){
        if(all(!values$puzzle[py,spx,])){ # special treatment for blank cells
          flipAll(spx, py);
        }
        if(values$puzzle[py,spx,pn]){
          flipNumber(spx, py, pn);
        }
      }
    }
  }
  
  lineLineSolver <- function(){
    singleValues <- apply(values$puzzle,c(1,2),
                        function(x){ifelse(length(which(x)) == 1, which(x), 0)});
    boxLookup <- outer(1:9, 1:9,
                       function(X,Y){floor((Y-1)/3)*3+floor((X-1)/3)+1});
    for(val in 1:9){
      poss <- which(singleValues == val, arr.ind = TRUE);
      boxPoss <- boxLookup;
      boxPoss[singleValues > 0] <- 0;
      boxPoss[poss[,1],] <- 0;
      boxPoss[,poss[,2]] <- 0;
      for(filledBox in boxLookup[poss]){
        boxPoss[boxPoss == filledBox] <- 0;
      }
      uniqueBoxes <- which(tabulate(boxPoss, nbins = 9) == 1);
      for(uniqueBox in uniqueBoxes){
        uniquePoss <- which(boxPoss == uniqueBox, arr.ind = TRUE);
        apply(uniquePoss,1,function(p){
          if(all(!values$puzzle[p[1],p[2],])){
            flipNumber(p[2],p[1],val);
          } else {
            for(otherVal in which(values$puzzle[p[1],p[2],])){
              if(otherVal != val){
                flipNumber(p[2],p[1],otherVal);
              }
            }
          }
        });
      }
    }
  }
  
  runSolver <- function(){
    values$changed <- TRUE;
    values$puzzleBuffer <- c(values$puzzleBuffer, "## START solve");
    while(values$changed){
      values$changed <- FALSE;
      if("line/line elimination" %in% input$solveLevels){
        lineLineSolver();
      }
      if(!values$changed && ("single elimination" %in% input$solveLevels)){
        singleCandidateEliminationSolver();
      }
    }
    if(tail(values$puzzleBuffer,1) == "## START solve"){ # solve did nothing
      values$puzzleBuffer <- head(values$puzzleBuffer, -1);
    } else {
      values$puzzleBuffer <- c(values$puzzleBuffer, "## END solve");
    }
  }
  
  makeGridPlot <- function(){
    par(mar=c(0,0,0,0));
    plot(NA, xlim=c(0.5,10.5), ylim=c(10.5,0.5), ann=FALSE, axes=FALSE);
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
               ytop = rep(boxPos,each=3)-0.1+yi, ybottom=rep(boxPos,each=3)+0.1+yi,
               border = NA, col = c(NA,"#0000FF")[values$puzzle[yi,xi,]+1]);
        }
      }
    }
    ## Show single choices
    textValues <- apply(values$puzzle,c(1,2), 
                        function(x){ifelse(length(which(x)) == 1, which(x), "")});
    text(x=rep(1:9+0.5, each=9), y=rep(1:9, 9)+0.5, textValues, 
         cex=c(1.5,1.75)[values$locked+1], 
         col=c("#4040E0","#000000")[values$locked+1]);
    
    ## Show hover cell
    if(all(values$hover >= 0) && all(values$hover == values$click)){
      rect(xleft=values$hover[1] + boxPos[values$hoverCell[1]] - 0.1,
           xright=values$hover[1] + boxPos[values$hoverCell[1]] + 0.1,
           ytop=values$hover[2] + boxPos[values$hoverCell[2]] - 0.1, 
           ybottom=values$hover[2] + boxPos[values$hoverCell[2]] + 0.1,
           lwd = 3, col = "#A0A02090", border=NA);
    }
    
    ## Show selected cell
    if(all(values$click >= 0)){
      rect(xleft=values$click[1], xright=values$click[1]+1,
           ytop=values$click[2], ybottom=values$click[2]+1,
           lwd = 5, border = "#0000FF60");
    }
    
    if(!checkValid(values$puzzle)){
      rect(xleft = 1, xright = 10, ytop=10, ybottom=1,
           col = "#A0600060", border=NA);
    }
  }
  
  ## Number line
  output$numberPlot <- renderPlot({
    par(mar=c(0,0,0,0), lwd=3);
    plot(NA, xlim=c(0.5,10.5), ylim=c(0.5,2.5), ann=FALSE, axes=FALSE);
    hoverNum <- (values$hoverCell[2]-1)*3 + values$hoverCell[1];
    segments(x0=(1:10), y0=1, y1=2);
    segments(x0=1, x1=10, y0=c(1,2));
    text(x=1:9+0.5, y=1.5, labels=1:9, cex=2, 
         col = ifelse((1:9 == hoverNum) & 
                        all(values$hover >= 0) & 
                        all(values$hover == values$click), 
                      rgb(0.4,0.4,0), 
                      "#000000"));
    if(values$numFade > 0.01){
      rect(xleft=values$numClick,
           xright=values$numClick + 1,
           ytop=2, 
           ybottom=1, col = gray(0.1, values$numFade*0.75), border=NA);
    }
  });
  
  ## Main sudoku grid
  output$sudokuPlot <- renderPlot({
    makeGridPlot();
  });
  
  ## create PDF version of Sudoku grid
  output$sudokugrid.pdf <- downloadHandler(
    filename = function(){
      fName <- sprintf("tsurudoku_%s.pdf",
                       format(Sys.Date(),"%Y-%b-%d"));
      return(fName);
    },
    content = function(con){
      pdf(con);
      makeGridPlot();
      invisible(dev.off());
    },
    contentType = "text/pdf"
  );

  ## create text version of Sudoku grid
  output$sudokugrid.txt <- downloadHandler(
    filename = function(){
      fName <- sprintf("tsurudoku_%s.txt",
                       format(Sys.Date(),"%Y-%b-%d"));
      return(fName);
    },
    content = function(con){
      cat(values$puzzleBuffer, sep="\n", file = con);
    },
    contentType = "text/plain"
  );
  
  
  observeEvent(!is.null(input$grid_hover$x) && floor(c(input$grid_hover$x, input$grid_hover$y)), {
    if(is.null(input$grid_hover$x)){
    } else {
      setHover(input$grid_hover$x, input$grid_hover$y);
    }
  });

  ## Clicking on a cell in the grid
  
  observeEvent(c(input$grid_click$x, input$grid_click$y), {
    if(is.null(input$grid_click$x)){
    } else {
      newClick <- as.integer(floor(c(input$grid_click$x, input$grid_click$y)));
      if(all(newClick == values$click)){
        hoverNum <- (values$hoverCell[2]-1)*3 + values$hoverCell[1];
        values$numClick <- as.integer(hoverNum);
        values$numFade <- 1.1;
        flipNumber(values$click[1], values$click[2], values$numClick);
      } else {
        values$click <- newClick;
        if(any((values$click > values$maxPos) | (values$click < 1))){
          values$click <- c(-1,-1);
        } else {
          setHover(input$grid_click$x, input$grid_click$y);
        }
      }
    }
  });
  
  ## Clicking on a number
  
  observeEvent(!is.null(input$num_click$x) && floor(input$num_click$x), {
    if(is.null(input$num_click$x)){
    } else {
      values$numClick <- floor(input$num_click$x);
      if(any((values$numClick > values$maxPos) | (values$numClick < 1))){
        values$numClick <- -1;
        values$numFade <- 0;
      } else {
        flipNumber(values$click[1], values$click[2], values$numClick);
      }
      values$hover <- c(-1,-1);
      values$hoverCell <- c(-1,-1);
    }
  });
  
  ## Button listeners
  
  observeEvent(input$lock, {
    lockBoard();
  });

  observeEvent(input$unlock, {
    unlockBoard();
  });

  observeEvent(input$clear, {
    clearSelected();
  });
  
  observeEvent(input$reset, {
    resetBoard();
  });
  
  observeEvent(input$undo, {
    undoMove();
  });

  observeEvent(input$solve, {
    runSolver();
  });
    
  observeEvent(input$sudoku_input.txt, {
    if(!is.null(input$sudoku_input.txt)){
      resultData <- readLines(input$sudoku_input.txt$datapath);
      if(loadBoard(resultData)){
        showModal(modalDialog("puzzle loaded successfully"));
      }
    }
  });
  
  observeEvent(input$showBuffer, {
    showModal(modalDialog(paste(values$puzzleBuffer,collapse="\n")));
  });
  
  ## Key presses, see http://unixpapa.com/js/key.html
  
  observeEvent(input$pressedKey, {
    if(input$pressedKeyId >= 48 && input$pressedKeyId <= 57){ # numbers
      values$numClick <- (input$pressedKeyId - 48);
      ## values$numFade <- 1.1; # not needed, because number was explicitly pressed
      if(values$numClick == 0){
        flipAll(values$click[1], values$click[2]);
      } else {
        flipNumber(values$click[1], values$click[2], values$numClick);
      }
    }
    if(input$pressedKeyId >= 37 && input$pressedKeyId <= 40){ # arrow keys
      arrowCode <- input$pressedKeyId - 37;
      xInc <- ((arrowCode+1) %% 2) * (arrowCode - 1);
      yInc <- ((arrowCode) %% 2) * (arrowCode - 2);
      if(!any(values$click == c(-1,-1))){
        values$click <- (((values$click - 1) + c(xInc, yInc) + 9) %% 9) + 1;
      }
    }
    if(input$pressedKeyId == 8){ ## backspace
      lastLine <- tail(values$puzzleBuffer, 1);
      if(!grepl("^##",lastLine)){
        posData <- as.numeric(unlist(strsplit(lastLine, "[,; ]")));
        if(all(posData[1:2] == values$click)){
          undoMove();
        } else {
          values$click <- posData[1:2];
        }
      }
    }
    if(input$pressedKeyId == 46){ ## delete
      clearSelected();
    }
  });

  observeEvent(input$releasedKey, {
    if(input$releasedKeyId >= 49 && input$releasedKeyId <= 57){ # numbers
      values$numFade <- 0;
    }
  });
  # ## Fade timers (these might cause memory leaks)
  # 
  observe({
    m <- isolate(values$numFade);
    if(m>0.01){
      values$numFade <- max(m - 0.25, 0);
      invalidateLater(100, session);
    }
    values$numFade;
  });

})

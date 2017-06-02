library(ggplot2)
library(gridExtra)
library(ggplot2)
library(MASS)
library(shiny)
library(MASS)
library(RColorBrewer)
library(grid)
library(plotly)
library(shinyBS)

source("getdata.R")
source("processData.R")
source("plist.R")
source("doPlot.R")

function(input, output, session) {
  
  #get data from getdata function  
  data.df <- getdata()
  
  #reactive variables v: n.col = numer of classes, counter, selected
  v <<- reactiveValues(counter = 1, n.col = nrow(unique(data.df["label"])), selected = list(), plotno = 0)
  
  #reactive values for df: df, labels, sel
  df <- reactiveValues(Df = getdata(), labels = getdata(), sel= list())
  
  #for loading screen
  loading <- reactiveValues(flag = 0)
  
  # initially get data and plot lists
  p <- getplotlist( 1, data.df , NULL , NULL)
  diag <- getdiaglist(1, data.df)
  auc <- getauclist(data.df)
  
  # Assign output names for created plots
  for (i in 1:length(p)) {
    local({
      n <- i # Make local variable
      plotname <- paste("plot", n , sep="")
      output[[plotname]] <<- renderPlotly({
        p[[n]]
      })
    })
  }
  
  for (i in 1:length(diag)) {
    local({
      n <- i # Make local variable
      plotname <- paste("diag", n , sep="")
      print(plotname)
      output[[plotname]] <<- renderPlotly({
        diag[[n]]
      })
    })
  }
  
  for (i in 1:length(auc)) {
    local({
      n <- i # Make local variable
      plotname <- paste("auc", n , sep="")
      print(plotname)
      output[[plotname]] <<- renderPlotly({
        auc[[n]]
        
      })
    })
  }
  
  
  
  # merge function
  observeEvent(input$merge,
               { loading$flag <- 0
               print(v$counter)
               print(df$Df)
               print(df$labels)
               old_col <- ifelse( v$counter == 1, "label", paste("label", v$counter, sep=""))
               new_col <- paste("label", v$counter + 1  , sep="")
               df$labels[new_col] <<-  ifelse((df$labels[[old_col]] == max(input$variable)) , 
                                              min(input$variable), df$labels[[old_col]])
               v$counter <<- v$counter + 1
               updateSelectizeInput(session, "variable",
                                    choices = unique(df$labels[,paste("label", v$counter , sep="")]))
               print(df$labels)
               
               df$Df['label'] <- df$labels[, ncol(df$labels)]
               
               print(df$Df)
               
               #update using df$Df
               p <- getplotlist( v$counter, df$Df , NULL , NULL)
               diag <- getdiaglist(v$counter, df$Df)
               auc <- getauclist(df$Df)
               #print(p)
               print(diag)
               
               for (i in 1:length(p)) {
                 local({
                   n <- i # Make local variable
                   plotname <- paste("plot", n , sep="")
                   print(plotname)
                   output[[plotname]] <<- renderPlotly({
                     p[[n]]
                     
                   })
                 })
               }
               
               for (i in 1:length(diag)) {
                 local({
                   n <- i # Make local variable
                   plotname <- paste("diag", n , sep="")
                   print(plotname)
                   output[[plotname]] <<- renderPlotly({
                     diag[[n]]
                   })
                 })
               }
               
               
               for (i in 1:length(auc)) {
                 local({
                   n <- i # Make local variable
                   plotname <- paste("auc", n , sep="")
                   print(plotname)
                   output[[plotname]] <<- renderPlotly({
                     auc[[n]]
                     
                   })
                 })
               }
               
               loading$flag <- 1
               
               uniq_labels <- unique(df$labels[,ncol(df$labels)])
               v$n.col <- length(uniq_labels)
               
               }
  )
  
  output$selected <- renderPrint({
    d <- event_data("plotly_selected")
    print(d)
    
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else d[, c("x","y")]
    
  })
  
  
  #get selected points
  #identify selected points and assign new class
  #reload graphs
  
  
  observeEvent(event_data("plotly_selected"), { 
    
    # display selected rows
    d <- event_data("plotly_selected")
    print(d)
    df$sel <- d[,c("x","y")]
    df$sel[,c("x")] <- round(df$sel[,c("x")],digits=6)
    df$sel[,c("y")] <- round(df$sel[,c("y")],digits=6)
    #df$Df %>% mutate(x = round(x, 5) )
    #xlist <- lapply(df$Df[,"x"], round, digits = 6)
    df$Df[,c("x")] <- round(df$Df[,c("x")],digits=6)
    df$Df[,c("y")] <- round(df$Df[,c("y")],digits=6)
    print("Selected points")
    print(df$sel)
    print("Data points")
    print(df$Df)
    presenceRoster <- data.frame(x=(df$Df$x %in% df$sel$x),y=(df$Df$y %in% df$sel$y))
    to <- df$Df[(presenceRoster$x & presenceRoster$y),]
    print("intersection")
    print(to)
    
    lvl <- levels(df$Df$label)
    levels(df$Df$label) <- c(lvl,as.character(as.integer(lvl[length(lvl)])+1)) 
    
    df$Df[(presenceRoster$x & presenceRoster$y),]$label <- levels(df$Df$label)[length(levels(df$Df$label))]
    print("Altered Data points")
    print(df$Df)
    
    #find class of selected points - if same , else throw error
    
   
    
    # plot with selected lines as brushedpoints
    
    
    # not giving same points as selected
    #p <- getplotlist( v$counter, df$Df , df$Df[df$sel, ] , NULL) 
    
  })
  
  #split function
  observeEvent(input$split,{ 
                   if(!is.null(event_data("plotly_selected")))
                   {
                     #create new class
                     lvl <- levels(df$Df$label)
                     levels(df$Df$label) <- c(lvl,as.character(as.integer(lvl[length(lvl)])+1)) 
                     
                     
                   }
                   else
                     showNotification("Please select datapoints to split.")
                 
                 
               }
               
  )
  
  
  # undo function
  observeEvent(input$undo,
               { 
                 loading$flag <- 0
                 last_col <- paste("label", v$counter, sep="")
                 # ADDING DYNAMIC COLUMNS
                 df$labels[last_col] <<-  NULL
                 v$counter <<- v$counter - 1
                 updateSelectizeInput(session, "variable",
                                      choices = unique(df$Df["label"]))
                 print(df$Df)
                 
                 df$Df['label'] <- data.df[, ncol(df$labels)]
                 p <- getplotlist( v$counter, df$Df , NULL , NULL)
                 #print(p)
                 
                 for (i in 1:length(p)) {
                   local({
                     n <- i # Make local variable
                     plotname <- paste("plot", n , sep="")
                     #print(plotname)
                     output[[plotname]] <<- renderPlot({
                       p[[n]]
                       
                     })
                   })
                 }
                 
                 loading$flag <- 1
                 uniq_labels <- unique(df$Df[,ncol(df$labels)])
                 v$n.col <- length(uniq_labels)
                 
               }
  )
  
  
  
  #plot main graph 
  output$bigplot <- renderPlotly({
    loading$flag <- 0
    coloursvec <- brewer.pal(8, "Dark2")
    g <- ggplot(df$Df, aes(x= x, y= y)) + geom_point(aes(color = label)) + theme_bw()  + theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())
    print("render again")
    p <- ggplotly(g, height= 500, width = 500) %>% config(displayModeBar = FALSE, scrollZoom = TRUE, doubleClick= 'reset') %>% layout(dragmode ="lasso")
    
    loading$flag <- 1
    return(p)
    
  }) 
  
  
  
  
  #Printing in grid
  output$plots <- renderUI({
    loading$flag <- 0
    col.width <- round(12/v$n.col) # Calculate bootstrap column width
    n.row <- v$n.col # calculate number of rows
    cnter <- 0 # Counter variable ------ counter for what?
    
    
    #assigning order of printing plots in triangle
    n <- v$n.col  #number of classes/catgeories
    noLowerTriangle <- (n^2-n)/2
    k <-  1:noLowerTriangle
    orderMatrix <- matrix(nrow = n, ncol = n)
    orderMatrix[lower.tri(orderMatrix, diag=FALSE)] <- k
    order <- c(t(orderMatrix))
    order <- order[!is.na(order)]
    
    
    
    #'global' variables as counters
    g <- 1 # for lda plot
    z <- 10 # for roc-auc plot
    
    
    # Create fluidRows with columns
    rows  <- lapply(1:n.row,function(row_num){
      
      #LDA plots
      if(row_num==1)
      {
        lda_cols <- list()
      }
      else
      {
        lda_cols <- lapply(1:(row_num-1), function(col_num){
          plotname <- paste("plot", order[g], sep="")
          g <<- g+1
          column(col.width, plotlyOutput(plotname, height = 150, width = 150))
        })
      }
      
      #diagonal
      diagname <- paste("diag", row_num, sep="")
      diag <- column(col.width, plotlyOutput(diagname, height = "10%", width = "10%"))
      
      #AUC plot
      auc_cols <- lapply(1:(n-row_num), function(col_num) {
        plotname <- paste("auc", order[z], sep="")
        z <<- z-1
        column(col.width, plotlyOutput(plotname, height = 150, width = 150))
      })
      
      # attach lda_cols, diag and cols2
      lda_cols[[length(lda_cols)+1]] <- diag
      cols <- append(lda_cols,auc_cols)
      fluidRow( do.call(tagList, cols ) )
    })
    
    uniq_labels <- unique(df$Df[,ncol(df$Df)])
    comblabels <- combn(uniq_labels,2)
    
    
    # USE TO RETURN LOCATION ON PLOT
    # USED IN SLEEP ANALYSIS DATA TO DISPLAY CORRESPONING HISTOGRAM
    # does not work inside for loop
    onevent("mouseenter",paste("plot", 1, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    onevent("mouseenter",paste("plot", 2, sep=""), v$selected <- c(comblabels[1,2], comblabels[2,2]))
    onevent("mouseenter",paste("plot", 3, sep=""), v$selected <- c(comblabels[1,3], comblabels[2,3]))
    onevent("mouseenter",paste("plot", 4, sep=""), v$selected <- c(comblabels[1,4], comblabels[2,4]))
    onevent("mouseenter",paste("plot", 5, sep=""), v$selected <- c(comblabels[1,5], comblabels[2,5]))
    onevent("mouseenter",paste("plot", 6, sep=""), v$selected <- c(comblabels[1,6], comblabels[2,6]))
    onevent("mouseenter",paste("plot", 7, sep=""), v$selected <- c(comblabels[1,7], comblabels[2,7]))
    onevent("mouseenter",paste("plot", 8, sep=""), v$selected <- c(comblabels[1,8], comblabels[2,8]))
    onevent("mouseenter",paste("plot", 9, sep=""), v$selected <- c(comblabels[1,9], comblabels[2,9]))
    onevent("mouseenter",paste("plot", 10, sep=""), v$selected <- c(comblabels[1,10], comblabels[2,10]))
    # onevent("mouseenter",paste("plot", 11, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 12, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 13, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 14, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 15, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 16, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    # onevent("mouseenter",paste("plot", 17, sep=""), v$selected <- c(comblabels[1,1], comblabels[2,1]))
    
    
    
    # change screen from loading
    loading$flag <- 1
    
    # Calling the rows that contain cols
    do.call(tagList, rows)
  })
  
  
  # To display loading page
  observe({
    if(loading$flag == 1){
      hide("loading_page")
      show("main_content")
    }
    else
    {
      hide("main_content")
      show("loading_page")
    }
  })
  
  # on clicking split
  observeEvent(input$split, {
    show("col", anim = T, time = 2)
    show("okay", anim = T, time = 1)
  })
  
  # get plot number (plot4) from hovered plot
  observeEvent(v$selected, {
    print(v$selected)
    uniq_labels <- unique(df$Df[,ncol(df$Df)])
    comb_labels <- combn(uniq_labels,2)
    v$plotno <- intersect(which(comb_labels[1,] == v$selected[1]) , which(comb_labels[2,] == v$selected[2]))
    print(v$plotno)
  })
  
  # To display bigger plot of hovered plot
  # output$plot <-  renderUI({
  #   cnter <- v$plotno
  #   plotname <- paste("plot", cnter, sep="")
  #   print("gotta display")
  #   plotlyOutput(plotname, height = 500, width = 500)})
  
  
  
  
}




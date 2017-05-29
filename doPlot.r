library(dplyr)
library(lazyeval)
library(plotly)


data.df <- getdata()

# big plot for selection
bigplot = function(data.df){
  coloursvec <- brewer.pal(8, "Dark2")
  g <- ggplot(data.df, aes(x= x, y= y)) + theme_bw() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())
  p <- ggplotly(g, width = 150, height =150) %>% config(displayModeBar = FALSE, scrollZoom = TRUE, doubleClick= 'reset') %>% layout(dragmode ="lasso")
  
  return(p)
}
  

# doplot function to generate scatterplots
doPlot = function(counter, data.df, label1, label2, brushedlines, clickedline, coloursvec){

  
  selected_classes <- c(label1 , label2)
  
  
  print(selected_classes)
  print(selected_classes[1])
  print(coloursvec[selected_classes[1]])
  
  formatCol <- names(data.df)[ncol(data.df)]
  formula <- interp(~ifelse(label %in% selected_classes , ifelse( label == selected_classes[1] , "b", "c"), "a"), label = as.name(formatCol))
  plot.df <- data.df %>% mutate_(formatCol = formula)
 
  print(plot.df %>% arrange(formatCol))
  
  
  
  if(!(label1 == label2)){
  g <- ggplot(plot.df  %>%
                arrange(formatCol) , aes(x= x, y= y)) + theme_bw() +
    geom_point(aes(color = formatCol)) + 
    scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank())

  
  }
  else
  {
  g <- ggplot(plot.df  %>%
                arrange(formatCol) , aes(x= x, y= y)) + theme_light() +
    geom_point(aes(color = formatCol)) + 
    scale_colour_manual(values= c("b" = coloursvec[as.integer(label1)], "c" = coloursvec[as.integer(label2)], "a" = "#E4E4E4")) +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position='none', axis.ticks = element_blank(), axis.text = element_blank(),
          panel.border = element_rect(colour = "black", size=3)) 
  
}
  # # # if brush is used highlight brushed points
    if (!is.null(brushedlines)){
      brushedlines <- brushedlines %>% mutate_(formatCol = formula)
      print(brushedlines)
      g <- g  + 
        geom_point(aes(x= x, y= y),
                   data= brushedlines,
                   color = "black")
      
    }
      
     # # if hover when brushedlines = 0 highlight hovered points
     # if (!is.null(clickedline)){
     #   g <- g  + 
     #     geom_point(aes(x= x, y= y, alpha = 1),
     #                data= clickedline,
     #                colour="blue",
     #                size=3)  
     # }
  
  p <- ggplotly(g, width = 150, height =150) %>% config(displayModeBar = FALSE) ##, scrollZoom = TRUE, doubleClick= 'reset') %>% layout(dragmode ="lasso") 
  
                                  #   to ADD Toolbar                     
                                  #   ,autosizable = T, fillFrame = T, showLink = FALSE, displaylogo = FALSE, scrollZoom = TRUE, doubleClick= 'reset',
                                  #   modeBarButtonsToRemove = list(
                                  #   'sendDataToCloud',
                                  #   'toImage',
                                  #   'autoScale2d',
                                  #   'resetScale2d',
                                  #   'hoverClosestCartesian',
                                  #   'hoverCompareCartesian'
                                  # )) 
  
  return(p)
}







 
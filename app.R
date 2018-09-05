
###--- SHINY APPLICATION ---###

#packages needed
library(shiny)
library(plotly)
library(maptools)
library(dplyr)
library(ggplot2)
library(maps)
library(beeswarm)
library(ggbeeswarm)
library(tidyverse)
library(geofacet)
library(fiftystater)
library(stringr)
library(gridExtra)
library(grid)
library(shinythemes)
library(rsconnect)
library(readr)
library(readxl)


top_10_drgs = read_csv("top_10_drgs.csv")
top_10_drgs_list = top_10_drgs$DRG

ui = fluidPage(theme = "flatly",
  titlePanel(title=h4("Top 10 DRGs for Surgery in USA", align = "center")),
  sidebarLayout(position = "left",
    sidebarPanel(
      "DRG",selectInput("DRG Definition", "Diagnosis-Related Group:", as.character(top_10_drgs_list))),
  mainPanel(
    plotlyOutput(outputId = "plot")
  )))

server = function(input, output){
  
  set.seed(1234)
  
  data = reactive({
    new_df = read_csv("new_df.csv")
    
    df = new_df[new_df$`DRG Definition` == input$`DRG Definition`,]
   # beeswarm data frame
    most_pop_states = c("CA", "TX", "IL", "FL", "NY")
    beeswarm = df[(df$State %in% most_pop_states),]
    beeswarm$hover = paste(beeswarm$State, "<br>","Hospital: ", beeswarm$`Hospital Name`,"<br>","Average Cost Of Treatment: $", beeswarm$`Average Total Payments`, sep="")
    beeswarm
  })
  
  output$plot = renderPlotly({
    p = ggplot(mapping = aes(`Average Total Payments`,State, text = hover), data = data()) +
      geom_quasirandom(aes(color = `Hospital Ownership`), groupOnX = FALSE) +
      theme(text = element_text(size=10),
            axis.text.x = element_text(vjust=1)) + theme(legend.position = c(1,1))
    ggplotly(p, tooltip = c("text"))
  })



}

shinyApp(ui, server)



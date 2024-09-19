library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

mod_scatter_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    plotlyOutput(outputId = ns("scatter")),
    actionButton(ns("browser"), "browser")
  )
} 

mod_scatter_server <- function(id, tbl_list, show_lines, point_colour, highlight, remove_legend) {

  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      observeEvent(input$browser, browser())
      
      output$scatter <- renderPlotly(gg_plot())
      
     # highlight_colours <- c(N = "#00000000", Y = "red")
      highlight_colours <- c(N = "transparent", Y = "red")

      # extract the dataset from the nested list of tibbles
      # get i from id
      i <- as.numeric(gsub(id, pattern = "tissue", replacement = ""))
      dataset <- reactive(tbl_list[i,2][[1]][[1]])
      
      gg_plot <- reactive({

          p <- ggplotly(complete_plot())
          
          if(remove_legend()) {
            return(hide_legend(p))
          } else return (p)

      })
      
      
      
      base_plot <- reactive({
        
        dataset() |>
          filter(Gene == top_ptp()) |>
          ggplot(aes(x = age, y = oxi_percent, key = residue)) +
          geom_point_display() +
          ggtitle(top_ptp())
        
      })
      
      complete_plot <- reactive({
        
        completed_plot <- base_plot()
        
        if (show_lines()){
          completed_plot <- completed_plot +
            geom_line(
              aes(group = residue), 
              linewidth = 0.5, 
              linetype  = "dashed", 
              colour    = "grey"
            ) +
            geom_point_display()
        }
        
        if ( highlight() != "None" ) {
          completed_plot <- completed_plot +
            geom_point(
              aes(colour = .data[[highlight()]]), 
              size   = 4, 
              shape  = 21, 
              stroke = 1.5
            ) +
            scale_colour_manual(values = highlight_colours)
        }
        
        return( completed_plot )
        
      })
      
      
      geom_point_display <- reactive({
        
        if (point_colour() == "residue") {
          return (
            geom_point(
              aes(fill = residue, colour = residue), 
              shape  = 21, 
              size   = 4, 
              stroke = 0.5
            )
          )
        } else {
          return (
            geom_point(
              fill   = "black", 
              colour = "black", 
              shape  = 21, 
              size   = 4 , 
              stroke = 0.5
            )
          )
        }
      })
      
      top_ptp <- reactive({
       dataset() |>
          count(Gene) |>
          arrange(desc(n)) |>
          slice(1) |>
          pull(Gene)
      })

  })
}      



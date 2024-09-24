library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

update_geom_defaults("point", 
   aes(
      fill   = "seagreen", 
      colour = "black", 
      shape  = 21, 
      size   = 4 , 
      stroke = 0.5
      )
)


mod_scatter_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    plotlyOutput(outputId = ns("scatter")),
   # actionButton(ns("browser"), "mod scatter browser")
  )
} 

mod_scatter_server <- function(id, tbl_list, show_lines, colour_by_residue, highlight, remove_legend, select_gene) {

  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      observeEvent(input$browser, browser())
      
      output$scatter <- renderPlotly(gg_plot())
      
      highlight_colours <- c(N = "transparent", Y = "red")

      # extract the dataset from the nested list of tibbles
      # get i from id
      i <- as.numeric(gsub(id, pattern = "tissue", replacement = ""))
      
      dataset <- reactive({
        
        j <- if_else(isTruthy(i), i, 1)
        return (tbl_list[j,2][[1]][[1]])
        
      })
      
      
      gg_plot <- reactive({

          p <- ggplotly(complete_plot())
          
          if(remove_legend()) {
            return(hide_legend(p))
          } else return (p)

      })
      
      
      gene_to_plot <- reactive({
        
        if (select_gene() == "top_ptp") {
          return (top_ptp())
        } else {
          return (select_gene())
        }
        
      })
      
      
      selected_data <- reactive({
        
        filter(dataset(), Gene == gene_to_plot())
        
      })
      
      base_plot <- reactive({
        
        selected_data() |>
          ggplot(aes(x = age, y = oxi_percent, key = residue)) +
          ggtitle(gene_to_plot())
        
      })
      
      
      # assembled plot
      complete_plot <- reactive({
        
        completed_plot <- base_plot() +
          geom_point_display()
        
        if (nrow(selected_data()) == 0) {
          completed_plot <- base_plot() + 
            annotate(geom="text", x = 1, y = 1, label = "no data")
        } else {
        
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
              geom_point_display() +
              geom_point(
                aes(colour = .data[[highlight()]]), 
                fill = "transparent",
                stroke = 1.5
              ) +
              scale_colour_manual(values = highlight_colours)
          }
        }
        return( completed_plot )
        
      })
      
      # get the point colours
      geom_point_display <- reactive({
        
        if (colour_by_residue()) {
          return (
            geom_point(aes(fill = residue, colour = residue))
          )
        } else return (geom_point())
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



library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

mod_card_ui <- function(id, card_name, all_ptps){
  
  ns <- NS(id)
  tagList(
    #layout_columns(
      #col_widths = 6,
      card(
        full_screen = TRUE,
        card_header(card_name),
        change_selected_gene_popover(id, all_choices = all_ptps),
        plot_options_popover_ns(id),
        mod_scatter_ui(ns("sub_mod")),
        actionButton(ns("browser"), "mod card browser")
      )#,
      # card(
      #   full_screen = TRUE,
      #   card_header(card_name),
      #   #change_selected_gene_popover(id, all_choices = all_ptps),
      #  # plot_options_popover_ns(id),
      #   mod_scatter2_ui(ns("sub_mod")),
      #   actionButton(ns("browser2"), "mod card browser")
      # )
   # )
  )
}

#small_ds

mod_card_server <- function(id, tbl, small_ds=NULL) {
  
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      observeEvent(input$browser, browser())
      
      display_lines <- reactive(input$show_lines)
      
      gene_to_plot <- reactive({
        
        if (input$show_top_ptp) return ("top_ptp")
        
        req(input$select_gene)
        return(input$select_gene)
      }) 
      
      mod_scatter_server(
        id                 = "sub_mod", 
        tbl_list           = tbl, 
        show_lines         = reactive(input$show_lines), 
        colour_by_residue  = reactive(input$colour_by_residue), 
        highlight          = reactive(input$highlight_cys), 
        remove_legend      = reactive(input$remove_legend),
        select_gene        = gene_to_plot
      )
      
      # mod_scatter2_server(
      #   id                 = "sub_mod", 
      #   tbl_list           = tbl, 
      #   small_ds           = small_ds,
      #   show_lines         = reactive(input$show_lines), 
      #   colour_by_residue  = reactive(input$colour_by_residue), 
      #   highlight          = reactive(input$highlight_cys), 
      #   remove_legend      = reactive(input$remove_legend),
      #   select_gene        = gene_to_plot
      # )
      
    }
  )
}
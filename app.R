library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)
library(dplyr)
library(nplyr)

# this is a list of tibbles so we don't need to filter for cell type within the app
nested_tissues  <- readRDS("data/nested_tissues.rds")

all_ptps <- nested_tissues[[2]] |>
  purrr::map(purrr::pluck(2)) |>
  unlist() |>
  unique()

# ========================================
#  Define card variables here
all_card_names <- nested_tissues[[1]]
n_cards <- length(all_card_names)
ID_variable <- "tissue"

create_card <- function(i, data = nested_tissues, id_name = ID_variable){
  card(
    full_screen = TRUE,
    card_header(nested_tissues[[1]][[i]]),
    mod_scatter_ui(paste0(ID_variable,i))
  )
}


ui <- page_fillable(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  title = "Data explorer",  
  tabsetPanel(
    id = "main_panel",
   # there are some examples of things to include here in the prototype data explorer project
    tabPanelBody(
      "data2",
      card(
        full_screen = TRUE,
        card_header(
          "PTP oxidation",
          plot_options_popover_ns(),
          change_selected_gene_popover(all_choices = all_ptps),
          # popover(
          #   actionButton(inputId = "change_gene", "Change gene"),
          #   title = "Select gene",
          #   checkboxInput(
          #     inputId = "show_top_ptp",
          #     label   = "show top ptp for each tissue",
          #     value = TRUE
          #   ),
          #   conditionalPanel(
          #     condition = "input.show_top_ptp == 0",
          #     selectInput(inputId = "select_gene", label = "select gene", choices = all_ptps)
          #   )
          # ),
          #actionButton(inputId = "back_to_main2", "Back to main"),
          class = "d-flex align-items-center gap-1"
        ),
        mod_card_ui("A card", "different name", all_ptps),
        layout_columns(
          col_widths = 3,
          !!!purrr::map(.x=1:n_cards, create_card)
        ),
        actionButton("browser", "browser")
      )
    )
  ) 
)

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())

  gene_to_plot <- reactive({
    
    if (input$show_top_ptp) return ("top_ptp")
    
    req(input$select_gene)
    return(input$select_gene)
  }) 
  
  mod_card_server("A card", nested_tissues)
  
  for (i in 1:n_cards){
    mod_scatter_server(
      id                 = paste0(ID_variable,i), 
      tbl_list           = nested_tissues, 
      show_lines         = reactive(input$show_lines), 
      colour_by_residue  = reactive(input$colour_by_residue), 
      highlight          = reactive(input$highlight_cys), 
      remove_legend      = reactive(input$remove_legend),
      select_gene        = gene_to_plot
    )
  }

}

shinyApp(ui, server)
  
  

  # This works - hardcoding the dataset number, but below that, trying to use i, does not. 
#   mod_scatters[[paste0(ID_variable,1)]] <- mod_scatter_server(id = paste0(ID_variable,1), nested_tissues[1,2][[1]][[1]], show_lines = display_lines)
#   mod_scatters[[paste0(ID_variable,2)]] <- mod_scatter_server(id = paste0(ID_variable,2), nested_tissues[2,2][[1]][[1]], show_lines = display_lines)
# #  mod_scatters[[paste0(ID_variable,3)]] <- mod_scatter_server(id = paste0(ID_variable,3), nested_tissues[3,2][[1]][[1]], show_lines = display_lines)
  
  #================================================================================
  # for reasons I do not understand, i works in the paste functions below, but it gets updated
  #to the last value i.e. 4 here in the server module, and the 4th element from the list is used as the dataset
  #======================================================================================
#   i <- 3
#   #  mod_scatters[[paste0(ID_variable,i)]] <- mod_scatter_server(id = paste0(ID_variable,i), nested_tissues[i,2][[1]][[1]], show_lines = display_lines)
#   i <- 4
#   mod_scatters[[paste0(ID_variable,i)]] <- mod_scatter_server(id = paste0(ID_variable,i), nested_tissues[i,2][[1]][[1]], show_lines = display_lines)




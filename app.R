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
   # type = "hidden",
    # tabPanelBody(
    #   "landing_page",
    #   papercard,
    #   br(),
    #   h4("Data types available to explore from this paper", style = "text-align: center;"),
    #   layout_columns(
    #     fill = FALSE,
    #     value_boxes[["PTP oxidation"]],
    #     #value_boxes[["proteomics"]],
    #     #value_boxes[["imaging"]]
    #   )
    # ),
    tabPanelBody(
      "data2",
      #h2("Some pretty pictures here", ),
      #actionButton(inputId = "back_to_main2", "Back to main"),
      card(
        full_screen = TRUE,
        card_header(
          "PTP oxidation",
          popover(
            bsicons::bs_icon("gear", class = "ms-auto"),
            title = "some options",
            p("hello")
          ),
          popover(
            actionButton("plot_options", label = "plot options"),
            title = "Highlight options",
            checkboxInput(
              inputId = "show_lines",
              label   = "show lines to highlight same residues"
            ),
            checkboxInput(
              inputId = "colour_by_residue",
              label   = "colour by residue"
            ),
            checkboxInput(
              inputId = "remove_legend",
              label   = "remove legends"
            ),
            radioButtons(
              inputId = "highlight_cys",
              label   = "highlight",
              choices = list(
                None = "None",
                `Catalytic cysteines` = "Catalytic_Cys",
                `Backdoor cysteines` = "Backdoor_Cys"
              )
            ),
            # selectizeInput(
            #   inputId   = "prot_to_highlight", 
            #   label     = 'Select protein(s) to highlight', 
            #   choices   = all_proteins, 
            #   multiple  = TRUE
            # )
          ),
          popover(
           # p("Change gene"),
            actionButton(inputId = "change_gene", "Change gene"),
            title = "Select gene",
            checkboxInput(
              inputId = "show_top_ptp",
              label   = "show top ptp for each tissue",
              value = TRUE
            ),
            conditionalPanel(
              condition = "input.show_top_ptp == 0",
              selectInput(inputId = "select_gene", label = "select gene", choices = all_ptps)
            )
          ),
          actionButton(inputId = "back_to_main2", "Back to main"),
          class = "d-flex align-items-center gap-1"
        ),
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
  
  display_lines <- reactive(input$show_lines)
  
  point_colour_sel <- reactive({
    if (input$colour_by_residue == TRUE) {
      return ("residue")
    } else {
      return ("black")
    }
  })

  highlight_points <- reactive({
    return (input$highlight_cys)
  })
  
  remove_legend <- reactive({
    return (input$remove_legend)
  })
  
  gene_to_plot <- reactive({
    
    if (input$show_top_ptp) return ("top_ptp")
    
    req(input$select_gene)
    return(input$select_gene)
  }) 
  
  
  for (i in 1:n_cards){
    mod_scatter_server(
      id            = paste0(ID_variable,i), 
      tbl_list      = nested_tissues, 
      show_lines    = display_lines, 
      point_colour  = point_colour_sel, 
      highlight     = highlight_points, 
      remove_legend = remove_legend,
      select_gene   = gene_to_plot
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




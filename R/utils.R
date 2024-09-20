library(bslib)
library(ggplot2)

# Popovers that could be used in the main app.R code, or within modules.
# If used within modules, the IDs need to be namespaced.
namespace_id <- function(ns = NULL, input_id){
  
  if_else(
    isTruthy(ns), 
    shiny::NS(ns, id = input_id),
    input_id
  )
}

# to use in UI
plot_options_popover_ns <- function(ns_id = NULL) {
  
  popover(
    actionButton("plot_options", label = "plot options"),
    title = "Highlight options",
    checkboxInput(
      inputId = namespace_id(ns_id, "show_lines"),
      label   = "show lines to highlight same residues"
    ),
    checkboxInput(
      inputId = namespace_id(ns_id, "colour_by_residue"),
      label   = "colour by residue"
    ),
    checkboxInput(
      inputId = namespace_id(ns_id, "remove_legend"),
      label   = "remove legends"
    ),
    radioButtons(
      inputId = namespace_id(ns_id, "highlight_cys"),
      label   = "highlight",
      choices = list(
        None = "None",
        `Catalytic cysteines` = "Catalytic_Cys",
        `Backdoor cysteines`  = "Backdoor_Cys"
      )
    )
  )
}

# gear logo
gear_popover <- function(){
  popover(
    bsicons::bs_icon("gear", class = "ms-auto"),
    title = "gear logo",
    p("some stuff here")
  )
}


change_selected_gene_popover <- function(ns_id = NULL, all_choices) {

  popover(
    actionButton(inputId = namespace_id(ns_id, "change_gene"), "Change gene"),
    title = "Select gene",
    checkboxInput(
      inputId = namespace_id(ns_id, "show_top_ptp"),
      label   = "show top ptp for each tissue",
      value = TRUE
    ),
    conditionalPanel(
      condition = "input.show_top_ptp == 0",
      selectInput(inputId = namespace_id(ns_id, "select_gene"), label = "select gene", choices = all_choices),
      if (isTruthy(ns_id)) ns=ns_id
    )
  )

}


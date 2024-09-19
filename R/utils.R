library(bslib)
library(ggplot2)

papercard <- card(
  #max_height = 350,
  full_screen = TRUE,
  card_header("Some PTP oxidation data"),
  p("Authors"),
  a("DOI: 10.1038/s41467-023-40621-2", target = "_blank", href = "https://doi.org/10.1038/s41467-023-40621-2"), 
  h3("Abstract"),
  p(".....")
)

value_boxes <- list(
  single_cell = value_box(
    title = NULL,
    value = actionButton("single_cell", "Single cell"),
    showcase = tags$img(src = "single_cell_umap.PNG", width = "100", height = "100")
  ),
  proteomics = value_box(
    title = NULL,
    value = actionButton("proteomics", "Proteomics"),
    showcase = tags$img(src = "proteomics_pic.PNG", width = "100", height = "100")
    #tags$img(src = "bioinformatics_logo_small.png", width = "200", height = "71")
  ),
  imaging = value_box(
    title = NULL,
    value = "Imaging",
    showcase = tags$img(src = "imaging_pic.PNG", width = "100", height = "100")
  )
)
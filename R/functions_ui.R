interactive_panel <- function(i, ns, columns){
  
  tags$div(id = ns(glue("interactive_panel_{i}")),
           h4(glue("Interactief element {i}")),
           selectInput(ns(glue("ia_select_input{i}")), 
                       "Soort input",
                       width = 300,
                       choices = list("None" = "",
                                      "Select category" = "selectInput",
                                      "Numeric slider" = "sliderInput",
                                      "Date range" = "dateRangeInput")),
           shinyjs::hidden(
             tags$div(id = ns(glue("ia_select_variable_box{i}")),
                      selectInput(ns(glue("ia_select_variable{i}")), 
                                  "Heeft effect op variabele",
                                  width = 300,
                                  choices = ""),
                      textInput(ns(glue("ia_element_label{i}")), "Label")
             )
           )
  )
  
}

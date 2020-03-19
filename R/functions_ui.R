interactive_panel <- function(i, ns, columns, args = NULL){
  
  element <- args[[paste0("element",i)]]
  variable <- args[[paste0("variable",i)]]
  label <- args[[paste0("label",i)]]
  
  tags$div(id = ns(glue("interactive_panel_{i}")),
           
           h4(glue("Interactief element {i}")),
           
           tags$div(style = "display: inline-block;",
             selectInput(ns(glue("ia_select_input{i}")), 
                         "Soort input",
                         width = 250,
                         choices = list("Geen" = "",
                                        "Selecteer categorieen" = "selectInput",
                                        "Slider voor numerieke waarden" = "sliderInput",
                                        "Datum reeks" = "dateRangeInput"),
                         selected = element
             )
           ),
           shinyjs::hidden(
               tags$div(id = ns(glue("ia_select_variable_box{i}")), style = "display: inline-block;",
                        
                        tags$div(style = "display: inline-block;",
                          selectInput(ns(glue("ia_select_variable{i}")), 
                                      "Heeft effect op kolom",
                                      width = 300,
                                      choices = columns,
                                      selected = variable,
                                      multiple = FALSE
                          )
                        ),
                        tags$div(style = "display: inline-block; vertical-align: top;",
                          textInput(ns(glue("ia_element_label{i}")), 
                                    "Label",
                                    value = label
                          )  
                        )
               )
           )
             
           )
           
           
  
}


datafilter_panel <- function(i, ns, columns, data = NULL, args = NULL){

  
  
  
  if(!is.null(args)){
    if(i > length(args)){
      args <- NULL
    } else {
      args <- args[[i]]
    }
    
  }
  
  if(!is.null(args)){

    choices <- sort(unique(data[[args$column]]))
  } else {
    choices <- NULL
  }
  
  tags$div(id = ns(glue("datafilter_panel_{i}")),
           
           h4(glue("Filter {i}")),
           
           side_by_side(vertical_align = TRUE,
             selectInput(ns(glue("il_select_variable{i}")), 
                         "Filter kolom",
                         width = 300,
                         choices = columns,
                         selected = args$column,
                         multiple = FALSE
             ),
             shinyWidgets::pickerInput(ns(glue("il_select_values{i}")), 
                                       "Selecteer",
                                       width = 300,
                                       choices = as.character(choices),
                                       selected = args$value,
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE,
                                                      `deselect-all-text` = "Alles uit",
                                                      `dropdown-align-right` = TRUE,
                                                      `selected-text-format` = "count > 3",
                                                      `none-selected-text` = "Geen selectie",
                                                      `select-all-text` = "Alles aan",
                                                      `none-results-text` = "Geen selectie",
                                                      `count-selected-text` = ">3 Geselecteerd",
                                                      `dropup-auto` = TRUE
                                       )
             )
           )
           
  )
  
  
  
}

fleetingMessage <- function(msg, status = c("info","success","danger","warning"), 
                            selector = getOption("fleetingmessage_location", "#msgplaceholder"), 
                            id = NULL, 
                            where = "beforeEnd",
                            class = "",
                            remove_delay = 4000){
  
  divid <- uuid::UUIDgenerate()
  status <- match.arg(status)
  
  if(is.null(msg) || msg == "")msg <- " "
  if(length(class) > 1)class <- paste(class, collapse = " ")
  if(!missing(id))selector <- paste0("#",id)
  
  ui <- tags$div(id = divid, 
                 class = glue("alert alert-{status} {class}"), 
                 msg,
                 shiny::actionButton("btn-close",
                              label = HTML("&times;"), 
                              class = "alert-close",
                              onclick = glue("document.getElementById('{divid}').outerHTML = '';")))
  
  shiny::insertUI(selector, where, ui)
  
  shinyjs::delay(remove_delay, removeUI(paste0("#", divid)))
  
}

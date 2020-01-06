interactive_panel <- function(i, ns, columns){
  
  tags$div(id = ns(glue("interactive_panel_{i}")),
           h4(glue("Interactief element {i}")),
           selectInput(ns(glue("ia_select_input{i}")), 
                       "Soort input",
                       width = 300,
                       choices = list("Geen" = "",
                                      "Selecteer categorieen" = "selectInput",
                                      "Slider voor numerieke waarden" = "sliderInput",
                                      "Datum reeks" = "dateRangeInput")),
           shinyjs::hidden(
             tags$div(id = ns(glue("ia_select_variable_box{i}")),
                      selectInput(ns(glue("ia_select_variable{i}")), 
                                  "Heeft effect op kolom",
                                  width = 300,
                                  choices = NULL),
                      textInput(ns(glue("ia_element_label{i}")), "Label")
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

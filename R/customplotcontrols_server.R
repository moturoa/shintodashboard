#' Shinto dashboard maker, server function
#' @export
customplotcontrols <- function(input, output, session, data_key, datasets, args = NULL){
  
  rv <- reactiveValues(
    all_ids = NULL,
    current_id_container = NULL,
    current_id_plot = NULL,
    filter_settings = NULL,
    plot_settings = NULL
  )
  
  
  filters <- read_filters(args)
  

  # Update keuze menu.
  updateSelectInput(session, "select_dataset", 
                    choices = data_key,
                    selected = args$dataset)
  
  # Lees geselecteerde dataset
  current_dataset <- reactive({
    req(input$select_dataset)
    datasets[[input$select_dataset]]
  })
  
  # Kolom namen van geselecteerde dataset
  current_available_columns <- reactive({
    names(current_dataset())
  })

  
  
  observeEvent(current_available_columns(), {
    
    cols <- current_available_columns()
    
    updateSelectInput(session, "plot_xvar", 
                      choices = cols, selected = args$xvar)
    updateSelectInput(session, "plot_yvar", 
                      choices = cols, selected = args$yvar)
    updateSelectInput(session, "plot_groupvar", 
                      choices = cols, selected = args$groupvar)  

    updateSelectInput(session, "ia_select_variable1",
                      choices = cols)
    updateSelectInput(session, "ia_select_variable2",
                      choices = cols)
    
    updateSelectInput(session, "il_select_variable1",
                      choices = cols,
                      selected = if(length(filters)>0)filters[[1]]$column else NULL)
    
    updateSelectInput(session, "il_select_variable2",
                      choices = cols,
                      selected = if(length(filters)>1)filters[[2]]$column else NULL)
    

    
  })
  
  

  
  
  observeEvent(input$il_select_variable1,  {

    sel <- input$il_select_variable1
    req(sel)
    
    dat <- current_dataset()[[sel]]
    
    selected <- sort(unique(dat))
    if(length(filters) > 0){
      if(sel == filters[[1]]$column){
        
        selected <- filters[[1]]$value
        
      }
    }
    
    updatePickerInput(session, "il_select_values1",
                      choices = sort(unique(dat)),
                      selected = selected)

  })

 
  observeEvent(input$il_select_variable2,  {
    
    sel <- input$il_select_variable2
    req(sel)
    
    dat <- current_dataset()[[sel]]
    
    selected <- sort(unique(dat))
    if(length(filters) > 1){
      if(sel == filters[[2]]$column){
        
        selected <- filters[[2]]$value
        
      }
    }
    
    updatePickerInput(session, "il_select_values2",
                      choices = sort(unique(dat)),
                      selected = selected)
    
  })
  
  
  # Data filter toevoegen
  # observeEvent(input$btn_add_filter, {
  # 
  #   new_id <- uuid::UUIDgenerate()
  # 
  #   insertUI(paste0("#", session$ns("filter_placeholder")),
  #            "beforeEnd",
  #            columnFilterUI(session$ns(new_id), current_dataset())
  #   )
  # 
  #   rv$filter_settings[[new_id]] <- callModule(columnFilter, new_id, current_dataset())
  # 
  # 
  # })
  
  # if(!is.null(args)){
  #   
  #   
  #   if(length(args$filters) > 0){
  #     
  #     for(i in seq_along(args$filters)){
  #       id <- names(args$filters)[i]
  #       insertUI(paste0("#", session$ns("filter_placeholder")),
  #                "beforeEnd",
  #                columnFilterUI(session$ns(id), current_dataset())
  #       )
  #       
  #       rv$filter_settings[[id]] <- callModule(columnFilter, id, datasets[[args$dataset]])  
  #     }
  # 
  #   }
  #   
  #   
  # }
  
  # Lokale functie om interactieve settings te lezen.
  read_interactive_controls <- function(){
    
    if(is_empty(input$ia_select_variable1) || input$ia_select_nelements == "0"){
      
      list(
        nelements = 0
      )
      
    } else {
      
      list(
        nelements = as.numeric(input$ia_select_nelements),
        element1 = input$ia_select_input1,
        variable1 = input$ia_select_variable1,
        label1 = input$ia_element_label1,
        element2 = input$ia_select_input2,
        variable2 = input$ia_select_variable2,
        label2 = input$ia_element_label2
        
      )
      
    }
    
  }
  
  # vreeeeeeselijke code
  read_datafilters <- function(){
    
    
    if(input$il_select_nelements == "0"){
      
      list()
      
    } else if (input$il_select_nelements == "1"){
      
      list(
        list(
          class = "character",
          column = input$il_select_variable1,
          value = input$il_select_values1
        )  
      )
      
    } else if(input$il_select_nelements == "2"){
      
      list(
        list(
          class = "character",
          column = input$il_select_variable1,
          value = input$il_select_values1
        ),
        list(
          class = "character",
          column = input$il_select_variable2,
          value = input$il_select_values2
        )
      )
      
    }
    
    
  }
  
  
  # Lokale functie om kleuren palette te lezen.
  read_palette <- function(){
    pal <- c()
    for(i in 1:12){
      pal <- c(pal, input[[paste0("sel_color", i)]])
    }
    
    return(pal)
  }
  
  
  observeEvent(input$btn_load_palette, {
    
    pal <- load_palette(input$select_palette)
    
    for(i in seq_along(pal)){
      colourpicker::updateColourInput(session, paste0("sel_color",input$num_start_palette + (i - 1)), value = pal[i])
    }
    
  })
  
  observeEvent(input$btn_randomize_palette, {
    
    new_pal <- sample(read_palette())
    
    for(i in 1:12){
      colourpicker::updateColourInput(session, paste0("sel_color",i), value = new_pal[i])
    }
    
  })
  
  observeEvent(input$btn_save_palette, {
    
    req(input$txt_palette_name)
    json <- toJSON(read_palette())
    
    writeLines(json, file.path("cache/palettes", paste0(input$txt_palette_name, ".json")))
    updateTextInput(session, "txt_palette_name", value = "")
  })
  
  
  #----- Read plot settings
  
  # Plot settings, panels 1 en 2.
  # 'Main' settings: dataset, kolommen, plot type.
  settings_plot_main <- reactive(
    
    list(
      dataset = input$select_dataset,
      
      plottype = as.character(input$plot_type),
      bar_position = input$bar_position,
      statfun = input$plot_stat,
      
      pietype = "Pie", #input$pietype,
      pienarm = input$pienarm,
      
      xvar = as.character(input$plot_xvar),
      yvar = as.character(input$plot_yvar),
      factor_x = input$chk_factor_x,
      factor_y = input$chk_factor_y,
      usegroup = input$chk_usegroup,
      groupvar = as.character(input$plot_groupvar)
    )
    
  )
  # 
  # update_plot_main <- function(a, session){
  #   
  #   updateSelectInput(session, "select_dataset", selected = a$dataset)
  #   
  #   # Make sure to load data first; reactive current_dataset() has not updated yet.
  #   current_columns <- names(datasets[[a$dataset]])
  #   
  #   updateSelectInput(session, "plot_type", selected = a$plottype)
  #   updateAwesomeRadio(session,  "bar_position", selected = a$bar_position)
  #   updateSelectInput(session, "plot_stat", selected = a$statfun)
  #   
  #   updateCheckboxInput(session, "pienarm", value = as.logical(a$pienarm))
  #   updateSelectInput(session, "pietype", selected = a$pietype)
  #   
  #   updateSelectInput(session, "plot_xvar", choices = current_columns, selected = a$xvar)
  #   updateSelectInput(session, "plot_yvar", choices = current_columns, selected = a$yvar)
  #   updateCheckboxInput(session, "chk_factor_x", value = as.logical(a$factor_x))
  #   updateCheckboxInput(session, "chk_factor_y", value = as.logical(a$factor_y))
  #   
  #   updateCheckboxInput(session, "chk_usegroup", value = as.logical(a$usegroup))
  #   updateSelectInput(session, "plot_groupvar", choices = current_columns, selected = a$groupvar)
  #   
  #   if(a$groupvar == ""){
  #     updateCheckboxInput(session, "chk_usegroup",value = FALSE)
  #   }
  # }
  
  # Labels, titels, legenda, etc.
  settings_plot_labels <- reactive({
    
    list(
      title = input$plot_title,
      subtitle = input$plot_subtitle,
      xlab = input$plot_xlab,
      ylab = input$plot_ylab,
      glab = input$plot_glab,
      
      includezerox = input$chk_includezerox,
      includezeroy = input$chk_includezeroy,
      labelsize = input$num_labelsize,
      labelmargin = input$num_labelmargin,
      
      labelanglex =  input$sel_labelanglex,
      labelangley =  input$sel_labelangley,
      nolabelsx = input$chk_removelabelsx,
      nolegend =  input$chk_nolegend
    )
    
  })
  
  # update_plot_labels <- function(a, session){
  #   
  #   updateTextInput(session, "plot_title", value = null_to_empty(a$title))
  #   updateTextInput(session, "plot_subtitle", value = null_to_empty(a$subtitle))
  #   updateTextInput(session, "plot_xlab", value = a$xlab)
  #   updateTextInput(session, "plot_ylab", value = a$ylab)
  #   updateTextInput(session, "plot_glab", value = a$glab)
  #   
  #   updateCheckboxInput(session, "chk_includezerox", value = as.logical(a$includezerox))
  #   updateCheckboxInput(session, "chk_includezeroy", value = as.logical(a$includezeroy))
  #   updateNumericInput(session, "num_labelsize", value = a$labelsize)
  #   updateNumericInput(session, "num_labelmargin", value = a$labelmargin)
  #   
  #   updateSelectInput(session, "sel_labelanglex", selected = a$labelanglex)
  #   updateSelectInput(session, "sel_labelangley", selected = a$labelangley)
  #   updateCheckboxInput(session, "chk_removelabelsx", value = as.logical(a$nolabelsx))
  #   updateCheckboxInput(session, "chk_nolegend", value = as.logical(a$nolegend))
  #   
  # }
  
  # Kleuren, symbool vorm, thema, etc.
  settings_plot_design <- reactive({
    
    list(
      palette = read_palette(),
      shape = input$scatter_shape,
      theme = "theme_minimal"
    )
    
  })        
  
  # update_plot_design <- function(a, session){
  #   
  #   # Panel 6 - Colors
  #   for(i in 1:12){
  #     colourpicker::updateColourInput(session, paste0("sel_color",i), value = a$palette[i])
  #   }
  #   updateSelectInput(session, "scatter_shape", selected = a$shape)
  #   
  # }
  
  # Annotatie : labels in bars, horizontale/verticale lijnen.
  settings_plot_annotation <- reactive({
    
    list(
      annotate_bars = input$check_annotate_bars,
      annotation_type = input$select_annotation,
      line_coordinate = input$num_line_coordinate,
      line_colour = input$colour_annotation
    )
    
  })
  
  # update_plot_annotation <- function(a, session){
  #   
  #   updateCheckboxInput(session, "check_annotate_bars", value = as.logical(a$annotate_bars))
  #   updateSelectInput(session, "select_annotation", selected = a$annotation_type)
  #   updateNumericInput(session, "num_line_coordinate", value = a$line_coordinate)
  #   colourpicker::updateColourInput(session, "colour_annotation", value = a$line_colour)
  #   
  # }
  
  # Data filters, interactieve filters.
  settings_plot_dynamic <- reactive({
    
    list(
      filters = read_datafilters(), #lapply(rv$filter_settings, reactiveValuesToList),
      interactive = read_interactive_controls()
    )  
    
  })
  
  #Interactieve elementen

  
  # update_plot_dynamic <- function(a, session){
  #   
  #   current_columns <- names(datasets[[a$dataset]])
  #     
  #   #---- Filters
  #   insert_defined_filter <- function(preset){
  #     
  #     new_id <- uuid::UUIDgenerate()
  #     
  #     insertUI(paste0("#", session$ns("filter_placeholder")), 
  #              "beforeEnd", 
  #              columnFilterUI(session$ns(new_id), current_dataset(), preset = preset)
  #     )
  #     
  #     rv$filter_settings[[new_id]] <- callModule(columnFilter, new_id, current_dataset(),
  #                                                preset = preset)
  #     
  #   }
  #   
  #   # Oude filters weghalen
  #   for(j in seq_along(rv$filter_settings)){
  #     id <- session$ns(names(rv$filter_settings)[j])
  #     removeUI(paste0("#",id))
  #   }
  #   rv$filter_settings <- NULL
  #   
  #   # Opgeslagen filters erin plakken.
  #   for(obj in a$filters){
  #     insert_defined_filter(obj)
  #   }
  #     
      
    # Interactieve elementen
    # update_interactive_panel <- function(i, a, session){
    #   if(!is.null(a$interactive[[glue("element{i}")]])){
    #     shinyjs::show(glue("interactive_panel_{i}"))
    #     shinyjs::show(glue("ia_select_input{i}"))
    #     updateSelectInput(session, glue("ia_select_input{i}"), 
    #                       selected = a$interactive[[glue("element{i}")]])
    #     shinyjs::show(glue("ia_select_variable_box{i}"))
    #     updateSelectInput(session, glue("ia_select_variable{i}"), 
    #                       choices = current_columns, 
    #                       selected = a$interactive[[glue("variable{i}")]])
    #     updateTextInput(session, glue("ia_element_label{i}"), 
    #                     value = a$interactive[[glue("label{i}")]])
    #   }
    # }
    # 
    # updateAwesomeRadio(session, "ia_select_nelements", selected = as.character(a$interactive$nelements))
    # update_interactive_panel(1, a, session)
    # update_interactive_panel(2, a, session)
   
  #}
  
  # Lees alle settings in 1 list.
  read_plot_settings <- function(){
    
    c(
      settings_plot_main(),
      settings_plot_labels(),
      settings_plot_design(),
      settings_plot_annotation(),
      settings_plot_dynamic()
    )
    
  }
  
  # Update alle settings: lees uit een list, zet alle waarden in hun plek.
  # update_inputs <- function(a, session){
  #   
  #   update_plot_main(a, session)
  #   update_plot_labels(a, session)
  #   update_plot_design(a, session)
  #   update_plot_annotation(a, session)
  #   update_plot_dynamic(a, session)
  #   
  # }
  
  observeEvent(input$plot_type, {
    
    if(input$plot_type == "Barplot"){
      shinyjs::show("plot_stat")
      shinyjs::hide("scatter_shape")
      shinyjs::hide("pienarm")
      shinyjs::show("chk_usegroup")
      shinyjs::show("barannotation_controls")
    }
    
    if(input$plot_type == "Scatter"){
      shinyjs::hide("plot_stat")
      shinyjs::show("scatter_shape")
      #shinyjs::hide("pietype")
      shinyjs::hide("pienarm")
      shinyjs::show("chk_usegroup")
      shinyjs::hide("barannotation_controls")
    }
    
    if(input$plot_type == "Pie chart"){
      shinyjs::hide("plot_stat")
      shinyjs::hide("scatter_shape")
      #shinyjs::show("pietype")
      shinyjs::show("pienarm")
      shinyjs::hide("chk_usegroup")
      shinyjs::hide("barannotation_controls")
    }
    
  })
  
  observe({
    
    req(input$plot_stat)
    
    if(input$plot_stat == "count"){
      shinyjs::hide("yvar_box")
    } else {
      shinyjs::show("yvar_box")
    }
    
  })
  
  observe({
    
    req(input$chk_usegroup)
    shinyjs::toggle("plot_groupvar", condition = input$chk_usegroup)
    
  })
  
  observe({
    
    req(input$plot_type)
    shinyjs::toggle("bar_position", condition = input$plot_type == "Barplot")
    
  })
  
  observe({
    
    item <- input$select_annotation
    req(item)
    shinyjs::toggle("abline_controls", condition = item != "None")
    
  })
  
  # observeEvent(input$btn_save_dashboard, {
  #   
  #   save_dashboard(rv$plot_settings[input$customplotids], input$txt_dashboard_name)
  #   
  #   updateSelectInput(session, "select_dashboard", 
  #                     choices = list_dashboards())
  # })
  
  clear_dashboard <- function(){
    ids <- paste0("#", names(rv$plot_settings))
    
    for(i in ids){
      removeUI(i)
    }
    
    rv$plot_settings <<- NULL
  }
  
  
  # observeEvent(input$btn_load_dashboard,{
  #   
  #   clear_dashboard()
  #   
  #   thisdash <- input$select_dashboard
  #   
  #   dash <- load_dashboard(thisdash)
  #   
  #   for(i in seq_along(out)){
  #     insert_widget(args = dash[[i]], datasets = datasets)
  #   }
  #   
  # })
  
  
  
  
  #   
  #   widget_ui <- function(id_container, id_plot, id_closebutton, id_editbutton, id_interactive,
  #                         interactive, interactive_vals=NULL, data = NULL){
  #     
  #     
  #     
  #   }
  #   
  #   
  # }
  # 
  # 
  # add_widget <- function(plotarguments = NULL){
  #   
  #   id_container <- ns(paste0("customplot", random_word(6)))
  #   id_plot <- paste0(id_container, "_plot")
  #   id_closebutton <- paste0(id_container,"_btn_close")
  #   id_editbutton <- paste0(id_container,"_btn_edit")
  #   id_interactive <- paste0(id_container, "_interactive_", 1:2)
  #   
  #   if(is.null(plotarguments)){
  #     rv$plot_settings[[id_container]] <<- read_plot_settings()
  #   } else {
  #     rv$plot_settings[[id_container]] <<- plotarguments
  #   }
  #   
  #   
  #   
  #   dataset <- datasets[[rv$plot_settings[[id_container]]$dataset]]
  #   
  #   insertUI(
  #     "#placeholder", where = "beforeEnd",
  #     
  #     widget_ui(id_container, id_plot, id_closebutton, id_editbutton, id_interactive,
  #               interactive = rv$plot_settings[[id_container]]$interactive,
  #               interactive_vals = rv$plot_settings[[id_container]]$interactive_vals,
  #               data = dataset)
  #   )
  #   
  #   observe({
  #     interactive_vals <- list(input[[id_interactive[1]]], 
  #                              input[[id_interactive[2]]])
  #     
  #     rv$plot_settings[[id_container]]$interactive_vals <<- interactive_vals
  #     
  #   })
  #   
  #   
  #   output[[id_plot]] <- renderPlot({
  #     
  #     interactive_vals <- list(input[[id_interactive[1]]], 
  #                              input[[id_interactive[2]]])
  #     
  #     isolate(
  #       custom_plot(plotarguments = rv$plot_settings[[id_container]],
  #                   data = dataset,
  #                   interactive = interactive_vals)
  #     )
  #     
  #   })
  #   
  #   
  #   observeEvent(input[[id_closebutton]], {
  #     
  #     rv$plot_settings[[id_container]] <<- NULL
  #     removeUI(selector = paste0("#", id_container), session = session)
  #     
  #     
  #   })
  #   
  #   
  #   observeEvent(input[[id_editbutton]], {
  #     
  #     update_inputs(rv$plot_settings[[id_container]], session)
  #     rv$current_id_container <- id_container
  #     rv$current_id_plot <- id_plot
  #     
  #     shinyjs::show("btn_updateplot")
  #     
  #   })
  #   
  # }
  
  # Button: plot maken
  # observeEvent(input$btn_addplot, {
  # 
  #   ok <- TRUE
  #   
  #   if(is_empty(input$plot_xvar)){
  #     fleetingMessage("Selecteer een variabele voor de X-as.", status = "danger")
  #     updateTabsetPanel(session, "controls_tab_box", selected = "kolommen")
  #     ok <- FALSE
  #   }
  #   if(is_empty(input$plot_yvar)){
  #     fleetingMessage("Selecteer een variabele voor de Y-as.", status = "danger")
  #     updateTabsetPanel(session, "controls_tab_box", selected = "kolommen")
  #     ok <- FALSE
  #   }
  #   
  #   if(ok){
  #     add_widget()
  #     shinyjs::hide("btn_updateplot")  
  #   }
  #   
  #   
  # })
  
  # # Button: update plot
  # observeEvent(input$btn_updateplot, {
  #   
  #   args <- read_plot_settings()
  #   rv$plot_settings[[rv$current_id_container]] <<- args
  #   
  #   # ids of the interactive elements on the current container, if any
  #   id_interactive <- paste0(rv$current_id_container, "_interactive_", 1:2)
  #   
  #   output[[rv$current_id_plot]] <- renderPlot({
  #     
  #     # settings of those interactive elements
  #     interactive_vals <- list(input[[id_interactive[1]]], 
  #                              input[[id_interactive[2]]])
  #     
  #     isolate(
  #       custom_plot(plotarguments = args, 
  #                   data = datasets[[args$dataset]],
  #                   interactive = interactive_vals)
  #     )
  #     
  #   }, height = 280)
  #   
  # })
  
  
  # Interactieve elementen
  observe({
    
    nel <- as.numeric(input$ia_select_nelements)
    req(nel)
    
    shinyjs::toggle("interactive_panel_1", condition = nel > 0)
    shinyjs::toggle("interactive_panel_2", condition = nel > 1)
    
  })
  
  observe({
    
    sel <- input$ia_select_input1
    req(sel)
    
    if(!sel %in% c("","None")){
      shinyjs::show("ia_select_variable_box1")
    }
    
  })
  
  observe({
    
    sel <- input$ia_select_input2
    req(sel)
    
    if(!sel %in% c("","None")){
      shinyjs::show("ia_select_variable_box2")
    }
    
  })
  
  
  observe({
    
    nel <- as.numeric(input$il_select_nelements)
    req(nel)
    
    shinyjs::toggle("datafilter_panel_1", condition = nel > 0)
    shinyjs::toggle("datafilter_panel_2", condition = nel > 1)
    
  })

return(reactive(read_plot_settings()))
  
  
}

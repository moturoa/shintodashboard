
customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
      column(4, id = "panel_controls",
           
            tabsetPanel( id = "controls_tab_box", type = "pills",
                tabPanel("Start",
                
                       selectInput(ns("select_dataset"), "Dataset", 
                                   choices = available_datasets),
                       
                       
                       selectInput(ns("plot_type"), "Plot type", 
                                   choices = c("Barplot", "Stacked barplot", "Scatter", "Pie chart"),
                                   selected = "Barplot"),
                       
                       shinyjs::hidden(
                         
                         selectInput(ns("scatter_shape"), "Markers", 
                                     choices = c("circles","squares"))
                         
                       ),

                       shinyjs::hidden(
                         radioButtons(ns("pietype"), "Pie chart type",
                                      choices = c("Pie","Waffle"), selected = "Waffle")
                       ),
                       shinyjs::hidden(
                         checkboxInput(ns("pienarm"), "Remove missing values (NA)",value = FALSE)
                         
                       )
                       
                ),       
                tabPanel("Columns",
                     
                       tags$h4("X-axis"),
                       side_by_side(
                         selectInput(ns("plot_xvar"), 
                                     label = "",
                                     choices = "", selected = ""),
                         checkboxInput(ns("chk_factor_x"), "Force factor", value = FALSE, width="50px")
                       ),
                       tags$br(),
                       tags$h4("Y-axis"),
                       tags$div(id = ns("yvar_box"),
                                side_by_side(
                                  selectInput(ns("plot_yvar"), 
                                              label = label_tooltip("Y Variable", "Select Y-axis variable"),
                                              choices = "", selected = ""),
                                  checkboxInput(ns("chk_factor_y"), 
                                                "Force factor",
                                                value = FALSE, width="50px")
                                )
                       ),
                       shinyjs::hidden(
                         selectInput(ns("plot_stat"), 
                                     label_tooltip("Function", "The function used to aggregate the Y-data into bars"),
                                     choices = c("Sum of Y" = "sum", "Mean of Y" = "mean","Count rows of X" = "count"),
                                     selected = "sum")
                       ),
                       

                       tags$br(),
                       checkboxInput(ns("chk_usegroup"), "Use grouping"),
                       shinyjs::hidden(
                         selectInput(ns("plot_groupvar"), 
                                     label = label_tooltip("Grouping Variable", "Select variable for colors or bar segments"),
                                     choices = "", selected = "")
                       )
                       
                       
                ),
                tabPanel("Filter",
                         
                         uiOutput(ns("filter_controls"))
                         
                ),
                tabPanel("Interactive",
                         
                      uiOutput(ns("interactive_controls"))
                      
                ),
                tabPanel("Labels",
                       textInput(ns("plot_title"), "Title"),
                       textInput(ns("plot_subtitle"), "Sub-title"),
                       textInput(ns("plot_xlab"), "X-axis label"),
                       textInput(ns("plot_ylab"), "Y-axis label"),
                       textInput(ns("plot_glab"), label_tooltip("Group label", "Title for the legend")),
                       side_by_side(
                         numericInput(ns("num_labelsize"), 
                                      label_tooltip("Font size","Adjusts the base font size, affects all text"),
                                      min =8, max=20, value=12, width = "148px"),
                         numericInput(ns("num_labelmargin"),
                                      label_tooltip("Label margin","Space between axis and axis labels"),
                                      min = 0, max=10, value=2, width = "148px")
                       ),
                       br(),
                       side_by_side(
                         selectInput(ns("sel_labelanglex"), 
                                     "Rotation X",
                                     choices = c(0,90), width = "148px"),
                         selectInput(ns("sel_labelangley"), 
                                     label_tooltip("Rotation Y",
                                                   "Axis label rotation. Select 90 for labels perpendicular to axis"),
                                     choices = c(0,90), width = "148px")
                       ),
                       br(),
                       side_by_side(vertical_align = TRUE,
                         checkboxInput(ns("chk_removelabelsx"), "Remove X-axis labels", width="60px"),
                         tags$div(style = "width: 30px;"),
                         checkboxInput(ns("chk_nolegend"), "Remove legend", width="60px")
                       ),
                       tags$br()
                       
                ),
                tabPanel("Axes",
                
                         
                       checkboxInput(ns("chk_includezerox"), 
                                     label_tooltip("X - include 0", "Start X-axis at zero"),
                                                   value = FALSE),
                       checkboxInput(ns("chk_includezeroy"), 
                                     label_tooltip("Y - include 0", "Start Y-axis at zero"),
                                                    value = FALSE)
                                  
                ),
                tabPanel("Colors",
                      
                      side_by_side(
                       checkboxInput(ns("chk_colorbrewer"), "", value = TRUE, width = "60px"), 
                       selectInput(ns("select_palette"), 
                                   "Color palette (Color Brewer) (>8 colors)",
                                   choices = rownames(brewer.pal.info), 
                                   selected = "Dark2", width = "300px")
                      ),
                      tags$br(),
                      side_by_side(
                        checkboxInput(ns("chk_canva"), "", value = FALSE, width = "60px"),
                        selectInput(ns("select_palette2"),
                                   "Color palette (canva.com) (4 colors)",
                                   choices = sort(names(canva_palettes)),
                                   selected = "", width = "300px")
                      ),
                      tags$br(),
                      side_by_side(
                        actionButton(ns("btn_load_palette"), 
                                   label_tooltip("Load", "Load colors from selected palette"),
                                   class = "btn btn-primary",
                                   icon = icon("chevron-down", lib = "glyphicon")),
                        tags$br(),
                        numericInput(ns("num_start_palette"), 
                                     label_tooltip("Start at", "Load colors starting from this color"),
                                     value = 1, min=1, max=12, step=1, width="100px")
                      ),
                      tags$hr(),
                      
                      lapply(1:12, function(i){  
                        
                        div(style="width: 150px; display: inline-block;", 
                          colourInput(ns(paste0("sel_color",i)), as.character(i), 
                                      value = gplots::rich.colors(12)[i])
                        )
                        
                      }),
                      tags$br(),
                      actionButton(ns("btn_erase_palette"), 
                                   label_tooltip("Erase","Clear all colors"),
                                   icon = icon("eraser"))
                      
                    
                ),
                
                tabPanel("Theme",
                
                         selectInput(ns("select_theme"),
                                     label_tooltip("Select theme","Select ggplot2 theme, affects styling"),
                                     choices = c("theme_minimal","theme_bw","theme_classic",
                                                 "theme_linedraw","theme_light",
                                                 "theme_base","theme_calc","theme_clean","theme_economist",
                                                 "theme_economist_white","theme_excel","theme_few",
                                                 "theme_fivethirtyeight","theme_foundation",
                                                 "theme_gdocs","theme_hc","theme_igray","theme_tufte","theme_wsj"))
                                  
                ),
                tabPanel("Dashboard",
                       
                       textInput(ns("txt_dashboard_name"), "Naam", 
                                 value = glue("dashboard_{sample(1:10^4,1)}")),
                       actionButton(ns("btn_save_dashboard"), 
                                    "Save Dashboard", icon=icon("save"),
                                    onclick = "customplotorder();"),
                       tags$hr(),
                       selectInput(ns("select_dashboard"), "Dashboard database",
                                   choices = list_dashboards()),
                       actionButton(ns("btn_load_dashboard"), "Load"),
                       actionButton(ns("btn_dashboard_wissen"), "Erase current dashboard")
                )
              ),
            
              tags$hr(),
              actionButton(ns("btn_addplot"), 
                           label_tooltip("Make plot","Add plot to dashboard"), 
                           class = "btn btn-primary", 
                           icon = icon("plus", lib = "glyphicon")),
              shinyjs::hidden(
                actionButton(ns("btn_updateplot"), 
                             label_tooltip("Update plot", "Update the last selected plot"), 
                             class = "btn btn-primary", 
                             icon = icon("refresh", lib = "glyphicon"))
              ),
              textOutput(ns("txt_info"))
    
            
      ),
      column(8, 
             fluidRow(
              div(id="placeholder")
            )  
      )
    
  )
  
   
}


customplotcontrols <- function(input, output, session){
  
  jqui_sortable('#placeholder', options = list(opacity = 0.5))
  
  rv <- reactiveValues(
    all_ids = NULL,
    current_id_container = NULL,
    current_id_plot = NULL
  )
  
  current_dataset <- reactive({
    get(input$select_dataset)
  })
  
  current_available_columns <- reactive({
    names(current_dataset())
  })
  
  ns <- session$ns
  
  
  read_interactive_controls <- function(){
    
    #req(input$ia_select_nelements)

    if(is_empty(input$ia_select_variable1) || input$ia_select_nelements == "0"){
      
      return(NULL)
      
    } else {
      
      list(
        element1 = input$ia_select_input1,
        variable1 = input$ia_select_variable1,
        label1 = input$ia_element_label1,
        element2 = input$ia_select_input2,
        variable2 = input$ia_select_variable2,
        label2 = input$ia_element_label2
        
      )
      
    }
    
  }
  
  
  read_plot_settings <- function(){
    
    pal <- c()
    for(i in 1:12){
      pal <- c(pal, input[[paste0("sel_color", i)]])
    }
    
    list(
      dataset = input$select_dataset,
      plottype = as.character(input$plot_type),
      xvar = as.character(input$plot_xvar),
      factor_x = input$chk_factor_x,
      factor_y = input$chk_factor_y,
      yvar = as.character(input$plot_yvar),
      usegroup = input$chk_usegroup,
      groupvar = as.character(input$plot_groupvar),
      title = input$plot_title,
      subtitle = input$plot_subtitle,
      xlab = input$plot_xlab,
      ylab = input$plot_ylab,
      glab = input$plot_glab,
      statfun = input$plot_stat,
      pietype = tolower(input$pietype),
      pienarm = input$pienarm,
      palette = pal,
      shape = input$scatter_shape,
      theme = input$select_theme,
      includezerox = input$chk_includezerox,
      includezeroy = input$chk_includezeroy,
      labelsize = input$num_labelsize,
      labelmargin = input$num_labelmargin,
      labelanglex =  input$sel_labelanglex,
      labelangley =  input$sel_labelangley,
      nolabelsx = input$chk_removelabelsx,
      nolegend =  input$chk_nolegend,
      filters = list(input$filterx1, input$filterx2, input$filterx3, input$filterx4,
                     input$filtery1, input$filtery2, input$filtery3, input$filtery4,
                     input$filterg1, input$filterg2, input$filterg3, input$filterg4),
      interactive = read_interactive_controls()
      
    )
  }

  observeEvent(input$btn_load_palette, {
    
    if(input$chk_colorbrewer){
      pal <- brewer.pal(8, input$select_palette)
    }
    if(input$chk_canva){
      pal <- canva_palettes[[input$select_palette2]]
    }
    
    for(i in seq_along(pal)){
      updateColourInput(session, paste0("sel_color",input$num_start_palette + (i - 1)), value = pal[i])
    }
      
  })
  
  observeEvent(input$btn_erase_palette, {

    for(i in 1:12){
      updateColourInput(session, paste0("sel_color",i), value = "white")
    }

  })
  
  
  observeEvent(input$select_dataset, {

    cols <- current_available_columns()
    
    updateSelectInput(session, "plot_xvar", 
                      choices = cols, 
                      selected = if(!is_empty(input$plot_xvar))input$plot_xvar else cols[1])
    updateSelectInput(session, "plot_yvar", choices = cols, 
                      selected = if(!is_empty(input$plot_yvar))input$plot_yvar else cols[2])
    updateSelectInput(session, "plot_groupvar", choices = cols, 
                      selected = if(!is_empty(input$plot_groupvar))input$plot_groupvar else cols[3])
    
  })
  
  observeEvent(input$plot_type, {
    
    if(input$plot_type %in% c("Barplot", "Stacked barplot") ){
      shinyjs::show("plot_stat")
      shinyjs::hide("scatter_shape")
      shinyjs::hide("pietype")
      shinyjs::hide("pienarm")
    }
    if(input$plot_type == "Scatter"){
      shinyjs::hide("plot_stat")
      shinyjs::show("scatter_shape")
      shinyjs::hide("pietype")
      shinyjs::hide("pienarm")
      
    }
    if(input$plot_type == "Pie chart"){
      shinyjs::hide("plot_stat")
      shinyjs::hide("scatter_shape")
      shinyjs::show("pietype")
      shinyjs::show("pienarm")
    }
    
  })
  
  observe({
    print(input$plot_stat)
    if(input$plot_stat == "count"){
      shinyjs::hide("yvar_box")
    } else {
      shinyjs::show("yvar_box")
    }
    
  })
  
  observe({
    
    if(input$chk_usegroup){
      shinyjs::show("plot_groupvar")
    } else {
      shinyjs::hide("plot_groupvar")
    }
  })
  
  observeEvent(input$btn_reset, {
    shinyjs::reset("panel_controls")
    updateSelectInput(session, "plot_xvar", selected = "")
    updateSelectInput(session, "plot_yvar", selected = "")
    updateSelectInput(session, "plot_groupvar", selected = "")
  })
  
  observeEvent(input$chk_colorbrewer, {
    if(as.logical(input$chk_colorbrewer)){
      updateCheckboxInput(session, "chk_canva", value = FALSE)
    }
  })
  
  observeEvent(input$chk_canva, {
    
    if(as.logical(input$chk_canva)){
      updateCheckboxInput(session, "chk_colorbrewer", value = FALSE)
    }
  })
  
  observeEvent(input$btn_save_dashboard, {
    
    save_dashboard(plot_settings[input$customplotids], input$txt_dashboard_name)
    
    updateSelectInput(session, "select_dashboard", 
                      choices = list_dashboards())
  })
  
  clear_dashboard <- function(){
    ids <- paste0("#", names(plot_settings))
    
    for(i in ids){
      removeUI(i)
    }
    
    current_ids <<- c()
    plot_settings <<- NULL
  }
  
  observeEvent(input$btn_dashboard_wissen, {
    
    clear_dashboard()
    
  })
  
  observeEvent(input$btn_load_dashboard,{
    
    clear_dashboard()
    
    thisdash <- input$select_dashboard

    out <- load_dashboard(thisdash)
    
    for(i in seq_along(out)){
      add_widget(plotarguments = out[[i]])
    }
    
  })
  
  
  widget_ui <- function(id_container, id_plot, id_closebutton, id_editbutton, id_interactive,
                        interactive, data = NULL){
    
    inner_content <- list(
      actionButton(ns(id_closebutton), 
                   label=HTML("&times;"), class="plotbutton"),
      actionButton(ns(id_editbutton), 
                   label="", icon=icon("edit"), class="plotbutton"),
      plotOutput(ns(id_plot), height = "280px")
    )
    
    if(!is.null(interactive)){
      
      make_interactive_element <- function(i){
        
        varlab <- paste0("variable",i)
        ellab <- paste0("element",i)
        label <- paste0("label",i)
        
        if(is_empty(interactive[[varlab]])){
          return(NULL)
        } else {
          
          column_data <- data[,interactive[[varlab]]][[1]]
          
          if(interactive[[ellab]] == "selectInput"){
            el <- shinyWidgets::pickerInput(ns(id_interactive[i]), 
                              label = interactive[[label]],
                              choices = unique(column_data),
                              selected = unique(column_data),
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `selected-text-format` = "count > 3"),
                              width = "200px"
            )
          } else if(interactive[[ellab]] == "sliderInput"){
            el <- sliderInput(ns(id_interactive[i]),
                              label = interactive[[label]],
                              min = min(column_data, na.rm=TRUE),
                              max = max(column_data, na.rm=TRUE),
                              value = c(min(column_data, na.rm=TRUE),max(column_data, na.rm=TRUE)),
                              width = "200px"
                              )
          } else if(interactive[[ellab]] == "dateRangeInput") {
            el <- dateRangeInput(ns(id_interactive[i]),
                              label = interactive[[label]],
                              start = min(column_data, na.rm=TRUE),
                              end = max(column_data, na.rm=TRUE),
                              # value = c(min(column_data, na.rm=TRUE),max(column_data, na.rm=TRUE)),
                              width = "200px"
            )
          }
          
          
          return(el)
        }
      }
      
      inner_content <- c(inner_content, list(
        side_by_side(
          make_interactive_element(1),
          tags$div(style="width:20px;"),
          make_interactive_element(2),
          vertical_align = TRUE
        )
      ))
      
      
    }
    
    withTags(
      div(id = id_container,  class = "customplot col-sm-6", 
             div(class = "box cpbox", style = "height: 400px;",
                      tags$div(class = "box-body",
                               inner_content
                      )
             )
      )
    )
  }
  
  add_widget <- function(plotarguments = NULL){
    
    id_container <- ns(paste0("customplot", random_word(6)))
    id_plot <- paste0(id_container, "_plot")
    id_closebutton <- paste0(id_container,"_btn_close")
    id_editbutton <- paste0(id_container,"_btn_edit")
    id_interactive <- paste0(id_container, "_interactive_", 1:2)
    
    if(is.null(plotarguments)){
      plot_settings[[id_container]] <<- read_plot_settings()
    } else {
      plot_settings[[id_container]] <<- plotarguments
    }
    
    current_ids <<- c(current_ids, id_container)
    
    dataset <- get(plot_settings[[id_container]]$dataset)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      
      widget_ui(id_container, id_plot,id_closebutton,id_editbutton,id_interactive,
                interactive = plot_settings[[id_container]]$interactive,
                data = dataset)
    )
    
    output[[id_plot]] <- renderPlot({
      
      interactive_vals <- list(input[[id_interactive[1]]], 
                            input[[id_interactive[2]]])
      
      isolate(
        custom_plot(plotarguments = plot_settings[[id_container]],
                    interactive = interactive_vals)
      )
      
    })
    
    
    observeEvent(input[[id_closebutton]], {
      
      plot_settings[[id_container]] <<- NULL
      removeUI(selector = paste0("#", id_container), session = session)
      current_ids <<- current_ids[-match(id_container, current_ids)]
      
    })
    
    
    update_inputs <- function(a, session){
      
      
      updateSelectInput(session, "select_dataset", selected = a$dataset)
      
      updateSelectInput(session, "plot_xvar", choices = current_available_columns(), selected = a$xvar)
      updateSelectInput(session, "plot_yvar", choices = current_available_columns(), selected = a$yvar)
      updateSelectInput(session, "plot_groupvar", choices = current_available_columns(), selected = a$groupvar)
      
      updateCheckboxInput(session, "chk_usegroup",value = as.logical(a$usegroup))
      
      if(a$groupvar == ""){
        updateCheckboxInput(session, "chk_usegroup",value = FALSE)
      }
      
      update_filter <- function(id,i,type){
        if(!is.null(input[[id]])){
          if(type == "numeric"){
            updateNumericInput(session, id, value = a$filters[i])
          } else if(type == "factor"){
            updateSelectInput(session, id, selected = a$filters[i])
          } else if(type == "date"){
            updateDateRangeInput(session, id, start = a$filters[i][1], end = a$filters[i][2])
          }
        }
      }
      
      fs <- c("filterx1", "filterx2", "filterx3", "filterx4", 
              "filtery1", "filtery2", "filtery3", "filtery4", 
              "filterg1", "filterg2", "filterg3", "filterg4")
      
      type <- rep(c("numeric","numeric","factor","date"), times = 3)
      for(i in seq_along(fs)){
        update_filter(fs[i], i, type[i])
      }
            
      updateSelectInput(session, "plot_type", 
                        selected = a$plottype)
      updateSelectInput(session, "scatter_shape", 
                        selected = a$shape)
      updateSelectInput(session, "plot_stat", 
                        selected = a$statfun)
      
      updateTextInput(session, "plot_xlab", value = a$xlab)
      updateTextInput(session, "plot_ylab", value = a$ylab)
      updateTextInput(session, "plot_glab", value = a$glab)
      
      maybe <- function(x){
        if(is.null(x)) {
          ""
        } else {
          x
        }
      }
      updateTextInput(session, "plot_title", value = maybe(a$title))
      updateTextInput(session, "plot_subtitle", value = maybe(a$subtitle))
      
      updateNumericInput(session, "num_labelsize", value = a$labelsize)
      updateNumericInput(session, "num_labelmargin", value = a$labelmargin)
      
      for(i in 1:12){
        updateColourInput(session, paste0("sel_color",i), value = a$palette[i])
      }
      
    }
    
    observeEvent(input[[id_editbutton]], {

      update_inputs(plot_settings[[id_container]], session)
      
      rv$current_id_container <- id_container
      rv$current_id_plot <- id_plot
      
      shinyjs::show("btn_updateplot")
      
    })
    
  }
  
  observeEvent(input$btn_addplot, {
 
      add_widget()  
      shinyjs::hide("btn_updateplot")
    
  })


  output$filter_controls <- renderUI({
        
      make_controls <- function(data, label, force_factor = FALSE, idbase="filter"){
        data <- data[!is.na(data)]
        
        if(force_factor | is.factor(data) | is.character(data)){
          
          el <- tagList(
            h4(label),
            selectInput(session$ns(glue("{idbase}3")), "", choices = sort(unique(data)), multiple=TRUE)
          )
          
        } else {
          
          if(is.numeric(data)){
            
            
            el <- tagList(
              h4(label),
              side_by_side(
                numericInput(session$ns(glue("{idbase}1")), "min", value=min(data), width="100px"),
                numericInput(session$ns(glue("{idbase}2")), "max", value=max(data), width="100px")
              ),
              br()
            ) 
            
          }
          
          if(inherits(data, "Date")){
            
            el <- tagList(
              h4(label),
              dateRangeInput(session$ns(glue("{idbase}4")), "", start = min(data), end = max(data),
                             format = "dd/mm/yy", language = "nl")
            )
            
          }
          
        }
        return(el)
      }
        
      tagList(
        make_controls(current_dataset()[[input$plot_xvar]], "X variable", 
                      idbase="filterx", force_factor = input$chk_factor_x),
        make_controls(current_dataset()[[input$plot_yvar]], "Y variable", 
                      idbase="filtery", force_factor = input$chk_factor_y),
        make_controls(current_dataset()[[input$plot_groupvar]], "Group variable", 
                      idbase="filterg", force_factor = TRUE)
      )
    
        
  })

  output$interactive_controls <- renderUI({
    
    
    tagList(
      
      awesomeRadio(ns("ia_select_nelements"),
                   "Number of interactive elements",
                   choices = c("0","1","2"),
                   selected = "0",
                   inline=TRUE),
      
      
      shinyjs::hidden(
        tags$div(id = ns("interactive_panel_1"),
          h4("Interactive element 1"),
          selectInput(ns("ia_select_input1"), 
                      "Selector type",
                      choices = list("None" = "",
                                     "Select category" = "selectInput",
                                     "Numeric slider" = "sliderInput",
                                     "Date range" = "dateRangeInput")),
          shinyjs::hidden(
            tags$div(id = ns("ia_select_variable_box1"),
              selectInput(ns("ia_select_variable1"), 
                          "Affected variable",
                          choices = c("", current_available_columns())),
              textInput(ns("ia_element_label1"), "Label")
            )
          )
        )
      ),
      
      
      shinyjs::hidden(
        tags$div(id = ns("interactive_panel_2"),
          h4("Interactive element 2"),
          selectInput(ns("ia_select_input2"), 
                      "Selector type",
                      choices = list("None" = "",
                                     "Select category" = "selectInput",
                                     "Numeric slider" = "sliderInput",
                                     "Date range" = "dateRangeInput")),
          shinyjs::hidden(
            tags$div(id = ns("ia_select_variable_box2"),
                     selectInput(ns("ia_select_variable2"), 
                                 "Affected variable",
                                 choices = c("", current_available_columns())),
                     textInput(ns("ia_element_label2"), "Label")
            )
          )
        )
      )
    )
      
  })
  
  observe({
    
    nel <- as.numeric(input$ia_select_nelements)
    req(nel)
    
    if(nel == 0){
      shinyjs::hide("interactive_panel_1")
      shinyjs::hide("interactive_panel_2")
    }
    if(nel == 1){
      shinyjs::show("interactive_panel_1")
      shinyjs::hide("interactive_panel_2")
    }
    if(nel == 2){
      shinyjs::show("interactive_panel_1")
      shinyjs::show("interactive_panel_2")
    }
    
    
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
  
  
  observeEvent(input$btn_updateplot, {
    
    args <- read_plot_settings()
    plot_settings[[rv$current_id_container]] <<- args
    
    # ids of the interactive elements on the current container, if any
    id_interactive <- paste0(rv$current_id_container, "_interactive_", 1:2)
    
    output[[rv$current_id_plot]] <- renderPlot({
      
      # settings of those interactive elements
      interactive_vals <- list(input[[id_interactive[1]]], 
                               input[[id_interactive[2]]])
      
      isolate(
        custom_plot(plotarguments = args, interactive = interactive_vals)
      )
      
    }, height = 280)
    
  })
    

  
}


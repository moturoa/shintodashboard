
customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
      column(4, id = "panel_controls",
           
            tabsetPanel( id = "controls_tab_box", type = "pills",
                tabPanel("Data",
                
                       selectInput(ns("select_dataset"), "Dataset", 
                                   choices = c("woning_productie","automobiles","mtcars","iris")),
                       
                       side_by_side(
                        selectInput(ns("plot_xvar"), 
                                    label = label_tooltip("X Variable", "Select variable to plot along the X-axis"),
                                   choices = "", selected = ""),
                        checkboxInput(ns("chk_factor_x"), "Force factor", value = FALSE, width="50px")
                       ),
                       tags$br(),
                       side_by_side(
                         selectInput(ns("plot_yvar"), 
                                     label = label_tooltip("Y Variable", "Select variable to plot along the X-axis"),
                                     choices = "", selected = ""),
                         checkboxInput(ns("chk_factor_y"), 
                                       "Force factor",
                                        value = FALSE, width="50px")
                       ),
                       tags$br(),
                       checkboxInput(ns("chk_usegroup"), "Use grouping"),
                       selectInput(ns("plot_groupvar"), 
                                   label = label_tooltip("Grouping Variable", "Select variable for colors or bar segments"),
                                   choices = "", selected = "")

                ),       
                tabPanel("Filter",
                  
                   uiOutput(ns("filter_controls"))
                         
                ),
                tabPanel("Plot type",
                       
                       selectInput(ns("plot_type"), "Plot type", 
                                   choices = c("Scatter", "Barplot", "Stacked barplot")),
                       shinyjs::hidden(
                         
                         selectInput(ns("scatter_shape"), "Markers", 
                                     choices = c("circles","squares"))
                         
                       ),
                       
                       shinyjs::hidden(
                        selectInput(ns("plot_stat"), 
                                    label_tooltip("Functie", "For barplots, the function used to aggregate the data into bars"),
                                    choices = c("mean","count","max", "sum"))
                       )
                       
                ),
                tabPanel("Labels",
                       textInput(ns("plot_title"), "Title"),
                       textInput(ns("plot_subtitle"), "Sub-title"),
                       textInput(ns("plot_xlab"), "X-axis label"),
                       textInput(ns("plot_ylab"), "Y-axis label"),
                       textInput(ns("plot_glab"), label_tooltip("Group label", "Title for the legend")),
                       numericInput(ns("num_labelsize"), 
                                    label_tooltip("Font size","Adjusts the base font size, affects all text"),
                                    min =8, max=20, value=12),
                       numericInput(ns("num_labelmargin"),
                                    label_tooltip("Label margin","Space between axis and axis labels"),
                                    min = 0, max=10, value=2),
                       side_by_side(
                         selectInput(ns("sel_labelanglex"), 
                                     "Rotation X",
                                     choices = c(0,90), width = "148px"),
                         selectInput(ns("sel_labelangley"), 
                                     label_tooltip("Rotation Y",
                                                   "Axis label rotation. Select 90 for labels perpendicular to axis"),
                                     choices = c(0,90), width = "148px")
                       ),
                       checkboxInput(ns("chk_removelabelsx"), "Remove X-axis labels", width="60px"),
                       checkboxInput(ns("chk_nolegend"), "Remove legend", width="60px"),
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
                       
                       textInput(ns("txt_dashboard_name"), "Naam", value = glue("dashboard_{sample(1:10^4,1)}")),
                       tags$hr(),
                       actionButton(ns("btn_save_dashboard"), "Save Dashboard", icon=icon("save")),
                       selectInput(ns("select_dashboard"), "Dashboard database",
                                   choices = list_dashboards()),
                       actionButton(ns("btn_load_dashboard"), "Load"),
                       actionButton(ns("btn_dashboard_wissen"), "Erase current dashboard")
                )
              ),
            
              tags$br(),
              tags$br(),
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
  
  ns <- session$ns
  
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
      palette = pal,
      shape = input$scatter_shape,
      theme = input$select_theme,
      includezerox = input$chk_includezerox,
      includezeroy = input$chk_includezeroy,
      labelsize = input$num_labelsize,
      labelmargin =input$num_labelmargin,
      labelanglex =  input$sel_labelanglex,
      labelangley =  input$sel_labelangley,
      nolabelsx = input$chk_removelabelsx,
      nolegend =  input$chk_nolegend,
      filters = list(input$filterx1, input$filterx2, input$filterx3, 
                     input$filtery1, input$filtery2, input$filtery3, 
                     input$filterg1, input$filterg2, input$filterg3)
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
    
    dataset <- get(input$select_dataset)
    updateSelectInput(session, "plot_xvar", choices = names(dataset), selected = names(dataset)[4])
    updateSelectInput(session, "plot_yvar", choices = names(dataset), selected = names(dataset)[5])
    updateSelectInput(session, "plot_groupvar", choices = names(dataset), selected = "")
    
  })
  
  observeEvent(input$plot_type, {
    
    if(input$plot_type %in% c("Barplot", "Stacked barplot") ){
      shinyjs::show("plot_stat")
      shinyjs::hide("scatter_shape")
    }
    if(input$plot_type == "Scatter"){
      shinyjs::hide("plot_stat")
      shinyjs::show("scatter_shape")
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
    
    save_dashboard(plot_settings[current_ids], input$txt_dashboard_name)
    
    updateSelectInput(session, "select_dashboard", 
                      choices = list_dashboards())
  })
  
  clear_dashboard <- function(){
    ids <- paste0("#", names(plot_settings))
    
    for(i in ids){
      removeUI(i)
    }
    
  }
  
  observeEvent(input$btn_dashboard_wissen, {
    
    clear_dashboard()
    current_ids <<- c()
    plot_settings <<- NULL
    
  })
  
  observeEvent(input$btn_load_dashboard,{
    
    clear_dashboard()
    current_ids <<- c()
    plot_settings <<- NULL
    thisdash <- input$select_dashboard

    out <- load_dashboard(thisdash)
    
    for(i in seq_along(out)){
      add_plot(plotarguments = out[[i]])
    }
    
  })
  
  
  add_plot <- function(plotarguments = NULL){
    
    id_container <- ns(paste0("customplot", random_word(6)))
    id_plot <- paste0(id_container, "_plot")
    id_closebutton <- paste0(id_container,"_btn_close")
    id_editbutton <- paste0(id_container,"_btn_edit")
    id_downloadbutton <- paste0(id_container,"_btn_download")
    
    current_ids <<- c(current_ids, id_container)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      
      tags$div(id = id_container,  class = "col-sm-6", 
               tags$div(class = "box cpbox",
                        tags$div(class = "box-body",
                                 actionButton(ns(id_closebutton), 
                                              label=HTML("&times;"), class="plotbutton"),
                                 actionButton(ns(id_editbutton), 
                                              label="", icon=icon("edit"), class="plotbutton"),
                                 # actionButton(ns(id_downloadbutton), 
                                 #              label="", icon=icon("download"), class="plotbutton"),
                                 plotOutput(ns(id_plot), height = "280px")
                        )
               )
      )
    )
    
    if(is.null(plotarguments)){
      plot_settings[[id_container]] <<- read_plot_settings()
    } else {
      plot_settings[[id_container]] <<- plotarguments
    }
    
    output[[id_plot]] <- renderPlot({
      
      isolate(
        custom_plot(plot_settings[[id_container]])
      )
      
    })
    
    
    observeEvent(input[[id_closebutton]], {
      
      plot_settings[[id_container]] <<- NULL
      removeUI(selector = paste0("#", id_container), session = session)
      current_ids <<- current_ids[-match(id_container, current_ids)]
      
    })
    
    
    update_inputs <- function(a, session){
      
      updateSelectInput(session, "select_data", 
                        selected = a$dataset)
      
      updateSelectInput(session, "plot_xvar", 
                              selected = a$xvar)
      
      updateSelectInput(session, "plot_yvar", 
                              selected = a$yvar)
      
      updateSelectInput(session, "plot_groupvar", 
                              selected = a$groupvar)

      updateCheckboxInput(session, "chk_usegroup",value = as.logical(a$usegroup))
      
      if(a$groupvar == ""){
        updateCheckboxInput(session, "chk_usegroup",value = FALSE)
      }
      
      update_filter <- function(id,i,numeric){
        if(!is.null(input[[id]])){
          if(numeric){
            updateNumericInput(session, id, value = a$filters[i])
          } else {
            updateSelectInput(session, id, selected = a$filters[i])
          }
        }
      }
      
      fs <- c("filterx1", "filterx2", "filterx3", 
              "filtery1", "filtery2", "filtery3", 
              "filterg1", "filterg2", "filterg3")
      
      type <- rep(c(TRUE,TRUE,FALSE),3)
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
    
    if(is_empty(input$plot_xvar) | is_empty(input$plot_yvar)){
      output$txt_info <- renderText({"First Select X and Y variables"})
    } else {
      add_plot()  
      shinyjs::hide("btn_updateplot")
    }
    
  })


  output$filter_controls <- renderUI({
        
      dataset <- get(input$select_dataset)

      make_controls <- function(data, label, force_factor = FALSE, idbase="filter"){
        data <- data[!is.na(data)]
        if(!force_factor && is.numeric(data)){
          tagList(
            h4(label),
            side_by_side(
              numericInput(session$ns(glue("{idbase}1")), "min", value=min(data), width="100px"),
              numericInput(session$ns(glue("{idbase}2")), "max", value=max(data), width="100px")
            ),
            br()
          ) 
        } else {
          tagList(
            h4(label),
            selectInput(session$ns(glue("{idbase}3")), "Choices", choices = sort(unique(data)), multiple=TRUE)
          )
        }
      }
      
      tagList(
        make_controls(dataset[[input$plot_xvar]], "X variable", idbase="filterx", force_factor = input$chk_factor_x),
        make_controls(dataset[[input$plot_yvar]], "Y variable", idbase="filtery", force_factor = input$chk_factor_y),
        make_controls(dataset[[input$plot_groupvar]], "Group variable", idbase="filterg", force_factor = TRUE)
      )
    
        
  })


  
  observeEvent(input$btn_updateplot, {
    
    args <- read_plot_settings()
    plot_settings[[rv$current_id_container]] <<- args
    
    output[[rv$current_id_plot]] <- renderPlot({
      
      isolate(
        custom_plot(args)
      )
    }, height = 280)
    
  })
    

  
}


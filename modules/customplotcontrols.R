
customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  
  fluidRow(
      column(4, id = "panel_controls",
           
            tabBox(width = 12, height = "800px", id = "controls_tab_box",
                tabPanel("Data",
                
                       selectInput(ns("select_dataset"), "Dataset", 
                                   choices = c("automobiles","mtcars","iris")),
                       
                       selectInput(ns("plot_xvar"), label = "X-as variabele", 
                                   choices = "", selected = ""),
                       selectInput(ns("plot_yvar"), label = "Y-as variabele", 
                                         choices = "", selected = ""),
                       checkboxInput(ns("chk_usegroup"), "Gebruik groep"),
                       selectInput(ns("plot_groupvar"), label = "Groep variabele", 
                                         choices = "", selected = "")
                       
                ),
                tabPanel("Plot type",
                       
                       selectInput(ns("plot_type"), "Plot type", 
                                   choices = c("Scatter", "Barplot", "Stacked barplot")),
                       shinyjs::hidden(
                         
                         selectInput(ns("scatter_shape"), "Markers", 
                                     choices = c("circles","squares"))
                         
                       ),
                       
                       shinyjs::hidden(
                        selectInput(ns("plot_stat"), "Functie", 
                                   choices = c("mean","count","max", "sum"))
                       )
                       
                ),
                tabPanel("Labels",
                       textInput(ns("plot_xlab"), "X-as label"),
                       textInput(ns("plot_ylab"), "Y-as label"),
                       textInput(ns("plot_glab"), "Groep label"),
                       numericInput(ns("num_labelsize"), "Font size", min =8, max=20, value=12),
                       numericInput(ns("num_labelmargin"), "Label margin", min = 0, max=10, value=2)
                       
                       
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
                      actionButton(ns("btn_load_palette"), 
                                   "Load", 
                                   class = "btn btn-primary",
                                   icon = icon("chevron-down", lib = "glyphicon")),
                      tags$br(),
                      lapply(1:6, function(i){
                        colourInput(ns(paste0("sel_color",i)), "", value = brewer.pal(6, "Dark2")[i])
                      })
                      
                    
                ),
                
                tabPanel("Theme",
                
                         selectInput(ns("select_theme"),
                                     "Select ggplot2 theme",
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
                       actionButton(ns("btn_save_dashboard"), "Dashboard opslaan", icon=icon("save")),
                       selectInput(ns("select_dashboard"), "Dashboard database",
                                   choices = list_dashboards()),
                       actionButton(ns("btn_load_dashboard"), "Laden"),
                       actionButton(ns("btn_dashboard_wissen"), "Wissen")
                )
              ),
            
              tags$br(),
              tags$br(),
              tags$hr(),
              actionButton(ns("btn_addplot"), "Make plot", class = "btn btn-primary", 
                           icon = icon("plus", lib = "glyphicon")),
              shinyjs::hidden(
                actionButton(ns("btn_updateplot"), "Update plot", class = "btn btn-primary", 
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
    for(i in 1:6){
      pal <- c(pal, input[[paste0("sel_color", i)]])
    }
    
    list(
      dataset = input$select_dataset,
      plottype = as.character(input$plot_type),
      xvar = as.character(input$plot_xvar),
      yvar = as.character(input$plot_yvar),
      usegroup = input$chk_usegroup,
      groupvar = as.character(input$plot_groupvar),
      xlab = input$plot_xlab,
      ylab = input$plot_ylab,
      glab = input$plot_glab,
      statfun = input$plot_stat,
      palette = pal,
      shape = input$scatter_shape,
      theme = input$select_theme,
      labelsize = input$num_labelsize,
      labelmargin =input$num_labelmargin
    )
  }

  observeEvent(input$btn_load_palette, {
    
    if(input$chk_colorbrewer){
      pal <- brewer.pal(8, input$select_palette)
    }
    if(input$chk_canva){
      pal <- canva_palettes[[input$select_palette2]]
    }
    
    for(i in 1:6){
      updateColourInput(session, paste0("sel_color",i), value = pal[i])
    }
      
  })
  
  
  observeEvent(input$select_dataset, {
    
    dataset <- get(input$select_dataset)
    updateSelectInput(session, "plot_xvar", choices = names(dataset), selected = names(dataset)[4])
    updateSelectInput(session, "plot_yvar", choices = names(dataset), selected = names(dataset)[5])
    updateSelectInput(session, "plot_groupvar", choices = names(dataset), selected = "")
    
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
  
  add_plot <- function(plotarguments = NULL){
    
    id_container <- ns(paste0("customplot", random_word(6)))
    id_plot <- paste0(id_container, "_plot")
    id_closebutton <- paste0(id_container,"_btn_close")
    id_editbutton <- paste0(id_container,"_btn_edit")
    id_downloadbutton <- paste0(id_container,"_btn_download")
    
    current_ids <<- c(current_ids, id_container)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      
      tags$div(id = id_container,  class = "col-sm-4", 
               tags$div(class = "box cpbox",
                        tags$div(class = "box-body",
                                 actionButton(ns(id_closebutton), 
                                              label=HTML("&times;"), class="plotbutton"),
                                 actionButton(ns(id_editbutton), 
                                              label="", icon=icon("edit"), class="plotbutton"),
                                 actionButton(ns(id_downloadbutton), 
                                              label="", icon=icon("download"), class="plotbutton"),
                                 plotOutput(ns(id_plot))
                        )
               )
      )
    )
    
    if(is.null(plotarguments)){
      plot_settings[[id_container]] <<- read_plot_settings()
      
      #print(jsonlite::toJSON(plot_settings[[id_container]]))
    } else {
      plot_settings[[id_container]] <<- plotarguments
    }
    
    output[[id_plot]] <- renderPlot({
      
      isolate(
        custom_plot(plot_settings[[id_container]])
      )
      
    }, height = 280)
    
    
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
      
            
      updateSelectInput(session, "plot_type", 
                        selected = a$plottype)
      
      updateSelectInput(session, "plot_stat", 
                        selected = a$statfun)
      
      updateTextInput(session, "plot_xlab", value = a$xlab)
      updateTextInput(session, "plot_ylab", value = a$ylab)
      updateTextInput(session, "plot_glab", value = a$glab)
      
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


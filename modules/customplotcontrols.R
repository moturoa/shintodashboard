
customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
      column(4, id = "panel_controls",
           
            tabBox(width = 12,
                tabPanel("Data",
                
                       
                       varSelectizeInput(ns("plot_xvar"), label = "X-as variabele", 
                                         data = automobiles, selected = "engine_volume"),
                       varSelectizeInput(ns("plot_yvar"), label = "Y-as variabele", 
                                         data = automobiles, selected = "fuel_efficiency"),
                       checkboxInput(ns("chk_usegroup"), "Gebruik groep"),
                       varSelectizeInput(ns("plot_groupvar"), label = "Groep variabele", 
                                         data = automobiles, selected = "cylinders")
                       
                ),
                tabPanel("Plot type",
                       
                       selectInput(ns("plot_type"), "Plot type", 
                                   choices = c("Scatter", "Barplot", "Stacked barplot")),
                       tags$p("Voor barplots select functie toe te passen op X en/of group variabele"),
                       selectInput(ns("plot_stat"), "Functie", 
                                   choices = c("mean","count","max", "sum"))
                       
                ),
                tabPanel("Labels",
                       textInput(ns("plot_xlab"), "X-as label"),
                       textInput(ns("plot_ylab"), "Y-as label"),
                       textInput(ns("plot_glab"), "Groep label")
                       
                ),
                tabPanel("Save",
                       
                       actionButton(ns("btn_addplot"), "Make plot"),
                       tags$hr(),
                       textInput(ns("txt_dashboard_name"), "Naam", value = glue("dashboard_{sample(1:10^4,1)}")),
                       actionButton(ns("btn_save_dashboard"), "Dashboard opslaan", icon=icon("save")),
                       selectInput(ns("select_dashboard"), "Dashboard database",
                                   choices = names(dashboards)),
                       actionButton(ns("btn_load_dashboard"), "Laden"),
                       actionButton(ns("btn_dashboard_wissen"), "Wissen")
                )
              )
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
    all_ids = NULL
  )
  
  ns <- session$ns

  
  observeEvent(input$btn_reset, {
    shinyjs::reset("panel_controls")
  })
  
  observeEvent(input$btn_save_dashboard, {
    
    dashboards[[input$txt_dashboard_name]] <<- plot_settings[current_ids]
    updateSelectInput(session, "select_dashboard", 
                      choices = names(dashboards))
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

    out <- dashboards[[thisdash]]
    
    for(i in seq_along(out)){
      add_plot(plotarguments = out[[i]])
    }
    
  })
  
  add_plot <- function(plotarguments = NULL){
    
    unique_hash <- random_word(6)
    
    id_container <- ns(paste0("customplot", unique_hash))
    id_plot <- paste0(id_container, "_plot")
    id_closebutton <- paste0(id_container,"_btn_close")
    id_editbutton <- paste0(id_container,"_btn_edit")
    id_downloadbutton <- paste0(id_container,"_btn_download")
    
    current_ids <<- c(current_ids, id_container)
    
    insertUI(
      "#placeholder", where = "beforeEnd",
      
      tags$div(class = "col-sm-4", id = id_container,
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
      plot_settings[[id_container]] <<- list(
        plottype = as.character(input$plot_type),
        xvar = as.character(input$plot_xvar), 
        yvar = as.character(input$plot_yvar),
        usegroup = input$chk_usegroup,
        groupvar = as.character(input$plot_groupvar),
        xlab = input$plot_xlab,
        ylab = input$plot_ylab,
        glab = input$plot_glab,
        statfun = input$plot_stat
      )
      
      print(jsonlite::toJSON(plot_settings[[id_container]]))
    } else {
      plot_settings[[id_container]] <<- plotarguments
    }
    
    output[[id_plot]] <- renderPlot({
      
      isolate(
        custom_plot(plot_arguments = plot_settings[[id_container]])
      )
      
    }, height = 280)
    
    
    observeEvent(input[[id_closebutton]], {
      
      plot_settings[[id_container]] <<- NULL
      removeUI(selector = paste0("#", id_container), session = session)
      current_ids <<- current_ids[-match(id_container, current_ids)]
      
    })
    
    
    update_inputs <- function(a, session){
      
      updateVarSelectizeInput(session, "plot_xvar", 
                              selected = a$xvar)
      
      updateVarSelectizeInput(session, "plot_yvar", 
                              selected = a$yvar)
      
      updateCheckboxInput(session, "chk_usegroup",value = as.logical(a$usegroup))
      
      updateVarSelectizeInput(session, "plot_groupvar", 
                              selected = a$groupvar)
      
      
      updateSelectInput(session, "plot_type", 
                        selected = a$plottype)
      
      updateSelectInput(session, "plot_stat", 
                        selected = a$statfun)
      
      updateTextInput(session, "plot_xlab", value = a$xlab)
      updateTextInput(session, "plot_ylab", value = a$ylab)
      updateTextInput(session, "plot_glab", value = a$glab)
      
    }
    
    shinyjs::onclick(id_plot,  {
      update_inputs(plot_settings[[id_container]], session)
    })
    
    observeEvent(input[[id_editbutton]], {
      
      args <- isolate(list(
        plottype = as.character(input$plot_type),
        xvar = as.character(input$plot_xvar), 
        yvar = as.character(input$plot_yvar),
        usegroup = input$chk_usegroup,
        groupvar = as.character(input$plot_groupvar),
        xlab = input$plot_xlab,
        ylab = input$plot_ylab,
        glab = input$plot_glab,
        statfun = input$plot_stat
      ))
      
      plot_settings[[id_container]] <<- args
      
      output[[id_plot]] <- renderPlot({
        
        isolate(
          custom_plot(plot_arguments = args)
        )
      }, width = 380, height = 280)
      
    }) 
    
  }
  
  observeEvent(input$btn_addplot, {
    
    add_plot()
    
  })
    

  
}



customplotcontrolsUI <- function(id){
  
  ns <- NS(id)
  
  
  fluidPage(
    fluidRow(

            tabBox( id = "controls_tab_box", width = 12, 
                tabPanel("1. Start",
                
                     tagList(
                       selectInput(ns("select_dataset"), 
                                   label_tooltip("Dataset", 
                                                 "Selecteer een dataset."),
                                   width = 300,
                                   choices = available_datasets),
                       
                       
                       selectInput(ns("plot_type"), 
                                   label_tooltip("Plot type", "Selecteer een van de beschikbare plot types."),
                                   width = 300,
                                   choices = c("Barplot", "Scatter", "Pie chart"),
                                   selected = "Barplot"),
                       
                       shinyjs::hidden(
                         
                         awesomeRadio(ns("bar_position"), 
                                      label_tooltip("Layout van deel bars",
                                                    "Voor een barplot, de positie van de delen: boven op elkaar (stacked) of naast elkaar"),
                                      choices = c("Stacked","Grouped"),
                                      selected = "Stacked", inline=TRUE)
                         
                       ),
                       
                       shinyjs::hidden(
                         
                         awesomeRadio(ns("scatter_shape"), 
                                      label_tooltip("Plot symbool",
                                                    "Soort markers voor de scatter plot"),
                                      choices = c("circles","squares"),
                                      inline=TRUE)
                         
                       ),
                       
                       shinyjs::hidden(
                         awesomeRadio(ns("pietype"), 
                                      label_tooltip("Pie chart type", 
                                                    "Soort pie chart. Alleen Pie mogelijk op het moment."),
                                      choices = c("Pie","Waffle"),
                                      inline=TRUE)
                       ),
                       shinyjs::hidden(
                         checkboxInput(ns("pienarm"), "Missende waarden weghalen.", value = FALSE)
                         
                       )
                     )
                       
                ),       
                tabPanel("2. Kolommen",
                     
                         
                     tagList(
                       
                       side_by_side(
                         selectInput(ns("plot_xvar"), 
                                     label = label_tooltip("X Variabele", 
                                                           "Selecteer variabele die langs de X-as wordt geplot"),
                                     choices = NULL, 
                                     width = 300),
                         checkboxInput(ns("chk_factor_x"), 
                                       "Maak factor", 
                                       value = FALSE, 
                                       width="50px")
                       ),
                       tags$br(),
                       
                       tags$div(id = ns("yvar_box"),
                                side_by_side(
                                  selectInput(ns("plot_yvar"), 
                                              label = label_tooltip("Y Variabele", 
                                                                    "Selecteer variabele die langs de Y-as wordt geplot"),
                                              choices = NULL, 
                                              width = 300),
                                  checkboxInput(ns("chk_factor_y"), 
                                                "Maak factor",
                                                value = FALSE, 
                                                width="50px")
                                )
                       ),
                       shinyjs::hidden(
                         selectInput(ns("plot_stat"), 
                                     label_tooltip("Functie", "De functie om de data in groepen samen te vatten."),
                                     width = 300,
                                     choices = c("Sum van Y" = "sum", 
                                                 "Gemiddelde van Y" = "mean",
                                                 "Tel aantal rijen in X" = "count"),
                                     selected = "sum")
                       ),
                       
                       tags$br(),
                       checkboxInput(ns("chk_usegroup"), 
                                     label_tooltip("Gebruik groep variabele", 
                                                   "Voeg een 3e kolom toe, die de data in groepen verdeeld"),
                                     value = FALSE),
                       shinyjs::hidden(
                         selectInput(ns("plot_groupvar"), 
                                     label = label_tooltip("Groep variabele", 
                                                           "Selecteer de kolom die de kleuren in de bar delen aangeeft."),
                                     width = 300,
                                     choices = NULL)
                       )
                     )
                       
                ),
                # tabPanel("3. Filter",
                #          
                #       uiOutput(ns("filter_controls"))
                #          
                # ),
                tabPanel("3. Interactief",
                         
                         
                     tagList(
                       awesomeRadio(ns("ia_select_nelements"),
                                    label_tooltip("Aantal interactieve elementen",
                                                  "Voeg hier (optioneel) interactieve elementen toe aan de plot"),
                                    choices = c("0","1","2"),
                                    selected = "0",
                                    inline = TRUE),
                       
                       shinyjs::hidden(
                         interactive_panel(1, ns)
                       ),
                       shinyjs::hidden(
                         interactive_panel(2, ns)
                       )
                     )
                       
                ),
                tabPanel("4. Labels",
                         
                         fluidRow(    
                           column(4,    
                                  textInput(ns("plot_title"), "Titel"),
                                  textInput(ns("plot_subtitle"), "Sub-titel"),
                                  textInput(ns("plot_xlab"), "X-as label"),
                                  textInput(ns("plot_ylab"), "Y-as label"),
                                  textInput(ns("plot_glab"), label_tooltip("Groep label", "Titel voor de legenda"))
                                  
                           ),
                           column(4, 
                                  side_by_side(
                                    numericInput(ns("num_labelsize"), 
                                                 "Tekst grootte",
                                                 min =8, max=20, value=12, width = "148px"),
                                    
                                    numericInput(ns("num_labelmargin"),
                                                 label_tooltip("Label marge","Ruimte tussen de as en de labels."),
                                                 min = 0, max=10, value=2, width = "148px")
                                  ),
                                  side_by_side(
                                    selectInput(ns("sel_labelanglex"), 
                                                "X-as label rotatie",
                                                choices = c(0,90), width = "148px"),
                                    selectInput(ns("sel_labelangley"), 
                                                label_tooltip("Y-as label rotatie",
                                                              "Rotatie voor de labels naast de as."),
                                                choices = c(0,90), width = "148px")
                                  ),
                                  br(),
                                  side_by_side(vertical_align = TRUE,
                                               checkboxInput(ns("chk_removelabelsx"), "Geen X-as labels", width="60px"),
                                               tags$div(style = "width: 30px;"),
                                               checkboxInput(ns("chk_nolegend"), "Geen legenda", width="60px")
                                  ), 
                                  tags$br()
                           ),
                           column(4, 
                                  checkboxInput(ns("chk_includezerox"), 
                                                label_tooltip("X - begin bij 0", "Start X-as bij nul"),
                                                value = FALSE),
                                  checkboxInput(ns("chk_includezeroy"), 
                                                label_tooltip("Y - begin bij 0", "Start Y-as bij nul"),
                                                value = FALSE)
                           )
                         )
                     
                ),
                

                tabPanel("5. Annotatie",
                      
                         tagList(
                           shinyjs::hidden(
                             tags$div(id = ns("barannotation_controls"),
                                      tags$h4("Annotatie voor bars"),
                                      checkboxInput(ns("check_annotate_bars"), 
                                                    label_tooltip("Label totalen boven de bars", 
                                                                  "Voegt een label toe per bar met de totale waarde"),
                                                    value = FALSE),
                                      tags$hr()
                             )
                           ),
                           
                           tags$h4("Rechte lijnen"),
                           selectInput(ns("select_annotation"),
                                       "Type",
                                       width = 300,
                                       choices = c("Geen" = "None", 
                                                   "Horizontale lijn" = "Horizontal line",
                                                   "Verticale lijn" = "Vertical line")),
                           shinyjs::hidden(
                             tags$div(id = ns("abline_controls"),
                                      numericInput(ns("num_line_coordinate"), "X-as waarde:", 
                                                   value = 0, width = 300),
                                      tags$div(style = "width: 200px;",
                                               colourInput(ns("colour_annotation"), "Kleur", value = "red")
                                      )
                             )
                           )
                         )
                         
                ),
                tabPanel("6. Kleuren",
                         
                     fluidRow(
                       column(8,
                              
                              side_by_side(
                                #checkboxInput(ns("chk_colorbrewer"), "", value = TRUE, width = "60px"), 
                                selectInput(ns("select_palette"), 
                                            "Palet",
                                            choices = color_palettes, 
                                            selected = "rich.colors", width = 300)
                              ),
                              
                              tags$br(),
                              side_by_side(
                                actionButton(ns("btn_load_palette"), 
                                             label_tooltip("Laden", 
                                                           "Laad kleuren uit het geselecteerde palet."),
                                             class = "btn btn-primary",
                                             icon = icon("chevron-down", lib = "glyphicon")),
                                tags$br(),
                                numericInput(ns("num_start_palette"), 
                                             label_tooltip("Begin bij", 
                                                           "Laad kleuren vanaf deze kleur."),
                                             value = 1, min=1, max=12, step=1, width = 100)
                              ),
                              tags$hr(),
                              
                              lapply(1:12, function(i){  
                                
                                div(style="width: 110px; display: inline-block;", 
                                    colourInput(ns(paste0("sel_color",i)), as.character(i), 
                                                value = gplots::rich.colors(12)[i])
                                )
                                
                              }),
                              tags$br(),
                              actionButton(ns("btn_randomize_palette"), 
                                           label_tooltip("Shuffle",
                                                         "Zet de kleuren in willekeurige volgorde"),
                                           icon = icon("random")),
                              tags$hr(),
                              side_by_side(
                                textInput(ns("txt_palette_name"), "Opslaan als...", width = 200),
                                actionButton(ns("btn_save_palette"), "", icon = icon("save"))
                              )
                              
                       )
                     )
                      
                ),
                
                # tabPanel("Thema",
                # 
                #     uiOutput(ns("theme_controls"))
                #          
                # ),

                
                tabPanel(tagList(icon("play"), "Voltooien"),
                         
                     tags$p("Maak de plot aan volgens de huidige instellingen.",
                            "De plot wordt op het dashboard geplaatst."),
                     tags$br(),
                     actionButton(ns("btn_addplot"), 
                                  label_tooltip("Plot maken","Voeg huidige plot toe aan dashboard."), 
                                  class = "btn btn-primary", 
                                  icon = icon("plus", lib = "glyphicon")),
                     shinyjs::hidden(
                       actionButton(ns("btn_updateplot"), 
                                    label_tooltip("Plot updaten", "Geselecteerde plot updaten."), 
                                    class = "btn btn-primary", 
                                    icon = icon("refresh", lib = "glyphicon"))
                     )
                ),
                
                tabPanel(tagList(icon("table"), "Dashboard"),
                         
                   tagList(
                     tags$p("Huidig dashboard opslaan, of een dashboard uit de database laden."),
                     textInput(ns("txt_dashboard_name"), "Dashboard opslaan", 
                               value = glue("dashboard_{sample(1:10^4,1)}")),
                     actionButton(ns("btn_save_dashboard"), 
                                  "Opslaan", icon=icon("save"), class="btn btn-info",
                                  onclick = "customplotorder();"),
                     tags$hr(),
                     selectInput(ns("select_dashboard"), "Dashboard laden",
                                 choices = list_dashboards()),
                     actionButton(ns("btn_load_dashboard"), "", 
                                  class="btn btn-info", 
                                  icon = icon("folder-open"))
                   )
                         
                )
                
                # tabPanel("Debug",
                #          
                #          verbatimTextOutput(ns("txt_debug"))
                # )
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
    req(input$select_dataset)
    get(input$select_dataset)
  })
  
  current_available_columns <- reactive({
    names(current_dataset())
  })
  
  ns <- session$ns
  
  
  # observe({
  #   
  #   out <- load_dashboard("wbmdemo5")
  #   
  #   for(i in seq_along(out)){
  #     add_widget(plotarguments = out[[i]])
  #   }
  #   
  # })
  
  # output$txt_debug <- renderPrint({
  #   reactiveValuesToList(input)
  # })
  
  
  observeEvent(current_available_columns(), {
    
    cols <- current_available_columns()
    
    updateSelectInput(session, "plot_xvar", 
                      choices = cols, selected = input$plot_xvar)
    updateSelectInput(session, "plot_yvar", 
                      choices = cols, selected = input$plot_yvar)
    updateSelectInput(session, "plot_groupvar", 
                      choices = cols, selected = input$plot_groupvar)  
    updateSelectInput(session, "ia_select_variable1", 
                      choices = cols)
    updateSelectInput(session, "ia_select_variable2", 
                      choices = cols)
    
  })
  # 
  # output$filter_controls <- renderUI({
  #   
  #   req(current_dataset())
  #   
  #   if(is_empty(input$plot_xvar)){
  #     tags$p("Selecteer eerst de X en Y variabelen.")
  #   } else {
  #     req(input$plot_yvar)
  #   
  #     make_controls <- function(data, label, force_factor = FALSE, idbase="filter"){
  #       data <- data[!is.na(data)]
  #       
  #       if(force_factor | is.factor(data) | is.character(data)){
  #         
  #         el <- tagList(
  #           h4(label),
  #           selectInput(session$ns(glue("{idbase}3")), "", 
  #                       width = 300,
  #                       choices = sort(unique(data)), multiple=TRUE)
  #         )
  #         
  #       } else {
  #         
  #         if(is.numeric(data)){
  #           
  #           el <- tagList(
  #             h4(label),
  #             side_by_side(
  #               numericInput(session$ns(glue("{idbase}1")), "min", value=min(data), width = 100),
  #               numericInput(session$ns(glue("{idbase}2")), "max", value=max(data), width = 100)
  #             ),
  #             br()
  #           ) 
  #           
  #         }
  #         
  #         if(inherits(data, "Date")){
  #           
  #           el <- tagList(
  #             h4(label),
  #             dateRangeInput(session$ns(glue("{idbase}4")), "", 
  #                            start = min(data), 
  #                            end = max(data),
  #                            width = 300,
  #                            format = "dd/mm/yy", 
  #                            language = "nl")
  #           )
  #           
  #         }
  #         
  #       }
  #       return(el)
  #     }
  #     
  #     tagList(
  #       make_controls(current_dataset()[[input$plot_xvar]], "X variabele", 
  #                     idbase="filterx", force_factor = input$chk_factor_x),
  #       make_controls(current_dataset()[[input$plot_yvar]], "Y variabele", 
  #                     idbase="filtery", force_factor = input$chk_factor_y),
  #       make_controls(current_dataset()[[input$plot_groupvar]], "Groep variabele", 
  #                     idbase="filterg", force_factor = TRUE)
  #     )
  #   }
  #   
  # })
  

  
  # 
  # output$theme_controls <- renderUI({
  #   
  #   tagList(
  #     
  #     selectInput(ns("select_theme"),
  #                 label_tooltip("Select theme","Select ggplot2 theme, affects styling"),
  #                 choices = c("theme_minimal","theme_bw","theme_classic",
  #                             "theme_linedraw","theme_light",
  #                             "theme_base","theme_calc","theme_clean","theme_economist",
  #                             "theme_economist_white","theme_excel","theme_few",
  #                             "theme_fivethirtyeight","theme_foundation",
  #                             "theme_gdocs","theme_hc","theme_igray","theme_tufte","theme_wsj"))
  #     
  #   )
  #   
  # })
  
  
  read_interactive_controls <- function(){
    
    #req(input$ia_select_nelements)

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
  
  read_palette <- function(){
    pal <- c()
    for(i in 1:12){
      pal <- c(pal, input[[paste0("sel_color", i)]])
    }
    
  return(pal)
  }
  
  read_plot_settings <- function(){
    
    list(
      dataset = input$select_dataset,
      plottype = as.character(input$plot_type),
      xvar = as.character(input$plot_xvar),
      factor_x = input$chk_factor_x,
      factor_y = input$chk_factor_y,
      yvar = as.character(input$plot_yvar),
      usegroup = input$chk_usegroup,
      groupvar = as.character(input$plot_groupvar),
      bar_position = input$bar_position,
      title = input$plot_title,
      subtitle = input$plot_subtitle,
      xlab = input$plot_xlab,
      ylab = input$plot_ylab,
      glab = input$plot_glab,
      statfun = input$plot_stat,
      pietype = input$pietype,
      pienarm = input$pienarm,
      palette = read_palette(),
      shape = input$scatter_shape,
      theme = "theme_minimal",
      includezerox = input$chk_includezerox,
      includezeroy = input$chk_includezeroy,
      labelsize = input$num_labelsize,
      labelmargin = input$num_labelmargin,
      labelanglex =  input$sel_labelanglex,
      labelangley =  input$sel_labelangley,
      nolabelsx = input$chk_removelabelsx,
      nolegend =  input$chk_nolegend,
      annotate_bars = input$check_annotate_bars,
      annotation_type = input$select_annotation,
      line_coordinate = input$num_line_coordinate,
      line_colour = input$colour_annotation,
      filters = list(input$filterx1, input$filterx2, input$filterx3, input$filterx4,
                     input$filtery1, input$filtery2, input$filtery3, input$filtery4,
                     input$filterg1, input$filterg2, input$filterg3, input$filterg4),
      interactive = read_interactive_controls()
      
      
    )
  }

  
  observeEvent(input$btn_load_palette, {
    
    pal <- load_palette(input$select_palette)
    
    for(i in seq_along(pal)){
      updateColourInput(session, paste0("sel_color",input$num_start_palette + (i - 1)), value = pal[i])
    }
      
  })
  
  observeEvent(input$btn_randomize_palette, {

    new_pal <- sample(read_palette())
    
    for(i in 1:12){
      updateColourInput(session, paste0("sel_color",i), value = new_pal[i])
    }

  })
  
  observeEvent(input$btn_save_palette, {
    
    req(input$txt_palette_name)
    json <- toJSON(read_palette())
    
    writeLines(json, file.path("cache/palettes", paste0(input$txt_palette_name, ".json")))
    updateTextInput(session, "txt_palette_name", value = "")
  })
  
  observeEvent(input$plot_type, {
    
    if(input$plot_type %in% c("Barplot", "Stacked barplot") ){
      shinyjs::show("plot_stat")
      shinyjs::hide("scatter_shape")
      shinyjs::hide("pietype")
      shinyjs::hide("pienarm")
      shinyjs::show("chk_usegroup")
      shinyjs::show("barannotation_controls")
    }
    
    if(input$plot_type == "Scatter"){
      shinyjs::hide("plot_stat")
      shinyjs::show("scatter_shape")
      shinyjs::hide("pietype")
      shinyjs::hide("pienarm")
      shinyjs::show("chk_usegroup")
      shinyjs::hide("barannotation_controls")
    }
    
    if(input$plot_type == "Pie chart"){
      shinyjs::hide("plot_stat")
      shinyjs::hide("scatter_shape")
      shinyjs::show("pietype")
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
    
    
    if(input$chk_usegroup){
      shinyjs::show("plot_groupvar")
    } else {
      shinyjs::hide("plot_groupvar")
    }
  })
  
  observe({
    req(input$plot_type)
    
    if(input$plot_type == "Barplot"){
      shinyjs::show("bar_position")
    } else {
      shinyjs::hide("bar_position")
    }
    
  })
  
  observe({
    
    item <- input$select_annotation
    req(item)
    
    if(item != "None"){
      shinyjs::show("abline_controls")
    } else {
      shinyjs::hide("abline_controls")
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
                        interactive, interactive_vals=NULL, data = NULL){
    
    inner_content <- list(
      actionButton(ns(id_closebutton), 
                   label=HTML("&times;"), class="plotbutton"),
      actionButton(ns(id_editbutton), 
                   label="", icon=icon("edit"), class="plotbutton"),
      plotOutput(ns(id_plot), height = "280px")
    )
    
    if(!(is.null(interactive) || interactive$nelements == 0)){
      
      make_interactive_element <- function(i){
        
        if(i > interactive$nelements)return(NULL)
        
        varlab <- paste0("variable",i)
        ellab <- paste0("element",i)
        label <- paste0("label",i)
        value <- interactive_vals[[i]]
        
          column_data <- data[,interactive[[varlab]]][[1]]
          
          if(interactive[[ellab]] == "selectInput"){
            el <- shinyWidgets::pickerInput(ns(id_interactive[i]), 
                              label = interactive[[label]],
                              choices = unique(column_data),
                              selected = if(is_empty(value)) unique(column_data) else value,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `selected-text-format` = "count > 3"),
                              width = "200px"
            )
          } else if(interactive[[ellab]] == "sliderInput"){
            el <- shiny::sliderInput(ns(id_interactive[i]),
                              label = interactive[[label]],
                              min = min(column_data, na.rm=TRUE),
                              max = max(column_data, na.rm=TRUE),
                              value = if(is_empty(value))c(min(column_data, na.rm=TRUE), max(column_data, na.rm=TRUE)) else value,
                              width = "200px"
            )
          } else if(interactive[[ellab]] == "dateRangeInput") {
            el <- shiny::dateRangeInput(ns(id_interactive[i]),
                              label = interactive[[label]],
                              min = min(column_data, na.rm=TRUE),
                              max = max(column_data, na.rm=TRUE),
                              start = if(is_empty(value))min(column_data, na.rm=TRUE) else value[1],
                              end = if(is_empty(value))max(column_data, na.rm=TRUE) else value[2],
                              width = "200px"
            )
          }
          
          
        return(el)
        
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
      
      widget_ui(id_container, id_plot, id_closebutton, id_editbutton, id_interactive,
                interactive = plot_settings[[id_container]]$interactive,
                interactive_vals = plot_settings[[id_container]]$interactive_vals,
                data = dataset)
    )
    
    observe({
      interactive_vals <- list(input[[id_interactive[1]]], 
                             input[[id_interactive[2]]])
      
      plot_settings[[id_container]]$interactive_vals <<- interactive_vals
      
    })
    
    
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
      
      
      # Panel 1 - Start
      updateSelectInput(session, "select_dataset", selected = a$dataset)
      
      # Make sure to load data first; reactive current_dataset() has not updated yet.
      current_columns <- names(get(a$dataset))
      
      updateSelectInput(session, "plot_type", selected = a$plottype)
      updateAwesomeRadio(session,  "bar_position", selected = a$bar_position)
      updateSelectInput(session, "scatter_shape", selected = a$shape)
      updateCheckboxInput(session, "pienarm", value = as.logical(a$pienarm))
      updateSelectInput(session, "pietype", selected = a$pietype)
      
      # Panel 2 - Columns
      updateSelectInput(session, "plot_xvar", choices = current_columns, selected = a$xvar)
      updateSelectInput(session, "plot_yvar", choices = current_columns, selected = a$yvar)
      updateCheckboxInput(session, "chk_factor_x", value = as.logical(a$factor_x))
      updateCheckboxInput(session, "chk_factor_y", value = as.logical(a$factor_y))
      
      updateSelectInput(session, "plot_stat", selected = a$statfun)
      updateCheckboxInput(session, "chk_usegroup", value = as.logical(a$usegroup))
      updateSelectInput(session, "plot_groupvar", choices = current_columns, selected = a$groupvar)
      
      if(a$groupvar == ""){
        updateCheckboxInput(session, "chk_usegroup",value = FALSE)
      }
      
      # Panel 3 - Filters
      update_filter <- function(id,i,type){
        
        if(length(a$filters[[i]])){
          if(type == "numeric"){
            updateNumericInput(session, id, value = a$filters[[i]])
          } else if(type == "factor"){
            updateSelectInput(session, id, selected = a$filters[[i]])
          } else if(type == "date"){
            updateDateRangeInput(session, id, start = a$filters[[i]][1], end = a$filters[[i]][2])
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
      
      # Panel 4 - Interactive
      update_interactive_panel <- function(i, a, session){
        if(!is.null(a$interactive[[glue("element{i}")]])){
          shinyjs::show(glue("interactive_panel_{i}"))
          shinyjs::show(glue("ia_select_input{i}"))
          updateSelectInput(session, glue("ia_select_input{i}"), 
                            selected = a$interactive[[glue("element{i}")]])
          shinyjs::show(glue("ia_select_variable_box{i}"))
          updateSelectInput(session, glue("ia_select_variable{i}"), 
                            choices = current_columns, 
                            selected = a$interactive[[glue("variable{i}")]])
          updateTextInput(session, glue("ia_element_label{i}"), 
                          value = a$interactive[[glue("label{i}")]])
        }
      }
      
      updateAwesomeRadio(session, "ia_select_nelements", selected = as.character(a$interactive$nelements))
      update_interactive_panel(1, a, session)
      update_interactive_panel(2, a, session)
         
      # Panel 5 - Labels
      updateTextInput(session, "plot_title", value = null_to_empty(a$title))
      updateTextInput(session, "plot_subtitle", value = null_to_empty(a$subtitle))
      updateTextInput(session, "plot_xlab", value = a$xlab)
      updateTextInput(session, "plot_ylab", value = a$ylab)
      updateTextInput(session, "plot_glab", value = a$glab)
      updateNumericInput(session, "num_labelsize", value = a$labelsize)
      updateNumericInput(session, "num_labelmargin", value = a$labelmargin)
      updateCheckboxInput(session, "chk_includezerox", value = as.logical(a$includezerox))
      updateCheckboxInput(session, "chk_includezeroy", value = as.logical(a$includezeroy))
      updateSelectInput(session, "sel_labelanglex", selected = a$labelanglex)
      updateSelectInput(session, "sel_labelangley", selected = a$labelangley)
      updateCheckboxInput(session, "chk_removelabelsx", value = as.logical(a$nolabelsx))
      updateCheckboxInput(session, "chk_nolegend", value = as.logical(a$nolegend))
      
      # Panel 6 - Colors
      for(i in 1:12){
        updateColourInput(session, paste0("sel_color",i), value = a$palette[i])
      }
      
      # Panel 7 - Theme
      updateSelectInput(session, "select_theme", selected = a$theme)
      
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


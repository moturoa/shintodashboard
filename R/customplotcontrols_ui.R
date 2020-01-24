#' Shinto dashboard maker, UI function
#' @export
customplotcontrolsUI <- function(id, args = NULL, data_key, datasets){
  
  ns <- NS(id)
  
  color_palettes <- c(tools::file_path_sans_ext(dir("cache/palettes", pattern = "[.]json$")), 
                      "rich.colors", rownames(brewer.pal.info))
  
  if(!is.null(args)){
    dataset <- datasets[[args$dataset]]  
  } else {
    dataset <- NULL
  }
  
  
  make_default <- function(x, default = ""){
    val <- args[[x]]
    if(is.null(val) || is.na(val)){
      default
    } else {
      val
    }
  }
  
  out <- fluidPage(
    fluidRow(

            tabBox( id = ns("controls_tab_box"), width = 12, 
                tabPanel("1. Start", value = "start",
                
                     tagList(
                       selectInput(ns("select_dataset"), 
                                   label_tooltip("Dataset", 
                                                 "Selecteer een dataset."),
                                   width = 300,
                                   choices = data_key,
                                   selected = make_default("dataset", NULL)
                                   ),
                       
                       
                       selectInput(ns("plot_type"), 
                                   label_tooltip("Plot type", "Selecteer een van de beschikbare plot types."),
                                   width = 300,
                                   choices = c("Barplot", "Scatter", "Pie chart"),
                                   selected = make_default("plottype", "Barplot")),
                       
                       shinyjs::hidden(
                         
                         awesomeRadio(ns("bar_position"), 
                                      label_tooltip("Layout van deel bars",
                                                    "Voor een barplot, de positie van de delen: boven op elkaar (stacked) of naast elkaar"),
                                      choices = c("Stacked","Grouped"),
                                      selected = make_default("bar_position", "Stacked"), 
                                      inline = TRUE)
                         
                       ),
                       
                       shinyjs::hidden(
                         
                         awesomeRadio(ns("scatter_shape"), 
                                      label_tooltip("Plot symbool",
                                                    "Soort markers voor de scatter plot"),
                                      choices = c("circles","squares"),
                                      selected = make_default("shape","circles"),
                                      inline = TRUE)
                         
                       ),
                       
                       shinyjs::hidden(
                         awesomeRadio(ns("pietype"), 
                                      label_tooltip("Pie chart type", 
                                                    "Soort pie chart. Alleen Pie mogelijk op het moment."),
                                      choices = c("Pie","Waffle"),
                                      selected = make_default("pietype", "Pie"),
                                      inline = TRUE)
                       ),
                       shinyjs::hidden(
                         checkboxInput(ns("pienarm"), 
                                       "Missende waarden weghalen.", 
                                       value = make_default("pienarm", FALSE))
                         
                       )
                     )
                       
                ),       
                tabPanel("2. Kolommen", value = "kolommen",
                     
                         
                     tagList(
                       
                       side_by_side(
                         selectInput(ns("plot_xvar"), 
                                     label = label_tooltip("X Variabele", 
                                                           "Selecteer variabele die langs de X-as wordt geplot"),
                                     choices = if(!is.null(args$dataset))names(dataset) else NULL, 
                                     selected = make_default("xvar", NULL),
                                     width = 300),
                         checkboxInput(ns("chk_factor_x"), 
                                       "Maak factor", 
                                       value = make_default("factor_x", FALSE), 
                                       width = "50px")
                       ),
                       tags$br(),
                       
                       tags$div(id = ns("yvar_box"),
                                side_by_side(
                                  selectInput(ns("plot_yvar"), 
                                              label = label_tooltip("Y Variabele", 
                                                                    "Selecteer variabele die langs de Y-as wordt geplot"),
                                              choices = if(!is.null(args$dataset))names(dataset) else NULL, 
                                              selected = make_default("yvar", NULL),
                                              width = 300),
                                  checkboxInput(ns("chk_factor_y"), 
                                                "Maak factor",
                                                value = make_default("factor_y", FALSE),
                                                width = "50px")
                                )
                       ),
                       shinyjs::hidden(
                         selectInput(ns("plot_stat"), 
                                     label_tooltip("Functie", "De functie om de data in groepen samen te vatten."),
                                     width = 300,
                                     choices = c("Sum van Y" = "sum", 
                                                 "Gemiddelde van Y" = "mean",
                                                 "Tel aantal rijen in X" = "count"),
                                     selected = make_default("statfun", "sum"))
                       ),
                       
                       tags$br(),
                       checkboxInput(ns("chk_usegroup"), 
                                     label_tooltip("Gebruik groep variabele", 
                                                   "Voeg een 3e kolom toe, die de data in groepen verdeeld"),
                                     value = make_default("usegroup", FALSE)),
                       
                       shinyjs::hidden(
                         selectInput(ns("plot_groupvar"), 
                                     label = label_tooltip("Groep variabele", 
                                                           "Selecteer de kolom die de kleuren in de bar delen aangeeft."),
                                     width = 300,
                                     choices = if(!is.null(args$dataset))names(dataset) else NULL,
                                     selected = make_default("groupvar", NULL))
                       )
                     )
                       
                ),
                
                tabPanel("3. Filter", value = "filter",

                         tagList(
                           tags$p("Selecteer het aantal data filters."),
                           tags$p("Deze filters worden eenmalig op de data toegepast, en zijn niet interactief."),
                           awesomeRadio(ns("il_select_nelements"),
                                        "Aantal data filters",
                                        choices = c("0","1","2"),
                                        selected = if(!is.null(args$filters))as.character(length(args$filters)) else "0",
                                        inline = TRUE),
                           
                           shinyjs::hidden(
                             datafilter_panel(1, ns, 
                                              columns = if(!is.null(args$dataset))names(dataset) else NULL, 
                                              data = dataset,
                                              args = args$filters)
                           ),
                           shinyjs::hidden(
                             datafilter_panel(2, ns,
                                              columns = if(!is.null(args$dataset))names(dataset) else NULL,
                                              data = dataset,
                                              args = args$filters)
                           )
                         )

                ),
                
                tabPanel("3. Interactief", value = "interactief",
                         
                     tagList(
                       tags$p("Selecteer het aantal interactieve filters voor de plot."),
                       awesomeRadio(ns("ia_select_nelements"),
                                    label_tooltip("Aantal interactieve elementen",
                                                  "Voeg hier (optioneel) interactieve elementen toe aan de plot"),
                                    choices = c("0","1","2"),
                                    selected = if(!is.null(args$interactive$nelements))as.character(args$interactive$nelements) else "0",
                                    inline = TRUE),
                       
                       shinyjs::hidden(
                         interactive_panel(1, ns, 
                                           columns = if(!is.null(args$dataset))names(dataset) else NULL, 
                                           args = args$interactive)
                       ),
                       shinyjs::hidden(
                         interactive_panel(2, ns,
                                           columns = if(!is.null(args$dataset))names(dataset) else NULL,
                                           args = args$interactive)
                       )
                     )
                       
                ),
                tabPanel("4. Labels", value = "labels",
                         
                         fluidRow(    
                           column(4,    
                                  textInput(ns("plot_title"), "Titel", value = make_default("title", "")),
                                  textInput(ns("plot_subtitle"), "Sub-titel", value = make_default("subtitle", "")),
                                  textInput(ns("plot_xlab"), "X-as label", value = make_default("xlab", "")),
                                  textInput(ns("plot_ylab"), "Y-as label", value = make_default("ylab", "")),
                                  textInput(ns("plot_glab"), label_tooltip("Groep label", "Titel voor de legenda"),
                                            value = make_default("glab", ""))
                                  
                           ),
                           column(4, 
                                  side_by_side(
                                    numericInput(ns("num_labelsize"), 
                                                 "Tekst grootte",
                                                 min =8, max=20, 
                                                 value = make_default("labelsize", 12), 
                                                 width = "148px"),
                                    
                                    numericInput(ns("num_labelmargin"),
                                                 label_tooltip("Label marge","Ruimte tussen de as en de labels."),
                                                 min = 0, max=10, 
                                                 value = make_default("labelmargin", 2), 
                                                 width = "148px")
                                  ),
                                  side_by_side(
                                    selectInput(ns("sel_labelanglex"), 
                                                "X-as label rotatie",
                                                choices = c(0,90), 
                                                selected = make_default("labelanglex", 0),
                                                width = "148px"),
                                    selectInput(ns("sel_labelangley"), 
                                                label_tooltip("Y-as label rotatie",
                                                              "Rotatie voor de labels naast de as."),
                                                choices = c(0,90), 
                                                selected = make_default("labelangley", 0),
                                                width = "148px")
                                  ),
                                  br(),
                                  side_by_side(vertical_align = TRUE,
                                               checkboxInput(ns("chk_removelabelsx"), 
                                                             "Geen X-as labels", 
                                                             value = make_default("nolabelsx", FALSE),
                                                             width="60px"),
                                               tags$div(style = "width: 30px;"),
                                               checkboxInput(ns("chk_nolegend"), 
                                                             "Geen legenda", 
                                                             value = make_default("nolegend", FALSE),
                                                             width="60px")
                                  ), 
                                  tags$br()
                           ),
                           column(4, 
                                  checkboxInput(ns("chk_includezerox"), 
                                                label_tooltip("X - begin bij 0", "Start X-as bij nul"),
                                                value = make_default("includezerox", FALSE)),
                                  checkboxInput(ns("chk_includezeroy"), 
                                                label_tooltip("Y - begin bij 0", "Start Y-as bij nul"),
                                                value = make_default("includezeroy", FALSE))
                           )
                         )
                     
                ),
                

                tabPanel("5. Annotatie", value = "annotatie",
                      
                         tagList(
                           shinyjs::hidden(
                             tags$div(id = ns("barannotation_controls"),
                                      tags$h4("Annotatie voor bars"),
                                      checkboxInput(ns("check_annotate_bars"), 
                                                    label_tooltip("Label totalen boven de bars", 
                                                                  "Voegt een label toe per bar met de totale waarde"),
                                                    value = make_default("annotate_bars", FALSE)),
                                      tags$hr()
                             )
                           ),
                           
                           tags$h4("Rechte lijnen"),
                           selectInput(ns("select_annotation"),
                                       "Type",
                                       width = 300,
                                       choices = c("Geen" = "None", 
                                                   "Horizontale lijn" = "Horizontal line",
                                                   "Verticale lijn" = "Vertical line"),
                                       selected = make_default("annotation_type", "None")
                                       ),
                           shinyjs::hidden(
                             tags$div(id = ns("abline_controls"),
                                      numericInput(ns("num_line_coordinate"), "X-as waarde:", 
                                                   value = make_default("line_coordinate", 0), 
                                                   width = 300),
                                      tags$div(style = "width: 200px;",
                                               colourpicker::colourInput(ns("colour_annotation"), 
                                                                         "Kleur", 
                                                                         value = make_default("line_colour","red"))
                                      )
                             )
                           )
                         )
                         
                ),
                tabPanel("6. Kleuren", value = "kleuren",
                         
                     fluidRow(
                       column(8,
                              
                              side_by_side(
                                #checkboxInput(ns("chk_colorbrewer"), "", value = TRUE, width = "60px"), 
                                selectInput(ns("select_palette"), 
                                            "Palet",
                                            choices = color_palettes, 
                                            selected = "rich.colors", 
                                            width = 300)
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
                                    colourpicker::colourInput(ns(paste0("sel_color",i)), as.character(i), 
                                                value = if(is.null(args$palette))gplots::rich.colors(12)[i] else args$palette[i])
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
                      
                )
 
                # tabPanel(tagList(icon("play"), "Voltooien"), value = "voltooien",
                #          
                #      tags$p("Maak de plot aan volgens de huidige instellingen.",
                #             "De plot wordt op het dashboard geplaatst."),
                #      tags$br(),
                #      actionButton(ns("btn_addplot"), 
                #                   label_tooltip("Plot maken","Voeg huidige plot toe aan dashboard."), 
                #                   class = "btn btn-primary", 
                #                   icon = icon("plus", lib = "glyphicon")),
                #      shinyjs::hidden(
                #        actionButton(ns("btn_updateplot"), 
                #                     label_tooltip("Plot updaten", "Geselecteerde plot updaten."), 
                #                     class = "btn btn-primary", 
                #                     icon = icon("refresh", lib = "glyphicon"))
                #      )
                # ),
                
                
                
              )
              
    
            
      )
      
  )
  
attachDependencies(out)
   
}



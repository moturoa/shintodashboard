
customplotUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(class = "col-sm-4", id = id,
           tags$div(class = "box cpbox",
                    tags$div(class = "box-body",
                             actionButton(ns("btn_close"), label=HTML("&times;"), class="plotbutton"),
                             actionButton(ns("btn_edit"), label="", icon=icon("edit"), class="plotbutton"),
                             actionButton(ns("btn_download"), label="", icon=icon("download"), class="plotbutton"),
                             plotOutput(ns("customplot_out"))
                    )
           )
  )
  
}


customplot <- function(input, output, session, this_id, 
                       plot_arguments
){
  
  output$customplot_out <- renderPlot({
    
    a <- plot_arguments
    if(a$plottype == "Scatter"){
      p <- scatter_plot(automobiles, 
                        a$xvar, 
                        a$yvar, 
                        a$groupvar, 
                        xlab=a$xlab, 
                        ylab=a$ylab, 
                        glab=a$glab)
      print(p)
    }
    if(a$plottype == "Barplot"){
      p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                           statfun=a$statfun,
                           xlab=a$xlab, 
                           ylab=a$ylab, glab=a$glab)
      print(p)
    }
    if(a$plottype == "Stacked barplot"){
      p <- grouped_barplot(automobiles, a$xvar, a$yvar, a$groupvar, 
                           statfun=a$statfun,
                           xlab=a$xlab, ylab=a$ylab, glab=a$glab,
                           position = "stacked")
      print(p)
    }
    
    
  }, width = 380, height = 280)
  
  observeEvent(input$btn_close, {
    
    removeUI(selector = paste0("#", this_id), session = session)
    
  })
  
  observeEvent(input$btn_edit, {
    
    removeUI(selector = paste0("#", this_id), session = session)
    insertUI(
      "#placeholder", where = "beforeEnd",
      ui = customplotUI(this_id)
    )
    
    
  }) 
}


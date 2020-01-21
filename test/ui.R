
dashboardPagePlus (
  header = dashboardHeaderPlus(
                  title = tagList(
                    span(class = "logo-lg logo-smaller", "Shinto App"), 
                    icon("home")
                  ),
                  enable_rightsidebar = TRUE,
                  
                  # Logout Menu (werkt niet lokaal, alleen op Shiny Server Pro)
                  tags$li(a(href = '__logout__', style="height:50px", 
                            icon("user-circle-o"),
                            #img(src = get_avatar(), class="user-image"),
                            title = "Logout"),
                            class = "dropdown user-menu")
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dashboard", icon = icon("chart-line"), tabName = "tabDashboard"), 
                menuItem("Configuratie", icon = icon("cog"), tabName = "tabConfigure")
                )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    
    useShinyjs(),
    #useShinyalert(),

    includeCSS("www/style.css"),
    includeCSS("www/buttons.css"),
    includeCSS("www/fleetingMessage.css"),
    
    includeScript("www/tooltip.js"),
    includeScript("www/plotorder.js"),
    
    tabItems(
      tabItem(tabName = "tabDashboard",

        fluidRow(
          jqui_sortable(
            div(id="placeholder"), 
            options = list(opacity = 0.5)
          )
        )  
              
      ),

      tabItem(tabName = "tabConfigure",
        
        fluidRow(
          customplotcontrolsUI("controls")
        )  
        
      )

    ),
    tags$div(id = "msgplaceholder")
  ),
  
  rightsidebar = rightSidebar(
      rightSidebarTabContent(
        id = 1,
        active = TRUE,
        icon = "heart",
        title = "Favorieten",
        tags$p("Klik op pand om te inspecteren"),
        DTOutput("dt_favorieten"),
        tags$hr(),
        actionButton("dt_favorieten_wis", "Wissen", 
                     icon = icon("remove-circle", lib="glyphicon"))
      ),
      background = "light"
      
  ) 
  
)

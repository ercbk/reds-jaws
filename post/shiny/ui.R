# ui

# rsconnect::deployApp(appName="jaws4-post")

# url: https://erbo.shinyapps.io/jaws4-post


library(shiny)
library(shinydashboard)
library(DT)
library(ggiraph)
library(shinycssloaders)

options(spinner.color="#C6011F")

dashHeader <- dashboardHeader(
      title="Evaluating Nominees for the Reds HOF",
      titleWidth = 375
)



dashSidebar <- dashboardSidebar(
      sidebarMenu(
            menuItem(
                  text = "JAWS-4",
                  tabName = "jawsTab",
                  icon = icon(name = "line-chart")
            )
      )
      
)


dashBody <- dashboardBody(
      
      # Allows for putting app in a webpage (part 1)
      tags$head(
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript")
      ),
      
      # Changes color of header
      tags$head(
            tags$style(HTML('
                            /* Changes color of title portion of header */
                            .skin-blue .main-header .logo {
                            background-color: #C6011F;
                            }
                            
                            .skin-blue .main-header .logo:hover {
                            background-color: #C6011F;
                            }
                            
                            /* Changes color of rest of header */
                            .skin-blue .main-header .navbar {
                            background-color: #C6011F;
                            }
                            
                            /* Changes color of sidebar toggle when hovered */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #000000;
                            }
                            
                            '))
            
            ),
      
      # Stops errors being displayed in plot windows
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      
      
      tabItem(
            tabName = 'jawsTab',
            fluidRow(
                  box(
                        width = 6,
                        
                        textInput(
                              inputId = 'jplayer',
                              label = 'Enter Player Name',
                              value = 'Johnny Bench'
                        ),
                        DTOutput(outputId = 'jTable')
                        
                  ),
                  
                  box(
                        width = 6,
                        
                        box(
                              width = 6,
                              withSpinner(plotOutput(outputId = 'jawsCleve'), type=1)
                        ),
                        box(
                              width = 6,
                              withSpinner(plotOutput(outputId = 'warCleve'), type=1)
                        ),
                        
                        box(
                              width = 12,
                              height = 380,
                              withSpinner(ggiraphOutput(outputId = 'lineChart'), type=1)
                        )
                        
                  )
            )
      ),
      
      # Allows for putting app in a webpage (part 2)
      HTML('<div data-iframe-height></div>')
            )


dashboardPage(
      header = dashHeader,
      sidebar = dashSidebar,
      body = dashBody
)
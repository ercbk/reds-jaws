# ui 
# rsconnect::deployApp(appName="jaws4")


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
                  text = "Home",
                  tabName = "homeTab",
                  icon = icon("home"),
                  badgeLabel = "2018 Class",
                  badgeColor = "light-blue"
            ),
            menuItem(
                  text = "JAWS-4",
                  tabName = "jawsTab",
                  icon = icon(name = "line-chart")
            ),
            menuItem(
                  text = "Profile",
                  tabName = "profTab",
                  icon = icon("folder-open-o")
            ),
            menuItem(
                  text = "Stat Rank",
                  tabName = "rankTab",
                  icon = icon("percent")
            ),
            menuItem(
                  text = "Numbers",
                  tabName = "numTab",
                  icon = icon("calculator"),
                  menuSubItem(
                        text = "Batting",
                        tabName = "numBatTab"
                  ),
                  menuSubItem(
                        text = "Pitching",
                        tabName = "numPitTab"
                  )
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
      
      
      tabItems(
            tabItem(
                  tabName = 'homeTab',
                  fluidRow(
                        box(
                              width = 8,
                              DTOutput(outputId = 'hTable')
                        ),
                        box(
                              width = 4,
                              plotOutput(outputId = 'ridge')
                        )
                  )
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
      
            
            tabItem(
                  tabName = 'profTab',
                  fluidRow(
                        tabBox(
                              width = 12,
                              tabPanel(
                                    title = 'Batting',
                                    box(
                                          width = 7,
                                          DTOutput(outputId = 'prof_bat_Table'),
                                          DTOutput(outputId = 'prof_field_Table'),
                                          DTOutput(outputId = 'prof_psb_Table')
                                    ),
                                    
                                    box(
                                          width = 5,
                                          textInput(
                                                inputId = 'prof_bat_player',
                                                label = 'Enter Player Name',
                                                value = 'Ken Griffey Jr'
                                          ),
                                          withSpinner(plotOutput(outputId = 'bat_dev'), type=1),
                                          DTOutput(outputId = 'prof_awab_Table')
                                    )
                              ),
                              tabPanel(
                                    title = 'Pitching',
                                    box(
                                          width = 7,
                                          DTOutput(outputId = 'prof_pit_Table'),
                                          DTOutput(outputId = 'prof_psp_Table'),
                                          DTOutput(outputId = 'prof_awap_Table')
                                    ),
                                    box(
                                          width = 5,
                                          textInput(
                                                inputId = 'prof_pit_player',
                                                label = 'Enter Player Name',
                                                value = 'Jose Rijo'
                                          ),
                                          withSpinner(plotOutput(outputId = 'pit_dev'), type=1)
                                    )
                              )
                        )
                  )
            ),
            
            tabItem(
                  tabName = 'rankTab',
                  fluidRow(
                        tabBox(
                              width = 12,
                              tabPanel(
                                    title = 'Batting',
                                    box(
                                          width = 6,
                                          
                                          DTOutput(outputId = 'sr_hof_bTable')
                                          
                                          
                                    ),
                                    box(
                                          width = 6,
                                          box(
                                                width = 6,
                                                textInput(
                                                      inputId = 'r_bat_player',
                                                      label = 'Enter Player Name',
                                                      value = 'Eric Davis'
                                                ),
                                                htmlOutput("hof_bvalue_box"),
                                                withSpinner(plotOutput(outputId = 'hof_bat_dens'), type=1)
                                          ),
                                          box(
                                                width = 6,
                                                textInput(
                                                      inputId = 'r_bat_stat',
                                                      label = 'Enter Statistic',
                                                      value = 'wRC+'
                                                ),
                                                
                                                htmlOutput("fran_bvalue_box"),
                                                withSpinner(plotOutput(outputId = 'fran_bat_dens'), type=1)
                                                
                                          )
                                    ),
                                    DTOutput(outputId = 'sr_fran_bTable')
                              ),
                              
                              tabPanel(
                                    title = 'Pitching',
                                    box(
                                          width = 6,
                                          
                                          DTOutput(outputId = 'sr_hof_pTable')
                                          
                                    ),
                                    
                                    box(
                                          width = 6,
                                          box(
                                                width = 6,
                                                textInput(
                                                      inputId = 'r_pit_player',
                                                      label = 'Enter Player Name',
                                                      value = 'Bucky Walters'
                                                ),
                                                htmlOutput("hof_pvalue_box"),
                                                withSpinner(plotOutput(outputId = 'hof_pit_dens'), type=1)
                                          ),
                                          box(
                                                width = 6,
                                                textInput(
                                                      inputId = 'r_pit_stat',
                                                      label = 'Enter Statistic',
                                                      value = 'ERA+'
                                                ),
                                                htmlOutput("fran_pvalue_box"),
                                                withSpinner(plotOutput(outputId = 'fran_pit_dens'), type=1)
                                          )
                                    ),
                                    DTOutput(outputId = 'sr_fran_pTable')
                              )
                              
                        )
                        
                  )
            ),
            
            tabItem(
                  tabName = 'numBatTab',
                  fluidRow(
                        tabBox(
                              width = 12,
                              tabPanel(
                                    title = 'Tenure',
                                    DTOutput(outputId = 'numBatTenTab')
                              ),
                              tabPanel(
                                    title = 'Season',
                                    DTOutput(outputId = 'numBatSeasTab')
                              ),
                              tabPanel(
                                    title = 'Fielding',
                                    DTOutput(outputId = 'numBatFldTab')
                              ),
                              tabPanel(
                                    title = 'Postseason',
                                    DTOutput(outputId = 'numBatPsTab')
                              ),
                              tabPanel(
                                    title = 'Awards',
                                    DTOutput(outputId = 'numBatAwaTab')
                              ),
                              tabPanel(
                                    title = 'Award Shares',
                                    DTOutput(outputId = 'numBatAsTab')
                              )
                        )
                  )
            ),
            tabItem(
                  tabName = 'numPitTab',
                  fluidRow(
                        tabBox(
                              width = 12,
                              tabPanel(
                                    title = 'Tenure',
                                    DTOutput(outputId = 'numPitTenTab')
                              ),
                              tabPanel(
                                    title = "Season",
                                    DTOutput(outputId = 'numPitSeasTab')
                              ),
                              tabPanel(
                                    title = "Postseason",
                                    DTOutput(outputId = 'numPitPsTab')
                              ),
                              tabPanel(
                                    title = "Awards",
                                    DTOutput(outputId = 'numPitAwaTab')
                              ),
                              tabPanel(
                                    title = "Award Shares",
                                    DTOutput(outputId = 'numPitAsTab')
                              )
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
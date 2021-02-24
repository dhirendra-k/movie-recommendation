## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("By Genre", tabName = "system_1", icon = icon("film"), badgeLabel = "system-1", badgeColor = "green"),
              menuItem("By Rating", tabName = "system_2", icon = icon("star"), badgeLabel = "system-2", badgeColor = "green")
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              
            tabItems(
                  ######## SYSTEM-1 Body ########
                    tabItem("system_1",
                            fluidRow(
                              box(width = 12, title = "Step 1: Select one genre from list", status = "info", solidHeader = TRUE, collapsible = FALSE,
                                  selectInput("genre", "Genre:",
                                              c("Action", "Adventure", "Animation", "Children", 
                                                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                                                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                                                "Sci-Fi", "Thriller", "War", "Western"))
                              )
                            ),
                            fluidRow(
                              useShinyjs(),
                              box(
                                width = 12, status = "info", solidHeader = TRUE,
                                title = "Step 2: Discover movie you might like",
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("btnsystem1", "Click here to get your recommendations", class = "btn-warning")
                                ),
                                br(),
                                tableOutput("system1results")
                              )
                            )
                    ),
                    
                    ######## SYSTEM-2 Body ########
                    tabItem("system_2",
                            fluidRow(
                              box(width = 12, title = "Step 1: Rate as many movie as possible", status = "info", solidHeader = TRUE, collapsed = FALSE, collapsible = TRUE,
                                  div(class = "rateitems",
                                      uiOutput('system2ratings')
                                  )
                              )
                            ),
                            fluidRow(
                              useShinyjs(),
                              box(
                                width = 12, status = "info", solidHeader = TRUE,
                                title = "Step 2: Discover movie you might like",
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("btnsystem2", "Click here to get your recommendations", class = "btn-warning")
                                ),
                                br(),
                                tableOutput("system2results")
                              )
                            )
                      )
                )
          )
    )
) 
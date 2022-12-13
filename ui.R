## ui.R
library(dplyr)
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
        skin="blue",
        dashboardHeader(title="Movie Recommender"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Recommender by Genre", icon=icon("film"), tabName="system_1"),
                menuItem("Recommender by Rating", icon=icon("star"), tabName="system_2")
            )
        ),
        dashboardBody(
            includeCSS("css/books.css"),
            tabItems(
                tabItem(
                    tabName="system_1",
                    fluidRow(
                        box(
                            width=12,
                            title="Step 1: Select your favorite genre",
                            status="info",
                            solidHeader=TRUE,
                            collapsible=TRUE,
                            div(class="genreitems"),
                            selectInput(inputId = "genre",
                                        label = "Select a genre...",
                                        choices = c("Action", "Adventure", "Animation", 
                                                    "Children's", "Comedy", "Crime",
                                                    "Documentary", "Drama", "Fantasy",
                                                    "Film-Noir", "Horror", "Musical", 
                                                    "Mystery", "Romance", "Sci-Fi", 
                                                    "Thriller", "War", "Western"),
                                        selected = "Action")
                        )
                    ),
                    fluidRow(
                        useShinyjs(),
                        box(
                            width=12,
                            status="info",
                            solidHeader=TRUE,
                            title="Step 2: Discover the top highly rated movies",
                            br(),
                            withBusyIndicatorUI(actionButton("system1_btn", "Click here to get your recommendations", class="btn-warning")),
                            br(),
                            tableOutput("system1")
                        )
                    )
                ),
                tabItem(
                    tabName="system_2",
                    fluidRow(
                        box(
                            width=12,
                            title="Step 1: Rate the following movies",
                            status="info",
                            solidHeader=TRUE,
                            collapsible=TRUE,
                            div(class="rateitems", uiOutput('ratings'))
                        )
                    ),
                    fluidRow(
                        useShinyjs(),
                        box(
                            width=12,
                            status="info",
                            solidHeader=TRUE,
                            title="Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(actionButton("btn", "Click here to get your recommendations", class="btn-warning")),
                            br(),
                            tableOutput("results")
                        )
                    )
                )
            )
        )
    )
)

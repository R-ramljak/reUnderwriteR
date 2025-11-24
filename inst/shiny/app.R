# Simple Shiny app for reUnderwriteR
# Includes:
# - Portfolio filtering
# - Portfolio summary
# - Quota share treaty simulation
# - Policy table view

library(shiny)
library(reUnderwriteR)
library(dplyr)
library(DT)

# Load portfolio once at startup
portfolio <- load_cas_portfolio()

ui <- fluidPage(
  titlePanel("reUnderwriteR – Quota Share Demo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Treaty settings"),
      sliderInput(
        inputId = "qs_share",
        label = "Quota share percentage:",
        min = 10,
        max = 70,
        value = 30,
        step = 5,
        post = "%"
      ),
      actionButton(
        inputId = "run_sim",
        label = "Run treaty simulation"
      ),
      hr(),
      
      # ---- Portfolio filtering controls ----
      h4("Portfolio Filters"),
      
      selectInput(
        inputId = "area_filter",
        label = "Area (risk region):",
        choices = sort(unique(portfolio$area)),
        multiple = TRUE,
        selected = unique(portfolio$area)
      ),
      
      sliderInput(
        inputId = "age_filter",
        label = "Driver age range:",
        min = min(portfolio$driv_age, na.rm = TRUE),
        max = max(portfolio$driv_age, na.rm = TRUE),
        value = c(
          min(portfolio$driv_age, na.rm = TRUE),
          max(portfolio$driv_age, na.rm = TRUE)
        )
      ),
      
      hr(),
      h4("Portfolio info"),
      verbatimTextOutput("portfolio_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary",
          h4("Portfolio KPIs"),
          tableOutput("portfolio_summary"),
          h4("Quota share treaty result"),
          tableOutput("treaty_result")
        ),
        tabPanel(
          "Sample policies",
          DTOutput("portfolio_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Filter the portfolio based on UI inputs ----
  filtered_portfolio <- reactive({
    p <- portfolio
    
    # Filter area selection
    if (!is.null(input$area_filter) && length(input$area_filter) > 0) {
      p <- dplyr::filter(p, area %in% input$area_filter)
    }
    
    # Filter driver age range
    p <- dplyr::filter(
      p,
      driv_age >= input$age_filter[1],
      driv_age <= input$age_filter[2]
    )
    
    p
  })
  
  # ---- Portfolio summary based on filtered portfolio ----
  portfolio_summary <- reactive({
    summarise_portfolio(filtered_portfolio())
  })
  
  output$portfolio_info <- renderText({
    ps <- portfolio_summary()
    paste0(
      "Number of policies: ", nrow(filtered_portfolio()), "\n",
      "Total exposure: ", round(ps$total_exposure, 2), "\n",
      "Total incurred loss: ", round(ps$total_incurred, 2), "\n",
      "Implied loss ratio: ", round(ps$loss_ratio, 3)
    )
  })
  
  output$portfolio_summary <- renderTable({
    portfolio_summary() |>
      mutate(
        total_exposure = round(total_exposure, 2),
        total_incurred = round(total_incurred, 2),
        loss_ratio = round(loss_ratio, 3)
      )
  })
  
  # ---- Quota share treaty result ----
  treaty_result <- eventReactive(input$run_sim, {
    share <- input$qs_share / 100
    simulate_quota_share(filtered_portfolio(), share = share) |>
      mutate(
        total_exposure   = round(total_exposure, 2),
        total_incurred   = round(total_incurred, 2),
        loss_ratio       = round(loss_ratio, 3),
        ceded_exposure   = round(ceded_exposure, 2),
        ceded_incurred   = round(ceded_incurred, 2),
        ceded_loss_ratio = round(ceded_loss_ratio, 3),
        qs_share         = paste0(input$qs_share, " %")
      )
  }, ignoreNULL = FALSE)
  
  output$treaty_result <- renderTable({
    tr <- treaty_result()
    
    tr[, c(
      "qs_share",
      "total_exposure", "total_incurred", "loss_ratio",
      "ceded_exposure", "ceded_incurred", "ceded_loss_ratio"
    )]
  })
  
  output$portfolio_table <- renderDT({
    datatable(
      filtered_portfolio() |> 
        select(policy_id, exposure, incurred_loss, area, veh_age, driv_age),
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui = ui, server = server)

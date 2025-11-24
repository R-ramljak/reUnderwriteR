# Simple Shiny app for reUnderwriteR
# - Loads CAS portfolio once at startup
# - Shows basic portfolio metrics
# - Lets the user choose a quota share percentage
# - Displays quota share treaty summary

library(shiny)
library(tidyverse)
library(reUnderwriteR)
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
        post = " %"
      ),
      actionButton(
        inputId = "run_sim",
        label = "Run treaty simulation"
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
  # Pre-compute overall portfolio summary
  portfolio_summary <- summarise_portfolio(portfolio)
  
  output$portfolio_info <- renderText({
    paste0(
      "Number of policies: ", nrow(portfolio), "\n",
      "Total exposure: ", round(portfolio_summary$total_exposure, 2), "\n",
      "Total incurred loss: ", round(portfolio_summary$total_incurred, 2), "\n",
      "Implied loss ratio: ", round(portfolio_summary$loss_ratio, 3)
    )
  })
  
  output$portfolio_summary <- renderTable({
    portfolio_summary |>
      mutate(
        total_exposure = round(total_exposure, 2),
        total_incurred = round(total_incurred, 2),
        loss_ratio = round(loss_ratio, 3)
      )
  })
  
  # Reactive value for treaty result
  treaty_result <- eventReactive(input$run_sim, {
    share <- input$qs_share / 100
    simulate_quota_share(portfolio, share = share) |>
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
    # Reorder columns for nicer display
    tr[, c(
      "qs_share",
      "total_exposure", "total_incurred", "loss_ratio",
      "ceded_exposure", "ceded_incurred", "ceded_loss_ratio"
    )]
  })
  
  output$portfolio_table <- renderDT({
    datatable(
      portfolio |> 
        select(policy_id, exposure, incurred_loss, area, veh_age, driv_age),
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui = ui, server = server)

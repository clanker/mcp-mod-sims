library(shiny)
#library(shinyjs)
#library(labeling)
library(tidyverse)

load("save-2500.Rdata")
load("rmse-data-2500.Rdata")

colors_rgb <- list(
  nbi_pink = c(208, 58, 144),
  nbi_teal = c(11, 186, 181),
  nbi_forest = c(43, 95, 81),
  nbi_crimson = c(153, 51, 102),
  nbi_green = c(6, 142, 137),
  nbi_purple = c(113, 107, 141),
  nbi_orange = c(234, 135, 52),
  nbi_lightteal = c(219, 237, 237),
  nbi_lightpink = c(239, 210, 220),
  nbi_lightorange = c(252, 226, 197)
)

# Convert RGB to Hexadecimal
nbi_colors <- sapply(colors_rgb, function(rgb) {
  rgb <- paste0(sprintf("%02X", rgb))
  paste0("#", paste0(rgb, collapse = ""))
})

candidate_list <- list("linear" = "linear", "quadratic" = "quadratic",
                       "exponential" = "exponential",
                       "Emax-100mg" = "emax1", "Emax-400mg" = "emax2",
                       "sigEmax-250mg" = "sigEmax1", "sigEmax-400mg" = "sigEmax2")

model_list <- c("linear", "quadratic", "exponential", "emax", "sigEmax", "none")

# Define the UI
ui <- fluidPage(
  #shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* CSS styles for increasing text size */
      body {
        font-size: 20px; /* Change the value to adjust the text size */
      }
    "))
  ),
  titlePanel("Exploring the MCP-MOD Dose Selection Methodology"),

  sidebarLayout(
    sidebarPanel(
      style = "background-color: #dbeded; border-color: #2b5f51; padding: 20px;",
      uiOutput("sidebarSingle"),
      conditionalPanel(
        condition = "input.mainPanel == 'dr' | input.mainPanel == 'ds'",
        fluidRow(
          selectInput("sampleSize", "Sample Size (n)", choices = c(120, 240), selected = 120),
          sliderInput("doseNumber", "Number of Doses", min = 4, max = 6, step = 1,
                      value = 5, round = TRUE, ticks = FALSE),
          selectInput("doseProfile", "Dose Profile",
                      choices = list("logarithmic" = "asymmetric",
                                     "linear" = "symmetric"),
                      selected = "asymmetric")
        )
      ),
      conditionalPanel(
        condition = "input.mainPanel == 'comp_wt' | input.mainPanel == 'comp_dose'",
        fluidRow(
          selectInput("facet_row", "Facet rows:", choices = c("n", "Dose profile"),
                      selected = "Dose profile"),
          selectInput("facet_col", "Facet columns:", choices = c("n", "Number of doses"),
                      selected = "n")
        )
      ),
      uiOutput("sidebarComp"),
      div(style = "height:72px"),
      checkboxGroupInput("caseType", "Best Fitting Cases:",
                         choices = candidate_list,
                         selected = candidate_list),
      fluidRow(
        column(5, actionButton("clearAll", "Clear All")),
        column(7, actionButton("selectAll", "Select All"))
      ),
      #textOutput("debugText"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "mainPanel",
                  tabPanel(
                    "Dose-Response Plots",
                    value = "dr",
                    fluidRow(
                      column(10, p()),
                      column(2, actionButton("newIds", "100 New Cases"))
                    ),
                    plotOutput("dosePlot", width = "100%", height = 600),
                    fluidRow(
                      column(4,
                             radioButtons("doseFacet", "Facet display by best model?",
                                          choices = list("Yes" = TRUE, "No" = FALSE),
                                          selected = FALSE,
                                          inline = TRUE),
                             radioButtons("modelDisplay", "Display:",
                                          choices = list("All cases" = "random",
                                                         "By model significance" = "best"),
                                          #"Model p-value" = "pvalue"),
                                          inline = FALSE)
                      ),
                      conditionalPanel(
                        condition = "input.modelDisplay == 'best'",
                        fluidRow(
                          radioButtons("signifModel", "Candidate Model",
                                       choices = candidate_list,
                                       selected = "emax1",
                                       inline = TRUE),
                          radioButtons("pValSig", "Model p-value",
                                       choices = list("Significant" = "sig", "Not significant" = "notsig"),
                                       inline = TRUE)
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "Dose Selection",
                    value = "ds",
                    fluidRow(
                      column(5, plotOutput("boxPlot", height = 600)),
                      column(7, plotOutput("histogramPlot", height = 600))
                    )
                  ),
                  tabPanel(
                    "Comparing Ensemble Weights",
                    value = "comp_wt",
                    plotOutput("compWeights", height = 750)
                  ),
                  tabPanel(
                    "Comparing Target Doses",
                    value = "comp_dose",
                    plotOutput("compTD_fail", height = 300),
                    conditionalPanel(
                      condition = "input.boxplotDisplay == 'dose'",
                      plotOutput("compTD_dose", height = 400)
                    ),
                    conditionalPanel(
                      condition = "input.boxplotDisplay == 'rmse'",
                      plotOutput("compTD_rmse", height = 400)
                    ),
                    radioButtons("boxplotDisplay", "Display:",
                                 choices = list("Calculated target dose distributions" = "dose",
                                                "RMSE of method error (true TD = 400mg)" = "rmse"),
                                 selected = "rmse",
                                 #"Model p-value" = "pvalue"),
                                 inline = TRUE)

                  )
      ),
      width = 9

    )
  )
)


# Define server logic
server <- function(input, output, session) {

  #selections <- reactiveVal(c("linear", "quadratic", "exponential", "emax1", "emax2", "sigEmax1", "sigEmax2"))
  selections <- reactiveVal(candidate_list)

  # random selection of IDs for dose plotting
  random_ids <- reactiveVal(sample(1:2500, 100))

  observeEvent(input$newIds, {
    random_ids(sample(1:2500, 100))
  })

  # Observe event for 'Select All' button
  observeEvent(input$selectAll, {
    # Update the reactive value to include all options
    #selections(c("linear", "quadratic", "exponential", "emax1", "emax2", "sigEmax1", "sigEmax2"))
    selections(candidate_list)
    # Update the checkboxGroupInput
    updateCheckboxGroupInput(session, "caseType",
                             selected = selections())
  })

  # Observe event for 'Clear All' button
  observeEvent(input$clearAll, {
    # Clear the selections
    selections(character(0))
    # Update the checkboxGroupInput
    updateCheckboxGroupInput(session, "caseType",
                             selected = selections())
  })


  output$debugText <- renderText({

    input$mainPanel
  })

  output$dosePlot <- renderPlot({
    if (input$modelDisplay == "best") {
      p1 <- ggplot(data = res_par |>
                     bind_cols(res_sim) |>
                     dplyr::filter(n == input$sampleSize,
                                   K == input$doseNumber,
                                   symmetric == input$doseProfile,
                                   lowest %in% input$caseType) |>
                     dplyr::select(id, best, mu, input$signifModel) |>
                     dplyr::filter_at(
                       input$signifModel,
                       ~ if (input$pValSig == "sig") {.x <= 0.025} else {.x > 0.025}
                     ) |>
                     dplyr::select(-input$signifModel) |>
                     unnest(cols = c(mu)) |>
                     dplyr::mutate(best = factor(best,
                                                 levels = c("linear", "quadratic", "exponential", "emax", "sigEmax", "none"))) |>
                     dplyr::filter(id %in% random_ids()),
                   aes(x = dose, y = mu, group = id, color = best))

    } else {
      p1 <- ggplot(data = res_par |>
                     bind_cols(res_sim |>
                                 dplyr::select(lowest)) |>
                     dplyr::filter(n == input$sampleSize,
                                   K == input$doseNumber,
                                   symmetric == input$doseProfile,
                                   lowest %in% input$caseType) |>
                     dplyr::select(id, best, mu) |>
                     unnest(cols = c(mu)) |>
                     dplyr::mutate(best = factor(best,
                                                 levels = c("linear", "quadratic", "exponential", "emax", "sigEmax", "none"))) |>
                     dplyr::filter(id %in% random_ids()),
                   aes(x = dose, y = mu, group = id, color = best))
    }

    p1 <- p1 +
      geom_line(alpha = 0.6, size = 1.2) +
      theme_bw(base_size = 24) +
      theme(legend.position = "top") +
      guides(color = guide_legend(nrow = 2))

    if (input$doseFacet == TRUE) {
      p1 <- p1 +
        facet_wrap(vars(best))
    }

    p1 +
      ggtitle("Dose-Response Plot") +
      scale_color_manual(values = c("linear" = nbi_colors[[3]],
                                    "quadratic" = nbi_colors[[4]],
                                    "exponential" = nbi_colors[[6]],
                                    "emax" = nbi_colors[[7]],
                                    "sigEmax" = nbi_colors[[5]],
                                    "none" = 'gray25')) +
      labs(x = "dose (mg)",
           y = "group mean (units)",
           color = "best model")
  })


  output$compWeights <- renderPlot({
    p1 <- ggplot(data = res_par |>
                   bind_cols(res_aic) |>
                   bind_cols(res_sim |>
                               dplyr::select(lowest)) |>
                   dplyr::filter(#n == input$sampleSize,
                     #K == input$doseNumber,
                     #symmetric == input$doseProfile,
                     lowest %in% input$caseType) |>
                   dplyr::select(n, K, symmetric, linear:sigEmax) |>
                   dplyr::mutate(n = factor(n, c("120", "240"))) |>
                   dplyr::mutate(K = factor(K, c("4", "5", "6"))) |>
                   dplyr::mutate(symmetric = factor(symmetric, c("symmetric", "asymmetric"))) |>
                   dplyr::mutate(symmetric = forcats::fct_recode(symmetric, "linear" = "symmetric", "logarithmic" = "asymmetric")) |>
                   dplyr::rename("dose profile" = symmetric) |>
                   tidyr::pivot_longer(linear:sigEmax, names_to = "model", values_to = "weight") |>
                   dplyr::mutate(model = factor(model,
                                                levels = c("linear", "quadratic", "exponential", "emax", "sigEmax"))) |>
                   tidyr::replace_na(list(weight = 0)),
                 aes(x = model, y = weight))

    if (input$facet_row == "Dose profile" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = n), position = position_dodge2()) +
        scale_fill_manual(values = list("120" = nbi_colors[[1]], "240" = nbi_colors[[2]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(K))
    } else if (input$facet_row == "Dose profile" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(n))
    } else if (input$facet_row == "n" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = `dose profile`), position = position_dodge2()) +
        scale_fill_manual(values = list("linear" = nbi_colors[[6]], "logarithmic" = nbi_colors[[7]])) +
        facet_grid(rows = vars(n), cols = vars(K))
    } else if (input$facet_row == "n" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(cols = vars(n))
    }

    p1 +
      theme_bw(base_size = 24) +
      #scale_fill_manual(values = list("120" = nbi_colors[[1]], "240" = nbi_colors[[2]])) +
      # scale_color_manual(values = list("linear" = nbi_colors[[3]],
      #                                  "quadratic" = nbi_colors[[4]],
      #                                  "exponential" = nbi_colors[[6]],
      #                                  "emax" = nbi_colors[[7]],
      #                                  "sigEmax" = nbi_colors[[5]])) +
      guides(x = guide_axis(angle = 30)) +
      ggtitle("AIC Ensemble Weights")
  })

  output$compTD_fail <- renderPlot({
    p1 <- ggplot(data = res_par |>
                   bind_cols(res_sim |>
                               dplyr::select(dose_aic:lowest)) |>
                   dplyr::filter(#n == input$sampleSize,
                     #K == input$doseNumber,
                     #symmetric == input$doseProfile,
                     lowest %in% input$caseType) |>
                   dplyr::select(id, n, K, symmetric, dose_aic, dose_best) |>
                   dplyr::mutate(n = factor(n, c("120", "240"))) |>
                   dplyr::mutate(K = factor(K, c("4", "5", "6"))) |>
                   dplyr::mutate(symmetric = factor(symmetric, c("symmetric", "asymmetric"))) |>
                   dplyr::mutate(symmetric = forcats::fct_recode(symmetric, "linear" = "symmetric", "logarithmic" = "asymmetric")) |>
                   dplyr::rename("dose profile" = symmetric) |>
                   dplyr::select(-id, -dose_aic) |>
                   dplyr::group_by(n, K, `dose profile`) |>
                   summarise(missing_prop = mean(is.na(dose_best))),
                 aes(x = missing_prop))

    if (input$facet_row == "Dose profile" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_bar(aes(y = n, fill = n), stat = "identity", position = position_dodge2()) +
        scale_fill_manual(values = list("120" = nbi_colors[[1]], "240" = nbi_colors[[2]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(K)) +
        labs(y = "Study subjects")
    } else if (input$facet_row == "Dose profile" & input$facet_col == "n") {
      p1 <- p1 +
        geom_bar(aes(y = K, fill = K), stat = "identity", position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(n)) +
        labs(y = "Dose groups")
    } else if (input$facet_row == "n" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_bar(aes(y = `dose profile`, fill = `dose profile`), stat = "identity", position = position_dodge2()) +
        scale_fill_manual(values = list("linear" = nbi_colors[[6]], "logarithmic" = nbi_colors[[7]])) +
        facet_grid(rows = vars(n), cols = vars(K)) +
        labs(y = "Dose profile")
    } else if (input$facet_row == "n" & input$facet_col == "n") {
      p1 <- p1 +
        geom_bar(aes(y = K, fill = `dose profile`), stat = "identity", position = position_dodge2()) +
        scale_fill_manual(values = list("linear" = nbi_colors[[6]], "logarithmic" = nbi_colors[[7]])) +
        facet_grid(cols = vars(n)) +
        labs(y = "Dose groups")
    }

    p1 +
      scale_x_continuous(limits = c(0, 1)) +
      theme_bw(base_size = 24) +
      theme(legend.position = "right") +
      ggtitle("Number of MCP-MOD Method Failures") +
      labs(x = "Proportion of MCP-MOD failures")
  })


  output$compTD_dose <- renderPlot({
    p1 <- ggplot(data = res_par |>
                   bind_cols(res_sim |>
                               dplyr::select(dose_aic:lowest)) |>
                   dplyr::filter(#n == input$sampleSize,
                     #K == input$doseNumber,
                     #symmetric == input$doseProfile,
                     lowest %in% input$caseType) |>
                   dplyr::select(id, n, K, symmetric, dose_aic, dose_best) |>
                   dplyr::mutate(n = factor(n, c("120", "240"))) |>
                   dplyr::mutate(K = factor(K, c("4", "5", "6"))) |>
                   dplyr::mutate(symmetric = factor(symmetric, c("symmetric", "asymmetric"))) |>
                   dplyr::mutate(symmetric = forcats::fct_recode(symmetric, "linear" = "symmetric", "logarithmic" = "asymmetric")) |>
                   dplyr::rename("dose profile" = symmetric) |>
                   tidyr::pivot_longer(dose_aic:dose_best, names_to = "method", values_to = "target dose") |>
                   dplyr::mutate(method = forcats::fct_recode(method, "Ensemble" = "dose_aic", "Best fit" = "dose_best")) |>
                   dplyr::mutate(`target dose` = pmin(`target dose`, 1000)) |>
                   dplyr::filter(!is.na(`target dose`)),
                 aes(y = method, x = `target dose`))

    if (input$facet_row == "Dose profile" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = n), position = position_dodge2()) +
        scale_fill_manual(values = list("120" = nbi_colors[[1]], "240" = nbi_colors[[2]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(K))
    } else if (input$facet_row == "Dose profile" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(n))
    } else if (input$facet_row == "n" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = `dose profile`), position = position_dodge2()) +
        scale_fill_manual(values = list("linear" = nbi_colors[[6]], "logarithmic" = nbi_colors[[7]])) +
        facet_grid(rows = vars(n), cols = vars(K))
    } else if (input$facet_row == "n" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(cols = vars(n))
    }

    p1 +
      scale_x_continuous(limits = c(0, 1000)) +
      theme_bw(base_size = 24) +
      theme(legend.position = "right") +
      guides(x = guide_axis(angle = 30)) +
      ggtitle("Target Dose Calculation")
  })

  output$compTD_rmse <- renderPlot({
    p1 <- ggplot(data = res_rmse |>
                   dplyr::select(n, K, symmetric, dose_aic, dose_best) |>
                   dplyr::mutate(n = factor(n, c("120", "240"))) |>
                   dplyr::mutate(K = factor(K, c("4", "5", "6"))) |>
                   dplyr::mutate(symmetric = factor(symmetric, c("symmetric", "asymmetric"))) |>
                   dplyr::mutate(symmetric = forcats::fct_recode(symmetric, "linear" = "symmetric", "logarithmic" = "asymmetric")) |>
                   dplyr::rename("dose profile" = symmetric) |>
                   tidyr::pivot_longer(dose_aic:dose_best, names_to = "method", values_to = "target dose") |>
                   dplyr::mutate(method = forcats::fct_recode(method, "Ensemble" = "dose_aic", "Best fit" = "dose_best")) |>
                   dplyr::mutate(`target dose` = pmin(`target dose`, 1000)) |>
                   dplyr::filter(!is.na(`target dose`)),
                 aes(y = method, x = `target dose`))

    if (input$facet_row == "Dose profile" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = n), position = position_dodge2()) +
        scale_fill_manual(values = list("120" = nbi_colors[[1]], "240" = nbi_colors[[2]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(K))
    } else if (input$facet_row == "Dose profile" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(rows = vars(`dose profile`), cols = vars(n))
    } else if (input$facet_row == "n" & input$facet_col == "Number of doses") {
      p1 <- p1 +
        geom_boxplot(aes(fill = `dose profile`), position = position_dodge2()) +
        scale_fill_manual(values = list("linear" = nbi_colors[[6]], "logarithmic" = nbi_colors[[7]])) +
        facet_grid(rows = vars(n), cols = vars(K))
    } else if (input$facet_row == "n" & input$facet_col == "n") {
      p1 <- p1 +
        geom_boxplot(aes(fill = K), position = position_dodge2()) +
        scale_fill_manual(values = list("4" = nbi_colors[[4]], "5" = nbi_colors[[2]], "6" = nbi_colors[[3]])) +
        facet_grid(cols = vars(n))
    }

    p1 +
      scale_x_continuous(limits = c(220, 270)) +
      theme_bw(base_size = 24) +
      theme(legend.position = "right") +
      guides(x = guide_axis(angle = 0)) +
      ggtitle("Bootstrap RMSE for Calculated Target Dose (Truth = 400mg)")
  })

  # output$compTD_dose <- renderPlot({
  #   ggplot(data = res_par |>
  #            bind_cols(res_sim |>
  #                        dplyr::select(dose_aic:lowest)) |>
  #            dplyr::filter(#n == input$sampleSize,
  #              #K == input$doseNumber,
  #              #symmetric == input$doseProfile,
  #              lowest %in% input$caseType) |>
  #            dplyr::mutate(n = factor(n)) |>
  #            dplyr::select(id, n, K, symmetric, dose_aic, dose_best) |>
  #            dplyr::mutate(symmetric = forcats::fct_recode(symmetric, "linear" = "symmetric", "logarithmic" = "asymmetric")) |>
  #            dplyr::rename("dose profile" = symmetric) |>
  #            tidyr::pivot_longer(dose_aic:dose_best, names_to = "method", values_to = "target dose") |>
  #            dplyr::mutate(`target dose` = pmin(`target dose`, 1000)) |>
  #            tidyr::replace_na(list(`target dose` = -200)),
  #          aes(x = `target dose`, fill = method)) +
  #     #geom_violin() +
  #     #coord_flip()
  #     geom_density(adjust = 1/3, alpha = 1/3)  +
  #     #scale_y_continuous(limits = c(-300, 1000)) +
  #     facet_grid(rows = vars(`dose profile`), cols = vars(K)) +
  #     theme_bw(base_size = 18) +
  #     theme(legend.position = "top") +
  #     ggtitle("Calculated Target Dose")
  # })




  output$boxPlot <- renderPlot({
    ggplot(data = res_par |>
             bind_cols(res_aic) |>
             bind_cols(res_sim |>
                         dplyr::select(lowest)) |>
             dplyr::filter(n == input$sampleSize,
                           K == input$doseNumber,
                           symmetric == input$doseProfile,
                           lowest %in% input$caseType) |>
             dplyr::select(linear:sigEmax) |>
             tidyr::pivot_longer(linear:sigEmax, names_to = "model", values_to = "weight") |>
             dplyr::mutate(model = factor(model,
                                          levels = c("linear", "quadratic", "exponential", "emax", "sigEmax"))) |>
             tidyr::replace_na(list(weight = 0)),
           aes(x = model, y = weight, fill = model)) +
      geom_boxplot(varwidth = TRUE) +
      #coord_flip() +
      theme_bw(base_size = 24) +
      scale_fill_manual(values = c("linear" = nbi_colors[[3]],
                                   "quadratic" = nbi_colors[[4]],
                                   "exponential" = nbi_colors[[6]],
                                   "emax" = nbi_colors[[7]],
                                   "sigEmax" = nbi_colors[[5]],
                                   "none" = 'gray25')) +
      guides(x = guide_axis(angle = 30)) +
      theme(legend.position = "none") +
      ggtitle("AIC Ensemble Weights")
  })

  output$histogramPlot <- renderPlot({
    ggplot(data = res_par |>
             bind_cols(res_sim |>
                         dplyr::select(dose_aic:lowest)) |>
             dplyr::filter(n == input$sampleSize,
                           K == input$doseNumber,
                           symmetric == input$doseProfile,
                           lowest %in% input$caseType) |>
             dplyr::select(id, best, dose_aic, dose_best) |>
             tidyr::pivot_longer(dose_aic:dose_best, names_to = "method", values_to = "target dose") |>
             dplyr::mutate(method = forcats::fct_recode(method, "Ensemble" = "dose_aic", "Best fit" = "dose_best")) |>
             dplyr::mutate(`target dose` = pmin(`target dose`, 1000)),
           aes(x = `target dose`, fill = best)) +
      geom_histogram(binwidth = 100) +
      facet_grid(rows = vars(method)) +
      theme_bw(base_size = 24) +
      theme(legend.position = "top") +
      #scale_fill_manual(values = list("Ensemble" = nbi_colors[[3]], "Best fit" = nbi_colors[[7]])) +
      scale_fill_manual(values = c("linear" = nbi_colors[[3]],
                                   "quadratic" = nbi_colors[[4]],
                                   "exponential" = nbi_colors[[6]],
                                   "emax" = nbi_colors[[7]],
                                   "sigEmax" = nbi_colors[[5]],
                                   "none" = 'gray25')) +
      ggtitle("Calculated Target Dose")
  })

}


# Run the application
shinyApp(ui = ui, server = server)

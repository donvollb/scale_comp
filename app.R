# Response Categories and Variance
# Shiny app illustrating how mapping latent values onto a few response
# categories can change variances. It compares a visual analogue scale (VAS)
# from 0 to 100 with a seven-point and a five-point Likert-type scale.
#
# Run locally: shiny::runApp("app.R")
# Required packages: shiny, shinythemes

library(shiny)
library(shinythemes)

n_occasions <- 50

# ---- Mapping ------------------------------------------------------------------

# Thresholds halfway between the category positions on the 0-1 line
equal_thresholds <- function(k) (seq_len(k - 1) - 0.5) / (k - 1)

# Category widths drawn from a Dirichlet distribution around the equidistant
# widths; higher irregularity gives more unequal widths
random_thresholds <- function(k, irregularity) {
  widths <- c(0.5, rep(1, k - 2), 0.5) / (k - 1)
  g <- rgamma(k, shape = widths * 4 / irregularity^2)
  cumsum(g / sum(g))[-k]
}

to_vas <- function(latent) round(100 * pmin(pmax(latent, 0), 1))
to_likert <- function(latent, thresholds) 1 + findInterval(latent, thresholds)

# Rescaling to the 0-1 interval; categories treated as equally spaced
rescale_vas <- function(y) y / 100
rescale_likert <- function(y, k) (y - 1) / (k - 1)

draw_latent <- function(n, distribution, mean, sd) {
  if (distribution == "normal") {
    rnorm(n, mean, sd)
  } else {
    runif(n, mean - sqrt(3) * sd, mean + sqrt(3) * sd)
  }
}

# Responses in all formats; works for vectors and person-by-occasion matrices
map_formats <- function(latent, thresholds) {
  vas <- to_vas(latent)
  l7 <- to_likert(latent, thresholds$t7)
  l5 <- to_likert(latent, thresholds$t5)
  dim(vas) <- dim(l7) <- dim(l5) <- dim(latent)
  list(
    own = list(latent = latent, vas = vas, l7 = l7, l5 = l5),
    rescaled = list(latent = latent, vas = rescale_vas(vas),
                    l7 = rescale_likert(l7, 7), l5 = rescale_likert(l5, 5)),
    at_ends = c(mean(latent < 0 | latent > 1), mean(vas %in% c(0, 100)),
                mean(l7 %in% c(1, 7)), mean(l5 %in% c(1, 5)))
  )
}

format_names <- c("Latent values (0–1)", "VAS (0–100)",
                  "Seven-point scale (1–7)", "Five-point scale (1–5)")

# ---- Tables -------------------------------------------------------------------

num <- function(x, digits) formatC(x, format = "f", digits = digits)

table_single <- function(m) {
  var_own <- sapply(m$own, var)
  var_01 <- sapply(m$rescaled, var)
  data.frame(
    Format = format_names,
    `Mean (own metric)` = num(sapply(m$own, mean), 2),
    `Variance (own metric)` = num(var_own, 3),
    `Mean (0–1)` = num(sapply(m$rescaled, mean), 3),
    `Variance (0–1)` = num(var_01, 4),
    `Ratio to VAS (0–1)` = num(var_01 / var_01[2], 2),
    `At the ends` = paste0(num(100 * m$at_ends, 1), "%"),
    check.names = FALSE
  )
}

variance_parts <- function(y) {
  within <- mean(apply(y, 1, var))
  between <- var(rowMeans(y))
  c(within = within, between = between, icc = between / (between + within))
}

table_multi <- function(m) {
  own <- sapply(m$own, variance_parts)
  r01 <- sapply(m$rescaled, variance_parts)
  data.frame(
    Format = format_names,
    `Within-person variance (own metric)` = num(own["within", ], 3),
    `Variance of person means (own metric)` = num(own["between", ], 3),
    `Within-person variance (0–1)` = num(r01["within", ], 4),
    `Variance of person means (0–1)` = num(r01["between", ], 4),
    `Within-person ratio to VAS` = num(r01["within", ] / r01["within", 2], 2),
    ICC = num(r01["icc", ], 2),
    `At the ends` = paste0(num(100 * m$at_ends, 1), "%"),
    check.names = FALSE
  )
}

table_thresholds <- function(thresholds) {
  data.frame(
    Scale = c("Seven-point scale", "Five-point scale"),
    `Thresholds on the 0–1 line` = c(paste(num(thresholds$t7, 3), collapse = ", "),
                                          paste(num(thresholds$t5, 3), collapse = ", ")),
    check.names = FALSE
  )
}

# ---- Plots --------------------------------------------------------------------

col_model <- "#cc4778"
col_threshold <- "grey55"
col_persons <- c("#0d0887", "#cc4778", "#f89540")

likert_positions <- function(k) (seq_len(k) - 1) / (k - 1)

# Distribution of responses in each format, shown at their positions on the 0-1 line
plot_single <- function(m, thresholds) {
  old <- par(mfrow = c(3, 1), mar = c(4.1, 4.6, 2.6, 1.1))
  on.exit(par(old))
  n <- length(m$own$vas)

  share_vas <- hist(m$rescaled$vas, breaks = seq(0, 1, 0.05), plot = FALSE)$counts / n
  plot(NA, xlim = c(-0.04, 1.04), ylim = c(0, max(share_vas, 0.05) * 1.15),
       xaxs = "i", yaxs = "i", xaxt = "n", xlab = "",
       ylab = expression(bold(Share)), main = "VAS (bins of 5 points)")
  axis(1, at = seq(0, 1, 0.25), labels = seq(0, 100, 25))
  rect(seq(0, 0.95, 0.05), 0, seq(0.05, 1, 0.05), share_vas, col = col_model,
       border = "white")

  for (k in c(7, 5)) {
    y <- if (k == 7) m$own$l7 else m$own$l5
    thr <- if (k == 7) thresholds$t7 else thresholds$t5
    share <- tabulate(y, k) / n
    pos <- likert_positions(k)
    plot(NA, xlim = c(-0.04, 1.04), ylim = c(0, max(share, 0.05) * 1.15),
         xaxs = "i", yaxs = "i", xaxt = "n",
         xlab = if (k == 5) expression(bold(Response ~ "(at its position on the 0–1 line)")) else "",
         ylab = expression(bold(Share)),
         main = paste0(if (k == 7) "Seven" else "Five",
                       "-point scale (dashed lines: thresholds)"))
    axis(1, at = pos, labels = seq_len(k))
    abline(v = thr, col = col_threshold, lty = 2, lwd = 1.5)
    rect(pos - 0.025, 0, pos + 0.025, share, col = col_model, border = NA)
  }
}

# Responses of three example persons across the occasions
plot_multi <- function(m, thresholds, persons) {
  old <- par(mfrow = c(3, 1), mar = c(4.1, 4.6, 2.6, 1.1))
  on.exit(par(old))
  panels <- list(
    list(y = m$rescaled$vas, main = "VAS", at = seq(0, 1, 0.25),
         labels = seq(0, 100, 25), thr = NULL),
    list(y = m$rescaled$l7, main = "Seven-point scale (dashed lines: thresholds)",
         at = likert_positions(7), labels = 1:7, thr = thresholds$t7),
    list(y = m$rescaled$l5, main = "Five-point scale (dashed lines: thresholds)",
         at = likert_positions(5), labels = 1:5, thr = thresholds$t5)
  )
  for (i in seq_along(panels)) {
    p <- panels[[i]]
    plot(NA, xlim = c(1, n_occasions), ylim = c(-0.04, 1.04), yaxs = "i",
         yaxt = "n", main = p$main, ylab = expression(bold(Response)),
         xlab = if (i == 3) expression(bold(Measurement ~ occasion)) else "")
    axis(2, at = p$at, labels = p$labels, las = 1)
    if (!is.null(p$thr)) abline(h = p$thr, col = col_threshold, lty = 2, lwd = 1.5)
    for (j in seq_along(persons)) {
      lines(seq_len(n_occasions), p$y[persons[j], ], col = col_persons[j],
            type = "o", pch = 16, cex = 0.7, lwd = 1.5)
    }
    if (i == 1) {
      legend("top", horiz = TRUE, bty = "n", col = col_persons, lwd = 1.5,
             pch = 16, legend = paste("Person", seq_along(persons)))
    }
  }
}

# ---- Tabs (module) ------------------------------------------------------------

txt <- list(
  distribution_single = "Distribution of the latent values",
  distribution_multi = "Distribution of the person means",
  mean_single = "Mean of the latent values (0–1 line)",
  sd_single = "Standard deviation of the latent values",
  mean_multi = "Mean of the person means (0–1 line)",
  sd_between = "Standard deviation of the person means",
  sd_within = "Within-person standard deviation (normal distribution around the person mean)",
  n = "Number of persons",
  thresholds = "Thresholds between the categories",
  irregularity = "Irregularity of the category widths"
)

scale_tab_ui <- function(id, title, multi = FALSE) {
  ns <- NS(id)
  latent <- if (multi) {
    list(
      radioButtons(ns("distribution"), txt$distribution_multi,
                   c("Normal" = "normal", "Uniform" = "uniform")),
      sliderInput(ns("mean"), txt$mean_multi, min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput(ns("sd"), txt$sd_between, min = 0, max = 0.5, value = 0.15, step = 0.01),
      sliderInput(ns("sd_within"), txt$sd_within, min = 0.01, max = 0.3, value = 0.1,
                  step = 0.01)
    )
  } else {
    list(
      radioButtons(ns("distribution"), txt$distribution_single,
                   c("Normal" = "normal", "Uniform" = "uniform")),
      sliderInput(ns("mean"), txt$mean_single, min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput(ns("sd"), txt$sd_single, min = 0.01, max = 0.5, value = 0.2, step = 0.01)
    )
  }

  tabPanel(
    title,
    sidebarLayout(
      sidebarPanel(
        h4(tags$b(if (multi) "Persons and Occasions" else "Latent Values")),
        latent,
        sliderInput(ns("n"), txt$n, min = 100, max = 5000, value = 1000, step = 100),
        actionButton(ns("resample"), "Draw a new sample"),
        tags$hr(style = "margin: 20px 0; border: 1px solid gray;"),
        h4(tags$b("Likert-Type Categories")),
        radioButtons(ns("threshold_type"), txt$thresholds,
                     c("Equidistant" = "equidistant", "Random" = "random")),
        conditionalPanel(
          "input.threshold_type == 'random'", ns = ns,
          sliderInput(ns("irregularity"), txt$irregularity, min = 0.1, max = 1,
                      value = 0.5, step = 0.1),
          actionButton(ns("new_thresholds"), "Draw new thresholds")
        )
      ),
      mainPanel(
        plotOutput(ns("plot"), height = "720px"),
        h4(tags$b("Means and Variances"), style = "text-align: center;"),
        tableOutput(ns("variances")),
        tags$p(if (multi) {
          "Within-person variance: variance across a person's 50 responses, averaged across persons. ICC: variance of the person means divided by the sum of both variances. Ratio: within-person variance on the 0–1 line relative to the VAS. At the ends: latent values beyond 0 or 1, VAS responses of 0 or 100, and responses in the lowest or highest category."
        } else {
          "Own metric: VAS from 0 to 100, categories from 1 to 5 or 1 to 7. 0–1: VAS responses divided by 100; category k of a K-point scale rescaled to (k − 1)/(K − 1). Ratio: variance on the 0–1 line relative to the VAS. At the ends: latent values beyond 0 or 1, VAS responses of 0 or 100, and responses in the lowest or highest category."
        }),
        h4(tags$b("Thresholds"), style = "text-align: center;"),
        tableOutput(ns("thresholds"))
      )
    )
  )
}

scale_tab_server <- function(id, multi = FALSE) {
  moduleServer(id, function(input, output, session) {
    # Same sample and thresholds until the buttons are pressed
    sample_seed <- reactiveVal(1)
    observeEvent(input$resample, sample_seed(sample_seed() + 1))
    threshold_seed <- reactiveVal(1)
    observeEvent(input$new_thresholds, threshold_seed(threshold_seed() + 1))

    thresholds <- reactive({
      if (input$threshold_type == "equidistant") {
        list(t7 = equal_thresholds(7), t5 = equal_thresholds(5))
      } else {
        set.seed(10000 + threshold_seed())
        list(t7 = random_thresholds(7, input$irregularity),
             t5 = random_thresholds(5, input$irregularity))
      }
    })

    person_means <- reactive({
      set.seed(sample_seed())
      draw_latent(input$n, input$distribution, input$mean, input$sd)
    })

    latent <- reactive({
      means <- person_means()
      if (!multi) return(means)
      set.seed(5000 + sample_seed())
      means + matrix(rnorm(length(means) * n_occasions, 0, input$sd_within),
                     nrow = length(means))
    })

    mapped <- reactive(map_formats(latent(), thresholds()))

    # Example persons at about the 10th, 50th, and 90th percentile of the means
    persons <- reactive({
      means <- person_means()
      sapply(quantile(means, c(0.1, 0.5, 0.9)), function(q) which.min(abs(means - q)))
    })

    output$plot <- renderPlot({
      if (multi) {
        plot_multi(mapped(), thresholds(), persons())
      } else {
        plot_single(mapped(), thresholds())
      }
    })
    output$variances <- renderTable({
      if (multi) table_multi(mapped()) else table_single(mapped())
    }, align = "l")
    output$thresholds <- renderTable(table_thresholds(thresholds()), align = "l")
  })
}

# ---- App ----------------------------------------------------------------------

ui <- navbarPage(
  title = "Response Categories and Variance: Likert-Type Scales and VAS",
  theme = shinytheme("cerulean"),
  scale_tab_ui("single", "1 Measurement per Person"),
  scale_tab_ui("multi", "50 Measurements per Person", multi = TRUE)
)

server <- function(input, output, session) {
  scale_tab_server("single")
  scale_tab_server("multi", multi = TRUE)
}

shinyApp(ui = ui, server = server)

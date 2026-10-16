library(shiny)
library(bslib)
library(ggplot2)

# ==============================================================================
# 1. GENERATE REPRODUCIBLE MYSTERY DATASETS ON STARTUP
# ==============================================================================
set.seed(2026)

# Mystery Dataset A: True = Normal(mu = 72, sigma^2 = 64), n = 250
data_mystery_1 <- rnorm(250, mean = 72, sd = sqrt(64))

# Mystery Dataset B: True = Gamma(alpha = 4.5, beta = 0.8 [scale]), n = 300
# Note: In R, scale = beta corresponds to rate = 1 / beta
data_mystery_2 <- rgamma(300, shape = 4.5, scale = 0.8)

# Mystery Dataset C: True = Beta(alpha = 2.5, beta = 6.0), n = 250
data_mystery_3 <- rbeta(250, shape1 = 2.5, shape2 = 6.0)


# ==============================================================================
# 2. UI HELPER: SLIDER PAIRED WITH NUMERIC INPUT BOX
# ==============================================================================
sliderAndBoxUI <- function(ns, id, label_html, min, max, value, step) {
  div(
    class = "mb-3",
    tags$label(class = "form-label fw-semibold mb-1", HTML(label_html)),
    div(
      class = "d-flex align-items-center gap-2",
      div(
        style = "flex: 1;",
        sliderInput(ns(id), label = NULL, min = min, max = max, value = value, step = step, width = "100%")
      ),
      div(
        style = "width: 95px; margin-bottom: 15px;",
        numericInput(ns(paste0(id, "_num")), label = NULL, value = value, min = min, max = max, step = step, width = "100%")
      )
    )
  )
}


# ==============================================================================
# 3. SHINY MODULE UI FOR EACH MYSTERY DATASET TAB
# ==============================================================================
mysteryTabUI <- function(id, tab_title, x_min, x_max, step_size, init_interval,
                         var_min, var_max, var_step, var_init) {
  ns <- NS(id)
  
  nav_panel(
    title = tab_title,
    layout_sidebar(
      sidebar = sidebar(
        width = 390,
        
        # Step 1: Select Candidate Distribution
        h5("1. Select Candidate Distribution"),
        selectInput(
          ns("dist"),
          "Choose a distribution family:",
          choices = c(
            "Uniform (a, b)"       = "unif",
            "Exponential (λ)"      = "exp",
            "Gamma (α, β)"         = "gamma",
            "Normal (μ, σ²)"       = "norm",
            "Beta (α, β)"          = "beta"
          ),
          selected = "unif"
        ),
        
        hr(),
        h5("2. Adjust Parameters to Match Density"),
        
        # ----------------------------------------------------------------------
        # CONDITIONAL SLIDERS + NUMERIC BOXES & PARAMETER INTERPRETATIONS
        # ----------------------------------------------------------------------
        
        # --- UNIFORM ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'unif'", ns("dist")),
          sliderAndBoxUI(ns, "unif_min", "Minimum (a):", min = x_min, max = x_max, value = x_min + 0.15 * (x_max - x_min), step = step_size),
          sliderAndBoxUI(ns, "unif_max", "Maximum (b):", min = x_min, max = x_max, value = x_min + 0.85 * (x_max - x_min), step = step_size),
          div(
            class = "alert alert-light border small mt-2 mb-0",
            tags$strong("Parameter Interpretation (Uniform):"),
            tags$ul(
              class = "mb-0 ps-3 mt-1",
              tags$li(HTML("<b>Support:</b> Bounded interval [<i>a</i>, <i>b</i>].")),
              tags$li(HTML("<b>Minimum (<i>a</i>) & Maximum (<i>b</i>):</b> Lower and upper boundary cutoffs. Every outcome between <i>a</i> and <i>b</i> has constant density, with zero density outside the bounds."))
            )
          )
        ),
        
        # --- EXPONENTIAL ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'exp'", ns("dist")),
          sliderAndBoxUI(ns, "exp_rate", "Rate (&lambda;):", min = 0.01, max = 5.0, value = 0.5, step = 0.01),
          div(
            class = "alert alert-light border small mt-2 mb-0",
            tags$strong("Parameter Interpretation (Exponential):"),
            tags$ul(
              class = "mb-0 ps-3 mt-1",
              tags$li(HTML("<b>Support:</b> [0, &infin;) &mdash; strictly positive, with its peak (mode) always at <i>x</i> = 0.")),
              tags$li(HTML("<b>Rate (&lambda;):</b> Average number of events per unit of time or space. Increasing &lambda; pulls the density steeply toward 0 (shorter wait times); decreasing &lambda; stretches the right tail out."))
            )
          )
        ),
        
        # --- GAMMA (Scale Parameterization: beta = scale = 1 / rate in R) ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'gamma'", ns("dist")),
          sliderAndBoxUI(ns, "gamma_shape", "Shape (&alpha;):", min = 0.5, max = 20.0, value = 2.0, step = 0.1),
          sliderAndBoxUI(ns, "gamma_scale", "Scale (&beta;):", min = 0.05, max = 5.0, value = 1.5, step = 0.05),
          div(
            class = "alert alert-light border small mt-2 mb-0",
            tags$strong("Parameter Interpretation (Gamma):"),
            tags$ul(
              class = "mb-0 ps-3 mt-1",
              tags$li(HTML("<b>Support:</b> (0, &infin;) &mdash; models positive, right-skewed continuous quantities.")),
              tags$li(HTML("<b>Shape (&alpha;):</b> Governs skewness and peakedness. When &alpha; &le; 1, the density decreases from 0; when &alpha; &gt; 1, it forms an interior peak that becomes more symmetric as &alpha; grows.")),
              tags$li(HTML("<b>Scale (&beta;):</b> Stretches or compresses the distribution horizontally (note: R's default <code>rate</code> argument is <code>1/&beta;</code>, or <code>scale = &beta;</code>). Increasing &beta; stretches the distribution to the right and lowers the peak; decreasing &beta; compresses it toward 0."))
            )
          )
        ),
        
        # --- NORMAL (Parameterized by Mean mu and Variance sigma^2) ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'norm'", ns("dist")),
          sliderAndBoxUI(ns, "norm_mean", "Mean (&mu;):", min = x_min, max = x_max, value = round((x_min + x_max) / 2, 2), step = step_size),
          sliderAndBoxUI(ns, "norm_var", "Variance (&sigma;<sup>2</sup>):", min = var_min, max = var_max, value = var_init, step = var_step),
          div(
            class = "alert alert-light border small mt-2 mb-0",
            tags$strong("Parameter Interpretation (Normal):"),
            tags$ul(
              class = "mb-0 ps-3 mt-1",
              tags$li(HTML("<b>Support:</b> (&minus;&infin;, &infin;) &mdash; symmetric bell-shaped curve.")),
              tags$li(HTML("<b>Mean (&mu;):</b> Location parameter marking the center of symmetry (peak). Changing &mu; shifts the entire curve left or right without changing its shape.")),
              tags$li(HTML("<b>Variance (&sigma;<sup>2</sup>):</b> Spread parameter controlling horizontal dispersion around &mu; (note: R's <code>dnorm</code>/<code>pnorm</code> take standard deviation <code>sd = &radic;&sigma;<sup>2</sup></code>). Larger &sigma;<sup>2</sup> flattens and widens the bell; smaller &sigma;<sup>2</sup> makes it tall and narrow."))
            )
          )
        ),
        
        # --- BETA ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'beta'", ns("dist")),
          sliderAndBoxUI(ns, "beta_a", "Shape 1 (&alpha;):", min = 0.5, max = 15.0, value = 2.0, step = 0.1),
          sliderAndBoxUI(ns, "beta_b", "Shape 2 (&beta;):", min = 0.5, max = 15.0, value = 2.0, step = 0.1),
          div(
            class = "alert alert-light border small mt-2 mb-0",
            tags$strong("Parameter Interpretation (Beta):"),
            tags$ul(
              class = "mb-0 ps-3 mt-1",
              tags$li(HTML("<b>Support:</b> Strictly bounded on [0, 1] &mdash; models proportions, rates, or probabilities.")),
              tags$li(HTML("<b>Shape 1 (&alpha;) & Shape 2 (&beta;):</b> Control where density weight accumulates between 0 and 1.")),
              tags$li(HTML("When &alpha; &lt; &beta;, the peak pulls toward 0 (right-skewed). When &alpha; &gt; &beta;, the peak pulls toward 1 (left-skewed). When &alpha; = &beta;, the curve is symmetric around 0.5. Increasing both parameters makes the peak narrower."))
            )
          )
        ),
        
        hr(),
        # Step 3: Interval Bounds (Slider + Synced Numeric Boxes for a and b)
        h5("3. Empirical Probability Interval [a, b]"),
        p(class = "text-muted small mb-1", "Drag the slider or enter exact bounds to find the empirical proportion of observations in [a, b]."),
        sliderInput(ns("interval"), label = NULL, min = x_min, max = x_max, value = init_interval, step = step_size, width = "100%"),
        div(
          class = "d-flex gap-2 mt-1",
          div(
            style = "flex: 1;",
            numericInput(ns("int_a"), "Lower Bound (a):", value = init_interval[1], min = x_min, max = x_max, step = step_size, width = "100%")
          ),
          div(
            style = "flex: 1;",
            numericInput(ns("int_b"), "Upper Bound (b):", value = init_interval[2], min = x_min, max = x_max, step = step_size, width = "100%")
          )
        ),
        
        hr(),
        downloadButton(ns("download_csv"), "Download Dataset (.csv)", class = "btn-sm btn-outline-primary w-100")
      ),
      
      # Main Panel Outputs (Empirical Only)
      layout_columns(
        col_widths = c(12, 6, 6),
        card(
          card_header("Empirical Density Histogram vs. Candidate Theoretical PDF"),
          plotOutput(ns("dist_plot"), height = "420px")
        ),
        card(
          card_header("Empirical Sample Summary Statistics"),
          tableOutput(ns("empirical_moments_table"))
        ),
        card(
          card_header("Empirical Interval Probability"),
          tableOutput(ns("empirical_probs_table"))
        )
      )
    )
  )
}


# ==============================================================================
# 4. SHINY MODULE SERVER LOGIC
# ==============================================================================
mysteryTabServer <- function(id, dataset, x_limits, step_size, var_step, file_label) {
  moduleServer(id, function(input, output, session) {
    
    # Helper to two-way sync a slider and its paired numeric input box
    # Preserves exact typed decimals even if they fall between slider steps
    syncPair <- function(slider_id, step_val) {
      num_id <- paste0(slider_id, "_num")
      
      # When slider moves -> update numeric box
      observeEvent(input[[slider_id]], {
        s_val <- input[[slider_id]]
        n_val <- input[[num_id]]
        if (is.na(n_val) || abs(s_val - n_val) > (step_val / 2 + 1e-9)) {
          updateNumericInput(session, num_id, value = s_val)
        }
      }, ignoreInit = TRUE)
      
      # When numeric box changes -> update slider position
      observeEvent(input[[num_id]], {
        n_val <- input[[num_id]]
        s_val <- input[[slider_id]]
        if (!is.null(n_val) && !is.na(n_val) && !isTRUE(all.equal(as.numeric(n_val), as.numeric(s_val)))) {
          updateSliderInput(session, slider_id, value = n_val)
        }
      }, ignoreInit = TRUE)
    }
    
    # Register sync for all parameter pairs
    syncPair("unif_min",    step_size)
    syncPair("unif_max",    step_size)
    syncPair("exp_rate",    0.01)
    syncPair("gamma_shape", 0.1)
    syncPair("gamma_scale", 0.05)
    syncPair("norm_mean",   step_size)
    syncPair("norm_var",    var_step)
    syncPair("beta_a",      0.1)
    syncPair("beta_b",      0.1)
    
    # Sync the 2-way Interval Slider [a, b] with Lower (int_a) and Upper (int_b) boxes
    observeEvent(input$interval, {
      s_int <- input$interval
      a_val <- input$int_a
      b_val <- input$int_b
      if (is.na(a_val) || abs(s_int[1] - a_val) > (step_size / 2 + 1e-9)) {
        updateNumericInput(session, "int_a", value = s_int[1])
      }
      if (is.na(b_val) || abs(s_int[2] - b_val) > (step_size / 2 + 1e-9)) {
        updateNumericInput(session, "int_b", value = s_int[2])
      }
    }, ignoreInit = TRUE)
    
    observeEvent(list(input$int_a, input$int_b), {
      a_val <- input$int_a
      b_val <- input$int_b
      s_int <- input$interval
      if (!is.null(a_val) && !is.na(a_val) && !is.null(b_val) && !is.na(b_val) && a_val <= b_val) {
        if (!isTRUE(all.equal(c(as.numeric(a_val), as.numeric(b_val)), as.numeric(s_int)))) {
          updateSliderInput(session, "interval", value = c(a_val, b_val))
        }
      }
    }, ignoreInit = TRUE)
    
    # Helper to safely read the active numeric value (falling back to slider if box is temporarily blank)
    getParam <- function(slider_id) {
      n_val <- input[[paste0(slider_id, "_num")]]
      if (!is.null(n_val) && !is.na(n_val)) return(as.numeric(n_val))
      as.numeric(input[[slider_id]])
    }
    
    getInterval <- reactive({
      a_val <- if (!is.null(input$int_a) && !is.na(input$int_a)) as.numeric(input$int_a) else input$interval[1]
      b_val <- if (!is.null(input$int_b) && !is.na(input$int_b)) as.numeric(input$int_b) else input$interval[2]
      validate(need(a_val <= b_val, "Lower bound (a) must be less than or equal to Upper bound (b)."))
      c(a_val, b_val)
    })
    
    # Compute candidate PDF curve for plotting ONLY
    curve_info <- reactive({
      dist <- input$dist
      x_grid <- seq(x_limits[1], x_limits[2], length.out = 600)
      
      if (dist == "unif") {
        u_min <- getParam("unif_min")
        u_max <- getParam("unif_max")
        validate(need(u_min < u_max, "For Uniform(a, b), Minimum (a) must be strictly less than Maximum (b)."))
        y_grid <- dunif(x_grid, min = u_min, max = u_max)
        label_str <- sprintf("Uniform(a = %g, b = %g)", u_min, u_max)
        
      } else if (dist == "exp") {
        rate <- getParam("exp_rate")
        validate(need(rate > 0, "Rate (λ) must be strictly positive."))
        y_grid <- dexp(x_grid, rate = rate)
        label_str <- sprintf("Exponential(λ = %g)", rate)
        
      } else if (dist == "gamma") {
        sh <- getParam("gamma_shape")
        sc <- getParam("gamma_scale")
        validate(need(sh > 0 && sc > 0, "Shape (α) and Scale (β) must be strictly positive."))
        # Using scale = beta (equivalent to rate = 1 / beta in R)
        y_grid <- dgamma(x_grid, shape = sh, scale = sc)
        label_str <- sprintf("Gamma(α = %g, β = %g)", sh, sc)
        
      } else if (dist == "norm") {
        mu <- getParam("norm_mean")
        var_val <- getParam("norm_var")
        validate(need(var_val > 0, "Variance (σ²) must be strictly positive."))
        y_grid <- dnorm(x_grid, mean = mu, sd = sqrt(var_val))
        label_str <- sprintf("Normal(μ = %g, σ² = %g)", mu, var_val)
        
      } else if (dist == "beta") {
        s1 <- getParam("beta_a")
        s2 <- getParam("beta_b")
        validate(need(s1 > 0 && s2 > 0, "Shape parameters (α, β) must be strictly positive."))
        y_grid <- dbeta(x_grid, shape1 = s1, shape2 = s2)
        label_str <- sprintf("Beta(α = %g, β = %g)", s1, s2)
      }
      
      # Avoid infinite spikes at boundaries when shape < 1
      y_grid[!is.finite(y_grid)] <- NA
      
      list(
        curve_df  = data.frame(x = x_grid, y = y_grid),
        label_str = label_str
      )
    })
    
    # Render Plot (Fixed at 25 histogram bins)
    output$dist_plot <- renderPlot({
      info <- curve_info()
      int_bounds <- getInterval()
      a <- int_bounds[1]
      b <- int_bounds[2]
      
      # Fixed 25 bins
      h <- hist(dataset, breaks = seq(x_limits[1], x_limits[2], length.out = 26), plot = FALSE)
      hist_df <- data.frame(
        xmin = h$breaks[-length(h$breaks)],
        xmax = h$breaks[-1],
        mids = h$mids,
        density = h$density
      )
      hist_df$in_interval <- ifelse(hist_df$mids >= a & hist_df$mids <= b, "Inside [a, b]", "Outside")
      
      ggplot() +
        # Empirical Histogram Bars
        geom_rect(
          data = hist_df,
          aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = density, fill = in_interval),
          color = "white", alpha = 0.7
        ) +
        # Candidate Theoretical PDF Curve
        geom_line(
          data = info$curve_df,
          aes(x = x, y = y),
          color = "#d95f02", linewidth = 1.4, na.rm = TRUE
        ) +
        # Interval Boundary Lines
        geom_vline(xintercept = c(a, b), linetype = "dashed", color = "#1b9e77", linewidth = 1.0) +
        scale_fill_manual(
          name = "Empirical Histogram Bins",
          values = c("Inside [a, b]" = "#7570b3", "Outside" = "#cbd5e1")
        ) +
        labs(
          x = "Observed Value (x)",
          y = "Density f(x)",
          title = paste("Candidate Overlay:", info$label_str),
          subtitle = sprintf("Interval Bounds: [%g, %g] | Orange curve = Your candidate density", a, b)
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "top",
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    })
    
    # Render Empirical Sample Moments ONLY
    output$empirical_moments_table <- renderTable({
      data.frame(
        `Sample Statistic` = c(
          "Sample Size (n)",
          "Empirical Mean (x̄)",
          "Empirical Variance (s²)",
          "Empirical Std. Dev. (s)"
        ),
        `Observed Value` = c(
          sprintf("%d", length(dataset)),
          sprintf("%.4f", mean(dataset)),
          sprintf("%.4f", var(dataset)),
          sprintf("%.4f", sd(dataset))
        ),
        check.names = FALSE
      )
    }, width = "100%")
    
    # Render Empirical Interval Probability ONLY
    output$empirical_probs_table <- renderTable({
      int_bounds <- getInterval()
      a <- int_bounds[1]
      b <- int_bounds[2]
      count_in <- sum(dataset >= a & dataset <= b)
      n_total  <- length(dataset)
      emp_prob <- count_in / n_total
      
      data.frame(
        `Empirical Metric` = c(
          "Selected Interval [a, b]",
          "Observations in [a, b]",
          "Empirical Probability P̂(a ≤ X ≤ b)"
        ),
        `Value` = c(
          sprintf("[%g, %g]", a, b),
          sprintf("%d out of %d", count_in, n_total),
          sprintf("%.4f", emp_prob)
        ),
        check.names = FALSE
      )
    }, width = "100%")
    
    # CSV Download Handler
    output$download_csv <- downloadHandler(
      filename = function() { paste0(file_label, ".csv") },
      content = function(file) { write.csv(data.frame(x = dataset), file, row.names = FALSE) }
    )
  })
}


# ==============================================================================
# 5. MAIN UI & SERVER INITIALIZATION
# ==============================================================================
ui <- page_navbar(
  title = "Continuous Distributions: Mystery Datasets",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  mysteryTabUI(
    id = "tab1",
    tab_title = "Problem 3: Mystery Dataset A",
    x_min = 35, x_max = 110, step_size = 0.5,
    init_interval = c(65, 80),
    var_min = 1, var_max = 150, var_step = 1, var_init = 25
  ),
  mysteryTabUI(
    id = "tab2",
    tab_title = "Problem 4: Mystery Dataset B",
    x_min = 0, x_max = 15, step_size = 0.1,
    init_interval = c(2.0, 5.0),
    var_min = 0.1, var_max = 25, var_step = 0.1, var_init = 4
  ),
  mysteryTabUI(
    id = "tab3",
    tab_title = "Problem 5: Mystery Dataset C",
    x_min = 0, x_max = 1, step_size = 0.01,
    init_interval = c(0.2, 0.5),
    var_min = 0.005, var_max = 0.25, var_step = 0.005, var_init = 0.05
  )
)

server <- function(input, output, session) {
  mysteryTabServer("tab1", dataset = data_mystery_1, x_limits = c(35, 110), step_size = 0.5,  var_step = 1,     file_label = "mystery_dataset_A")
  mysteryTabServer("tab2", dataset = data_mystery_2, x_limits = c(0, 15),   step_size = 0.1,  var_step = 0.1,   file_label = "mystery_dataset_B")
  mysteryTabServer("tab3", dataset = data_mystery_3, x_limits = c(0, 1),    step_size = 0.01, var_step = 0.005, file_label = "mystery_dataset_C")
}

shinyApp(ui = ui, server = server)
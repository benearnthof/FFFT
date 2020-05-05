# simulation with cdf
dta <- read.xlsx("I:/airworthiness.p/01_Mitarbeiter/Benedikt Arnthof/Next Steps/cdf.xlsx",
                 sheetIndex = 1
)
dta
plot(dta$cdf ~ dta$size)
x <- rnorm(10000000, 0.16, 0.08)
x <- x[x > 0]
x <- x[x <= 0.4]

hist(x, probability = TRUE, breaks = 40, labels = FALSE, ylim = c(0, 6))
lines(c(0, 0.4), y = c(5, 5))
lines(c(0, 0.4), y = c(4, 4))
lines(c(0, 0.4), y = c(3, 3))
lines(c(0, 0.4), y = c(2, 2))
lines(c(0, 0.4), y = c(1, 1))

# seems to fit the graph we have pretty nicely

# expression for crack prop cycles to failure:
library(rlang)
call <- eval("-2520 * log(x) + 6614.4")
vect <- seq(from = 0.01, to = 0.60, by = 0.01)

# construct temporary environment in which to evaluate function
envi <- environment()
envi$x <- vect

call <- rlang::parse_expr("-2520 * log(x) + 6614.4")
res <- rlang::eval_tidy(call, env = envi)
plot(res, ylim = c(0, 20000))

# inputs: user defined function
crack_fun <- "-2520 * log(x) + 6614.4"
# fleet
crack_fleet <- purrr::rdunif(100, 1000, 1)
# proportion of fleet per crackmode
crack_proportion <- c(50, 50, 0)
# parameters of the distributions of every mode
crack_means <- c(0.16, 0.97060, 0)
crack_sds <- c(0.08, 0.05444, 1)
crack_mins <- c(0, 0, 0)
crack_maxs <- c(0.4, Inf, Inf)

crack_dta <- data.frame(fleet = crack_fleet)
crack_tmp <- rmultinom(1, nrow(crack_dta), crack_proportion)
crack_dta$mode <- c(
  rep(1, times = crack_tmp[1, 1]),
  rep(2, times = crack_tmp[2, 1]),
  rep(3, times = crack_tmp[3, 1])
)
# data is sorted usually, to avoid old engine bias we need to reshuffle
crack_dta$mode <- sample(crack_dta$mode, replace = FALSE)

# we are interested in the proportion of the fleet that fails at every discrete
# time interval in the loop. we should be able to just approximate that shit
# without having to use a fleet
# but we are only interested in the fleet so we need to eval anyway

crack_dta$init <- 0

truncated_rnorm <- function(n, mean, sd, min, max, maxdepth = 1000) {
  res <- rnorm(n, mean, sd)
  if ((sum(res <= min) == 0) & (sum(res > max) == 0)) {
    return(res)
  } else {
    nn <- length(res[res < min | res > max])
    if (rlang::trace_length(rlang::trace_back()) >= maxdepth) {
      warning("Truncating present, consider recursion limit.")
    }
    res[res <= min | res > max] <- truncated_rnorm(nn, mean, sd, min, max)
    return(res)
  }
}

crack_dta$init[crack_dta$mode == 1] <- rnorm(nrow(crack_dta[crack_dta$mode == 1, ]), crack_means[1], crack_sds[1])

for (i in seq_along(crack_means)) {
  crack_dta$init[crack_dta$mode == i] <- truncated_rnorm(
    n = length(crack_dta$init[crack_dta$mode == i]),
    mean = crack_means[i],
    sd = crack_sds[i],
    min = crack_mins[i],
    max = crack_maxs[i]
  )
}

# evaluate max lyf on that shiet
crack_envi <- new.env()
crack_envi$x <- crack_dta$init
crack_call <- rlang::parse_expr(crack_fun)
crack_dta$maxlife <- rlang::eval_tidy(crack_call, env = crack_envi)

crack_dta$faulty <- crack_dta$maxlife <= crack_dta$fleet
proportion <- sum(crack_dta$faulty) / nrow(crack_dta)

crack_n_intervals <- 375
crack_intervalsize <- 100

crack_dta$fleet <- 1
proportion <- numeric(length = crack_n_intervals)
for (i in seq_len(crack_n_intervals)) {
  crack_dta$faulty <- crack_dta$maxlife <= crack_dta$fleet
  crack_dta$fleet <- crack_dta$fleet + crack_intervalsize
  proportion[i] <- sum(crack_dta$faulty) / nrow(crack_dta)
}


# Wrapperloop ==================================================================
# wrap everything in a nice loop and construct user interface for all of this
crack_simulation <- function(crack_fun = "-2520 * log(x) + 6614.4", crack_n_intervals = 200,
                             crack_intervalsize = 100, crack_fleet = purrr::rdunif(100, 1, 1),
                             crack_proportion = c(1, 1, 0), crack_means = c(0.16, 0.97060, 2.4),
                             crack_sds = c(0.08, 0.05444, 0.08), crack_mins = c(0, 0, 0),
                             crack_maxs = c(0.4, Inf, Inf), crack_nruns = 100) {
  crack_dta <- data.frame(fleet = crack_fleet)
  # initialize proportion matrix
  crack_proportions <- matrix(0, nrow = crack_n_intervals, ncol = crack_nruns)
  
  for (j in seq_len(crack_nruns)) {
    # do the sampling for every run in here
    # multinorm sample
    crack_dta$fleet <- crack_fleet
    crack_tmp <- rmultinom(1, nrow(crack_dta), crack_proportion)
    crack_dta$mode <- c(
      rep(1, times = crack_tmp[1, 1]),
      rep(2, times = crack_tmp[2, 1]),
      rep(3, times = crack_tmp[3, 1])
    )
    # reshuffle to avoid sorting bias
    crack_dta$mode <- sample(crack_dta$mode, replace = FALSE)
    crack_dta$init <- 0
    # calculate the proportions for every run in here
    for (i in seq_len(crack_n_intervals)) {
      crack_dta$init[crack_dta$mode == i] <- truncated_rnorm(
        n = length(crack_dta$init[crack_dta$mode == i]),
        mean = crack_means[i],
        sd = crack_sds[i],
        min = crack_mins[i],
        max = crack_maxs[i]
      )
      # evaluate max lyf on that shiet
      crack_envi <- new.env()
      crack_envi$x <- crack_dta$init
      crack_call <- rlang::parse_expr(crack_fun)
      crack_dta$maxlife <- rlang::eval_tidy(crack_call, env = crack_envi)
      crack_dta$faulty <- crack_dta$maxlife <= crack_dta$fleet
      crack_dta$fleet <- crack_dta$fleet + crack_intervalsize
      crack_proportions[i, j] <- sum(crack_dta$faulty) / nrow(crack_dta)
    }
  }
  colnames(crack_proportions) <- as.character(1:ncol(crack_proportions))
  crackprop_fds <- rainbow::fds(y = crack_proportions, x = 1:nrow(crack_proportions))
  ret <- data.frame(x = crackprop_fds$x, y = crackprop_fds$y)
  ret <- reshape2::melt(ret, id = "x")
  return(ret)
}

test <- crack_simulation(crack_proportion = c(80, 20, 0), crack_fleet = purrr::rdunif(100, 7500, 1))




# cant return the plot object properly => write own ggplot method to plot fds objects
plt_fds <- function(data) {
  plt <- ggplot(data) +
    geom_line(aes(x = x, y = value, group = variable), col = "#0000ff", alpha = 0.3, inherit.aes = FALSE) +
    theme(legend.position = "none") +
    theme_bw() +
    xlab("Intervals") +
    ylab("Proportion of Failures") +
    ggtitle(label = paste0("Proportion of Failures over ", length(levels(unique(data$variable))), " runs:"))
  return(plt)
}




library(shiny)
library(shinyWidgets)


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  sidebarLayout(
    sidebarPanel(
      splitLayout(
        radioButtons(
          "crack_filetype",
          label = h4("Choose File Type"),
          choices = list(".csv" = 1, ".xlsx" = 2),
          selected = 2,
          inline = TRUE
        ),
        fileInput(
          "crack_file",
          h4("Upload File:"),
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx"
          )
        ),
        checkboxInput(
          "crack_header",
          label = h4("Use header?"),
          value = FALSE
        )
      ),
      textInput(
        "crack_fun",
        label = "f(x) = Crack prop cycles to failure",
        value = "-2520 * log(x) + 6614.4"
      ),
      splitLayout(
        numericInput(
          "crack_n_intervals",
          "Number of Cycles",
          value = 50,
          min = 1,
          max = 1e6,
          step = 1
        ),
        numericInput(
          "crack_intervalsize",
          "Hours per Cycle",
          value = 50,
          min = 1,
          max = 1e6
        ),
        numericInput(
          "crack_nruns",
          "Number of Runs",
          value = 50,
          min = 1,
          max = 10000
        )
      ),
      h4("Proportion of Cracks in Fleet"),
      splitLayout(
        numericInput(
          "crack_proportion1",
          "Prop. 1:",
          value = 98.9,
          min = 0
        ),
        numericInput(
          "crack_proportion2",
          "Prop. 2:",
          value = 1.1,
          min = 0
        ),
        numericInput(
          "crack_proportion3",
          "Prop. 3:",
          value = 0,
          min = 0
        )
      ),
      h4("Crack means in mm:"),
      splitLayout(
        numericInput(
          "crack_mean1",
          "Mean 1",
          value = 0.16,
          min = 0,
          max = 5
        ),
        numericInput(
          "crack_mean2",
          "Mean 2",
          value = 0.9706,
          min = 0,
          max = 5
        ),
        numericInput(
          "crack_mean3",
          "Mean 3",
          value = 1,
          min = 0,
          max = 5
        )
      ),
      h4("Crack std. Deviations in mm:"),
      splitLayout(
        numericInput(
          "crack_sd1",
          "SD 1",
          value = 0.08,
          min = 0,
          max = 5
        ),
        numericInput(
          "crack_sd2",
          "SD 2",
          value = 0.05444,
          min = 0,
          max = 5
        ),
        numericInput(
          "crack_sd3",
          "SD 3",
          value = 1,
          min = 0,
          max = 5
        )
      ),
      h4("Maximum Crack Sizes in mm:"),
      splitLayout(
        numericInput(
          "crack_max1",
          "Max 1",
          value = 0.4,
          min = 0.01,
          max = Inf
        ),
        numericInput(
          "crack_max2",
          "Max 2",
          value = 5,
          min = 0.01,
          max = 5
        ),
        numericInput(
          "crack_max3",
          "Max 3",
          value = 5,
          min = 0.01,
          max = 5
        )
      ),
      actionButton(
        "crack_button",
        "Refresh!"
      )
    ),
    mainPanel(
      plotOutput("crack_plot"),
      verbatimTextOutput("crack_tjekst"),
      plotOutput("crack_fun_plot")
    )
  )
  
)

server <- function(input, output, session) {
  crack_preprocessing <- reactive({
    in_file <- input$crack_file
    if (is.null(in_file)) {
      return(NULL)
    }
    if (input$crack_filetype == "1") {
      read.csv(in_file$datapath,
               header = input$crack_header,
               stringsAsFactors = FALSE)
    } else {
      xlsx::read.xlsx(in_file$datapath,
                      header = input$crack_header,
                      sheetIndex = 1,
                      stringsAsFactors = FALSE)
    }
  })
  output$crack_tjekst <- renderPrint({
    str(crack_preprocessing()[,1])
  })
  crack_processing <- eventReactive(input$crack_button, {
    shiny::validate(need(input$crack_file, message = FALSE))
    tmp <- crack_simulation(crack_fun = input$crack_fun,
                            crack_n_intervals = input$crack_n_intervals,
                            crack_intervalsize = input$crack_intervalsize,
                            crack_fleet = crack_preprocessing()[,1],
                            crack_proportion = c(input$crack_proportion1, input$crack_proportion2, input$crack_proportion3),
                            crack_means = c(input$crack_mean1, input$crack_mean2, input$crack_mean3),
                            crack_sds = c(input$crack_sd1, input$crack_sd2, input$crack_sd3),
                            crack_mins = c(0, 0, 0),
                            crack_maxs = c(input$crack_max1, input$crack_max2, input$crack_max3),
                            crack_nruns = input$crack_nruns
    )
    list(crack_result = tmp)
  })
  
  output$crack_fun_plot <- renderPlot({
    shiny::validate(need(input$crack_fun, message = FALSE))
    crack_fun <- input$crack_fun
    crack_maxs = c(input$crack_max1, input$crack_max2, input$crack_max3)
    f <- as.function(alist(x = , eval(parse(text = crack_fun))))
    curve(f, to = max(crack_maxs), n = 101, main = "Expected Life by Initial Crack Size",
          xlab = "Initial Crack Size")
  })
  
  output$crack_plot <- renderPlot({
    plt_fds(crack_processing()$crack_result)
  })
}

shinyApp(ui, server)



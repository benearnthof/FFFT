# multimode multiruns ui and server
ui <- shinyUI(
  navbarPage(
    theme = shinytheme("cosmo"),
    title = "NRA-T",
    # Suspension I/O ====
    sidebarLayout(
      sidebarPanel(
        h4("Shapes:"),
        splitLayout(
          numericInput("mm_shape1", label = "Shape 1", value = 2.5, min = 0, max = 25),
          numericInput("mm_shape2", label = "Shape 2", value = 3.5, min = 0, max = 25),
          numericInput("mm_shape3", label = "Shape 3", value = 4.5, min = 0, max = 25)
        ),
        h4("Scales:"),
        splitLayout(
          numericInput("mm_scale1", label = "Scale 1", value = 14400, min = 0, max = 100000),
          numericInput("mm_scale2", label = "Scale 2", value = 15500, min = 0, max = 100000),
          numericInput("mm_scale3", label = "Scale 3", value = 16600, min = 0, max = 100000)
        ),
        h4("Number of Runs"),
        numericInput("mm_nruns", label = "n Runs", value = 10, min = 1, max = 100, step = 1),
        actionButton("mm_refresh", "Refresh!")
      ),
      mainPanel(
        verbatimTextOutput("mm_shapes"),
        verbatimTextOutput("mm_scales"),
        plotOutput("mm_plot")
      )
      
    )
  )
)

server <- function(input, output, session) {
  source("packageloader.R")
  source("debugweibull.R")
  source("simulation_uirework.R")
  source("simulation_multi.R")
  # preprocess shape and scale inputs to lists
  mm_data_params <- reactive({
    shapes <- list(one = input$mm_shape1, two = input$mm_shape2, three = input$mm_shape3)
    scales <- list(one = input$mm_scale1, two = input$mm_scale2, three = input$mm_scale3)
    check_shapes <- shapes == 0
    check_scales <- scales == 0
    check <- !(check_shapes | check_scales)
    shapes <- shapes[check]
    scales <- scales[check]
    list(shapes = shapes, scales = scales)
  })
  # preprocessing data for multi multi
  mm_data_sim <- eventReactive(input$mm_refresh, {
    in_removals <- input$removal_file
    in_removals_amnts <- input$removal_amounts
    if (input$useremovals == F) {
      in_removals <- NULL
      rem_dist <- NULL
    } else {
      in_removals <- input$removal_file
      if (is.null(in_removals)) {
        rem_dist <- NULL
      } else {
        rem_dist <- scan(in_removals$datapath)
      }
    }
    if (is.null(in_removals_amnts)) {
      rem_amnts <- 0
    } else {
      rem_amnts <- scan(in_removals_amnts$datapath)
    }
    # pass all arguments from other input panel in here
    # return list and plot outputs
    # also construct an output to display the approximate proportions of every
    # failuremode which is basically all that is needed here
    ls <- mm_simulation(
      hmonth = input$hmonth,
      errorfreeperiod = input$errorfreeperiod,
      nmonths = input$nmonths,
      failures = fal_data_suspensions()$dta,
      shapes = mm_data_params()$shapes,
      scales = mm_data_params()$scales,
      simdist = "weibull",
      simple = TRUE,
      suspensions = sin_data_suspensions()$dta,
      removaldist = rem_dist,
      removalamnts = rem_amnts,
      removalmethod = input$rem_method,
      removaltype = input$rem_distr,
      removeyesno = input$removeyesno,
      removefaulty = input$removefaulty,
      replacefaulty = input$replacefaulty,
      inspectionmatrix = datainsmat()$insmat,
      doinflux = datainflux()$doinflux,
      influxelps = datainflux()$influxelps,
      influxamounts = datainflux()$influxamounts,
      influxmethod = datainflux()$influxmethod,
      betaalpha = input$betaalpha,
      betabeta = input$betabeta,
      maxlyf = input$maxlyf,
      nruns = input$mm_nruns
    )
    list(plot = ls$plt, data = ls$mlt)
  })
  
  output$mm_shapes <- renderPrint(
    str(mm_data_params()$shapes)
  )
  output$mm_scales <- renderPrint(
    str(mm_data_params()$scales)
  )
  
  output$mm_plot <- renderPlot(
    mm_data_sim()$plt
  )
}

# run ====
shinyApp(ui = ui, server = server)
# shiny::runApp(list(ui = ui, server = server), display.mode = "showcase")


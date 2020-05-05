source("packageloader.R")
source("debugweibull.R")
source("functions.R")
options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
# options(shiny.reactlog = TRUE)

ui <- shinyUI(
  navbarPage(
    theme = shinytheme("cosmo"),
    title = "NRA-T",
    # Suspension I/O ====
    tabPanel(
      "Suspensions",
      sidebarLayout(
        sidebarPanel(
          filterDF_UI("sin_filtering")
        ),
        mainPanel(
          splitLayout(
            csvFileInputReadr("sin_dataset", label = "Input Data"),
            textInput(
              inputId = "sin_suspension_ID",
              label = "Suspension Column:",
              value = NULL
            ),
            textInput(
              inputId = "sin_counts",
              label = "Counts Column:",
              value = NULL
            ),
            checkboxInput(
              inputId = "sin_usebands",
              label = "Use Bands?",
              value = FALSE
            )
          ),
          progressBar(
            id = "sin_pbar", value = 100,
            total = 100, display_pct = TRUE
          ),
          tags$p("Suspensions"),
          verbatimTextOutput(outputId = "sin_susp"),
          tags$p("Counts"),
          verbatimTextOutput(outputId = "sin_counts"),
          DT::dataTableOutput(outputId = "sin_table"),
          tags$p("Code dplyr:"),
          verbatimTextOutput(outputId = "sin_code_dplyr"),
          tags$p("Expression:"),
          verbatimTextOutput(outputId = "sin_code")
          
        )
      )
    ),
    # Failure I/O ====
    tabPanel(
      "Failures",
      sidebarLayout(
        sidebarPanel(
          filterDF_UI("fal_filtering")
        ),
        mainPanel(
          splitLayout(
            csvFileInputReadr("fal_dataset", label = "Input Data"),
            textInput(
              inputId = "fal_suspension_ID",
              label = "Failures Column:",
              value = NULL
            ),
            textInput(
              inputId = "fal_counts",
              label = "Failure ID:",
              value = NULL
            )
          ),
          
          progressBar(
            id = "fal_pbar", value = 100,
            total = 100, display_pct = TRUE
          ),
          tags$p("Failures"),
          verbatimTextOutput(outputId = "fal_susp"),
          tags$p("IDs"),
          verbatimTextOutput(outputId = "fal_counts"),
          DT::dataTableOutput(outputId = "fal_table"),
          tags$p("Code dplyr:"),
          verbatimTextOutput(outputId = "fal_code_dplyr"),
          tags$p("Expression:"),
          verbatimTextOutput(outputId = "fal_code")
          # tags$p("Filtered data:"),
          # verbatimTextOutput(outputId = "sin_res_str")
          # handsontable extension allows flexible dynamic inputs.
          # rHandsontableOutput("falhot", width = 300),
          # values that have been read from the handsontable serves debugging don't turn off
          # textOutput("failureinputdebugging")
        )
      )
    ),
    # Parameter Estimation ====
    tabPanel(
      "Param. Estim.",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Select a method for parameter estimation."
          ),
          selectInput(
            inputId = "estmeth",
            label = "Select Estimation Method",
            # All currently in WeibullR implemented methods for estimation
            choices = c(
              "Lognormal MLE 2 Par", "Lognormal MLE 3 Par",
              "Weibull MLE 2 Par", "Weibull MLE 3 Par",
              "Lognormal MRR 2 Par", "Lognormal MRR 3 Par",
              "Weibull MRR 2 Par", "Weibull MRR 3 Par",
              "Weibayes MLE", "Weibayes"
            ),
            selected = "Weibull MRR 2 Par"
          ),
          conditionalPanel(
            # Weibayes methods need a prior beta
            condition = "input.estmeth == 'Weibayes MLE' || input.estmeth == 'Weibayes'",
            numericInput("priorbeta", "Prior Beta:", value = 2.0, min = 0.01, max = 10, step = 0.001)
          )
        ),
        mainPanel(
          # quickfit of the weibullR package
          plotOutput("estimationplot", width = '800px', height = '800px'),
          # distribution parameters that have been estimated under the specified conditions
          tableOutput("estimationparams1")
        )
      )
    ),
    # Competing Modes ####
    tabPanel(
      "Comp. Modes",
      sidebarLayout(
        sidebarPanel(
          # more than 3-4 competing failure modes tend to be non-separable because
          # their mixture distribution looks like a normal distribution.
          numericInput("number_weibulls", "Number of Modes", min = 1, max = 10,
                       step = 1, value = 1, width = '200px'),
          # every mode needs shape and scale parameters to accomplish this, UI is
          # being generated here.
          uiOutput("competing_modes_UI1"),
          actionButton("update_competingmodes", "Refresh!"),
          width = 2
        ),
        
        mainPanel(
          # competing modes output, uses a canvas as constructed by the quickfit
          # methods of WeibullR; see server and functions.R for plotweibullcanvas
          plotOutput("competing_modes", width = '800px', height = '800px')
        )
      )
    ),
    # Custom Visualization ====
    tabPanel(
      "Plots",
      sidebarLayout(
        sidebarPanel(
          # put user inputs here grouped by fit, conf, plot, and contour arguments
          # This section allows for a deeper weibull analysis than the quickplots
          # in parameter estimation
          splitLayout(
            # all currently supported methods to fit a distribution in WeibullR
            selectInput("fit_distribution", "Distribution",
                        c("weibull", "weibull2p", "weibull3p", "lognormal", "lognormal3p"),
                        selected = "weibull"),
            # Weibull and lognormal canvas to yield straight lines on fit distributions
            selectInput("plt_canvas",
                        "Canvas",
                        c("weibull", "lognormal")
            ),
            # fix for a bug that caused overlap when using splitlayout
            tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                    overflow: visible;
                                    }
                                    ")))
          ),
          # all fit methods implemented in weibullr
          # rank regression, ML estimation with- and without reduced bias assessment
          selectInput("method_fit",
                      "Fit Method",
                      c("rr-xony", "rr-yonx", "mle", "mle-rba", "mle-unbias")),
          splitLayout(
            # should confidence bounds be plotted
            checkboxInput("conf_yesno",
                          "Plot CI",
                          value = FALSE),
            # method for the calculation of confidence bounds
            selectInput("method_conf",
                        "CI Method",
                        c("pivotal-rr", "bbb", "fm", "fmbounds", "lrb"))
          ),
          splitLayout(
            # how broad should the confidence interval be?
            numericInput("confidence_interval",
                         "CI %",
                         value = 0.95, min = 0.01, max = 0.99, step = 0.01),
            # what B-life should be displayed in the legend?
            numericInput("blife_pts",
                         "B-life point",
                         value = 0.5, min = 0.01, max = 0.99, step = 0.01)
          ),
          # Main Title of the plot
          textInput("plt_main",
                    "Title",
                    "Reliability Plot"),
          splitLayout(
            # color of the line in the plot.
            colourInput("plot_color",
                        "Plot Color",
                        "blue"),
            # color of the confidence bounds
            colourInput("bound_color",
                        "Bound Color",
                        "blue")
          ),
          splitLayout(
            # should legend be displayed in the plot?
            checkboxInput("plt_legend",
                          "Legend",
                          value = TRUE),
            # should goodness of fit (R squared) be displayed in the legend?
            checkboxInput("legend_gof",
                          "Goodness of Fit",
                          value = FALSE)
          ),
          splitLayout(
            # legend position (not ideal at the moment, only 9 positions are supported)
            selectInput("legend_position",
                        "Legend position",
                        c("bottomright", "bottom", "bottomleft", "left",
                          "topleft", "top", "topright", "right", "center")),
            # text size of the legend, currently scales up too quickly
            numericInput("legend_text_size",
                         "Text Size",
                         value = 0.7, min = 0, max = 5)
          )
          
          
        ),
        mainPanel(
          # output of the plot that has been customized in this panel
          plotOutput("prob_plt", width = '800px', height = '800px')
        )
      )
    ),
    
    # Removal I/O ====
    tabPanel(
      "Removal",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Upload files to model removal process. Distribution to model a prior known distribution; Amounts to set the amounts to be removed."
          ),
          # should a distribution for the weighting of removals be estimated based on
          # a file of prior information?
          checkboxInput("useremovals", "Use Distribution File?"),
          # should removals be carried out?
          checkboxInput("removeyesno", "Remove Engines?"),
          conditionalPanel(
            condition = "input.useremovals == true",
            fileInput(
              inputId = "removal_file",
              label = "Select Distribution File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
            # method for the weighting of the removals, both estimated density and
            # ECDF are supported.
            radioButtons("rem_method",
                         label = "Removal Method",
                         choices = c("Density" = T, "ECDF" = F),
                         selected = "Density"
            )
          ),
          
          conditionalPanel(
            # no prior removal data is available => get the removal weights from
            # a uniform (default), normal or beta distribution
            condition = "input.useremovals == false",
            selectInput("rem_distr",
                        label = "Sampling Weights",
                        choices = c(
                          "Uniform" = "dunif", "ECDF Uniform" = "punif",
                          "Normal" = "dnorm", "ECDF Normal" = "pnorm",
                          "Beta" = "beta"
                        ),
                        selected = "dunif"
            )
          ),
          conditionalPanel(
            # parameters for beta distribution
            condition = "input.rem_distr == 'beta' && input.useremovals == false",
            sliderInput("betaalpha", "Alpha", 0.01, 10, 1, step = 0.01),
            sliderInput("betabeta", "Beta", 0.01, 10, 1, step = 0.01)
          ),
          fileInput(
            # amount of engines removed from the fleet for every discrete timestep
            # in the simulation.
            inputId = "removal_amounts",
            label = "Select Removal Amounts",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          )
        ),
        mainPanel(
          # visualization of the chosen weights. Grants the user the visual intuition
          # to understand how the engines are being weighted for removal.
          plotOutput("densityplot"),
          plotOutput("weightsplot")
        )
      )
    ),
    # Influx I/O ====
    tabPanel(
      "Influx",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "Perform Influx: Upload a file containing the monthly amounts of engines to be added to the fleet.",
            "And select a method to sample the ages of the inflowing engines."
          ),
          # should an amount of engines be added to the fleet in every timestep?
          checkboxInput("useinflux", "Influx?"),
          conditionalPanel(
            condition = "input.useinflux == true",
            # vector of number of engines to be added to the fleet in every timestep
            fileInput(
              inputId = "influxamnts",
              label = "Select Influx amounts.csv",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            ),
            # method to assign starting ages to the inflowing engines. Constant assigns
            # a constant value to all engines, Prior samples from a distribution of prior
            # ages, and all other methods sample from distributions specified by the user.
            # Distributions cannot produce negative values, as only positive ages are
            # supported.
            selectInput("influxage", "Influx Age-Distribution",
                        choices = c(
                          "Constant",
                          "Prior",
                          "Norm",
                          "Unif",
                          "Exp",
                          "Lnorm",
                          "Pois",
                          "Weibull"
                        )
            ),
            # based on inputs above, a dynamic user interface is generated in the server
            # and rendered here.
            uiOutput("influxUI1")
          )
        ),
        
        mainPanel(
          # visualizations to guide user and debugging procedures
          tableOutput("influxtable"),
          textOutput("influxdebug")
        )
      )
    ),
    
    # Inspections ====
    tabPanel(
      "Inspections",
      sidebarLayout(
        sidebarPanel(
          filterDF_UI("insmat_filtering")
        ),
        mainPanel(
          splitLayout(
            checkboxInput("inspectyesno", "Do inspections?"),
            csvFileInputReadr("insmat_dataset", label = "Inspection Table"),
            textInput(
              inputId = "insmat_values",
              label = "Hours Column:",
              value = NULL
            ),
            textInput(
              inputId = "insmat_pod",
              label = "POD Column:",
              value = NULL
            )
          ),
          progressBar(
            id = "insmat_pbar", value = 100,
            total = 100, display_pct = TRUE
          ),
          tags$p("Values"),
          verbatimTextOutput(outputId = "insmat_values"),
          tags$p("PODs"),
          verbatimTextOutput(outputId = "insmat_pods"),
          DT::dataTableOutput(outputId = "insmat_table"),
          tags$p("Code dplyr:"),
          verbatimTextOutput(outputId = "insmat_code_dplyr"),
          tags$p("Expression:"),
          verbatimTextOutput(outputId = "insmat_code"),
          tags$p("Filtered data:"),
          verbatimTextOutput(outputId = "insmat_res_str"),
          tags$p("Insmat:"),
          verbatimTextOutput(outputId = "insmat_insmat")
        )
      )
    ),
    # Simulation ====
    tabPanel(
      "Forecasting",
      sidebarLayout(
        sidebarPanel(
          # Should failure probabilities be approximated or calculated exactly.
          # Default value is exact calculation because it is more robust to changes in
          # fleet size and is 500 - 5000 times faster
          checkboxInput(
            "fast",
            "Deterministic?",
            value = TRUE
          ),
          # should the floor of the faulty engines every month be removed from the fleet?
          checkboxInput(
            "removefaulty",
            "Remove sample of faulty engines?",
            value = FALSE
          ),
          # should the floor of faulty engines that may be removed in the step above
          # be replaced by new engines with 0 age?
          checkboxInput(
            "replacefaulty",
            "Replace faulty engines?",
            value = FALSE
          ),
          # number of months (discrete time steps) the simulation is going to run for
          splitLayout(
            numericInput(
              "nmonths",
              "Months",
              min = 1,
              max = 1000,
              step = 1,
              value = 1
            ),
            # amount of hours/cycles/uses in every discrete timestep
            numericInput(
              "hmonth",
              "Monthly Hours",
              min = 0,
              max = 1000,
              step = 1,
              value = 0
            )
          ),
          splitLayout(
            # period in which no errors are occurring (t0 in the 3param weibull)
            numericInput(
              "errorfreeperiod",
              "Incubation Time (in hours)",
              min = 0,
              max = 25000,
              step = 1,
              value = 0
            ),
            # maximum life after which the engines are removed from the fleet no
            # matter their current failure probability
            numericInput(
              "maxlyf",
              "Max. Life (in hours)",
              min = 0,
              max = 1e+10,
              step = 1,
              value = 1e+9
            )
          ),
          # (Deprecated) Amount of draws from the weibull distribution to approximate
          # the monthly relative error probability of every engine
          # There seems to be a bug with multiruns and this sampling procedure.
          # Use at your own discretion, it is no longer recommended. This procedure
          # artificially inflates variance in the resulting estimates. It would be
          # more robust (and actually based on sound mathematics) to sample the
          # parameters of the weibull distribution used to approximate error terms
          # in every month from a posterior distribution as mentioned in
          # Markov Chain Monte Carlo in Practice by Gilks et al.
          numericInput(
            "nweibull",
            "Number of draws",
            min = 0,
            max = 10000,
            step = 1,
            value = 500
          ),
          # distribution to use for error calc.
          selectInput("errorcalcdistribution", "Error Calc. Dist.",
                      choices = c("Weibull", "LNorm", "Norm")
          ),
          actionButton("updateforecasting", "Refresh!", icon = icon("refresh"), width = NULL),
          # a tiny amount of flexible user interface is required because the
          # names of the parameters of the 3 different distributions differ from
          # one another
          uiOutput("forecastingUI1"),
          # output the table generated from parameter estimation as a quality of
          # life feature to avoid having to switch tabs to copy and paste parameters.
          tableOutput("estimationparams2")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              # output simulation results
              "Expected Failures:",
              h3("Expected Failures Now:"),
              h3(textOutput("initial")),
              h3("Expected Failures Now RBA"),
              h3(textOutput("initial_rba")),
              h3("Expected Future Failures:"),
              h3(textOutput("errors")),
              # plot of the distribution that was specified, helps avoid errors with
              # unintuitive log parameters of the lognormal distribution and gives
              # further visual intuition of the error distribution
              div(
                style = "display: inline-block;vertical-align:top; width: 100px;",
                numericInput("simdistpltmin", "Plot Min.", value = 0, width = "100px")
              ),
              div(
                style = "display: inline-block;vertical-align:top; width: 100px;",
                numericInput("simdistpltmax", "Plot Max.", value = 35000, width = "100px")
              ),
              plotOutput("simdistplt")
            )
          )
        )
      )
    ),
    # Diagnostics ====
    tabPanel(
      "Diagnostics",
      sidebarLayout(
        sidebarPanel(
          # provide a csv of the simulation results for validation and debugging purposes
          downloadButton("downloadData", "Download Fleetinfo", size = "50px"),
          # set target diagnostic plot
          selectInput("diagnostikplot",
                      label = "Diagnostic Plot:",
                      choices = c("Cumulative", "Monthly", "Fleet", "Relative", "Errorbounds")
          )
        ),
        mainPanel(
          plotOutput("errorplot"),
          # table used to debug influx and removal, aswell as inspection process
          tableOutput("resfleet")
        )
      )
    ),
    # MultirunsUI ====
    # Run simulation multiple times to gauge effect of processes that introduce
    # randomness, such as influx with non constant ages and or amounts, removal
    # sampling and inspection with probabilities of failure less than 1.
    tabPanel(
      "Multiruns",
      sidebarLayout(
        sidebarPanel(
          numericInput("numberofruns",
                       "Number of Runs",
                       value = 10, min = 1, max = 1000, step = 1
          ),
          numericInput("multiseed",
                       "Random Seed",
                       value = sample.int(1000000000, 1),
                       min = 1, max = 1000000001, step = 1
          ),
          selectInput("multirunsplot",
                      "Type of Plot",
                      choices = c("Points", "Violin", "Singlemonth"),
                      selected = "Points"
          ),
          uiOutput("multirunsui"),
          actionButton("updatemultiruns", "Refresh!", icon = icon("refresh"), width = NULL)
        ),
        mainPanel(
          plotOutput("multirunsplot")
        )
      )
    ),
    # weirdevents (POF) ====
    tabPanel(
      "POF",
      sidebarLayout(
        sidebarPanel(width = 4,
                     helpText(
                       "Upload .csv files containing Fleet, Influx, and Simulation Data.
          Adjust maximum ages/hours and the hours per discrete timestep (CPT)
          accordingly."
                     ),
                     # modular input allows for a rigid return structure see functions.R
                     splitLayout(
                       csvFileInputGeneral("we_fleet1_csv", "Fleet 1 .csv"),
                       csvFileInputGeneral("we_fleet2_csv", "Fleet 2 .csv")
                     ),
                     
                     splitLayout(
                       csvFileInputGeneral("we_influx1_csv", "Influx 1 .csv"),
                       csvFileInputGeneral("we_influx2_csv", "Influx 2 .csv")
                     ),
                     splitLayout(
                       numericInput("we_maxage1", "Max Age 1", value = 25000, min = 0, step = 1),
                       numericInput("we_maxage2", "Max Age 2", value = 25000, min = 0, step = 1)
                     ),
                     splitLayout(
                       numericInput("we_cpt1", "CPT 1", value = 100, min = 0, step = 1),
                       numericInput("we_cpt2", "CPT 2", value = 100, min = 0, step = 1)
                     ),
                     splitLayout(
                       csvFileInputGeneral("we_dta1", "Simulation 1 .csv"),
                       csvFileInputGeneral("we_dta2", "Simulation 2 .csv")
                     )
        ),
        mainPanel(
          plotOutput("weirdevents_plt", width = "800px"),
          tableOutput("we_show_inputs")
          # 1-3 numeric outputs
          # one plot output
        )
      )
    ),
    # Crackprop Lyfe ====
    tabPanel(
      "Cracks",
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
              "Number of Months",
              value = 50,
              min = 1,
              max = 1e6,
              step = 1
            ),
            numericInput(
              "crack_intervalsize",
              "Cycles per Month",
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
          splitLayout(
            actionButton(
              "crack_button",
              "Refresh!"
            ),
            downloadButton("crack_download", "Download Data", size = "50px")
          )
        ),
        mainPanel(
          plotOutput("crack_plot"),
          verbatimTextOutput("crack_tjekst"),
          plotOutput("crack_fun_plot")
        )
      )
    ),
    tabPanel(
      "MM",
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
    # End of UI ====
  )
)

# Server ====
server <- function(input, output, session) {
  source("packageloader.R")
  source("debugweibull.R")
  source("simulation_uirework.R")
  source("simulation_multi.R")
  # suspension data handling ====
  sin_dta <- callModule(
    module = csvFileReadr,
    id = "sin_dataset"
  )
  
  sin_res_filter <- callModule(
    module = filterDF,
    id = "sin_filtering",
    # data_table has to be a reactive expression
    data_table = reactive(sin_dta()$inputdata)
  )
  
  observeEvent(sin_res_filter$data_filtered(), {
    updateProgressBar(
      session = session, id = "sin_pbar",
      value = nrow(sin_res_filter$data_filtered()), total = nrow(sin_dta()$inputdata)
    )
  })
  
  output$sin_table <- DT::renderDT({
    sin_res_filter$data_filtered()
  }, options = list(pageLength = 5)
  )
  
  output$sin_code_dplyr <- renderPrint({
    sin_res_filter$code$dplyr
  })
  
  output$sin_code <- renderPrint({
    sin_res_filter$code$expr
  })
  
  output$sin_res_str <- renderPrint({
    str(sin_res_filter$data_filtered())
  })
  
  sin_data_counts <- reactive({
    shiny::validate(need(input$sin_counts, message = FALSE))
    counts <- eval(parse(text = paste0("sin_res_filter$data_filtered()$", get("input")$sin_counts)))
    list(counts = counts)
  })
  
  sin_data_suspensions <- reactive({
    shiny::validate(need(input$sin_suspension_ID, message = FALSE))
    dta <- eval(parse(text = paste0("sin_res_filter$data_filtered()$", get("input")$sin_suspension_ID)))
    if (input$sin_usebands) {dta <- disagg(dta, sin_data_counts()$counts)}
    list(dta = dta)
  })
  
  output$sin_susp <- renderPrint(
    str(sin_data_suspensions()$dta)
  )
  
  output$sin_counts <- renderPrint(
    str(sin_data_counts()$counts)
  )
  # datasuspensions <- callModule(csvFile, "suspensions_csv")
  # datasuspensions <- sin_data_suspensions()$dta
  # datafailures <- callModule(csvFile, "failures_csv")
  # fal_data_suspensions()$dta
  # Failureinput ====
  
  fal_dta <- callModule(
    module = csvFileReadr,
    id = "fal_dataset"
  )
  
  fal_res_filter <- callModule(
    module = filterDF,
    id = "fal_filtering",
    # data_table has to be a reactive expression
    data_table = reactive(fal_dta()$inputdata)
  )
  
  observeEvent(fal_res_filter$data_filtered(), {
    updateProgressBar(
      session = session, id = "fal_pbar",
      value = nrow(fal_res_filter$data_filtered()), total = nrow(fal_dta()$inputdata)
    )
  })
  
  output$fal_table <- DT::renderDT({
    fal_res_filter$data_filtered()
  }, options = list(pageLength = 5)
  )
  
  output$fal_code_dplyr <- renderPrint({
    fal_res_filter$code$dplyr
  })
  
  output$fal_code <- renderPrint({
    fal_res_filter$code$expr
  })
  
  output$fal_res_str <- renderPrint({
    str(fal_res_filter$data_filtered())
  })
  
  fal_data_suspensions <- reactive({
    shiny::validate(need(input$fal_suspension_ID, message = FALSE))
    dta <- eval(parse(text = paste0("fal_res_filter$data_filtered()$", get("input")$fal_suspension_ID)))
    list(dta = dta)
  })
  
  fal_data_counts <- reactive({
    shiny::validate(need(input$fal_counts, message = FALSE))
    counts <- eval(parse(text = paste0("fal_res_filter$data_filtered()$", get("input")$fal_counts)))
    list(counts = counts)
  })
  
  output$fal_susp <- renderPrint(
    str(fal_data_suspensions()$dta)
  )
  
  output$fal_counts <- renderPrint(
    str(fal_data_counts()$counts)
  )
  
  # Three step process to make interactive failure input possible
  # 1. Construct a data.frame from the .csv that has been uploaded and, if no such
  # file has been supplied, construct a one line data frame (this might no longer be needed,
  # because the modular file upload allows doing nothing quietly when the uploaded
  # file is missing. )
  # 2. Pass data.frame containing IDs, Failuretimes and Boolean values to the
  # rhandsontable to allow modification of the values by the user.
  # 3. Pass the modified values to datafailurevector so other parts of the program
  # can utilize the data as needed.
  values <- reactiveValues()
  # Step 1
  datafailuretable <- reactive({
    # in_failures <- datafailures()$inputdata$Hours
    in_failures <- fal_data_suspensions()$dta
    # in_failures_id <- datafailures()$inputdata$ID
    in_failures_id <- fal_data_suspensions()$counts
    if (is.null(in_failures_id)) {in_failures_id <- LETTERS[1:length(in_failures)]}
    # this is a failsafe, may no longer be needed.
    if (is.null(values[["failuredta"]])) {
      failuredta <- data.frame(Values = 1, Status = TRUE, ID = LETTERS[1],
                               stringsAsFactors = FALSE)
    } else if (!is.null(in_failures)) {
      # stringsasfactors = FALSE is needed to make ID columns editable
      failuredta <- data.frame(Values = in_failures, Status = TRUE, ID = in_failures_id,
                               stringsAsFactors = FALSE)
    } else {
      failuredta <- data.frame(Values = values[["failuredta"]], Status = TRUE, in_failures_id,
                               stringsAsFactors = FALSE)
    }
    values[["failuredta"]] <- failuredta
    list(failuredta = failuredta)
  })
  
  # ForecastingUI ====
  # Here a dynamic UI for the forecasting panel is constructed.
  # I should probably wrap this procedure into a module to enhance readability
  # and make debugging and extending the program less of a chore.
  output$forecastingUI1 <- renderUI({
    meth <- tolower(input$errorcalcdistribution)
    call <- paste0("r", tolower(meth))
    forms <- formals(call)[2:length(formals(call))]
    numberinputs <- length(forms)
    lapply(1:numberinputs, function(i) {
      numericInput(paste0("forecastformal", i), paste0(names(forms)[i]),
                   min = 0, max = 100000, value = 1, step = NA, width = NULL
      )
    })
  })
  dataforecast <- eventReactive(input$updateforecasting, {
    params <- numeric(length = 2)
    for (i in 1:length(params)) {
      params[i] <- eval(parse(text = paste0("input$forecastformal", i)))
    }
    list(params = params)
  })
  # Data customviz ====
  data_customplots <- reactive({
    # handle an error that caused the R session to hard crash
    shiny::validate(need(input$blife_pts, message = FALSE))
    # sus <- datasuspensions()$inputdata$Hours
    sus <- sin_data_suspensions()$dta
    plot_color <- tolower(input$plot_color)
    bound_color <- tolower(input$bound_color)
    weib <- wblr(x = fal_data_suspensions()$dta, s = sus, pch = 1, cex.points = 0.7)
    # construct object that can be passed to plot.wblr as quickplot method
    prob_fit <- wblr.fit(weib,
                         dist = input$fit_distribution, col = plot_color,
                         method.fit = input$method_fit
    )
    # data needed by WeibullR quickfit methods for confidence bounds
    if (input$conf_yesno == "TRUE") {
      prob_fit <- wblr.conf(prob_fit,
                            method.conf = input$method_conf, ci = input$confidence_interval,
                            blife.pts =  c(input$blife_pts),col = bound_color, lwd = 1
      )
      
    }
    
    list(prob_fit = prob_fit)
  })
  # render custom plot
  output$prob_plt <- renderPlot({
    # plot custom wblr object
    plot.wblr(data_customplots()$prob_fit,
              canvas = input$plt_canvas, main = input$plt_main, col.grid = "gray",
              is.plot.legend = input$plt_legend, legend.position = input$legend_position,
              legend.text.size = input$legend_text_size, in.legend.gof = input$legend_gof
    )
  })
  # datasim ====
  # this block constructs the input data needed to run the simulation
  # and returns a list containing all simulation results.
  datasim <- eventReactive(input$updateforecasting, {
    # TODO: modularize the removal and influx file inputs to avoid checking
    # every time
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
    if (is.null(input$rem_distr)) {
      removaltype <- "dunif"
    } else {removaltype <- input$rem_distr}
    # Simulation ====
    ls <- simulation(
      # suspensions = datasuspensions()$inputdata$Hours,
      suspensions = sin_data_suspensions()$dta,
      failures = fal_data_suspensions()$dta,
      hmonth = input$hmonth,
      errorfreeperiod = input$errorfreeperiod,
      age_amnt = input$age_amnt,
      nmonths = input$nmonths,
      nweibull = input$nweibull,
      beta = dataforecast()$params[[1]],
      theta = dataforecast()$params[[2]],
      simdist = tolower(input$errorcalcdistribution),
      fast = input$fast,
      removaldist = rem_dist,
      removalamnts = rem_amnts,
      removalmethod = input$rem_method,
      removaltype = removaltype,
      removeyesno = input$removeyesno,
      removefaulty = input$removefaulty,
      replacefaulty = input$replacefaulty,
      # inspectionmatrix = inspectiondata()$matrix,
      inspectionmatrix = datainsmat()$insmat,
      doinflux = datainflux()$doinflux,
      influxelps = datainflux()$influxelps,
      influxamounts = datainflux()$influxamounts,
      influxmethod = datainflux()$influxmethod,
      betaalpha = input$betaalpha,
      betabeta = input$betabeta,
      maxlyf = input$maxlyf
    )
    # simulation output
    # initial errors; expected errors in timeframe; all error plots; the resulting
    # fleet plot; the export frame to be downloaded as a xlsx later on; and the
    # initial errors after reduced bias assessment
    list(
      initial = ls[[1]], errors = ls[[2]], errorplot = ls[[3]], fleetplot = ls[[4]],
      relerrorplot = ls[[5]], resfleet = ls[[6]], exportframe = ls[[7]],
      initial_rba = ls[[8]], errorplot_bounds = ls[[9]]
    )
  })
  # dataremovals ====
  datarem <- reactive({
    if (input$useremovals == F) {
      in_removals <- NULL
      removals <- NULL
    } else {
      in_removals <- input$removal_file
      if (is.null(in_removals)) {
        removals <- NULL
      } else {
        removals <- scan(in_removals$datapath)
      }
    }
    # suspensions <- datasuspensions()$inputdata$Hours
    suspensions <- sin_data_suspensions()$dta
    
    list(rem = removals, sus = suspensions, in_rem = in_removals, in_sus = suspensions)
  })
  # rem densplt ====
  output$densityplot <- renderPlot({
    shiny::validate(need(datarem()$in_rem, message = FALSE))
    mlt <- reshape2::melt(datarem()$rem)
    mlt$Index <- 1:nrow(mlt)
    mlt$Value <- mlt$value
    ggplot(mlt, aes(Value)) +
      geom_density() +
      ggtitle("Kernel Density Estimate of Prior Removals") +
      xlab("Hour Values") +
      ylab("Density Estimate")
  })
  # rem weightsplt ====
  output$weightsplot <- renderPlot({
    shiny::validate(need(datarem()$in_sus, message = FALSE))
    removals <- sort(datarem()$rem)
    suspensions <- sort(datarem()$sus)
    weights <- gen_smp_weights(suspensions, removals,
                               density = input$rem_method,
                               type = input$rem_distr, alpha = input$betaalpha, beta = input$betabeta
    )
    df <- reshape2::melt(weights)
    df$`Suspension Index` <- 1:length(weights)
    df$Weight <- df$value
    ggplot(df, aes(x = `Suspension Index`, y = Weight)) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
      ggtitle("Sampling Weights for Removal")
  })
  # Simout ====
  # All simulation outputs are handled to objects interpretable by shiny here
  # initial errors
  output$initial <- renderText({
    datasim()$initial
  })
  # initial errors with reduced bias assessment
  output$initial_rba <- renderText({
    datasim()$initial_rba
  })
  # expected errors in timeframe of simulation
  output$errors <- renderText({
    datasim()$errors
  })
  # switch for the diagnosticplot to be displayed
  output$errorplot <- renderPlot({
    meth <- input$diagnostikplot
    switch(meth,
           "Cumulative" = {
             datasim()$errorplot$total
           },
           "Monthly" = {
             datasim()$errorplot$monthly
           },
           "Fleet" = {
             datasim()$fleetplot
           },
           "Relative" = {
             datasim()$relerrorplot
           },
           "Errorbounds" = {
             datasim()$errorplot_bounds
           },
           {
             print("default")
           }
    )
  })
  # table to display the exportframe for debugging
  output$exportframe <- renderTable({
    datasim()$exportframe
  })
  # functions for xlsx download of bnds data.frame
  datasetInput <- reactive({
    datasim()$exportframe
  })
  # i have no idea what i used this table for; may delete later
  output$table <- renderTable({
    datasetInput()
  })
  # construct downloadable xlsx file
  output$downloadData <- downloadHandler(
    filename = function(file) {
      paste0("fleetdata", Sys.Date(), ".xlsx")
    },
    content = function(con) {
      # print(con) default connection seems to be in AppData/Local/Temp
      write.xlsx2(datasetInput(), con, sheetName = "Fleet Data", row.names = FALSE)
    }
  )
  # Veriftable ====
  # veriftable is used to assess file upload of suspensions. Not necessary but nice
  # to verify that the correct data was uploaded.
  output$verif_table <- renderTable({
    # sus <- datasuspensions()$inputdata$Hours
    sus <- sin_data_suspensions()$dta
    tbl <- qpcR:::cbind.na(head(sus, n = 5L), tail(sus, n = 5L))
    names(tbl) <- c("hSUS", "tSUS")
    return(tbl)
  })
  # hist_susp
  # histogram of suspension data on the first panel of the app
  output$susp_histogram <- renderPlot({
    # sus <- datasuspensions()$inputdata$Hours
    sus <- sin_data_suspensions()$dta
    mlt <- reshape2::melt(sus)
    ggplot(data = mlt, aes(mlt$value)) +
      geom_histogram(binwidth = 50, fill = "darkblue") +
      ggtitle("Histogram of Suspensions") +
      ylab("Count") +
      xlab("Hours") +
      xlim(c(0, (max(sus) + 100))) +
      theme(
        plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_line(colour = "black"),
        panel.grid.minor = element_line(colour = "black")
      )
  })
  # estimplt
  # plot the quickfit plots for the chosen method or in case of weibayes plot the
  # contour and ridge plots for weibayes
  output$estimationplot <- renderPlot({
    vecfails <- fal_data_suspensions()$dta
    shiny::validate(need(vecfails, message = FALSE))
    # suspensions <- datasuspensions()$inputdata$Hours
    suspensions <- sin_data_suspensions()$dta
    meth <- input$estmeth
    switch(meth,
           "Lognormal MLE 2 Par" = {
             MLEln2p(vecfails, suspensions, show = T)
           },
           "Lognormal MLE 3 Par" = {
             MLEln3p(vecfails, suspensions, show = T)
           },
           "Weibull MLE 2 Par" = {
             MLEw2p(vecfails, suspensions, show = T)
           },
           "Weibull MLE 3 Par" = {
             MLEw3p(vecfails, suspensions, show = T)
           },
           "Lognormal MRR 2 Par" = {
             MRRln2p(vecfails, suspensions, show = T)
           },
           "Lognormal MRR 3 Par" = {
             MRRln3p(vecfails, suspensions, show = T)
           },
           "Weibull MRR 2 Par" = {
             MRRw2p(vecfails, suspensions, show = T)
           },
           "Weibull MRR 3 Par" = {
             MRRw3p(vecfails, suspensions, show = T)
           },
           "Weibayes MLE" = {
             plt_weibayesridge(vecfails, suspensions)
           },
           "Weibayes" = {
             plt_weibayesridge(vecfails, suspensions)
           },
           {
             print("default")
           }
    )
  })
  # estimationparameters
  # copy paste modify just for the parameter table, i should change this later on
  # very bad style and waste of space
  dataestimationparams <- reactive({
    vecfails <- fal_data_suspensions()$dta
    shiny::validate(need(vecfails, message = FALSE))
    # suspensions <- datasuspensions()$inputdata$Hours
    suspensions <- sin_data_suspensions()$dta
    meth <- input$estmeth
    obj <- switch(meth,
                  "Lognormal MLE 2 Par" = {
                    MLEln2p(vecfails, suspensions, show = T)
                  },
                  "Lognormal MLE 3 Par" = {
                    MLEln3p(vecfails, suspensions, show = T)
                  },
                  "Weibull MLE 2 Par" = {
                    MLEw2p(vecfails, suspensions, show = T)
                  },
                  "Weibull MLE 3 Par" = {
                    MLEw3p(vecfails, suspensions, show = T)
                  },
                  "Lognormal MRR 2 Par" = {
                    MRRln2p(vecfails, suspensions, show = T)
                  },
                  "Lognormal MRR 3 Par" = {
                    MRRln3p(vecfails, suspensions, show = T)
                  },
                  "Weibull MRR 2 Par" = {
                    MRRw2p(vecfails, suspensions, show = T)
                  },
                  "Weibull MRR 3 Par" = {
                    MRRw3p(vecfails, suspensions, show = T)
                  },
                  "Weibayes MLE" = {
                    c("Eta" = weibayesfix(vecfails, suspensions, beta = input$priorbeta), "Beta" = input$priorbeta)
                  },
                  "Weibayes" = {
                    c("Eta" = weibayes.mle(x = mleframe(vecfails, s = suspensions), beta = input$priorbeta), "Beta" = input$priorbeta)
                  },
                  {
                    print("default")
                  }
    )
    df <- as.data.frame(c(obj[2], obj[1]))
    names(df) <- "Coefficients"
    list(table = df)
  })
  # construct table of shape and scale parameters used in param estim and forecasting
  output$estimationparams1 <- renderTable({
    dataestimationparams()$table
  }, digits = 3, rownames = TRUE)
  output$estimationparams2 <- renderTable({
    df <- dataestimationparams()$table
    if (is.null(df)) {
      return(NULL)
    }
    row.names(df) <- c("Shape", "Scale")
    return(df)
  }, digits = 3, rownames = TRUE)
  
  # Inspections ====
  
  insmat_dta <- callModule(
    module = csvFileReadr,
    id = "insmat_dataset"
  )
  
  insmat_res_filter <- callModule(
    module = filterDF,
    id = "insmat_filtering",
    # data_table has to be a reactive expression
    data_table = reactive(insmat_dta()$inputdata),
    data_vars = reactive(names(insmat_dta()$inputdata))
  )
  
  observeEvent(insmat_res_filter$data_filtered(), {
    updateProgressBar(
      session = session, id = "insmat_pbar",
      value = nrow(insmat_res_filter$data_filtered()), total = nrow(insmat_dta()$inputdata)
    )
  })
  
  output$insmat_table <- DT::renderDT({
    insmat_res_filter$data_filtered()
  }, options = list(pageLength = 5)
  )
  
  output$insmat_code_dplyr <- renderPrint({
    insmat_res_filter$code$dplyr
  })
  
  output$insmat_code <- renderPrint({
    insmat_res_filter$code$expr
  })
  
  output$insmat_res_str <- renderPrint({
    str(insmat_res_filter$data_filtered())
  })
  
  insmat_data_values <- reactive({
    shiny::validate(need(input$insmat_values, message = FALSE))
    dta <- eval(parse(text = paste0("insmat_res_filter$data_filtered()$", get("input")$insmat_values)))
    list(dta = dta)
  })
  
  insmat_data_pods <- reactive({
    shiny::validate(need(input$insmat_pod, message = FALSE))
    counts <- eval(parse(text = paste0("insmat_res_filter$data_filtered()$", get("input")$insmat_pod)))
    list(counts = counts)
  })
  
  datainsmat <- reactive({
    if (input$inspectyesno == F) {
      insmat <- matrix(nrow = 1, ncol = 2)
      # If there are no inspections then all data are in the first interval forever
      insmat[] <- c(Inf, 0)
    } else {
      shiny::validate(need(input$insmat_values, message = FALSE))
      shiny::validate(need(input$insmat_pod, message = FALSE))
      insmat <- cbind(insmat_data_values()$dta, insmat_data_pods()$counts)
    }
    list(insmat = insmat)
  })
  
  output$insmat_values <- renderPrint(
    str(insmat_data_values()$dta)
  )
  
  output$insmat_pods <- renderPrint(
    str(insmat_data_pods()$counts)
  )
  
  output$insmat_insmat <- renderPrint(
    str(datainsmat()$insmat)
  )
  
  # DynamicInfluxUI ====
  output$influxUI1 <- renderUI({
    if (input$useinflux == F) {
      return(NULL)
    }
    meth <- input$influxage
    if (is.null(meth)) {
      meth <- "Constant"
    }
    switch(meth,
           "Constant" = {
             numericInput("influxconstant", "Constant:", value = 0, min = 0, max = NA, step = 1)
           },
           "Prior" = {
             fileInput(
               inputId = "influxprior",
               label = "Select Influx Prior.csv",
               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
             )
           }, {
             call <- paste0("r", tolower(meth))
             forms <- formals(call)[2:length(formals(call))]
             numberinputs <- length(forms)
             lapply(1:numberinputs, function(i) {
               numericInput(paste0("influxformal", i), paste0(names(forms)[i]),
                            min = 0, max = 100000, value = 1, step = NA, width = NULL
               )
             })
           }
    )
  })
  # Influx Data and Output ====
  datainfamounts <- reactive({
    if (input$useinflux == F) {
      in_influx <- NULL
      influx <- NULL
    } else {
      in_influx <- input$influxamnts
      if (is.null(in_influx)) {
        influx <- NULL
      } else {
        influx <- scan(in_influx$datapath)
      }
    }
    list(inf = influx)
  })
  output$influxtable <- renderTable({
    datainfamounts()$inf
  })
  datainflux <- reactive({
    doinflux <- input$useinflux
    
    influxprior <- input$influxprior
    if (is.null(influxprior)) {
      influxprior <- 0
    } else {
      influxprior <- scan(influxprior$datapath)
    }
    influxamounts <- input$influxamnts
    if (is.null(influxamounts)) {
      influxamounts <- 0
    } else {
      influxamounts <- scan(influxamounts$datapath)
    }
    
    constantage <- input$influxconstant
    if (is.null(constantage)) {
      constantage <- 0
    }
    if (is.null(input$influxage)) {
      meth <- "Constant"
    } else {
      meth <- input$influxage
    }
    formalslist <- get_formals(meth)
    dump <- switch(meth,
                   "Constant" = {
                     list(age = constantage)
                   },
                   "Prior" = {
                     list(influxprior = influxprior)
                   }, {
                     formalslist
                   }
    )
    list(doinflux = doinflux, influxelps = dump, influxamounts = influxamounts, influxmethod = meth)
  })
  output$influxdebug <- renderPrint({
    print(datainflux())
  })
  # Debugging ====
  output$resfleet <- renderTable({
    tbl <- head(datasim()$resfleet, n = 5)
    return(tbl)
  })
  # output$debugsuspensiontable <- renderTable(head(datasuspensions()$inputdata, n = 5))
  # Simulation Distribution plot ====
  output$simdistplt <- renderPlot({
    beta <- dataforecast()$params[[1]]
    theta <- dataforecast()$params[[2]]
    simdist <- tolower(input$errorcalcdistribution)
    call <- paste0("d", simdist)
    curve(do.call(call, list(x, beta, theta)), from = input$simdistpltmin, to = input$simdistpltmax, col = "blue", xlab = "", ylab = "")
    title(
      main = "Density of the specified distribution",
      xlab = "Time in hours",
      ylab = ""
    )
  })
  # multirunsui & plots ====
  output$multirunsui <- renderUI({
    if (is.null(input$multirunsplot)) {
      return(NULL)
    }
    meth <- input$multirunsplot
    switch(meth,
           "Points" = {
             return(NULL)
           },
           "Violin" = {
             return(NULL)
           },
           "Singlemonth" = {
             numericInput("monthofinterest", "Month of Interest", min = 1, max = NA, step = 1, value = 1)
           },
           return(NULL)
    )
  })
  
  datamultiruns <- eventReactive(input$updatemultiruns, {
    in_removals <- input$removal_file
    in_removals_amnts <- input$removal_amounts
    # sus <- datasuspensions()$inputdata$Hours
    sus <- sin_data_suspensions()$dta
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
    if (is.null(input$rem_distr)) {
      removaltype <- "dunif"
    } else {removaltype <- input$rem_distr}
    # Simulation Multiruns ====
    # single call to be evaluated multiple times
    call <- quote(simulation_multi(
      hmonth = input$hmonth,
      errorfreeperiod = input$errorfreeperiod,
      nmonths = input$nmonths,
      failures = fal_data_suspensions()$dta,
      beta = dataforecast()$params[[1]],
      theta = dataforecast()$params[[2]],
      simdist = tolower(input$errorcalcdistribution),
      suspensions = sus,
      removaldist = rem_dist,
      removalmethod = input$rem_method,
      removaltype = removaltype,
      removeyesno = input$removeyesno,
      removefaulty = input$removefaulty,
      removalamnts = rem_amnts,
      # inspectionmatrix = inspectiondata()$matrix,
      inspectionmatrix = datainsmat()$insmat,
      doinflux = datainflux()$doinflux,
      influxelps = datainflux()$influxelps,
      influxamounts = datainflux()$influxamounts,
      influxmethod = datainflux()$influxmethod,
      betaalpha = input$betaalpha,
      betabeta = input$betabeta,
      replacefaulty = input$replacefaulty,
      maxlyf = input$maxlyf
    ))
    # preparation of the reactive data frame
    vmonths <- 1:input$nmonths
    nruns <- input$numberofruns
    dta <- matrix(0, nrow = length(vmonths), ncol = nruns)
    set.seed(input$multiseed)
    # where the magic happens
    for (i in 1:ncol(dta)) {
      dta[, i] <- eval(call)
    }
    list(
      multidata = dta
    )
  })
  
  output$multirunsplot <- renderPlot({
    meth <- input$multirunsplot
    switch(meth,
           "Points" = {
             plt_multiruns(data = datamultiruns()$multidata, meth = "point")
           },
           "Violin" = {
             plt_multiruns(data = datamultiruns()$multidata, meth = "violin")
           },
           "Singlemonth" = {
             plt_singlemonth(data = datamultiruns()$multidata, month = input$monthofinterest)
           },
           return(NULL)
    )
  })
  # Competing Modes ====
  # Nested loop to construct ui elements
  output$competing_modes_UI1 <- renderUI({
    forms <- formals(FAdist::rweibull3)[2:length(formals(FAdist::rweibull3))]
    forms <- c("Shape", "Scale", "t0")
    numberinputs <- length(forms)
    tags <- tagList()
    for (j in seq_len(input$number_weibulls)) {
      tags[[j]] <- lapply(1:numberinputs, function(i) {
        numericInput(paste0("competing_modes_formal", i, j), paste0(forms[i], " ", j),
                     min = 0, max = 100000, value = 1, step = NA, width = '200px'
        )
      })
    }
    tags
  })
  
  # loop to access data passed to the dynamically generated UI elements
  data_competingmodes <- eventReactive(input$update_competingmodes, {
    beta <- numeric(length = input$number_weibulls)
    eta <- numeric(length = input$number_weibulls)
    t0 <- numeric(length = input$number_weibulls)
    for (i in seq_len(input$number_weibulls)) {
      beta[i] <- eval(parse(text = paste0("input$competing_modes_formal", 1, i)))
      eta[i] <- eval(parse(text = paste0("input$competing_modes_formal", 2, i)))
      t0[i] <- eval(parse(text = paste0("input$competing_modes_formal", 3, i)))
    }
    list(beta = beta, eta = eta, t0 = t0)
  })
  
  data_competingmodes_number_weibulls <- eventReactive(input$update_competingmodes, {
    list(number_weibulls = input$number_weibulls)
  })
  
  output$competing_modes <- renderPlot({
    params <- data_competingmodes()
    colors <- c(
      "#000075", "#4363d8", "#3cb44b", "#f58231", "#911eb4", "#f032e6",
      "#bfef45", "#9A6324", "#000000", "#ffe119"
    )
    # step 1: construct list of wblr objects from the parameters
    wblr_list <- list()
    for (i in seq_len(data_competingmodes_number_weibulls()$number_weibulls)) {
      wblr_list[[i]] <- wblr.fit(
        wblr(
          FAdist::rweibull3(
            1000,
            shape = params$beta[i],
            scale = params$eta[i],
            thres = params$t0[i]
          ),
          dist = if (params$t0[i] == 0) {
            "weibull"
          } else {"weibull3p"}
        ),
        col = "blue", method.fit = "mle"
      )
    }
    # step 2 use these objects to plot the canvas
    plotweibullcanvas(wblr_list)
    # step 3 draw the lines on the damn thing
    for (i in seq_along(params$beta)) {
      curve(
        p2y(
          pweibull(
            x - params[["t0"]][i], params[["beta"]][i], params[["eta"]][[i]]
          ),
          "weibull"
        ),
        n = 10001, from = 1, to = 5e+05, add = TRUE, lwd = 2, col = colors[i]
      )
    }
    # step 4 add the system unreliability line
    sysunrel <- get_system_unrel(params)
    lines(x = sysunrel$x, y = p2y(sysunrel$y, canvas = "weibull"), lwd = 2, col = "#e6194B")
  })
  # POF ====
  # call modules to generate data frames for every input file
  data_we_fleet1 <- callModule(csvFileGeneral, "we_fleet1_csv")
  data_we_fleet2 <- callModule(csvFileGeneral, "we_fleet2_csv")
  data_we_influx1 <- callModule(csvFileGeneral, "we_influx1_csv")
  data_we_influx2 <- callModule(csvFileGeneral, "we_influx2_csv")
  data_we_dta1 <- callModule(csvFileWeird, "we_dta1")
  data_we_dta2 <- callModule(csvFileWeird, "we_dta2")
  # pack everything into list structure needed by purrr
  
  we_processing <- reactive({
    # data to lists
    fleetdata <- list(fleet1 = data_we_fleet1()$inputdata$V1, fleet2 = data_we_fleet2()$inputdata$V1)
    influxdata <- list(influx1 = data_we_influx1()$inputdata$V1, influx2 = data_we_influx2()$inputdata$V1)
    maxage <- list(input$we_maxage1, input$we_maxage2)
    cycles <- list(input$we_cpt1, input$we_cpt2)
    
    riskdata <- list(data_we_dta1()$inputdata, data_we_dta2()$inputdata)
    
    riskdata <- purrr::map(riskdata, plyr::rename, replace = c("# -" = "cycles", "per engine over life" = "pe"))
    risk <- get_risks(fleetdata, influxdata, maxage, cycles, riskdata)
    # handle useinflux case
    df <- plyr::ldply(risk, rbind)
    df <- t(df);df <- as.data.frame(df);
    df <- df[-1, ]; colnames(df) <- c("fleet1", "fleet2")
    
    for (i in 1:ncol(df)) {
      if (sum(is.na(df[,i])) > 0) {
        df[(1 + max(which(!is.na(df[,i])))):nrow(df), i] <- df[max(which(!is.na(df[,i]))), i]
      }
      df[,i] <- as.numeric(levels(df[,i]))[df[,i]]
    }
    
    df$sum <- df$fleet1 + df$fleet2
    
    #structure needed: df with colnames above and time column
    df$time <- 1:nrow(df)
    mlt <- reshape2::melt(df, id.vars = "time")
    
    plt <- plt_weirdevents(mlt)
    
    list(fleet = fleetdata, influx = influxdata, maxage = maxage, cycles = cycles,
         riskdata = riskdata, risk = risk, df = df, plt = plt)
  })
  
  output$we_show_inputs <- renderTable({
    # reshape to proper table
    v <- colMax(we_processing()$df)[c("fleet1", "fleet2", "sum")]
    tbl <- data.frame(v)
    colnames(tbl) <- "Events"
    rownames(tbl) <- c("Fleet 1", "Fleet 2", "Sum")
    tbl
  }, rownames = TRUE)
  
  output$weirdevents_plt <- renderPlot({
    we_processing()$plt
  })
  
  # crackprop ====
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
    plt_fds(crack_processing()$crack_result$mlt)
  })
  
  # functions for xlsx download of crackdata
  crack_datasetInput <- reactive({
    crack_processing()$crack_result$ret
  })
  
  # construct downloadable xlsx file
  output$crack_download <- downloadHandler(
    filename = function(file) {
      paste0("crackprop", Sys.Date(), ".xlsx")
    },
    content = function(con) {
      # print(con) default connection seems to be in AppData/Local/Temp
      write.xlsx2(crack_datasetInput(), con, sheetName = "Crackprop Data", row.names = FALSE)
    }
  )
  
  # multi multi ====
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
    if (is.null(input$rem_distr)) {
      removaltype <- "dunif"
    } else {removaltype <- input$rem_distr}
    # pass all arguments from other input panel in here
    # return list and plot outputs
    # also construct an output to display the approximate proportions of every
    # failuremode which is basically all that is needed here
    ls <- mm_simulation(
      hmonth = input$hmonth,
      errorfreeperiod = input$errorfreeperiod,
      nmonths = input$nmonths,
      failures = fal_data_suspensions()$dta,
      #failures = failures,
      shapes = mm_data_params()$shapes,
      scales = mm_data_params()$scales,
      # shapes = list(one = 2.51),
      # scales = list(one = 14400),
      simdist = "weibull",
      simple = TRUE,
      suspensions = sin_data_suspensions()$dta,
      #suspensions = suspensions,
      removaldist = rem_dist,
      #removaldist = NULL,
      removalamnts = rem_amnts,
      #removalamnts = removalamnts,
      removalmethod = input$rem_method,
      removaltype = removaltype,
      #removaltype = "dunif",
      removeyesno = input$removeyesno,
      removefaulty = input$removefaulty,
      replacefaulty = input$replacefaulty,
      inspectionmatrix = datainsmat()$insmat,
      #inspectionmatrix = inspectionmatrix,
      doinflux = datainflux()$doinflux,
      #doinflux = FALSE,
      influxelps = datainflux()$influxelps,
      #influxelps = list(),
      influxamounts = datainflux()$influxamounts,
      #influxamounts = numeric(length = 120),
      influxmethod = datainflux()$influxmethod,
      # influxmethod = "Constant",
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
    mm_data_sim()$plot
  )
  
}

# run ====
shinyApp(ui = ui, server = server)
# shiny::runApp(list(ui = ui, server = server), display.mode = "showcase")


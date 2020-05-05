# helper function from the esquisse package
badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class = "label label-discrete badge-dad", col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class = "label label-datetime badge-dad", col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class = "label label-continue badge-dad", col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class = "label label-default badge-dad", col_name_i)
      }
    }
  )
  res
}

# helper function from the esquisse package
col_type <- function(x, no_id = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (is.data.frame(x) && inherits(x, what = "sf")) {
    x <- x[, setdiff(names(x), attr(x, "sf_column")), drop = FALSE]
  }
  
  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u / n < 0.99 | u <= 30 | no_id) {
        return("discrete")
      } else {
        return("id")
      }
    }
    
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return("time")
    }
    
    if (inherits(x, c("numeric", "integer", "double"))) {
      return("continuous")
    }
  }
  
  NULL
}

# helper function from the esquisse package
geomIcons <- function() {
  geoms <- c(
    "auto", "line", "area", "bar", "histogram",
    "point", "boxplot", "violin", "density",
    "tile", "sf"
  )
  href <- "esquisse/geomIcon/gg-%s.png"
  geomsChoices <- lapply(
    X = geoms,
    FUN = function(x) {
      list(inputId = x, img = sprintf(fmt = href, x), label = capitalize(x))
    }
  )
  
  geomsChoicesNames <- lapply(
    X = geomsChoices,
    FUN = function(x) {
      list(
        style = "width: 90px;",
        tags$img(src = x$img, width = 56, height = 56),
        tags$br(), x$label
      )
    }
  )
  geomsChoicesValues <- unlist(lapply(geomsChoices, `[[`, "label"), use.names = FALSE)
  geomsChoicesValues <- tolower(geomsChoicesValues)
  
  list(names = geomsChoicesNames, values = geomsChoicesValues)
}

# helper function from the esquisse package
capitalize <- function(x) {
  lo <- substring(text = x, first = 2)
  up <- substring(text = x, first = 1, last = 1)
  lo <- tolower(lo)
  up <- toupper(up)
  lo <- gsub(pattern = "_", replacement = " ", x = lo)
  paste0(up, lo)
}

# helper function from the esquisse package
dropListColumns <- function(x) {
  type_col <- vapply(X = x, FUN = typeof, FUN.VALUE = character(1), USE.NAMES = FALSE)
  x[, type_col != "list", drop = FALSE]
}

# helper function to drop factor and character IDs
drop_id <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (inherits(x, c("factor", "character"))) {
        values <- unique(as.character(x))
        # trim whitespace
        values <- values[trimws(values) != ""]
        if (length(values) <= 1) {
          return(NULL)
        }
        if (length(values) >= length(x) * 0.9) {
          return(NULL)
        }
        if (length(values) >= 50) {
          return(NULL)
        }
      }
      x
    }
  )
  data
}

# function to drop NAs
drop_na <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (all(is.na(x))) {
        return(NULL)
      }
      x
    }
  )
  data
}

# function to find out if number has decimal point or not
hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return(!identical(value, truncatedValue))
}

# function to find optimal steps for UI elements
find_range_step <- function(x) {
  max <- max(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  range <- max - min
  if (range < 2 || hasDecimals(min) || hasDecimals(max)) {
    pretty_steps <- pretty(c(min, max), n = 100, high.u.bias = 1)
    n_steps <- length(pretty_steps) - 1
    list(
      range = range(pretty_steps),
      min = min(pretty_steps),
      max = max(pretty_steps),
      step = signif(digits = 10, (max(pretty_steps) - min(pretty_steps)) / n_steps)
    )
  }
  else {
    list(
      range = range(x, na.rm = TRUE),
      min = min,
      max = max,
      step = 1
    )
  }
}

# wrapper to match classes
match_class <- function(x, y) {
  char <- c("character", "factor")
  num <- c("numeric", "integer")
  date <- c("Date", "POSIXt")
  if (inherits(x, num) & inherits(y, num)) {
    return(TRUE)
  }
  if (inherits(x, char) & inherits(y, char)) {
    return(TRUE)
  }
  if (inherits(x, date) & inherits(y, date)) {
    return(TRUE)
  }
  return(FALSE)
}

# fancy or
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# fancy comparator
`%empty%` <- function(a, b) {
  if (a != "") a else b
}

# fancy comparator
`%|e|%` <- function(a, b) {
  if (!is.null(a) && a != "") a else b
}

# base::Negate wrapper for %in%
`%nin%` <- Negate(`%in%`)

# helper function from the esquisse package
list1 <- function(x) {
  if (is.null(x)) {
    return(x)
  }
  if (length(x) == 1 & !is.list(x)) {
    list(x)
  } else {
    x
  }
}

# helper function from the esquisse package
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

# helper function from the esquisse package
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}

# helper function from the esquisse package
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

# helper function from the esquisse package
clean_string <- function(str) {
  str <- stri_trans_general(str = str, id = "Latin-ASCII")
  str <- stri_trans_tolower(str)
  str <- make.unique(str)
  str <- stri_replace_all_regex(str = str, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
  return(str)
}

# helper function from the esquisse package
get_df <- function(df, env = globalenv()) {
  if (df %in% ls(name = env)) {
    get(x = df, envir = env)
  } else if (df %in% data(package = "ggplot2", envir = environment())$results[, "Item"]) {
    get(utils::data(list = df, package = "ggplot2", envir = environment()))
  } else {
    NULL
  }
}

# helper function from the esquisse package
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}

# Utils -------------------------------------------------------------------
# create filters based on data input
create_filters <- function(data, vars = NULL,
                           drop_ids = TRUE,
                           picker = FALSE,
                           width = "100%", session = getDefaultReactiveDomain()) {
  ns <- session$ns
  data <- drop_na(data)
  if (isTRUE(drop_ids)) {
    data <- drop_id(data)
  }
  data <- dropListColumns(data)
  if (is.null(vars)) {
    vars <- names(data)
  } else {
    vars <- intersect(names(data), vars)
  }
  
  filters_id <- paste0("filter_", clean_string(vars))
  filters_id <- setNames(as.list(filters_id), vars)
  filters_na_id <- setNames(as.list(paste0("na_", filters_id)), vars)
  # construct ui to be returned as a list through the use of lapply
  ui <- lapply(
    X = vars,
    # "inline" function to construct UI
    FUN = function(variable) {
      var <- data[[variable]]
      any_na <- anyNA(var)
      var <- var[!is.na(var)]
      id <- filters_id[[variable]]
      tag_label <- if (any_na) {
        tags$span(
          tags$label(variable), HTML("&nbsp;&nbsp;"),
          na_filter(id = ns(paste0("na_", id)))
        )
      } else {
        tags$span(tags$label(variable), HTML("&nbsp;&nbsp;"))
      }
      # numeric inputs
      if (inherits(x = var, what = c("numeric", "integer"))) {
        params <- find_range_step(var)
        tags$div(
          style = "position: relative;",
          tag_label,
          set_slider_attr(sliderInput(
            inputId = ns(id),
            min = params$min,
            max = params$max,
            width = width,
            value = params$range,
            step = params$step,
            label = NULL
          ))
        )
        # date inputs
      } else if (inherits(x = var, what = c("Date", "POSIXct"))) {
        range_var <- range(var)
        tags$div(
          style = "position: relative;",
          tag_label,
          set_slider_attr(sliderInput(
            inputId = ns(id),
            min = min(var),
            max = max(var),
            width = width,
            value = range(var),
            label = NULL
          ))
        )
      } else {
        # character inputs
        values <- unique(as.character(var))
        values <- values[trimws(values) != ""]
        if (isTRUE(picker)) {
          tags$div(
            style = "position: relative;",
            tag_label,
            pickerInput(
              inputId = ns(id),
              choices = values,
              selected = values,
              label = NULL,
              multiple = TRUE,
              width = width,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = "count",
                liveSearch = TRUE
              )
            )
          )
        } else {
          tags$div(
            style = "position: relative;",
            class = if (length(values) > 15) "selectize-big",
            tag_label,
            selectizeInput(
              inputId = ns(id),
              choices = values,
              selected = values,
              label = NULL,
              multiple = TRUE,
              width = width,
              options = list(plugins = list("remove_button"))
            )
          )
        }
      }
    }
  )
  list(
    ui = tagList(ui),
    filters_id = filters_id,
    filters_na_id = filters_na_id
  )
}

# helper function to set tag attributes
tagSetAttributes <- function(tag, ...) {
  tag$attribs[names(list(...))] <- NULL
  tag$attribs <- c(tag$attribs, list(...))
  tag
}

# helper function to set slider attributes
set_slider_attr <- function(slider) {
  slider$children[[2]] <- tagSetAttributes(
    tag = slider$children[[2]],
    `data-force-edges` = "true",
    `data-grid-num` = "4"
  )
  slider
}

# na switch to filter NA values out of data frame
na_filter <- function(id) {
  tags$span(
    style = "position: absolute; right: 0px; margin-right: -20px;",
    prettySwitch(
      inputId = id,
      label = "NA",
      value = TRUE,
      slim = TRUE,
      status = "primary",
      inline = TRUE
    )
  )
}

# black magic fuckery to construct dplyr and base R calls according to chosen filter criteria
make_expr_filter <- function(filters, filters_na, data, data_name) {
  expressions <- lapply(
    X = names(filters),
    FUN = function(var) {
      values <- filters[[var]]
      nas <- filters_na[[var]]
      data_values <- data[[var]]
      if (!is.null(values) & !match_class(values, data_values)) {
        return(NULL)
      }
      values_expr <- NULL
      if (inherits(x = values, what = c("numeric", "integer"))) {
        data_range <- find_range_step(data_values)$range
        if (!isTRUE(all.equal(values, data_range))) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else if (inherits(x = values, what = c("Date", "POSIXct"))) {
        values <- format(values)
        data_range <- range(data_values, na.rm = TRUE)
        data_range <- format(data_range)
        if (!identical(values, data_range)) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else {
        data_values <- unique(as.character(data_values))
        if (!identical(sort(values), sort(data_values))) {
          if (length(values) == 0) {
            if (isTRUE(nas)) {
              values_expr <- expr(is.na(!!sym(var)))
            } else {
              values_expr <- expr(!(!!sym(var) %in% !!data_values[!is.na(data_values)]) & !is.na(!!sym(var)))
            }
          } else {
            if (length(values) <= length(data_values) / 2) {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!!sym(var) %in% !!values | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!!sym(var) %in% !!values)
                }
              } else {
                values_expr <- expr(!!sym(var) %in% !!values)
              }
            } else {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              } else {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) & !is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              }
            }
          }
        }
      }
      if (is.null(values_expr) & !isTRUE(nas) & anyNA(data_values)) {
        expr(!is.na(!!sym(var)))
      } else {
        values_expr
      }
    }
  )
  expressions <- dropNullsOrEmpty(expressions)
  expr_dplyr <- Reduce(
    f = function(x, y) expr(!!x %>% filter(!!y)),
    x = expressions,
    init = expr(!!sym(data_name))
  )
  expression <- Reduce(
    f = function(x, y) expr(!!x & !!y),
    x = expressions
  )
  return(list(
    expr_dplyr = expr_dplyr,
    expr = expression
  ))
}

filterDF_UI <- function(id, show_nrow = TRUE) {
  # set namespace needed for shiny module
  ns <- NS(id)
  # set taglist for ui element
  tagList(
    # wrap contents (tags) that should be included in the underlying generated
    # HTML only once, yet may appear in the document generating code more than
    # once. Only the first appearance of the content will be used.
    singleton(
      tags$style(
        ".selectize-big .selectize-input {height: 72px; overflow-y: scroll;}"
      )
    ),
    if (isTRUE(show_nrow)) uiOutput(outputId = ns("nrow")),
    tags$div(id = ns("placeholder-filters"))
  )
}

filterDF <- function(input, output, session,
                     data_table = reactive(),
                     data_vars = shiny::reactive(NULL),
                     data_name = reactive("data"),
                     label_nrow = "Number of rows:",
                     drop_ids = TRUE,
                     picker = FALSE) {
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  output$nrow <- renderUI({
    if (!is.null(label_nrow)) {
      tags$p(label_nrow, tags$b(nrow(data_filtered()), "/", nrow(data_table())))
    }
  })
  
  rv_filters <- reactiveValues(mapping = NULL, mapping_na = NULL)
  rv_code <- reactiveValues(expr = NULL, dplyr = NULL)
  
  observe({
    data <- data_table()
    vars <- data_vars()
    removeUI(selector = jns("filters_inputs"), immediate = TRUE)
    filters <- create_filters(
      data = data, vars = vars,
      drop_ids = drop_ids, picker = picker
    )
    insertUI(
      selector = jns("placeholder-filters"),
      ui = tags$div(
        id = ns("filters_inputs"),
        filters$ui
      ),
      immediate = TRUE
    )
    rv_filters$mapping <- filters$filters_id
    rv_filters$mapping_na <- filters$filters_na_id
  })
  
  data_filtered <- reactive({
    data <- data_table()
    req(all(names(rv_filters$mapping) %in% names(data)))
    filter_inputs <- lapply(
      X = rv_filters$mapping,
      FUN = function(x) {
        input[[x]]
      }
    )
    filter_nas <- lapply(
      X = rv_filters$mapping_na,
      FUN = function(x) {
        input[[x]]
      }
    )
    filters <- make_expr_filter(
      filters = filter_inputs,
      filters_na = filter_nas,
      data = data,
      data_name = isolate(data_name())
    )
    rv_code$expr <- filters$expr
    rv_code$dplyr <- filters$expr_dplyr
    if (length(rv_code$expr) > 0) {
      result <- eval_tidy(expr = rv_code$expr, data = data)
      data[result, ]
    } else {
      data
    }
  })
  
  list(
    data_filtered = data_filtered,
    code = rv_code
  )
}

# file input module
csvFileInputReadr <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # all input or output IDs of any kind need to be wrapped in a call to ns()
  # wrap everything in a tagList to return multiple UI elements
  tagList(
    fileInput(ns("file"), label)
    
  )
}

# Module server function
csvFileReadr <- function(input, output, session, stringsAsFactors) {
  userFile <- reactive({
    # If no file is selected, don't do anything
    # for future reference: shiny::validate is masked by jsonlite::validate
    shiny::validate(need(input$file, message = FALSE))
    input$file
  })
  
  # parse file to data frame
  dataframe <- reactive({
    dta <- read.xlsx(
      file = userFile()$datapath,
      sheetIndex = 1,
      stringsAsFactors = FALSE
    )
    dta[is.na(dta)] <- 0
    dta <- read.xlsx(
      file = userFile()$datapath,
      sheetIndex = 1,
      stringsAsFactors = FALSE
    )
    list(inputdata = dta)
  })
  
  # verify file upload
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  # return the list that contains the dataframe object
  return(dataframe)
}

ui <- fluidPage(
  tags$h2("Filtered data input"),
  splitLayout(
    csvFileInputReadr("dataset", label = "Input Data"),
    textInput(
      inputId = "suspension_ID",
      label = "Suspension ID:",
      value = "Default"
    ),
    textInput(
      inputId = "failure_ID",
      label = "Failure ID:",
      value = "Default"
    )
  ),
  
  fluidRow(
    column(
      width = 3,
      filterDF_UI("filtering")
    ),
    column(
      width = 9,
      progressBar(
        id = "pbar", value = 100,
        total = 100, display_pct = TRUE
      ),
      DT::dataTableOutput(outputId = "table"),
      tags$p("Code dplyr:"),
      verbatimTextOutput(outputId = "code_dplyr"),
      tags$p("Expression:"),
      verbatimTextOutput(outputId = "code"),
      tags$p("Filtered data:"),
      verbatimTextOutput(outputId = "res_str")
    )
  )
)

server <- function(input, output, session) {
  dta <- callModule(
    module = csvFileReadr,
    id = "dataset"
  )
  
  res_filter <- callModule(
    module = filterDF,
    id = "filtering",
    # data_table has to be a reactive expression
    data_table = reactive(dta()$inputdata)
  )
  
  observeEvent(res_filter$data_filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$data_filtered()), total = nrow(dta()$inputdata)
    )
  })
  
  output$table <- DT::renderDT(
    {
      res_filter$data_filtered()
    },
    options = list(pageLength = 5)
  )
  
  
  output$code_dplyr <- renderPrint({
    res_filter$code$dplyr
  })
  output$code <- renderPrint({
    res_filter$code$expr
  })
  
  output$res_str <- renderPrint({
    str(res_filter$data_filtered())
  })
}


shinyApp(ui, server)

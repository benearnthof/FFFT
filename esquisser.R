# addin to interactively make ggplots
library("miniUI")
esquisserServer <- function(input, output, session, data = NULL, dataModule = c("GlobalEnv", "ImportFile"), sizeDataModule = "m") {
  ggplotCall <- reactiveValues(code = "")
  
  observeEvent(data$data,
               {
                 dataChart$data <- data$data
                 dataChart$name <- data$name
               },
               ignoreInit = FALSE
  )
  
  dataChart <- callModule(
    module = chooseDataServer,
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = is.null(isolate(data$data)),
    coerceVars = getOption(x = "esquisse.coerceVars", default = FALSE),
    dataModule = dataModule, size = sizeDataModule
  )
  observeEvent(dataChart$data, {
    # special case: geom_sf
    if (inherits(dataChart$data, what = "sf")) {
      geom_possible$x <- c("sf", geom_possible$x)
    }
    var_choices <- setdiff(names(dataChart$data), attr(dataChart$data, "sf_column"))
    updateDragulaInput(
      session = session,
      inputId = "dragvars", status = NULL,
      choiceValues = var_choices,
      choiceNames = badgeType(
        col_name = var_choices,
        col_type = col_type(dataChart$data[, var_choices])
      ),
      badge = FALSE
    )
  })
  
  geom_possible <- reactiveValues(x = "auto")
  geom_controls <- reactiveValues(x = "auto")
  observeEvent(list(input$dragvars$target, input$geom), {
    geoms <- potential_geoms(
      data = dataChart$data,
      mapping = build_aes(
        data = dataChart$data,
        x = input$dragvars$target$xvar,
        y = input$dragvars$target$yvar
      )
    )
    geom_possible$x <- c("auto", geoms)
    
    geom_controls$x <- select_geom_controls(input$geom, geoms)
    
    if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  })
  
  observeEvent(geom_possible$x, {
    geoms <- c(
      "auto", "line", "area", "bar", "histogram",
      "point", "boxplot", "violin", "density",
      "tile", "sf"
    )
    updateDropInput(
      session = session,
      inputId = "geom",
      selected = setdiff(geom_possible$x, "auto")[1],
      disabled = setdiff(geoms, geom_possible$x)
    )
  })
  
  # Module chart controls : title, xlabs, colors, export...
  paramsChart <- reactiveValues(inputs = NULL)
  paramsChart <- callModule(
    module = chartControlsServer,
    id = "controls",
    type = geom_controls,
    data_table = reactive(dataChart$data),
    data_name = reactive({
      req(dataChart$name)
      dataChart$name
    }),
    ggplot_rv = ggplotCall,
    aesthetics = reactive({
      vars <- dropNullsOrEmpty(input$dragvars$target)
      names(vars)
    }),
    use_facet = reactive({
      !is.null(input$dragvars$target$facet)
    }),
    use_transX = reactive({
      if (is.null(input$dragvars$target$xvar)) {
        return(FALSE)
      }
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$xvar]]),
        y = "continuous"
      )
    }),
    use_transY = reactive({
      if (is.null(input$dragvars$target$yvar)) {
        return(FALSE)
      }
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$yvar]]),
        y = "continuous"
      )
    })
  )
  
  
  output$plooooooot <- renderPlot({
    req(input$play_plot, cancelOutput = TRUE)
    req(dataChart$data)
    req(paramsChart$data)
    req(paramsChart$inputs)
    req(input$geom)
    
    aes_input <- make_aes(input$dragvars$target)
    
    req(unlist(aes_input) %in% names(dataChart$data))
    
    mapping <- build_aes(
      data = dataChart$data,
      .list = aes_input,
      geom = input$geom
    )
    
    geoms <- potential_geoms(
      data = dataChart$data,
      mapping = mapping
    )
    req(input$geom %in% geoms)
    
    data <- paramsChart$data
    
    scales <- which_pal_scale(
      mapping = mapping,
      palette = paramsChart$inputs$palette,
      data = data
    )
    
    if (identical(input$geom, "auto")) {
      geom <- "blank"
    } else {
      geom <- input$geom
    }
    
    geom_args <- match_geom_args(input$geom, paramsChart$inputs, mapping = mapping)
    
    if (isTRUE(paramsChart$smooth$add) & input$geom %in% c("point", "line")) {
      geom <- c(geom, "smooth")
      geom_args <- c(
        setNames(list(geom_args), input$geom),
        list(smooth = paramsChart$smooth$args)
      )
    }
    
    scales_args <- scales$args
    scales <- scales$scales
    
    if (isTRUE(paramsChart$transX$use)) {
      scales <- c(scales, "x_continuous")
      scales_args <- c(scales_args, list(x_continuous = paramsChart$transX$args))
    }
    
    if (isTRUE(paramsChart$transY$use)) {
      scales <- c(scales, "y_continuous")
      scales_args <- c(scales_args, list(y_continuous = paramsChart$transY$args))
    }
    
    gg_call <- ggcall(
      data = dataChart$name,
      mapping = mapping,
      geom = geom,
      geom_args = geom_args,
      scales = scales,
      scales_args = scales_args,
      labs = paramsChart$labs,
      theme = paramsChart$theme$theme,
      theme_args = paramsChart$theme$args,
      coord = paramsChart$coord,
      facet = input$dragvars$target$facet,
      facet_args = paramsChart$facet
    )
    
    ggplotCall$code <- expr_deparse(gg_call, width = 1e4)
    ggplotCall$call <- gg_call
    
    ggplotCall$ggobj <- safe_ggplot(
      expr = gg_call,
      data = setNames(list(data), dataChart$name)
    )
    ggplotCall$ggobj$plot
  })
  
  
  # Close addin
  observeEvent(input$close, shiny::stopApp())
  
  # Ouput of module (if used in Shiny)
  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
  observeEvent(ggplotCall$code,
               {
                 output_module$code_plot <- ggplotCall$code
               },
               ignoreInit = TRUE
  )
  observeEvent(paramsChart$data,
               {
                 output_module$code_filters <- reactiveValuesToList(paramsChart$code)
                 output_module$data <- paramsChart$data
               },
               ignoreInit = TRUE
  )
  
  return(output_module)
}

esquisserUI <- function(id, header = TRUE,
                        container = esquisseContainer(),
                        choose_data = TRUE,
                        insert_code = FALSE,
                        disable_filters = FALSE) {
  ns <- NS(id)
  
  box_title <- tags$div(
    class = "gadget-title dreamrs-title-box",
    tags$h1(shiny::icon("wrench"), "ggplot2 builder", class = "dreamrs-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),
    if (isTRUE(choose_data) & isTRUE(header)) {
      tags$div(
        class = "pull-left",
        chooseDataUI(id = ns("choose-data"), class = "btn-sm")
      )
    }
  )
  
  addin <- miniPage(
    
    # style sheet
    singleton(x = tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "esquisse/styles.css"),
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),
    
    if (isTRUE(header)) box_title,
    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = if (isTRUE(choose_data) & !isTRUE(header)) "padding: 10px;" else "padding: 8px; height: 108%;",
          dropInput(
            inputId = ns("geom"),
            choicesNames = geomIcons()$names,
            choicesValues = geomIcons()$values,
            dropWidth = "290px",
            width = "100%"
          ),
          if (isTRUE(choose_data) & !isTRUE(header)) chooseDataUI(id = ns("choose-data"))
        )
      ),
      top_right = dragulaInput(
        inputId = ns("dragvars"),
        sourceLabel = "Variables",
        targetsLabels = c("X", "Y", "Fill", "Color", "Size", "Group", "Facet"),
        targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet"),
        choices = "",
        badge = FALSE,
        width = "100%",
        height = "100%",
        replace = TRUE
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        tags$div(
          style = "position: absolute; right: 0; top: 10px; font-weight: bold; z-index: 1000;",
          prettyToggle(
            inputId = ns("play_plot"),
            value = TRUE,
            label_on = "Play",
            label_off = "Pause",
            outline = TRUE,
            plain = TRUE,
            bigger = TRUE,
            inline = TRUE,
            icon_on = icon("play-circle-o", class = "fa-2x"),
            icon_off = icon("pause-circle-o", class = "fa-2x")
          )
        ),
        shiny::plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
      )
    ),
    
    chartControlsUI(
      id = ns("controls"),
      insert_code = insert_code,
      disable_filters = disable_filters
    )
  )
  
  if (is.function(container)) {
    addin <- container(addin)
  }
  return(addin)
}

esquisseContainer <- function(width = "100%", height = "700px", fixed = FALSE) {
  force(width)
  force(height)
  force(fixed)
  function(...) {
    if (identical(fixed, FALSE)) {
      tag <- tags$div(
        style = sprintf("width: %s;", validateCssUnit(width)),
        style = sprintf("height: %s;", validateCssUnit(height)),
        ...
      )
    } else {
      if (identical(fixed, TRUE)) {
        tag <- tags$div(
          style = "position: fixed; top: 0; bottom: 0; right: 0; left: 0;",
          ...
        )
      } else if (length(fixed) == 4) {
        tag <- tags$div(
          style = do.call(
            sprintf,
            c(list(
              fmt = "position: fixed; top: %s; right: %s; bottom: %s; left: %s;"
            ), lapply(fixed, validateCssUnit))
          ),
          ...
        )
      } else {
        stop(
          "fixed must be ever a logical TRUE/FALSE or a vector of length 4 of valid CSS unit.",
          call. = FALSE
        )
      }
    }
    tagList(
      singleton(tags$head(
        tags$style("html, body {overflow: visible !important;")
      )), tag
    )
  }
}

esquisser <- function(data = NULL,
                      coerce_vars = getOption(x = "esquisse.coerceVars", default = TRUE),
                      disable_filters = getOption(x = "esquisse.disable_filters", default = FALSE),
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {
  options("esquisse.coerceVars" = coerce_vars)
  
  res_data <- get_data(data, name = deparse(substitute(data)))
  if (!is.null(res_data$esquisse_data)) {
    res_data$esquisse_data <- dropListColumns(res_data$esquisse_data)
  }
  rv <- reactiveValues(
    data = res_data$esquisse_data,
    name = res_data$esquisse_data_name
  )
  
  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      "Esquisse 0.2.0 - Codename 'Bootleg'",
      width = 1000, height = 750
    )
  }
  
  runGadget(
    app = esquisserUI(id = "esquisse", container = NULL, insert_code = TRUE),
    server = function(input, output, session) {
      callModule(
        module = esquisserServer,
        id = "esquisse",
        data = rv
      )
    },
    viewer = inviewer
  )
}

get_data <- function(data = NULL, name = NULL) {
  
  if (!is.null(data)) {
    if (is.character(data)) {
      esquisse_data <- try({
        dat <- get(x = data, envir = globalenv())
        if (inherits(dat, what = "sf")) {
          dat
        } else {
          as.data.frame(dat)
        }
      }, silent = TRUE)
      esquisse_data_name <- data
      if ("try-error" %in% class(esquisse_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        esquisse_data <- NULL
        esquisse_data_name <- ""
      }
    } else if (inherits(x = data, what = "data.frame")) {
      esquisse_data <- try({
        if (inherits(data, what = "sf")) {
          data
        } else {
          as.data.frame(data)
        }
      }, silent = TRUE)
      if ("try-error" %in% class(esquisse_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        esquisse_data <- NULL
        esquisse_data_name <- ""
      } else {
        if (!is.null(name)) {
          esquisse_data_name <- as.character(name)
        } else {
          esquisse_data_name <- deparse(substitute(data))
        }
      }
      
      # esquisse_data_name <- gsub("\\[.*", "", esquisse_data_name)
    } else {
      esquisse_data <- NULL
      esquisse_data_name <- ""
    }
  } else {
    if (rstudioapi::isAvailable()) {
      context <- try(rstudioapi::getSourceEditorContext(), silent = TRUE)
      if ("try-error" %in% class(context) || is.null(context)) {
        esquisse_data <- NULL
        esquisse_data_name <- ""
      } else {
        context_select <- context$selection[[1]]$text
        if (isTRUE(nzchar(context_select))) {
          esquisse_data <- try(as.data.frame(get(x = context_select, envir = globalenv())), silent = TRUE)
          esquisse_data_name <- context_select
          if ("try-error" %in% class(esquisse_data)) {
            warning(paste0("Failed to retrieve data from the selection"), call. = FALSE)
            esquisse_data <- NULL
            esquisse_data_name <- ""
          }
        } else {
          esquisse_data <- NULL
          esquisse_data_name <- ""
        }
      }
    } else {
      esquisse_data <- NULL
      esquisse_data_name <- ""
    }
  }
  
  list(esquisse_data = esquisse_data, esquisse_data_name = esquisse_data_name)
}

chooseDataUI <- function(id, label = "Data", icon = "database", ...) {
  
  ns <- NS(id)
  
  if (is.character(icon))
    icon <- icon(icon)
  
  tagList(
    singleton(
      tags$link(rel="stylesheet", type="text/css",
                href="esquisse/styles-dad.css")
    ),
    useShinyUtils(),
    actionButton(
      inputId = ns("changeData"), label = label,
      icon = icon, width = "100%", ...
    )
  )
}

useShinyUtils <- function() {
  singleton(tags$head(tags$script(src = "esquisse/shiny-utils.js")))
}

layoutAddin <- function(top_left, top_right, main) {
  shiny::fillPage(
    shiny::fillCol(
      flex = c(1, 4),
      shiny::fillRow(
        flex = c(1, 6), width = "100%", style = "margin: auto;",
        top_left,
        top_right
      ),
      main
    )
  )
}

dropInput <- function(inputId, choicesNames, choicesValues, selected = NULL,
                      dropUp = FALSE, dropWidth = NULL, dropMaxHeight = NULL,
                      dropPreScrollable = FALSE, btnClass = "btn-link",
                      width = NULL) {
  if (length(choicesValues) != length(choicesNames))
    stop("dropInput: 'choicesNames' and 'choicesValues' must have same length!", call. = FALSE)
  if (is.null(selected))
    selected <- choicesValues[1]
  if (!selected %in% choicesValues)
    stop("dropInput: 'selected' must be an element of 'choicesValues'", call. = FALSE)
  if (!inherits(choicesNames, "list"))
    stop("dropInput: 'choicesNames' must be a list or a tagList", call. = FALSE)
  tagSelected <- choicesNames[[which(choicesValues == selected)]]
  btn <- do.call(
    what = tags$button,
    args = c(tagSelected, list(
      style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"),
      class = "btn btn-default drop-input-main dropdown-toggle",
      `data-toggle` = "dropdown",
      `data-value` = selected
    ))
  )
  dropTag <- tags$div(
    class = "dropdown-menu",
    class = if (isTRUE(dropPreScrollable)) "pre-scrollable",
    style = "padding: 5px;",
    style = if (!is.null(dropMaxHeight))
      paste0("max-height: ", validateCssUnit(dropMaxHeight), ";"),
    style = if (!is.null(dropWidth))
      paste0("width: ", validateCssUnit(dropWidth), ";"),
    lapply(
      X = seq_along(choicesNames),
      FUN = function(i) {
        do.call(
          what = tags$button,
          args = c(choicesNames[[i]], list(
            class = "btn drop-input-button",
            class = btnClass,
            style = "text-decoration: none !important;",
            `data-value` = choicesValues[i]
          ))
        )
      }
    )
  )
  tagList(
    singleton(
      tags$head(
        tags$script(src = "esquisse/drop/dropInput-bindings.js")
      )
    ),
    tags$div(
      id = inputId, class = "drop-input",
      class = ifelse(dropUp, "dropup", "dropdown"),
      style = "margin-bottom: 1vh;",
      btn, dropTag
    )
  )
}

updateDropInput <- function(session, inputId, selected = NULL, disabled = NULL) {
  if (!is.null(disabled) && length(disabled) == 1)
    disabled <- list(disabled)
  message <- dropNulls(list(
    selected = selected,
    disabled = disabled
  ))
  session$sendInputMessage(inputId, message)
}

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

capitalize <- function(x) {
  lo <- substring(text = x, first = 2)
  up <- substring(text = x, first = 1, last = 1)
  lo <- tolower(lo)
  up <- toupper(up)
  lo <- gsub(pattern = "_", replacement = " ", x = lo)
  paste0(up, lo)
}

dragulaInput <- function(inputId, sourceLabel, targetsLabels,
                         targetsIds = NULL,
                         choices = NULL, choiceNames = NULL,
                         choiceValues = NULL, status = "primary",
                         replace = FALSE, badge = TRUE,
                         dragulaOpts = list(),
                         boxStyle = NULL,
                         width = NULL, height = "200px") {
  
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  
  if (is.null(targetsIds)) {
    targetsIds <- gsub(pattern = "[^[:alnum:]]", replacement = "", x = targetsLabels)
  } else {
    stopifnot(length(targetsLabels) == length(targetsIds))
  }
  
  replaceTargets <- targetsIds
  if (is.numeric(replace)) {
    replaceTargets <- targetsIds[replace]
    replace <- TRUE
  } else {
    stopifnot(is.logical(replace))
  }
  replaceTargets <- paste0(inputId, "-target-", replaceTargets)
  
  target_list <- lapply(
    X = seq_along(targetsLabels),
    FUN = function(i) {
      tags$div(
        style = "height: 95%; margin: 0;",
        style = boxStyle,
        class = "box-dad xyvar",
        id = paste(inputId, "target", targetsIds[i], sep = "-"),
        style = make_bg_svg(targetsLabels[i])
      )
    }
  )
  target_list$style <- "height: 50%; font-size: 0;"
  target_list$cellArgs <- list(style = "height:90%; padding: 0; margin-right: 0.5%;")
  target_list$width <- width
  
  tgw <- 100 / length(targetsIds)
  tgw <- tgw - 0.5 # / (length(targetsIds)-1)
  tgw <- paste0(tgw, "%")
  target_list$cellWidths <- tgw
  
  tagList(
    html_dependency_dragula(),
    tags$div(
      class="form-group shiny-input-container shiny-input-dragula shiny-input-container-inline",
      style = if(!is.null(width)) paste("width:", validateCssUnit(width), ";"),
      style = if(!is.null(height)) paste("height:", validateCssUnit(height), ";"),
      id = inputId,
      tags$div(
        class = "container-drag-source",
        style = boxStyle,
        style = make_bg_svg(sourceLabel),
        tags$div(
          id = paste(inputId, "source", sep = "-"),
          style = "margin: 5px; width: 100%; min-height: 15px; margin-right: 0;",
          makeDragulaChoices(inputId = inputId, args = args, status = status, badge = badge)
        )
      ),
      do.call(splitLayout, target_list),
      tags$script(
        type = "application/json",
        `data-for` = inputId,
        toJSON(list(
          source = list1(paste(inputId, "source", sep = "-")),
          targets = list1(paste(inputId, "target", targetsIds, sep = "-")),
          replace = replace,
          replaceIds = list1(replaceTargets),
          options = dragulaOpts
        ), auto_unbox = TRUE, json_verbatim = TRUE)
      )
    )
  )
}

make_bg_svg <- function(text) {
  svg <- tag("svg", list(
    xmlns = "http://www.w3.org/2000/svg",
    version = "1.1",
    tag("text", list(
      x = "100%",
      y = "20",
      opacity = "0.15",
      fill = "E6E6E6",
      "font-weight" = "bold",
      "font-family" = "Helvetica, Arial, sans-serif",
      "font-size" = "24",
      "text-anchor" = "end",
      text
    ))
  ))
  svg <- doRenderTags(svg)
  svg <- base64_enc(svg)
  svg <- stri_replace_all_fixed(svg, pattern = "\n", replacement = "")
  svg <- sprintf(
    "background-image:url(\"data:image/svg+xml;base64,%s\");",
    svg
  )
  paste0(svg, "background-color:white; background-repeat:no-repeat; background-position:right bottom;")
}

updateDragulaInput <- function(session, inputId, choices = NULL, choiceNames = NULL,
                               choiceValues = NULL, badge = TRUE, status = "primary") {
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  choices <- htmltools::doRenderTags(makeDragulaChoices(
    inputId = session$ns(inputId), args = args, status = status, badge = badge
  ))
  message <- list(choices = choices)
  session$sendInputMessage(inputId, message)
}

makeDragulaChoices <- function(inputId, args, status = NULL, badge = TRUE) {
  lapply(
    X = seq_along(args$choiceNames),
    FUN = function(i) {
      tags$span(
        class = "label-dragula",
        class = if (badge) "label",
        class = if (badge & !is.null(status)) paste0("label-", status),
        # id = paste(inputId, "target-label", sep = "-"),
        id = paste(inputId, "target-label", clean_string(args$choiceValues[[i]]), sep = "-"),
        `data-value` = args$choiceValues[[i]],
        args$choiceNames[[i]]
      )
    }
  )
}

html_dependency_dragula <- function() {
  htmlDependency(
    name = "esquisse-dragula",
    version = "0.1.0",
    src = c(file = "assets", href = "esquisse"),
    package = "esquisse",
    script = c("dragula/dragula.min.js", "dragula/dragula-bindings.js"),
    stylesheet = c("dragula/dragula.min.css", "styles-dad.css"),
    all_files = FALSE
  )
}

normalizeChoicesArgs <- function(choices, choiceNames, choiceValues, mustExist = TRUE) {
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ",
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      }
      else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          return(list(choiceNames = NULL, choiceValues = NULL))
        }
        else {
          stop("One of `choiceNames` or `choiceValues` was set to ",
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    if (anyNamed(choiceNames) || anyNamed(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must not be named.")
    }
  }
  else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices)
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }
  return(list(choiceNames = as.list(choiceNames), choiceValues = as.list(as.character(choiceValues))))
}


choicesWithNames <- function(choices) {
  listify <- function(obj) {
    makeNamed <- function(x) {
      if (is.null(names(x)))
        names(x) <- character(length(x))
      x
    }
    res <- lapply(obj, function(val) {
      if (is.list(val))
        listify(val)
      else if (length(val) == 1 && is.null(names(val)))
        val
      else makeNamed(as.list(val))
    })
    makeNamed(res)
  }
  choices <- listify(choices)
  if (length(choices) == 0)
    return(choices)
  choices <- mapply(choices, names(choices), FUN = function(choice,
                                                            name) {
    if (!is.list(choice))
      return(choice)
    if (name == "")
      stop("All sub-lists in \"choices\" must be named.")
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]
  choices
}


anyNamed <- function(x) {
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  any(nzchar(nms))
}

library(jsonlite)
library(htmltools)

clean_string <- function(str) {
  str <- stri_trans_general(str = str, id = "Latin-ASCII")
  str <- stri_trans_tolower(str)
  str <- make.unique(str)
  str <- stri_replace_all_regex(str = str, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
  return(str)
}

list1 <- function(x) {
  if (is.null(x))
    return(x)
  if (length(x) == 1 & !is.list(x)) {
    list(x)
  } else {
    x
  }
}

chartControlsUI <- function(id, insert_code = FALSE, disable_filters = FALSE) {
  
  # Namespace
  ns <- NS(id)
  
  # ui
  tags$div(
    class = "btn-group-charter btn-group-justified-charter",
    tags$style(sprintf(
      "#%s .sw-dropdown-in {margin: 8px 0 8px 10px !important; padding: 0 !important;}",
      "sw-content-filterdrop"
    )),
    dropdown(
      controls_labs(ns),
      inputId = "labsdrop",
      style = "default",
      label = "Labels & Title",
      up = TRUE,
      icon = icon("font"),
      status = "default btn-controls"
    ),
    dropdown(
      controls_params(ns),
      controls_appearance(ns),
      style = "default",
      label = "Plot options",
      up = TRUE,
      inputId = "paramsdrop",
      icon = icon("gears"),
      status = "default btn-controls"
    ),
    if (!isTRUE(disable_filters)) {
      dropdown(
        filterDF_UI(id = ns("filter-data")),
        style = "default",
        label = "Data",
        up = TRUE,
        icon = icon("filter"),
        right = TRUE,
        inputId = "filterdrop",
        status = "default btn-controls"
      )
    },
    dropdown(
      controls_code(ns, insert_code = insert_code),
      style = "default",
      label = "Export & code",
      up = TRUE,
      right = TRUE,
      inputId = "codedrop",
      icon = icon("code"),
      status = "default btn-controls"
    ),
    tags$script("$('.sw-dropdown').addClass('btn-group-charter');"),
    tags$script(HTML("$('.sw-dropdown > .btn').addClass('btn-charter');")),
    tags$script("$('#sw-content-filterdrop').click(function (e) {e.stopPropagation();});"),
    useShinyUtils()
  )
}

chartControlsServer <- function(input, output, session,
                                type, data_table, data_name,
                                ggplot_rv,
                                aesthetics = reactive(NULL),
                                use_facet = shiny::reactive(FALSE),
                                use_transX = shiny::reactive(FALSE),
                                use_transY = shiny::reactive(FALSE)) {
  
  ns <- session$ns
  
  # Export ----
  
  output$export_png <- downloadHandler(
    filename = function() {
      paste0("esquisse_", format(Sys.time(), format = "%Y%m%dT%H%M%S"), ".png")
    },
    content = function(file) {
      pngg <- try(ggsave(filename = file, plot = ggplot_rv$ggobj$plot, width = 12, height = 8, dpi = "retina"))
      if ("try-error" %in% class(pngg)) {
        shiny::showNotification(ui = "Export to PNG failed...", type = "error")
      }
    }
  )
  output$export_ppt <- downloadHandler(
    filename = function() {
      paste0("esquisse_", format(Sys.time(), format = "%Y%m%dT%H%M%S"), ".pptx")
    },
    content = function(file) {
      if (requireNamespace(package = "rvg") & requireNamespace(package = "officer")) {
        gg <- ggplot_rv$ggobj$plot
        ppt <- officer::read_pptx()
        ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
        ppt <- try(officer::ph_with(ppt, rvg::dml(ggobj = gg), location = officer::ph_location_type(type = "body")), silent = TRUE)
        if ("try-error" %in% class(ppt)) {
          shiny::showNotification(ui = "Export to PowerPoint failed...", type = "error")
        } else {
          tmp <- tempfile(pattern = "esquisse", fileext = ".pptx")
          print(ppt, target = tmp)
          file.copy(from = tmp, to = file)
        }
      } else {
        warn <- "Packages 'officer' and 'rvg' are required to use this functionality."
        warning(warn, call. = FALSE)
        shiny::showNotification(ui = warn, type = "warning")
      }
    }
  )
  
  observeEvent(input$insert_code, {
    context <- rstudioapi::getSourceEditorContext()
    code <- ggplot_rv$code
    code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
    if (!is.null(output_filter$code$expr)) {
      code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
      code_dplyr <- paste(code_dplyr, collapse = "\n")
      nm_dat <- data_name()
      code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
      code <- stri_replace_all(str = code, replacement = " ggplot()", fixed = sprintf("ggplot(%s)", nm_dat))
      code <- paste(code_dplyr, code, sep = " %>%\n")
      if (input$insert_code == 1) {
        code <- paste("library(dplyr)\nlibrary(ggplot2)", code, sep = "\n\n")
      }
    } else {
      if (input$insert_code == 1) {
        code <- paste("library(ggplot2)", code, sep = "\n\n")
      }
    }
    rstudioapi::insertText(text = paste0("\n", code, "\n"), id = context$id)
  })
  
  output$code <- renderUI({
    code <- ggplot_rv$code
    code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
    if (!is.null(output_filter$code$expr)) {
      code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
      code_dplyr <- paste(code_dplyr, collapse = "\n")
      nm_dat <- data_name()
      code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
      code <- stri_replace_all(str = code, replacement = " ggplot()", fixed = sprintf("ggplot(%s)", nm_dat))
      code <- paste(code_dplyr, code, sep = " %>%\n")
    }
    htmltools::tagList(
      rCodeContainer(id = ns("codeggplot"), code)
    )
  })
  
  observeEvent(aesthetics(), {
    aesthetics <- aesthetics()
    if ("fill" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-fill"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-fill"), display = "none")
    }
    if ("color" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-color"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-color"), display = "none")
    }
    if ("size" %in% aesthetics) {
      toggleDisplay(id = ns("controls-labs-size"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-labs-size"), display = "none")
    }
  })
  
  observeEvent(use_facet(), {
    if (isTRUE(use_facet())) {
      toggleDisplay(id = ns("controls-facet"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-facet"), display = "none")
    }
  })
  
  observeEvent(use_transX(), {
    if (isTRUE(use_transX())) {
      toggleDisplay(id = ns("controls-scale-trans-x"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scale-trans-x"), display = "none")
    }
  })
  
  observeEvent(use_transY(), {
    if (isTRUE(use_transY())) {
      toggleDisplay(id = ns("controls-scale-trans-y"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scale-trans-y"), display = "none")
    }
  })
  
  observeEvent(type$palette, {
    if (isTRUE(type$palette)) {
      toggleDisplay(id = ns("controls-palette"), display = "block")
      toggleDisplay(id = ns("controls-spectrum"), display = "none")
    } else {
      toggleDisplay(id = ns("controls-palette"), display = "none")
      toggleDisplay(id = ns("controls-spectrum"), display = "block")
    }
  })
  
  observeEvent(type$x, {
    if (type$x %in% c("bar", "line", "area")) {
      toggleDisplay(id = ns("controls-position"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-position"), display = "none")
    }
    if (type$x %in% "bar") {
      toggleDisplay(id = ns("controls-flip"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-flip"), display = "none")
    }
    if (type$x %in% "histogram") {
      toggleDisplay(id = ns("controls-histogram"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-histogram"), display = "none")
    }
    if (type$x %in% c("density", "violin")) {
      toggleDisplay(id = ns("controls-density"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-density"), display = "none")
    }
    if (type$x %in% "point") {
      toggleDisplay(id = ns("controls-scatter"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-scatter"), display = "none")
    }
    if (type$x %in% c("point", "line")) {
      toggleDisplay(id = ns("controls-size"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-size"), display = "none")
    }
    if (type$x %in% "violin") {
      toggleDisplay(id = ns("controls-violin"), display = "block")
    } else {
      toggleDisplay(id = ns("controls-violin"), display = "none")
    }
  })
  
  output_filter <- callModule(
    module = filterDF,
    id = "filter-data",
    data_table = data_table,
    data_name = data_name
  )
  
  outin <- reactiveValues(
    inputs = NULL,
    export_ppt = NULL,
    export_png = NULL
  )
  
  observeEvent(data_table(), {
    outin$data <- data_table()
    outin$code <- reactiveValues(expr = NULL, dplyr = NULL)
  })
  
  observeEvent({
    all_inputs <- reactiveValuesToList(input)
    all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
  }, {
    all_inputs <- reactiveValuesToList(input)
    # remove inputs from filterDataServer module with ID "filter-data"
    inputs <- all_inputs[grep(pattern = "filter-data", x = names(all_inputs), invert = TRUE)]
    inputs <- inputs[grep(pattern = "^labs_", x = names(inputs), invert = TRUE)]
    inputs <- inputs[grep(pattern = "^export_", x = names(inputs), invert = TRUE)]
    inputs <- inputs[order(names(inputs))]
    outin$inputs <- inputs
  })
  
  # labs input
  observe({
    asth <- aesthetics()
    labs_fill <- ifelse("fill" %in% asth, input$labs_fill, "")
    labs_color <- ifelse("color" %in% asth, input$labs_color, "")
    labs_size <- ifelse("size" %in% asth, input$labs_size, "")
    outin$labs <- list(
      x = input$labs_x %empty% NULL,
      y = input$labs_y %empty% NULL,
      title = input$labs_title %empty% NULL,
      subtitle = input$labs_subtitle %empty% NULL,
      caption = input$labs_caption %empty% NULL,
      fill = labs_fill %empty% NULL,
      color = labs_color %empty% NULL,
      size = labs_size %empty% NULL
    )
  })
  
  # facet input
  observe({
    outin$facet <- list(
      scales = if (identical(input$facet_scales, "fixed")) NULL else input$facet_scales
    )
  })
  
  # theme input
  observe({
    outin$theme <- list(
      theme = input$theme,
      args = list(
        legend.position = if (identical(input$legend_position, "right")) NULL else input$legend_position
      )
    )
  })
  
  # coord input
  observe({
    outin$coord <- if (input$flip) "flip" else NULL
  })
  
  # smooth input
  observe({
    outin$smooth <- list(
      add = input$smooth_add,
      args = list(
        span = input$smooth_span
      )
    )
  })
  
  # transX input
  observe({
    outin$transX <- list(
      use = use_transX() & !identical(input$transX, "identity"),
      args = list(
        trans = input$transX
      )
    )
  })
  
  # transY input
  observe({
    outin$transY <- list(
      use = use_transY() & !identical(input$transY, "identity"),
      args = list(
        trans = input$transY
      )
    )
  })
  
  observeEvent(output_filter$data_filtered(), {
    outin$data <- output_filter$data_filtered()
    outin$code <- output_filter$code
  })
  
  return(outin)
}

controls_labs <- function(ns) {
  tags$div(
    class = "form-group",
    textInput(inputId = ns("labs_title"), placeholder = "Title", label = NULL),
    textInput(inputId = ns("labs_subtitle"), placeholder = "Subtitle", label = NULL),
    textInput(inputId = ns("labs_caption"), placeholder = "Caption", label = NULL),
    textInput(inputId = ns("labs_x"), placeholder = "X label", label = NULL),
    textInput(inputId = ns("labs_y"), placeholder = "Y label", label = NULL),
    tags$div(
      id = ns("controls-labs-fill"), style = "display: none;",
      textInput(inputId = ns("labs_fill"), placeholder = "Fill label", label = NULL)
    ),
    tags$div(
      id = ns("controls-labs-color"), style = "display: none;",
      textInput(inputId = ns("labs_color"), placeholder = "Color label", label = NULL)
    ),
    tags$div(
      id = ns("controls-labs-size"), style = "display: none;",
      textInput(inputId = ns("labs_size"), placeholder = "Size label", label = NULL)
    )
  )
}

controls_appearance <- function(ns) {
  
  themes <- get_themes()
  cols <- get_colors()
  pals <- get_palettes()
  
  tagList(
    tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      spectrumInput(
        inputId = ns("fill_color"),
        label = "Choose a color:",
        choices = unname(cols),
        width = "100%"
      )
    ),
    tags$div(
      id = ns("controls-palette"), style = "display: none;",
      palettePicker(
        inputId = ns("palette"),
        label = "Choose a palette:",
        choices = pals$choices,
        textColor = pals$textColor
      )
    ),
    pickerInput(
      inputId = ns("theme"),
      label = "Theme:",
      choices = themes,
      selected = "minimal",
      options = list(size = 10),
      width = "100%"
    ),
    tags$script(
      paste0("$('#", ns("theme"), "').addClass('dropup');")
    ),
    radioGroupButtons(
      inputId = ns("legend_position"),
      label = "Legend position:",
      choiceNames = list(
        icon("arrow-left"), icon("arrow-up"),
        icon("arrow-down"), icon("arrow-right"), icon("close")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE,
      size = "sm"
    )
  )
}

controls_params <- function(ns) {
  
  scales_trans <- c(
    "asn", "atanh", "boxcox", "exp", "identity",
    "log", "log10", "log1p", "log2", "logit",
    "probability", "probit", "reciprocal",
    "reverse", "sqrt"
  )
  
  tagList(
    tags$div(
      id = ns("controls-scatter"), style = "display: none; padding-top: 10px;",
      conditionalPanel(
        condition = paste0("input['",  ns("smooth_add"), "']==true"),
        sliderInput(
          inputId = ns("smooth_span"),
          label = "Span:",
          min = 0.1, max = 1,
          value = 0.75, step = 0.01
        )
      ),
      materialSwitch(
        inputId = ns("smooth_add"),
        label = "Smooth line:",
        right = TRUE,
        status = "primary"
      )
    ),
    tags$div(
      id = ns("controls-size"), style = "display: none;",
      sliderInput(
        inputId = ns("size"),
        label = "Size:",
        min = 0.5, max = 3,
        value = 1
      )
    ),
    tags$div(
      id = ns("controls-facet"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("facet_scales"),
        label = "Facet scales:",
        inline = TRUE,
        status = "primary",
        choices = c("fixed", "free", "free_x", "free_y"),
        outline = TRUE,
        icon = icon("check")
      )
    ),
    tags$div(
      id = ns("controls-histogram"), style = "display: none;",
      sliderInput(
        inputId = ns("bins"),
        label = "Numbers of bins:",
        min = 10, max = 100,
        value = 30
      )
    ),
    tags$div(
      id = ns("controls-violin"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("scale"),
        label = "Scale:",
        inline = TRUE,
        status = "primary",
        choices = c("area", "count", "width"),
        outline = TRUE,
        icon = icon("check")
      )
    ),
    tags$div(
      id = ns("controls-scale-trans-x"), style = "display: none;",
      selectInput(
        inputId = ns("transX"),
        label = "X-Axis transform:",
        selected = "identity",
        choices = scales_trans
      )
    ),
    tags$div(
      id = ns("controls-scale-trans-y"), style = "display: none;",
      selectInput(
        inputId = ns("transY"),
        label = "Y-Axis transform:",
        selected = "identity",
        choices = scales_trans
      )
    ),
    tags$div(
      id = ns("controls-density"), style = "display: none;",
      sliderInput(
        inputId = ns("adjust"),
        label = "Bandwidth adjustment:",
        min = 0.2, max = 6,
        value = 1, step = 0.1
      )
    ),
    tags$div(
      id = ns("controls-position"), style = "display: none;",
      prettyRadioButtons(
        inputId = ns("position"), label = "Position:",
        choices = c("stack", "dodge", "fill"), inline = TRUE,
        selected = "stack", status = "primary",
        outline = TRUE, icon = icon("check")
      )
    ),
    tags$div(
      id = ns("controls-flip"), style = "display: none;",
      materialSwitch(
        inputId = ns("flip"),
        label = "Flip coordinates:",
        value = FALSE,
        status = "primary"
      )
    )
  )
}

controls_code <- function(ns, insert_code = FALSE) {
  tagList(
    tags$button(
      class = "btn btn-default btn-xs pull-right btn-copy-code",
      "Copy to clipboard", `data-clipboard-target` = paste0("#", ns("codeggplot"))
    ), tags$script("new Clipboard('.btn-copy-code');"),
    tags$br(),
    tags$b("Code:"),
    uiOutput(outputId = ns("code")),
    tags$textarea(id = ns("holderCode"), style = "display: none;"),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = "Insert code in script",
        icon = icon("arrow-circle-left")
      )
    },
    tags$br(),
    tags$b("Export:"),
    tags$br(),
    tags$div(
      class = "btn-group btn-group-justified",
      downloadButton(
        outputId = ns("export_png"),
        label = ".png",
        class = "btn-primary btn-xs"
      ),
      downloadButton(
        outputId = ns("export_ppt"),
        label = ".pptx",
        class = "btn-primary btn-xs"
      )
    )
  )
}

get_themes <- function() {
  themes <- getOption("esquisse.themes")
  if (is.null(themes)) {
    options("esquisse.themes" = default_themes())
    themes <- options("esquisse.themes")
  }
  if (is.function(themes))
    themes <- themes()
  if (!is.list(themes)) {
    stop("Option 'esquisse.themes' must be a list", call. = FALSE)
  }
  themes
}

get_palettes <- function() {
  pals <- getOption("esquisse.palettes")
  if (is.null(pals)) {
    options("esquisse.palettes" = default_pals())
    pals <- options("esquisse.palettes")
  }
  if (is.function(pals))
    pals <- pals()
  if (!is.list(pals)) {
    stop("Option 'esquisse.palettes' must be a list with at least one slot : 'choices'", call. = FALSE)
  }
  if (is.null(pals$textColor))
    pals$textColor <- "white"
  pals
}

get_colors <- function() {
  cols <- getOption("esquisse.colors")
  if (is.null(cols)) {
    options("esquisse.colors" = default_cols())
    cols <- options("esquisse.palettes")
  }
  if (is.function(cols))
    cols <- cols()
  if (!is.list(cols)) {
    stop("Option 'esquisse.colors' must be a list", call. = FALSE)
  }
  cols
}

select_geom_controls <- function(x, geoms) {
  if (is.null(x) | is.null(geoms)) {
    "auto"
  } else if ("bar" %in% geoms & x %in% c("auto", "bar")) {
    "bar"
  } else if ("histogram" %in% geoms & x %in% c("auto", "histogram")) {
    "histogram"
  } else if ("density" %in% geoms & x %in% c("auto", "density")) {
    "density"
  } else if ("point" %in% geoms & x %in% c("auto", "point")) {
    "point"
  } else if ("line" %in% geoms & x %in% c("auto", "line")) {
    "line"
  } else if ("area" %in% geoms & x %in% c("auto", "area")) {
    "area"
  } else if ("violin" %in% geoms & x %in% c("violin")) {
    "violin"
  } else {
    "auto"
  }
}

default_themes <- function() {
  list(
    ggplot2 = list(
      "bw", "classic", "dark", "gray",
      "light", "linedraw", "minimal",
      "void"
    ),
    # hrbrthemes is not supported on these computers :/
    ggthemes = list(
      "base", "calc", "economist", "economist_white",
      "excel", "few", "fivethirtyeight", "foundation",
      "gdocs", "hc", "igray", "map", "pander",
      "par", "solarized", "solarized_2", "solid",
      "stata", "tufte", "wsj"
    )
  )
}

library(scales)
default_pals <- function() {
  list(
    choices = list(
      Default = list("ggplot2" = hue_pal()(9)),
      Viridis = list(
        "viridis" = viridis_pal(option = "viridis")(10),
        "magma" = viridis_pal(option = "magma")(10),
        "inferno" = viridis_pal(option = "inferno")(10),
        "plasma" = viridis_pal(option = "plasma")(10),
        "cividis" = viridis_pal(option = "cividis")(10)
      ),
      Diverging = list(
        "BrBG" = brewer_pal(palette = "BrBG")(11),
        "PiYG" = brewer_pal(palette = "PiYG")(11),
        "PRGn" = brewer_pal(palette = "PRGn")(11),
        "PuOr" = brewer_pal(palette = "PuOr")(11),
        "RdBu" = brewer_pal(palette = "RdBu")(11),
        "RdGy" = brewer_pal(palette = "RdGy")(11),
        "RdYlBu" = brewer_pal(palette = "RdYlBu")(11),
        "RdYlGn" = brewer_pal(palette = "RdYlGn")(11),
        "Spectral" = brewer_pal(palette = "Spectral")(11)
      ),
      Qualitative = list(
        "Accent" = brewer_pal(palette = "Accent")(8),
        "Dark2" = brewer_pal(palette = "Dark2")(8),
        "Paired" = brewer_pal(palette = "Paired")(12),
        "Pastel1" = brewer_pal(palette = "Pastel1")(9),
        "Pastel2" = brewer_pal(palette = "Pastel2")(8),
        "Set1" = brewer_pal(palette = "Set1")(8),
        "Set2" = brewer_pal(palette = "Set2")(8),
        "Set3" = brewer_pal(palette = "Set3")(12)
      ),
      Sequential = list(
        "Blues" = brewer_pal(palette = "Blues")(9),
        "BuGn" = brewer_pal(palette = "BuGn")(9),
        "BuPu" = brewer_pal(palette = "BuPu")(9),
        "GnBu" = brewer_pal(palette = "GnBu")(9),
        "Greens" = brewer_pal(palette = "Greens")(9),
        "Greys" = brewer_pal(palette = "Greys")(9),
        "Oranges" = brewer_pal(palette = "Oranges")(9),
        "OrRd" = brewer_pal(palette = "OrRd")(9),
        "PuBu" = brewer_pal(palette = "PuBu")(9),
        "PuBuGn" = brewer_pal(palette = "PuBuGn")(9),
        "PuRd" = brewer_pal(palette = "PuRd")(9),
        "Purples" = brewer_pal(palette = "Purples")(9),
        "RdPu" = brewer_pal(palette = "RdPu")(9),
        "Reds" = brewer_pal(palette = "Reds")(9),
        "YlGn" = brewer_pal(palette = "YlGn")(9),
        "YlGnBu" = brewer_pal(palette = "YlGnBu")(9),
        "YlOrBr" = brewer_pal(palette = "YlOrBr")(9),
        "YlOrRd" = brewer_pal(palette = "YlOrRd")(9)
      )
    ),
    textColor = c(
      rep(c("white", "black"), times = c(25, 18))
    )
  )
}

default_cols <- function() {
  list(
    "custom" = c("#0C4C8A", "#EF562D"),
    "viridis" = col2Hex(viridis_pal(option = "viridis")(10)),
    "plasma" = col2Hex(viridis_pal(option = "plasma")(10)),
    "Blues" = brewer_pal(palette = "Blues")(9),
    "Greens" = brewer_pal(palette = "Greens")(9),
    "Reds" = brewer_pal(palette = "Reds")(9),
    "Greys" = brewer_pal(palette = "Greys")(9)
  )
}

col2Hex <- function(col) {
  mat <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3,]/255)
}

colorPicker <- function(inputId,
                        label,
                        choices,
                        selected = NULL,
                        textColor = "#000",
                        plainColor = FALSE,
                        multiple = FALSE,
                        pickerOpts = list(),
                        width = NULL) {
  choices <- choicesWithNames(choices)
  cols <- unlist(x = choices, recursive = TRUE, use.names = FALSE)
  colsNames <- unlist(lapply(
    X = seq_along(choices),
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        names(choices[[x]])
      } else {
        names(choices)[x]
      }
    }
  ))
  if (isTRUE(plainColor)) {
    style <- sprintf(
      "background: %s; color: %s;",
      cols, rep_len(textColor, length.out = length(cols))
    )
  } else {
    style <- NULL
  }
  if (isTRUE(multiple)) {
    content_str <- "<span style='border-radius:4px; padding: 2px;background:%s;color:%s'>%s</span>"
  } else {
    content_str <- "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>"
  }
  colPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        cols,
        rep_len(textColor, length.out = length(cols)),
        colsNames
      )
    )),
    options = pickerOpts,
    width = width
  )
  colPicTag <- tagAppendAttributes(
    tag = colPicTag,
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    singleton(tags$head(tags$style(
      ".color-picker .bootstrap-select .dropdown-menu li a span.text {width: 100%;}"
    ))),
    colPicTag
  )
}

palettePicker <- function(inputId,
                          label,
                          choices,
                          selected = NULL,
                          textColor = "#000",
                          plainColor = FALSE,
                          pickerOpts = list(),
                          width = NULL) {
  
  choicesNames <- lapply(
    X = seq_along(choices),
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        names(choices[[x]])
      } else {
        names(choices)[x]
      }
    }
  )
  names(choicesNames) <- names(choices)
  choicesColors <- lapply(
    X = seq_along(choices),
    FUN = function(x) {
      if (is.list(choices[[x]])) {
        lapply(choices[[x]], linear_gradient)
      } else {
        linear_gradient(choices[[x]])
      }
    }
  )
  choicesColors <- unlist(
    x = choicesColors,
    recursive = TRUE,
    use.names = FALSE
  )
  if (isTRUE(plainColor)) {
    style <- sprintf(
      "background: %s; color: %s;",
      choicesColors, rep_len(textColor, length.out = length(choicesColors))
    )
  } else {
    style <- NULL
  }
  content_str <- "<div style='width:100%%;border-radius:4px; padding: 2px;background:%s;color:%s'>%s</div>"
  palPicTag <- pickerInput(
    inputId = inputId,
    label = label,
    choices = choicesNames,
    selected = selected,
    choicesOpt = dropNulls(list(
      style = style,
      content = sprintf(
        content_str,
        choicesColors,
        rep_len(textColor, length.out = length(choicesColors)),
        unlist(choicesNames, recursive = TRUE, use.names = FALSE)
      )
    )),
    options = pickerOpts,
    width = width
  )
  palPicTag <- tagAppendAttributes(
    tag = palPicTag,
    class = ifelse(isTRUE(plainColor), "color-picker-plain", "color-picker")
  )
  tagList(
    singleton(tags$head(tags$style(
      ".color-picker .bootstrap-select .dropdown-menu li a span.text {width: 100%;}",
      ".color-picker-plain .bootstrap-select .dropdown-menu li a span.text div {background:rgba(0,0,0,0) !important;}"
    ))),
    palPicTag
  )
}

linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

filterDF_UI <- function(id, show_nrow = TRUE) {
  ns <- NS(id)
  tagList(
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
      tags$p(label_nrow, tags$b(nrow(data_filtered()) , "/", nrow(data_table())))
    }
  })
  
  rv_filters <- reactiveValues(mapping = NULL, mapping_na = NULL)
  rv_code <- reactiveValues(expr = NULL, dplyr = NULL)
  
  observe({
    data <- data_table()
    vars <- data_vars()
    # req(nrow(data) > 0)
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
        # req(input[[x]])
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
  # filters_id <- paste0("filter_", sample.int(1e9, length(vars)))
  filters_id <- paste0("filter_", clean_string(vars))
  filters_id <- setNames(as.list(filters_id), vars)
  filters_na_id <- setNames(as.list(paste0("na_", filters_id)), vars)
  ui <- lapply(
    X = vars,
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

tagSetAttributes <- function(tag, ...) {
  tag$attribs[names(list(...))] <- NULL
  tag$attribs <- c(tag$attribs, list(...))
  tag
}

set_slider_attr <- function(slider) {
  slider$children[[2]] <- tagSetAttributes(
    tag = slider$children[[2]],
    `data-force-edges` = "true",
    `data-grid-num` = "4"
  )
  slider
}

library(shinyWidgets)

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

library(rlang)
make_expr_filter <- function(filters, filters_na, data, data_name) {
  expressions <- lapply(
    X = names(filters),
    FUN = function(var) {
      values <- filters[[var]]
      nas <- filters_na[[var]]
      data_values <- data[[var]]
      if (!is.null(values) & !match_class(values, data_values))
        return(NULL)
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
            if (length(values) <= length(data_values)/2) {
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


drop_id <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (inherits(x, c("factor", "character"))) {
        values <- unique(as.character(x))
        values <- values[trimws(values) != ""]
        if (length(values) <= 1)
          return(NULL)
        if (length(values) >= length(x) * 0.9)
          return(NULL)
        if (length(values) >= 50)
          return(NULL)
      }
      x
    }
  )
  data
}

drop_na <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (all(is.na(x)))
        return(NULL)
      x
    }
  )
  data
}


# borrowed from shiny
hasDecimals <- function (value) {
  truncatedValue <- round(value)
  return(!identical(value, truncatedValue))
}

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
      step = signif(digits = 10, (max(pretty_steps) - min(pretty_steps))/n_steps)
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

match_class <- function(x, y) {
  char <- c("character", "factor")
  num <- c("numeric", "integer")
  date <- c("Date", "POSIXt")
  if (inherits(x, num) & inherits(y, num))
    return(TRUE)
  if (inherits(x, char) & inherits(y, char))
    return(TRUE)
  if (inherits(x, date) & inherits(y, date))
    return(TRUE)
  return(FALSE)
}

chooseDataUI <- function(id, label = "Data", icon = "database", ...) {
  
  ns <- NS(id)
  
  if (is.character(icon))
    icon <- icon(icon)
  
  tagList(
    singleton(
      tags$link(rel="stylesheet", type="text/css",
                href="esquisse/styles-dad.css")
    ),
    useShinyUtils(),
    actionButton(
      inputId = ns("changeData"), label = label,
      icon = icon, width = "100%", ...
    )
  )
}

chooseDataServer <- function(input, output, session,
                             dataModule = c("GlobalEnv", "ImportFile"),
                             data = NULL, name = NULL,
                             selectVars = TRUE, coerceVars = FALSE,
                             launchOnStart = TRUE, size = "m") {
  
  dataModule <- match.arg(dataModule)
  datModUI <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvUI,
    "ImportFile" = dataImportFileUI
  )
  datModServer <- switch(
    dataModule,
    "GlobalEnv" = dataGlobalEnvServer,
    "ImportFile" = dataImportFileServer
  )
  
  ns <- session$ns
  return_data <- reactiveValues(data = data, name = name)
  
  if (isTRUE(launchOnStart)) {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"),
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"),
        selectVars = selectVars,
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  }
  
  observeEvent(input$changeData, {
    showModal(modalDialog(tagList(
      tags$button(
        icon("close"),
        class = "btn btn-link pull-right",
        `data-dismiss` = "modal"
      ),
      datModUI(
        id = ns("chooseData"),
        selectVars = selectVars,
        coerceVars = coerceVars
      )
    ), size = size, fade = FALSE, footer = NULL))
  })
  
  return_data <- callModule(
    module = datModServer,
    id = "chooseData",
    data = data,
    name = name
  )
  
  return(return_data)
}

dataGlobalEnvUI <- function(id, dismissOnValidate = TRUE, selectVars = TRUE, coerceVars = TRUE) {
  ns <- NS(id)
  
  # List of data.frame
  dfs <- search_obj(what = "data.frame")
  if (is.null(dfs)) {
    dfs <- data(package = "ggplot2", envir = environment())$results[, "Item"]
  }
  
  info_dfs <- lapply(
    X = dfs,
    FUN = function(x) {
      tmp <- get_df(x)
      sprintf("%d obs. of  %d variables", nrow(tmp), ncol(tmp))
    }
  )
  info_dfs <- unlist(info_dfs)
  
  tagList(
    useShinyUtils(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("dataGlobalEnv"), as.numeric(Sys.time()))
    ),
    tags$h2("Select a dataset"),
    pickerInput(
      inputId = ns("data"),
      label = "Choose a data.frame :",
      choices = dfs, width = "100%",
      options = list(title = "List of data.frame..."),
      choicesOpt = list(subtext = info_dfs)
    ),
    
    tags$div(
      id = ns("placeholder-result-import"),
      tags$div(
        id = ns("result-import"), class = "alert alert-info",
        tags$b("No data selected"), "Use a data.frame from user environment"
      )
    ),
    
    tags$div(
      style = if (!isTRUE(selectVars)) "display: none;",
      tags$br(),
      selectVarsUI(id = ns("selected"))
    ),
    tags$div(
      style = if (!isTRUE(coerceVars)) "display: none;",
      style = "margin: 10px;",
      tags$br(),
      tags$br(),
      coerceUI(id = ns("coerce"))
    ),
    
    tags$br(), tags$br(),
    actionButton(
      inputId = ns("validate"),
      label = "Validate imported data",
      icon = icon("arrow-circle-right"),
      width = "100%", disabled = "disabled",
      class = "btn-primary",
      `data-dismiss` = if (isTRUE(dismissOnValidate)) "modal" else NULL
    )
  )
}

dataGlobalEnvServer <- function(input, output, session, data = NULL, name = NULL) {
  
  ns <- session$ns
  jns <- function(x) paste0("#", ns(x))
  
  imported_data <- reactiveValues(data = data, name = name)
  tmp_name <- reactiveValues(name = name)
  select_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  coerce_data <- reactiveValues(data = NULL, name = NULL, timestamp = Sys.time())
  
  observeEvent(input$dataGlobalEnv, {
    imported_data$data <- NULL
    imported_data$name <- NULL
  })
  
  observeEvent(input$data, {
    req(input$data)
    imported <- try(get_df(input$data), silent = TRUE)
    if ("try-error" %in% class(imported) || NROW(imported) < 1) {
      toggleInput(inputId = ns("validate"), enable = FALSE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"), class = "alert alert-danger",
          tags$b("Ooops"), "Something went wrong"
        )
      )
      select_data$data <- NULL
      coerce_data$data <- NULL
      tmp_name$name <- NULL
      select_data$timestamp <- Sys.time()
    } else {
      toggleInput(inputId = ns("validate"), enable = TRUE)
      removeUI(selector = jns("result-import"))
      insertUI(
        selector = jns("placeholder-result-import"),
        ui = tags$div(
          id = ns("result-import"), class = "alert alert-success",
          tags$b("Success"),
          sprintf("%s obs. of %s variables imported",
                  nrow(imported), ncol(imported))
        )
      )
      select_data$data <- imported
      coerce_data$data <- imported
      tmp_name$name <- input$data
      select_data$timestamp <- Sys.time()
    }
  }, ignoreInit = TRUE)
  
  sv <- callModule(module = selectVarsServer, id = "selected", data = select_data)
  
  observeEvent(sv$selected_vars, {
    if (length(sv$selected_vars) > 0) {
      toggleInput(inputId = ns("validate"), enable = TRUE)
      coerce_data$data <- select_data$data[, sv$selected_vars, drop = FALSE]
    } else {
      toggleInput(inputId = ns("validate"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  coerced_data <- callModule(module = coerceServer, id = "coerce", data = coerce_data)
  
  observeEvent(input$validate, {
    if (!is.null(coerced_data$data)) {
      dat <- coerced_data$data
    } else {
      dat <- select_data$data
    }
    imported_data$data <- dat
    imported_data$name <- tmp_name$name
  })
  
  return(imported_data)
}

selectVarsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyUtils(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("launchSelectVars"), as.numeric(Sys.time()))
    ),
    tags$style(HTML(paste(
      paste0("#", ns("col_chooser-container")),
      ".bootstrap-select .dropdown-menu li a span.text {width: 96%;}"
    ))),
    tags$div(
      id = ns("col_chooser-container"),
      tags$label(
        class = "control-label",
        style = "width: 100%;",
        "Select variables to keep :",
        tags$a(
          id = ns("help-select-vars"), style = "float: right;",
          style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
          `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
          `data-container` = "body", tabindex = "0", role = "button",
          `data-content` = "Select the variables you want to use to create graphs",
          tags$script(sprintf("$('#%s').popover();", ns("help-select-vars")))
        )
      ),
      pickerInput(
        inputId = ns("col_chooser"),
        label = NULL,
        choices = "No data", multiple = TRUE, width = "100%",
        selected = NULL,
        options = list(
          `actions-box` = TRUE, `multiple-separator` = " ",
          `selected-text-format`= "count > 0",
          `count-selected-text` = "{0} variables chosen (on a total of {1})"
        )
      )
    ),
    tags$em("Legend :"),
    HTML(paste(
      doRenderTags(
        badgeType(col_name = c("discrete", "continuous", "time", "id"),
                  col_type = c("discrete", "continuous", "time", "id"))
      ),
      collapse = ", "
    ))
  )
}

selectVarsServer <- function(input, output, session, data = list()) {
  
  ns <- session$ns
  
  observeEvent(input$launchSelectVars, {
    toggleInput(inputId = ns("col_chooser"), enable = FALSE)
  })
  
  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      toggleInput(inputId = ns("col_chooser"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("col_chooser"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      res_col_type <- unlist(lapply(data$data, col_type))
      updatePickerInput(
        session = session,
        inputId = "col_chooser",
        choices =  names(res_col_type),
        selected = names(res_col_type)[unname(res_col_type) != "id"],
        choicesOpt = list(
          # content = unlist(lapply(
          #   X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
          #   FUN = doRenderTags
          # ))
          content = paste0(
            unlist(lapply(
              X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
              FUN = doRenderTags
            )),
            "<span style='float: right; margin-right: 15px; white-space: pre;'>",
            "Class: ",
            "<span style='display: inline-block; width: 65px; text-align: right;'>",
            unlist(lapply(data$data, function(x) class(x)[1]), use.names = FALSE),
            "</span>",
            " |   ",
            "Missing values: ",
            "<span style='display: inline-block; width: 30px; text-align: right;'>",
            unlist(lapply(data$data, function(x) sum(is.na(x))), use.names = FALSE),
            "</span>",
            "</span>"
          )
        )
      )
    }
  }, ignoreNULL = FALSE)
  
  res <- reactiveValues(selected_vars = NULL)
  
  observeEvent(input$col_chooser, {
    res$selected_vars <- input$col_chooser
  }, ignoreNULL = FALSE)
  
  return(res)
}

coerceUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    tags$style(
      ".col-coerce {padding-right: 5px; padding-left: 5px;}"
    ),
    useShinyUtils(),
    column(
      width = 5, class = "col-coerce",
      pickerInput(
        inputId = ns("var"),
        label = "Choose a variable to coerce:",
        choices = NULL,# names(data),
        multiple = FALSE,
        width = "100%"
      )
    ),
    column(
      width = 4, class = "col-coerce",
      selectizeInput(
        inputId = ns("coerce_to"),
        label = uiOutput(outputId = ns("coerce_to_label"), inline = FALSE, style = "min-height: 15px;"),
        choices = c("character", "factor", "numeric", "Date", "POSIXct"),
        multiple = FALSE,
        width = "100%"
      ),
      tags$div(
        id = ns("placeholder-date")
      )
    ),
    column(
      width = 3, class = "col-coerce",
      tags$div(
        style = "height: 25px;",
        tags$a(
          id = ns("help-coerce-vars"), style = "float: right;",
          style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
          `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
          `data-container` = "body", tabindex = "0", role = "button",
          `data-content` = "Select a variable to change its class (for example to convert numbers into characters)",
          tags$script(sprintf("$('#%s').popover();", ns("help-coerce-vars")))
        )
      ),
      actionButton(
        inputId = ns("valid_coerce"),
        label = "Coerce",
        icon = icon("play"),
        width = "100%",
        class = "btn-primary",
        disabled = "disabled"
      )
    )
  )
}

coerceServer <- function(input, output, session, data, reactiveValuesSlot = "data") {
  
  ns <- session$ns
  jns <- function(id) paste0("#", ns(id))
  
  return_data <- reactiveValues(data = NULL, names = NULL)
  
  observe({
    if (is.reactive(data)) {
      toggleInput(inputId = ns("valid_coerce"), enable = TRUE)
      toggleInput(inputId = ns("var"), enable = TRUE)
    } else if (is.reactivevalues(data) && !is.null(data[[reactiveValuesSlot]])) {
      toggleInput(inputId = ns("valid_coerce"), enable = TRUE)
      toggleInput(inputId = ns("var"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("valid_coerce"), enable = FALSE)
      toggleInput(inputId = ns("var"), enable = FALSE)
    }
  })
  
  observe({
    req(data)
    if (is.reactive(data)) {
      data <- data()
    } else if (is.reactivevalues(data)) {
      req(data[[reactiveValuesSlot]])
      # data$timestamp
      data <- data[[reactiveValuesSlot]]
    }
    
    updatePickerInput(
      session = session,
      inputId = "var",
      choices = names(data),
      choicesOpt = list(
        subtext = unlist(lapply(
          X = data, FUN = function(x) class(x)[1]
        ), use.names = FALSE)
      )
    )
    return_data$data <- data
    return_data$names <- names(data)
  })
  
  output$coerce_to_label <- renderUI({
    req(return_data$data); req(input$var)
    if (input$var %in% names(return_data$data)) {
      var <- return_data$data[[input$var]]
      tags$span(
        "From", tags$code(class(var)[1]), "to:"
      )
    }
  })
  
  observe({
    req(return_data$data); req(input$var)
    if (input$var %in% names(return_data$data)) {
      data <- return_data$data
      removeUI(selector = jns("options-date"))
      classvar <- class(data[[input$var]])[1]
      if (input$coerce_to == "Date" & classvar %in% c("character", "factor")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("date_format"),
              label = "Specify format:",
              value = "%Y-%m-%d"
            )
          )
        )
      } else if (input$coerce_to == "Date" & classvar %in% c("numeric", "integer")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("date_origin"),
              label = "Specify origin:",
              value = "1970-01-01"
            )
          )
        )
      } else if (input$coerce_to == "POSIXct" & classvar %in% c("character", "factor")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("posixct_format"),
              label = "Specify format:",
              value = "%Y-%m-%d %H:%M:%S"
            )
          )
        )
      } else if (input$coerce_to == "POSIXct" & classvar %in% c("numeric", "integer")) {
        insertUI(
          selector = jns("placeholder-date"),
          ui = tags$div(
            id = ns("options-date"),
            textInput(
              inputId = ns("posixct_origin"),
              label = "Specify origin:",
              value = "1970-01-01 00:00:00"
            )
          )
        )
      }
    }
  })
  
  observeEvent(input$valid_coerce, {
    var <- return_data$data[[input$var]]
    classvar <- class(var)[1]
    args <- list(x = var)
    argsup <- ""
    if (input$coerce_to %in% "Date") {
      if (classvar %in% c("numeric", "integer")) {
        args$origin <- input$date_origin
        argsup <- sprintf(", origin = \"%s\"", input$date_origin)
      } else {
        args$format <- input$date_format
        argsup <- sprintf(", format = \"%s\"", input$date_format)
      }
    } else if (input$coerce_to %in% "POSIXct") {
      if (classvar %in% c("numeric", "integer")) {
        args$origin <- input$posixct_origin
        argsup <- sprintf(", origin = \"%s\"", input$posixct_origin)
      } else {
        args$format <- input$posixct_format
        argsup <- sprintf(", format = \"%s\"", input$posixct_format)
      }
    }
    var <- withCallingHandlers(
      expr = tryCatch(
        expr = {
          do.call(what = paste0("as.", input$coerce_to), args = args)
        },
        error = function(e) {
          shiny::showNotification(ui = conditionMessage(e), type = "error", session = session)
        }
      ),
      warning = function(w) {
        shiny::showNotification(ui = conditionMessage(w), type = "warning", session = session)
      }
    )
    return_data$data[[input$var]] <- var
    return_data$names <- replace(
      x = return_data$names,
      list = which(return_data$names == input$var),
      values = sprintf("as.%s(%s%s)", input$coerce_to, input$var, argsup)
    )
    updateActionButton(
      session = session,
      inputId = "valid_coerce",
      label = "Coerced !",
      icon = icon("check")
    )
    session$sendCustomMessage(
      type = "toggleClass",
      message = list(id = ns("valid_coerce"), class = "success")
    )
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$var, input$coerce_to), {
    updateActionButton(
      session = session,
      inputId = "valid_coerce",
      label = "Coerce",
      icon = icon("play")
    )
    session$sendCustomMessage(
      type = "toggleClass",
      message = list(id = ns("valid_coerce"), class = "primary")
    )
  }, ignoreInit = TRUE)
  
  return(return_data)
}

`%empty%` <- function(a, b) {
  if (a != "") a else b
}

potential_geoms <- function(data, mapping, auto = FALSE) {
  
  data_mapped <- lapply(mapping, rlang::eval_tidy, data = data)
  
  x_type <- col_type(data_mapped$x, no_id = TRUE)
  y_type <- col_type(data_mapped$y, no_id = TRUE)
  
  potential_geoms_ref <- potential_geoms_ref()
  
  if (is.null(x_type) & !is.null(y_type)) {
    if (all(y_type %nin% "continuous")) {
      x_type <- y_type
      y_type <- "empty"
    } else {
      x_type <- "empty"
    }
  }
  if (!is.null(x_type) & is.null(y_type)) {
    y_type <- "empty"
  }
  
  if (isTRUE(auto)) {
    auto_ <- 1
  } else {
    auto_ <- c(0, 1)
  }
  
  geoms_ind <- eval_tidy(
    expr = expr(!!sym("x") == !!x_type & !!sym("y") %in% !!y_type & auto %in% !!auto_),
    data = potential_geoms_ref
  )
  geoms <- potential_geoms_ref[geoms_ind, "geom"]
  
  if (inherits(data, what = "sf")) {
    if (isTRUE(auto)) {
      geoms <- "sf"
    } else {
      geoms <- c(geoms, "sf")
    }
  }
  
  return(geoms)
}

library(ggplot2)
potential_geoms_ref <- function() {
  x <- matrix(
    data = c(
      "continuous",  "empty",       "histogram", "1",
      "continuous",  "empty",       "boxplot",   "0",
      "continuous",  "empty",       "violin",    "0",
      "continuous",  "empty",       "density",   "0",
      "discrete",    "empty",       "bar",       "1",
      "time",        "empty",       "histogram", "1",
      "time",        "empty",       "bar",       "0",
      "continuous",  "discrete",    "boxplot",   "0",
      "continuous",  "discrete",    "violin",    "0",
      "continuous",  "discrete",    "bar",       "1",
      "discrete",    "continuous",  "boxplot",   "0",
      "discrete",    "continuous",  "violin",    "0",
      "discrete",    "continuous",  "bar",       "1",
      "continuous",  "continuous",  "point",     "1",
      "continuous",  "continuous",  "line",      "0",
      "continuous",  "continuous",  "area",      "0",
      "discrete",    "discrete",    "tile",      "1",
      "time",        "continuous",  "line",      "1",
      "time",        "continuous",  "area",      "0",
      "time",        "continuous",  "bar",       "0",
      "empty",       "continuous",  "line",      "1",
      "empty",       "continuous",  "area",      "0",
      "continuous",  "continuous",  "tile",      "0",
      "discrete",    "time",        "tile",      "0",
      "time",        "discrete",    "tile",      "0"
    ), ncol = 4, byrow = TRUE
  )
  x <- data.frame(x, stringsAsFactors = FALSE)
  names(x) <- c("x", "y", "geom", "auto")
  x$auto <- as.numeric(x$auto)
  return(x)
}

match_geom_args <- function(geom, args, add_aes = TRUE, mapping = list(), envir = "ggplot2") {
  if (!is.null(args$fill_color)) {
    if (geom %in% c("bar", "histogram", "boxplot", "violin", "density")) {
      args$fill <- args$fill_color %||% "#0C4C8A"
    }
    if (geom %in% c("line", "point")) {
      args$colour <- args$fill_color %||% "#0C4C8A"
    }
  }
  if (geom %in% c("bar", "histogram", "boxplot", "violin", "density")) {
    args$size <- NULL
  }
  if (identical(args$position, "stack")) {
    args$position <- NULL
  }
  if (!geom %in% c("bar", "histogram")) {
    args$position <- NULL
  }
  pkg_envir <- getNamespace(envir)
  if (!grepl(pattern = "^geom_", x = geom))
    geom <- paste0("geom_", geom)
  geom_args <- try(formals(fun = get(geom, envir = pkg_envir)), silent = TRUE)
  if (inherits(geom_args, "try-error"))
    stop(paste(geom, "not found in", envir), call. = FALSE)
  if (!is.null(geom_args$stat)) {
    stat_args <- try(
      formals(fun = get(paste0("stat_", geom_args$stat), envir = pkg_envir)),
      silent = TRUE
    )
    if (!inherits(stat_args, "try-error")) {
      geom_args <- c(geom_args, stat_args)
    }
  }
  if (isTRUE(add_aes)) {
    GeomFun <- paste0("Geom", capitalize(gsub("geom_", "", geom)))
    GeomFun <- try(get(GeomFun, envir = pkg_envir), silent = TRUE)
    if (inherits(GeomFun, "try-error") & !is.null(geom_args$geom)) {
      GeomFun <- paste0("Geom", capitalize(geom_args$geom))
      GeomFun <- try(get(GeomFun, envir = pkg_envir), silent = TRUE)
    }
    if (!inherits(GeomFun, "try-error")) {
      aes_args <- GeomFun$aesthetics()
      geom_args <- c(geom_args, setNames(aes_args, aes_args))
    }
  }
  args[names(args) %in% setdiff(names(geom_args), names(mapping))]
}

badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class='label label-discrete badge-dad', col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class='label label-datetime badge-dad', col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class='label label-continue badge-dad', col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class='label label-default badge-dad', col_name_i)
      }
    }
  )
  res
}

toggleInput <- function(inputId, enable = TRUE, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = 'toggleInput',
    message = list(id = inputId, enable = enable)
  )
}

toggleDisplay <- function(id, display = c("none", "block", "inline-block"), session = shiny::getDefaultReactiveDomain()) {
  display <- match.arg(display)
  session$sendCustomMessage(
    type = 'toggleDisplay',
    message = list(id = id, display = display)
  )
}

col_type <- function(x, no_id = FALSE) {
  if (is.null(x))
    return(NULL)
  
  if (is.data.frame(x) && inherits(x, what = "sf")) {
    x <- x[, setdiff(names(x), attr(x, "sf_column")), drop = FALSE]
  }
  
  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u/n < 0.99 | u <= 30 | no_id) {
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

build_aes <- function(data, ..., .list = NULL, geom = NULL) {
  args <- c(list(...), .list)
  args <- dropNulls(args)
  
  if (is.null(geom))
    geom <- "auto"
  args <- syms(args)
  data_mapped <- lapply(args, rlang::eval_tidy, data = data)
  x_type <- col_type(data_mapped$x, no_id = TRUE)
  y_type <- col_type(data_mapped$y, no_id = TRUE)
  if (is.null(args$x) & !is.null(args$y) & geom %in% "line") {
    args$x <- expr(seq_along(!!args$y))
  }
  if (!is.null(args$x) & is.null(args$y) & geom %in% c("boxplot", "violin")) {
    args$y <- args$x
    args$x <- I("")
  }
  if (is.null(args$x) & !is.null(args$y) & geom %nin% c("boxplot", "violin")) {
    tmp <- args$y
    args$y <- args$x
    args$x <- tmp
  }
  if (!is.null(args$x) & !is.null(args$y) & geom %in% c("boxplot", "violin")) {
    if (x_type == "continuous" & (!is.null(y_type) && y_type == "discrete")) {
      tmp <- args$y
      args$y <- args$x
      args$x <- tmp
    }
  }
  if (!is.null(args$x) & !is.null(args$y) & geom == "bar") {
    if (x_type == "continuous" & y_type == "discrete") {
      args$weight <- args$x
      args$x <- args$y
      args$y <- NULL
    }
    if (x_type %in% c("discrete", "time") & y_type == "continuous") {
      args$weight <- args$y
      args$y <- NULL
    }
  }
  eval(expr(aes(!!!args)))
}

make_aes <- function(.list) {
  .list$facet <- NULL
  if (!is.null(.list$xvar)) {
    .list$x <- .list$xvar
    .list$xvar <- NULL
  }
  if (!is.null(.list$yvar)) {
    .list$y <- .list$yvar
    .list$yvar <- NULL
  }
  .list
}

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



attach(iris)
esquisser(iris)

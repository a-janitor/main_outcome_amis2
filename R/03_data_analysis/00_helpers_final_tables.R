#-----------------------------------------------------------------------
##### HELPERS #####
#-----------------------------------------------------------------------

clean_text <- function(x) {
  toupper(gsub("[^A-Za-z0-9]+", "", trimws(as.character(x))))
}

resolve_model_path <- function(
    filenames,
    directory = mplus_input_dir
) {
  paths <- file.path(
    directory,
    filenames
  )
  
  existing_paths <- paths[
    file.exists(paths)
  ]
  
  if (length(existing_paths) == 0) {
    stop(
      "None of the expected Mplus outputs was found:\n",
      paste(paths, collapse = "\n")
    )
  }
  
  if (length(existing_paths) > 1) {
    message(
      "Multiple candidate outputs found; using:\n",
      existing_paths[1]
    )
  }
  
  existing_paths[1]
}


read_model <- function(
    filenames,
    directory = mplus_input_dir
) {
  path <- resolve_model_path(
    filenames = filenames,
    directory = directory
  )
  
  model <- MplusAutomation::readModels(
    path,
    what = c(
      "summaries",
      "parameters",
      "warn_err"
    ),
    quiet = TRUE
  )
  
  if (length(model$errors) > 0) {
    stop(
      "Mplus errors found in:\n",
      path,
      "\n",
      paste(
        unlist(model$errors),
        collapse = "\n"
      )
    )
  }
  
  attr(model, "source_file") <- path
  
  model
}

parameter_table <- function(model) {
  x <- tibble::as_tibble(model$parameters$unstandardized)
  names(x) <- tolower(names(x))
  x |>
    dplyr::mutate(
      header_clean = clean_text(.data$paramheader),
      param_clean = clean_text(.data$param)
    )
}

extract_new <- function(model, parameter_name) {
  pars <- parameter_table(model)
  match <- pars |>
    dplyr::filter(
      grepl("NEW|ADDITIONAL", .data$header_clean),
      .data$param_clean == clean_text(parameter_name)
    )
  if (nrow(match) != 1) {
    stop("Expected one new parameter ", parameter_name, "; found ", nrow(match))
  }
  list(
    estimate = as.numeric(match$est[1]),
    se = as.numeric(match$se[1]),
    p = as.numeric(match$pval[1])
  )
}

extract_regression <- function(model, header, predictor) {
  pars <- parameter_table(model)
  match <- pars |>
    dplyr::filter(
      .data$header_clean == clean_text(header),
      .data$param_clean == clean_text(predictor)
    )
  if (nrow(match) != 1) {
    stop(
      "Expected one regression for ", header, " / ", predictor,
      "; found ", nrow(match)
    )
  }
  list(
    estimate = as.numeric(match$est[1]),
    se = as.numeric(match$se[1]),
    p = as.numeric(match$pval[1])
  )
}

format_p <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "",
    x < .001 ~ "< .001",
    TRUE ~ sub("^0", "", sprintf("%.3f", x))
  )
}

format_estimate <- function(b, se) {
  sprintf("%.2f [%.2f, %.2f]", b, b - 1.96 * se, b + 1.96 * se)
}

apa_rule <- officer::fp_border(
  color = "#000000",
  width = 2.75,
  style = "single"
)

format_apa_table <- function(
    data,
    left_columns,
    widths = NULL,
    font_size = 10,
    bold_rows = NULL
) {
  
  ft <- flextable::flextable(data) |>
    flextable::font(
      fontname = "Times New Roman",
      part = "all"
    ) |>
    flextable::fontsize(
      size = font_size,
      part = "all"
    ) |>
    flextable::bold(
      part = "header"
    ) |>
    flextable::align(
      j = left_columns,
      align = "left",
      part = "all"
    ) |>
    flextable::align(
      j = setdiff(names(data), left_columns),
      align = "center",
      part = "all"
    ) |>
    flextable::valign(
      valign = "center",
      part = "all"
    ) |>
    flextable::padding(
      padding.top = 3,
      padding.bottom = 3,
      padding.left = 3,
      padding.right = 3,
      part = "all"
    ) |>
    flextable::border_remove() |>
    flextable::set_table_properties(
      layout = "fixed",
      width = 1,
      align = "left"
    )
  
  if (!is.null(widths)) {
    for (column in names(widths)) {
      ft <- flextable::width(
        ft,
        j = column,
        width = widths[[column]]
      )
    }
  }
  
  if (!is.null(bold_rows)) {
    if (length(bold_rows) != nrow(data)) {
      stop(
        "`bold_rows` must have the same length as the number of table rows."
      )
    }
    
    rows_to_bold <- which(
      !is.na(bold_rows) & bold_rows
    )
    
    if (length(rows_to_bold) > 0) {
      ft <- flextable::bold(
        ft,
        i = rows_to_bold,
        bold = TRUE,
        part = "body"
      )
    }
  }
  
  ft <- flextable::fix_border_issues(ft)
  
  ft <- flextable::hline_top(
    ft,
    part = "header",
    border = apa_rule
  )
  
  ft <- flextable::hline_bottom(
    ft,
    part = "header",
    border = apa_rule
  )
  
  ft <- flextable::hline_bottom(
    ft,
    part = "body",
    border = apa_rule
  )
  
  ft
}

add_table_to_doc <- function(doc, number, title, ft, note = NULL, page_break = FALSE) {
  if (page_break) doc <- officer::body_add_break(doc)
  doc <- officer::body_add_par(doc, number, style = "Normal")
  doc <- officer::body_add_par(doc, title, style = "Normal")
  doc <- flextable::body_add_flextable(doc, value = ft)
  if (!is.null(note)) doc <- officer::body_add_par(doc, note, style = "Normal")
  doc
}

##### EXTRACT MPLUS FIT INDICES #####

read_mplus_fit <- function(file) {
  
  output <- MplusAutomation::readModels(
    target = file,
    what = c("summaries", "warn_err"),
    quiet = TRUE
  )
  
  if (length(output$errors) > 0) {
    stop(
      "Mplus errors found in:\n",
      file,
      "\n",
      paste(unlist(output$errors), collapse = "\n")
    )
  }
  
  summary <- output$summaries
  
  if (is.null(summary) || nrow(summary) != 1) {
    stop("Expected one model summary in: ", file)
  }
  
  tibble::tibble(
    model = tools::file_path_sans_ext(basename(file)),
    n = summary$Observations[1],
    parameters = summary$Parameters[1],
    chisq = summary$ChiSqM_Value[1],
    df = summary$ChiSqM_DF[1],
    p = summary$ChiSqM_PValue[1],
    cfi = summary$CFI[1],
    tli = summary$TLI[1],
    rmsea = summary$RMSEA_Estimate[1],
    rmsea_lb = summary$RMSEA_90CI_LB[1],
    rmsea_ub = summary$RMSEA_90CI_UB[1],
    srmr = summary$SRMR[1],
    aic = summary$AIC[1],
    bic = summary$BIC[1]
  )
}


##### FORMAT NUMERIC RESULTS #####

format_decimal <- function(
    x,
    digits = 3,
    signed = FALSE
) {
  
  format_string <- if (signed) {
    paste0("%+.", digits, "f")
  } else {
    paste0("%.", digits, "f")
  }
  
  result <- sprintf(format_string, x)
  
  # APA style: .950 instead of 0.950
  result <- sub(
    "^([+-]?)0\\.",
    "\\1.",
    result
  )
  
  result[is.na(x)] <- ""
  
  result
}


format_ci <- function(
    estimate,
    lower,
    upper,
    digits = 3
) {
  paste0(
    format_decimal(estimate, digits),
    " [",
    format_decimal(lower, digits),
    ", ",
    format_decimal(upper, digits),
    "]"
  )
}


##### FORMAT APA TABLE #####

apa_rule <- officer::fp_border(
  color = "#000000",
  width = 1,
  style = "single"
)


format_apa_table <- function(
    data,
    left_columns = character(),
    widths = NULL,
    font_size = 8,
    bold_rows = NULL
) {
  
  center_columns <- setdiff(
    names(data),
    left_columns
  )
  
  ft <- flextable::flextable(data) |>
    flextable::font(
      fontname = "Times New Roman",
      part = "all"
    ) |>
    flextable::fontsize(
      size = font_size,
      part = "all"
    ) |>
    flextable::bold(part = "header") |>
    flextable::valign(
      valign = "top",
      part = "body"
    ) |>
    flextable::padding(
      padding = 2,
      part = "all"
    ) |>
    flextable::border_remove() |>
    flextable::set_table_properties(
      layout = "fixed",
      width = 1,
      align = "left"
    )
  
  if (length(left_columns) > 0) {
    ft <- flextable::align(
      ft,
      j = left_columns,
      align = "left",
      part = "all"
    )
  }
  
  if (length(center_columns) > 0) {
    ft <- flextable::align(
      ft,
      j = center_columns,
      align = "center",
      part = "all"
    )
  }
  
  if (!is.null(bold_rows) && any(bold_rows)) {
    ft <- flextable::bold(
      ft,
      i = bold_rows,
      part = "body"
    )
  }
  
  if (!is.null(widths)) {
    
    unknown_columns <- setdiff(
      names(widths),
      names(data)
    )
    
    if (length(unknown_columns) > 0) {
      stop(
        "Unknown columns in widths: ",
        paste(unknown_columns, collapse = ", ")
      )
    }
    
    for (column in names(widths)) {
      ft <- flextable::width(
        ft,
        j = column,
        width = widths[[column]]
      )
    }
  }
  
  ft |>
    flextable::hline_top(
      part = "header",
      border = apa_rule
    ) |>
    flextable::hline_bottom(
      part = "header",
      border = apa_rule
    ) |>
    flextable::hline_bottom(
      # i = nrow(data),
      part = "body",
      border = apa_rule
    )
}


##### SAVE APA TABLE AS WORD DOCUMENT #####

save_apa_table <- function(
    ft,
    number,
    title,
    note,
    target,
    landscape = FALSE
) {
  
  dir.create(
    dirname(target),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  doc <- officer::read_docx()
  
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        paste("Table", number),
        prop = officer::fp_text(
          font.family = "Times New Roman",
          font.size = 12,
          bold = TRUE
        )
      ),
      fp_p = officer::fp_par(
        line_spacing = 2,
        padding.top = 0,
        padding.bottom = 0
      )
    )
  )
  
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        title,
        prop = officer::fp_text(
          font.family = "Times New Roman",
          font.size = 12,
          italic = TRUE
        )
      ),
      fp_p = officer::fp_par(
        line_spacing = 2,
        padding.top = 0,
        padding.bottom = 0
      )
    )
  )
  
  doc <- flextable::body_add_flextable(
    doc,
    value = ft
  )
  
  if (!is.null(note)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          "Note.",
          officer::fp_text(
            font.family = "Times New Roman",
            font.size = 8,
            italic = TRUE
          )
        ),
        officer::ftext(
          paste0(" ", note),
          officer::fp_text(
            font.family = "Times New Roman",
            font.size = 8
          )
        )
      )
    )
  }
  
  if (landscape) {
    doc <- officer::body_set_default_section(
      doc,
      value = officer::prop_section(
        page_size = officer::page_size(
          orient = "landscape",
          width = 11.69,
          height = 8.27
        ),
        page_margins = officer::page_mar(
          top = 0.40,
          bottom = 0.40,
          left = 0.35,
          right = 0.35
        )
      )
    )
  }
  
  print(
    doc,
    target = target
  )
  
  if (!file.exists(target)) {
    stop("The Word table could not be created: ", target)
  }
  
  message("APA table saved to: ", target)
  
  invisible(target)
}


################ MODELL HELPERS ################

##### FORMAT NUMERIC VALUES FOR MPLUS ####

format_mplus_number <- function(
    number,
    digits = 6
) {
  
  formatted_number <- formatC(
    number,
    format = "f",
    digits = digits,
    decimal.mark = "."
  )
  
  formatted_number <- sub(
    "0+$",
    "",
    formatted_number
  )
  
  formatted_number <- sub(
    "\\.$",
    "",
    formatted_number
  )
  
  if (formatted_number == "-0") {
    formatted_number <- "0"
  }
  
  formatted_number
}

#-------------------------------------------------------------------------
##### MPLUS OUTPUT HELPERS FOR FIGURES #####
#-------------------------------------------------------------------------

extract_growth_mean_parameter_indices <- function(
    output_lines,
    class_numbers = 1:3,
    growth_parameters = c(
      "BUR_I",
      "BUR_S",
      "BUR_Q"
    )
) {
  technical_1_start <- grep(
    "^[[:space:]]*TECHNICAL 1 OUTPUT[[:space:]]*$",
    output_lines
  )
  
  if (length(technical_1_start) != 1L) {
    stop(
      "TECHNICAL 1 OUTPUT could not be identified uniquely."
    )
  }
  
  starting_values_start <- grep(
    paste0(
      "^[[:space:]]*STARTING VALUES FOR ",
      "LATENT CLASS 1[[:space:]]*$"
    ),
    output_lines
  )
  
  starting_values_start <- starting_values_start[
    starting_values_start > technical_1_start
  ]
  
  if (length(starting_values_start) < 1L) {
    stop(
      "The end of TECHNICAL 1 OUTPUT could not be identified."
    )
  }
  
  technical_1_lines <- output_lines[
    technical_1_start:
      (starting_values_start[1] - 1L)
  ]
  
  class_results <- lapply(
    class_numbers,
    function(class_number) {
      class_start <- grep(
        paste0(
          "^[[:space:]]*PARAMETER SPECIFICATION FOR ",
          "LATENT CLASS ",
          class_number,
          "[[:space:]]*$"
        ),
        technical_1_lines
      )
      
      if (length(class_start) != 1L) {
        stop(
          "The TECH1 specification for latent class ",
          class_number,
          " could not be identified uniquely."
        )
      }
      
      all_class_sections <- grep(
        paste0(
          "^[[:space:]]*PARAMETER SPECIFICATION FOR ",
          "LATENT CLASS"
        ),
        technical_1_lines
      )
      
      later_class_sections <- all_class_sections[
        all_class_sections > class_start
      ]
      
      class_end <- if (
        length(later_class_sections) == 0L
      ) {
        length(technical_1_lines)
      } else {
        later_class_sections[1] - 1L
      }
      
      class_lines <- technical_1_lines[
        class_start:class_end
      ]
      
      alpha_start <- which(
        trimws(class_lines) == "ALPHA"
      )
      
      if (length(alpha_start) != 1L) {
        stop(
          "The ALPHA block for latent class ",
          class_number,
          " could not be identified uniquely."
        )
      }
      
      later_model_sections <- which(
        seq_along(class_lines) > alpha_start &
          trimws(class_lines) %in% c(
            "BETA",
            "PSI",
            "THETA",
            "NU",
            "LAMBDA"
          )
      )
      
      alpha_end <- if (
        length(later_model_sections) == 0L
      ) {
        length(class_lines)
      } else {
        later_model_sections[1] - 1L
      }
      
      alpha_lines <- class_lines[
        alpha_start:alpha_end
      ]
      
      parameter_name_position <- which(
        vapply(
          alpha_lines,
          function(line) {
            line_parameters <- strsplit(
              trimws(line),
              "[[:space:]]+"
            )[[1]]
            
            identical(
              line_parameters,
              growth_parameters
            )
          },
          logical(1)
        )
      )
      
      if (length(parameter_name_position) != 1L) {
        stop(
          "The growth-parameter columns for latent class ",
          class_number,
          " could not be identified uniquely in ALPHA."
        )
      }
      
      parameter_number_pattern <- paste0(
        "^[[:space:]]*1",
        paste(
          rep(
            "[[:space:]]+[0-9]+",
            length(growth_parameters)
          ),
          collapse = ""
        ),
        "[[:space:]]*$"
      )
      
      parameter_number_position <- grep(
        parameter_number_pattern,
        alpha_lines
      )
      
      parameter_number_position <-
        parameter_number_position[
          parameter_number_position >
            parameter_name_position
        ]
      
      if (length(parameter_number_position) != 1L) {
        stop(
          "The TECH3 parameter numbers for latent class ",
          class_number,
          " could not be identified uniquely."
        )
      }
      
      parameter_numbers <- as.integer(
        strsplit(
          trimws(
            alpha_lines[
              parameter_number_position
            ]
          ),
          "[[:space:]]+"
        )[[1]][-1]
      )
      
      tibble::tibble(
        .mplus_class = class_number,
        .parameter = growth_parameters,
        tech3_index = parameter_numbers
      )
    }
  )
  
  parameter_lookup <- dplyr::bind_rows(
    class_results
  )
  
  if (
    nrow(parameter_lookup) !=
    length(class_numbers) *
    length(growth_parameters) ||
    anyNA(parameter_lookup$tech3_index) ||
    anyDuplicated(
      parameter_lookup$tech3_index
    ) > 0L
  ) {
    stop(
      paste(
        "The class-specific TECH3 parameter mapping",
        "is incomplete or duplicated."
      )
    )
  }
  
  parameter_lookup
}

extract_tech3_covariance_matrix <- function(
    output_lines
) {
  technical_3_start <- which(
    grepl(
      "^\\s*TECHNICAL 3 OUTPUT\\s*$",
      output_lines
    )
  )
  
  if (length(technical_3_start) != 1L) {
    stop(
      paste(
        "TECHNICAL 3 OUTPUT is missing or occurs more than once.",
        "Request TECH3 in the OUTPUT section and rerun the model."
      )
    )
  }
  
  correlation_start <- which(
    seq_along(output_lines) > technical_3_start &
      grepl(
        paste(
          "ESTIMATED CORRELATION MATRIX",
          "FOR PARAMETER ESTIMATES"
        ),
        output_lines,
        fixed = TRUE
      )
  )
  
  if (length(correlation_start) < 1L) {
    stop(
      "The end of the TECH3 covariance matrix could not be identified."
    )
  }
  
  covariance_lines <- output_lines[
    technical_3_start:
      (correlation_start[1] - 1L)
  ]
  
  covariance_header_positions <- which(
    grepl(
      paste(
        "ESTIMATED COVARIANCE MATRIX",
        "FOR PARAMETER ESTIMATES"
      ),
      covariance_lines,
      fixed = TRUE
    )
  )
  
  if (length(covariance_header_positions) < 1L) {
    stop(
      "No parameter-estimate covariance blocks were found in TECH3."
    )
  }
  
  covariance_entries <- list()
  maximum_parameter_number <- 0L
  
  for (block_number in seq_along(covariance_header_positions)) {
    block_start <- covariance_header_positions[block_number]
    
    block_end <- if (
      block_number < length(covariance_header_positions)
    ) {
      covariance_header_positions[block_number + 1L] - 1L
    } else {
      length(covariance_lines)
    }
    
    block_lines <- covariance_lines[
      block_start:block_end
    ]
    
    column_header_position <- which(
      grepl(
        "^\\s*[0-9]+(?:\\s+[0-9]+)*\\s*$",
        block_lines,
        perl = TRUE
      )
    )
    
    if (length(column_header_position) < 1L) {
      stop(
        "A TECH3 covariance-matrix column header could not be read."
      )
    }
    
    column_header_position <- column_header_position[1]
    
    column_numbers <- as.integer(
      strsplit(
        trimws(
          block_lines[column_header_position]
        ),
        "\\s+"
      )[[1]]
    )
    
    maximum_parameter_number <- max(
      maximum_parameter_number,
      column_numbers
    )
    
    data_lines <- block_lines[
      (column_header_position + 1L):
        length(block_lines)
    ]
    
    for (line in data_lines) {
      tokens <- strsplit(
        trimws(line),
        "\\s+"
      )[[1]]
      
      if (length(tokens) < 2L) {
        next
      }
      
      row_number <- suppressWarnings(
        as.integer(tokens[1])
      )
      
      values <- suppressWarnings(
        as.numeric(
          gsub(
            "[dD]",
            "E",
            tokens[-1]
          )
        )
      )
      
      if (
        !is.finite(row_number) ||
        any(!is.finite(values))
      ) {
        next
      }
      
      eligible_columns <- column_numbers[
        column_numbers <= row_number
      ]
      
      if (length(values) != length(eligible_columns)) {
        stop(
          "A TECH3 covariance row has an unexpected number of values: ",
          trimws(line)
        )
      }
      
      covariance_entries[[
        length(covariance_entries) + 1L
      ]] <- tibble::tibble(
        row = row_number,
        column = eligible_columns,
        covariance = values
      )
      
      maximum_parameter_number <- max(
        maximum_parameter_number,
        row_number
      )
    }
  }
  
  covariance_entries <- dplyr::bind_rows(
    covariance_entries
  )
  
  if (
    nrow(covariance_entries) == 0L ||
    anyDuplicated(
      paste(
        covariance_entries$row,
        covariance_entries$column,
        sep = "_"
      )
    ) > 0L
  ) {
    stop(
      "The TECH3 covariance entries are missing or duplicated."
    )
  }
  
  covariance_matrix <- matrix(
    NA_real_,
    nrow = maximum_parameter_number,
    ncol = maximum_parameter_number
  )
  
  covariance_matrix[
    cbind(
      covariance_entries$row,
      covariance_entries$column
    )
  ] <- covariance_entries$covariance
  
  covariance_matrix[upper.tri(covariance_matrix)] <-
    t(covariance_matrix)[upper.tri(covariance_matrix)]
  
  if (
    any(!is.finite(diag(covariance_matrix))) ||
    any(!is.finite(covariance_matrix)) ||
    !isTRUE(
      all.equal(
        covariance_matrix,
        t(covariance_matrix),
        tolerance = 1e-10
      )
    )
  ) {
    stop(
      "The reconstructed TECH3 covariance matrix is incomplete or asymmetric."
    )
  }
  
  covariance_matrix
}


#-------------------------------------------------------------------------
##### MODERATION-MODEL OUTPUT HELPERS ####
#-------------------------------------------------------------------------

extract_tech1_additional_parameter_indices <- function(
    output_lines,
    parameter_names = NULL
) {
  section_start <- which(
    trimws(output_lines) ==
      "PARAMETER SPECIFICATION FOR THE ADDITIONAL PARAMETERS"
  )

  if (length(section_start) != 1L) {
    stop(
      paste(
        "The TECH1 specification for additional parameters",
        "could not be identified uniquely."
      )
    )
  }

  section_end <- which(
    seq_along(output_lines) > section_start &
      trimws(output_lines) == "STARTING VALUES"
  )

  if (length(section_end) < 1L) {
    stop(
      paste(
        "The end of the TECH1 additional-parameter",
        "specification could not be identified."
      )
    )
  }

  section_lines <- output_lines[
    section_start:(section_end[1L] - 1L)
  ]

  block_starts <- which(
    trimws(section_lines) == "New/Additional Parameters"
  )

  if (length(block_starts) < 1L) {
    stop(
      "No additional-parameter blocks were found in TECH1."
    )
  }

  parameter_blocks <- lapply(
    seq_along(block_starts),
    function(block_number) {
      block_start <- block_starts[block_number]

      block_end <- if (
        block_number < length(block_starts)
      ) {
        block_starts[block_number + 1L] - 1L
      } else {
        length(section_lines)
      }

      block_lines <- section_lines[
        block_start:block_end
      ]

      nonempty_positions <- which(
        nzchar(trimws(block_lines))
      )

      name_position <- nonempty_positions[
        nonempty_positions > 1L
      ][1L]

      number_positions <- grep(
        "^[[:space:]]*1(?:[[:space:]]+[0-9]+)+[[:space:]]*$",
        block_lines,
        perl = TRUE
      )

      number_positions <- number_positions[
        number_positions > name_position
      ]

      if (
        !is.finite(name_position) ||
        length(number_positions) < 1L
      ) {
        stop(
          "A TECH1 additional-parameter block could not be parsed."
        )
      }

      names_in_block <- strsplit(
        trimws(block_lines[name_position]),
        "[[:space:]]+"
      )[[1L]]

      indices_in_block <- as.integer(
        strsplit(
          trimws(block_lines[number_positions[1L]]),
          "[[:space:]]+"
        )[[1L]][-1L]
      )

      if (
        length(names_in_block) != length(indices_in_block) ||
        anyNA(indices_in_block)
      ) {
        stop(
          paste(
            "Names and parameter numbers did not match in a",
            "TECH1 additional-parameter block."
          )
        )
      }

      tibble::tibble(
        parameter = names_in_block,
        parameter_clean = clean_text(names_in_block),
        tech3_index = indices_in_block
      )
    }
  )

  parameter_lookup <- dplyr::bind_rows(
    parameter_blocks
  )

  if (
    nrow(parameter_lookup) < 1L ||
    anyDuplicated(parameter_lookup$parameter_clean) > 0L ||
    anyDuplicated(parameter_lookup$tech3_index) > 0L
  ) {
    stop(
      paste(
        "The TECH1 additional-parameter mapping",
        "is empty or duplicated."
      )
    )
  }

  if (!is.null(parameter_names)) {
    requested_names <- clean_text(parameter_names)
    requested_positions <- match(
      requested_names,
      parameter_lookup$parameter_clean
    )

    if (anyNA(requested_positions)) {
      stop(
        "TECH1 indices were not found for: ",
        paste(
          parameter_names[is.na(requested_positions)],
          collapse = ", "
        )
      )
    }

    parameter_lookup <- parameter_lookup[
      requested_positions,
      ,
      drop = FALSE
    ]
  }

  parameter_lookup
}


extract_global_wald_test <- function(output_lines) {
  section_start <- which(
    trimws(output_lines) ==
      "Wald Test of Parameter Constraints"
  )

  if (length(section_start) != 1L) {
    stop(
      paste(
        "The global Wald test could not be",
        "identified uniquely."
      )
    )
  }

  section_lines <- output_lines[
    section_start:min(
      section_start + 15L,
      length(output_lines)
    )
  ]

  extract_last_numeric_value <- function(pattern) {
    matching_lines <- grep(
      pattern,
      section_lines,
      value = TRUE
    )

    if (length(matching_lines) < 1L) {
      stop(
        "A global Wald-test statistic could not be read: ",
        pattern
      )
    }

    last_token <- tail(
      strsplit(
        trimws(matching_lines[1L]),
        "[[:space:]]+"
      )[[1L]],
      1L
    )

    as.numeric(
      gsub(
        "[dD]",
        "E",
        sub("\\*$", "", last_token)
      )
    )
  }

  tibble::tibble(
    test = "All class-by-moderator interactions",
    chi_square = extract_last_numeric_value(
      "^[[:space:]]*Value[[:space:]]+"
    ),
    df = as.integer(
      extract_last_numeric_value(
        "^[[:space:]]*Degrees of Freedom[[:space:]]+"
      )
    ),
    p_value = extract_last_numeric_value(
      "^[[:space:]]*P-Value[[:space:]]+"
    )
  )
}


extract_moderation_parameter_rows <- function(
    model,
    parameter_names
) {
  parameters <- parameter_table(model) |>
    dplyr::filter(
      grepl(
        "NEW|ADDITIONAL",
        .data$header_clean
      )
    )

  requested_names <- clean_text(parameter_names)
  parameter_positions <- match(
    requested_names,
    parameters$param_clean
  )

  if (
    anyNA(parameter_positions) ||
    anyDuplicated(parameters$param_clean) > 0L
  ) {
    stop(
      "Additional-parameter estimates were not found uniquely for: ",
      paste(
        parameter_names[is.na(parameter_positions)],
        collapse = ", "
      )
    )
  }

  parameters[
    parameter_positions,
    ,
    drop = FALSE
  ]
}


extract_moderation_warning_summary <- function(output_lines) {
  output_text <- paste(
    output_lines,
    collapse = "\n"
  )

  output_text_upper <- toupper(
    output_text
  )

  extract_first_capture <- function(pattern) {
    match_result <- regexec(
      pattern,
      output_text,
      ignore.case = TRUE,
      perl = TRUE
    )

    capture <- regmatches(
      output_text,
      match_result
    )[[1L]]

    if (length(capture) < 2L) {
      return(NA_character_)
    }

    trimws(capture[2L])
  }

  normal_termination <- grepl(
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    output_text_upper,
    fixed = TRUE
  )

  missing_x_n <- suppressWarnings(
    as.integer(
      extract_first_capture(
        paste0(
          "Number of cases with missing on x-variables:",
          "\\s+([0-9]+)"
        )
      )
    )
  )

  low_covariance_coverage <- grepl(
    "LOW COVARIANCE COVERAGE",
    output_text_upper,
    fixed = TRUE
  )

  robust_chi_square_unavailable <- grepl(
    "THE ROBUST CHI-SQUARE COULD NOT BE COMPUTED",
    output_text_upper,
    fixed = TRUE
  )

  residual_covariance_warning <- grepl(
    "RESIDUAL COVARIANCE MATRIX (THETA) IS NOT POSITIVE DEFINITE",
    output_text_upper,
    fixed = TRUE
  )

  derivative_matrix_warning <- grepl(
    paste0(
      "NON-POSITIVE DEFINITE\\s+",
      "FIRST-ORDER DERIVATIVE PRODUCT MATRIX"
    ),
    output_text_upper,
    perl = TRUE
  )

  nonreplicated_solution <- grepl(
    "BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",
    output_text_upper,
    fixed = TRUE
  )

  residual_problem_variable <- extract_first_capture(
    "PROBLEM INVOLVING VARIABLE\\s+([A-Z0-9_]+)"
  )

  problem_parameter <- extract_first_capture(
    "Parameter\\s+[0-9]+,\\s*([^\\r\\n]+)"
  )

  warning_messages <- character()

  if (!is.na(missing_x_n)) {
    warning_messages <- c(
      warning_messages,
      paste0(
        "Cases with incomplete predictor or covariate data were excluded (n = ",
        missing_x_n,
        ")."
      )
    )
  }

  if (low_covariance_coverage) {
    warning_messages <- c(
      warning_messages,
      "Low covariance coverage was reported."
    )
  }

  if (robust_chi_square_unavailable) {
    warning_messages <- c(
      warning_messages,
      "The robust model chi-square was unavailable."
    )
  }

  if (residual_covariance_warning) {
    residual_message <- paste(
      "The residual covariance matrix was not positive definite."
    )

    if (!is.na(residual_problem_variable)) {
      residual_message <- paste0(
        residual_message,
        " Problem variable: ",
        residual_problem_variable,
        "."
      )
    }

    warning_messages <- c(
      warning_messages,
      residual_message
    )
  }

  if (derivative_matrix_warning) {
    derivative_message <- paste(
      "The first-order derivative product matrix was not",
      "positive definite."
    )

    if (!is.na(problem_parameter)) {
      derivative_message <- paste0(
        derivative_message,
        " Problem parameter: ",
        problem_parameter,
        "."
      )
    }

    warning_messages <- c(
      warning_messages,
      derivative_message
    )
  }

  if (nonreplicated_solution) {
    warning_messages <- c(
      warning_messages,
      "The best loglikelihood value was not replicated."
    )
  }

  critical_warning <-
    low_covariance_coverage ||
    residual_covariance_warning ||
    derivative_matrix_warning ||
    nonreplicated_solution

  technical_status <- dplyr::case_when(
    !normal_termination ~ "Did not terminate normally",
    critical_warning ~ "Converged; warning requires review",
    TRUE ~ "Converged; no critical warning"
  )

  tibble::tibble(
    normal_termination = normal_termination,
    missing_x_n = missing_x_n,
    low_covariance_coverage = low_covariance_coverage,
    robust_chi_square_unavailable =
      robust_chi_square_unavailable,
    residual_covariance_warning =
      residual_covariance_warning,
    derivative_matrix_warning =
      derivative_matrix_warning,
    nonreplicated_solution = nonreplicated_solution,
    problem_parameter = dplyr::coalesce(
      problem_parameter,
      residual_problem_variable
    ),
    technical_status = technical_status,
    technical_warnings = if (
      length(warning_messages) == 0L
    ) {
      "None"
    } else {
      paste(
        warning_messages,
        collapse = " "
      )
    }
  )
}


extract_moderation_model_results <- function(
    output_file,
    model_id,
    moderator,
    adjustment,
    sample_filter,
    covariates,
    analysis_role
) {
  if (!file.exists(output_file)) {
    stop(
      "Mplus moderation output not found: ",
      output_file
    )
  }

  output_lines <- readLines(
    output_file,
    warn = FALSE
  )

  model <- MplusAutomation::readModels(
    output_file,
    what = c(
      "summaries",
      "parameters",
      "warn_err"
    ),
    quiet = TRUE
  )

  model_errors <- unlist(
    model$errors,
    use.names = FALSE
  )

  normal_termination_in_output <- any(
    grepl(
      "THE MODEL ESTIMATION TERMINATED NORMALLY",
      output_lines,
      fixed = TRUE
    )
  )

  if (
    length(model_errors) > 0L &&
    !normal_termination_in_output
  ) {
    stop(
      "Mplus errors found in ",
      output_file,
      ":\n",
      paste(
        model_errors,
        collapse = "\n"
      )
    )
  }

  if (
    is.null(model$summaries) ||
    nrow(model$summaries) != 1L
  ) {
    stop(
      "Expected exactly one Mplus summary in: ",
      output_file
    )
  }

  global_test <- extract_global_wald_test(
    output_lines
  )

  warning_summary <- extract_moderation_warning_summary(
    output_lines
  )

  test_definitions <- tibble::tribble(
    ~test_id, ~outcome, ~parameter_prefix,
    "baseline_externalizing",
    "Externalizing problems at baseline",
    "IBE",
    "baseline_emotional",
    "Emotional problems at baseline",
    "IBM",
    "change_externalizing",
    "Change in externalizing problems",
    "IDX",
    "change_emotional",
    "Change in emotional problems",
    "IDM"
  )

  interaction_parameter_names <- unlist(
    lapply(
      test_definitions$parameter_prefix,
      function(parameter_prefix) {
        paste0(
          parameter_prefix,
          c(
            "_2V1",
            "_3V1",
            "_4V1"
          )
        )
      }
    ),
    use.names = FALSE
  )

  parameter_estimates <- extract_moderation_parameter_rows(
    model,
    interaction_parameter_names
  )

  parameter_indices <-
    extract_tech1_additional_parameter_indices(
      output_lines,
      interaction_parameter_names
    )

  covariance_matrix <- extract_tech3_covariance_matrix(
    output_lines
  )

  separate_tests <- purrr::pmap_dfr(
    test_definitions,
    function(
      test_id,
      outcome,
      parameter_prefix
    ) {
      requested_names <- paste0(
        parameter_prefix,
        c(
          "_2V1",
          "_3V1",
          "_4V1"
        )
      )

      requested_positions <- match(
        clean_text(requested_names),
        clean_text(interaction_parameter_names)
      )

      estimates <- as.numeric(
        parameter_estimates$est[
          requested_positions
        ]
      )

      covariance_indices <- parameter_indices$tech3_index[
        requested_positions
      ]

      parameter_covariance <- covariance_matrix[
        covariance_indices,
        covariance_indices,
        drop = FALSE
      ]

      wald_statistic <- as.numeric(
        t(estimates) %*%
          solve(
            parameter_covariance,
            estimates
          )
      )

      tibble::tibble(
        test_id = test_id,
        outcome = outcome,
        chi_square = wald_statistic,
        df = length(estimates),
        p_value = stats::pchisq(
          wald_statistic,
          df = length(estimates),
          lower.tail = FALSE
        )
      )
    }
  )

  class_lookup <- tibble::tribble(
    ~class_number, ~class_label,
    1L, "Non-maltreated",
    2L, "Moderate/Early-increasing burden",
    3L, "Elevated/Declining burden",
    4L, "High/Rebound burden"
  )

  slope_definitions <- tibble::tribble(
    ~outcome_id, ~outcome, ~parameter_prefix,
    "baseline_externalizing",
    "Externalizing problems at baseline",
    "SBE",
    "baseline_emotional",
    "Emotional problems at baseline",
    "SBM",
    "change_externalizing",
    "Change in externalizing problems",
    "SDX",
    "change_emotional",
    "Change in emotional problems",
    "SDM"
  )

  simple_slopes <- purrr::pmap_dfr(
    slope_definitions,
    function(
      outcome_id,
      outcome,
      parameter_prefix
    ) {
      parameter_names <- paste0(
        parameter_prefix,
        "_C",
        class_lookup$class_number
      )

      estimates <- extract_moderation_parameter_rows(
        model,
        parameter_names
      )

      tibble::tibble(
        outcome_id = outcome_id,
        outcome = outcome,
        class_number = class_lookup$class_number,
        class_label = class_lookup$class_label,
        parameter = parameter_names,
        estimate = as.numeric(estimates$est),
        standard_error = as.numeric(estimates$se),
        ci_lower = estimate - 1.96 * standard_error,
        ci_upper = estimate + 1.96 * standard_error,
        standardized_estimate = NA_real_,
        p_value = as.numeric(estimates$pval)
      )
    }
  )

  contrast_lookup <- tibble::tribble(
    ~contrast_code, ~class_a, ~class_b,
    "2V1", 2L, 1L,
    "3V1", 3L, 1L,
    "4V1", 4L, 1L,
    "3V2", 3L, 2L,
    "4V2", 4L, 2L,
    "4V3", 4L, 3L
  ) |>
    dplyr::left_join(
      class_lookup |>
        dplyr::rename(
          class_a = class_number,
          class_a_label = class_label
        ),
      by = "class_a"
    ) |>
    dplyr::left_join(
      class_lookup |>
        dplyr::rename(
          class_b = class_number,
          class_b_label = class_label
        ),
      by = "class_b"
    ) |>
    dplyr::mutate(
      contrast = paste(
        .data$class_a_label,
        "vs.",
        .data$class_b_label
      )
    )

  contrast_definitions <- tibble::tribble(
    ~outcome_id, ~outcome, ~parameter_prefix,
    "baseline_externalizing",
    "Externalizing problems at baseline",
    "IBE",
    "baseline_emotional",
    "Emotional problems at baseline",
    "IBM",
    "change_externalizing",
    "Change in externalizing problems",
    "IDX",
    "change_emotional",
    "Change in emotional problems",
    "IDM"
  )

  class_contrasts <- purrr::pmap_dfr(
    contrast_definitions,
    function(
      outcome_id,
      outcome,
      parameter_prefix
    ) {
      parameter_names <- paste0(
        parameter_prefix,
        "_",
        contrast_lookup$contrast_code
      )

      estimates <- extract_moderation_parameter_rows(
        model,
        parameter_names
      )

      tibble::tibble(
        outcome_id = outcome_id,
        outcome = outcome,
        contrast_code = contrast_lookup$contrast_code,
        contrast = contrast_lookup$contrast,
        parameter = parameter_names,
        estimate = as.numeric(estimates$est),
        standard_error = as.numeric(estimates$se),
        ci_lower = estimate - 1.96 * standard_error,
        ci_upper = estimate + 1.96 * standard_error,
        standardized_estimate = NA_real_,
        p_value = as.numeric(estimates$pval)
      )
    }
  )

  model_information <- tibble::tibble(
    model_id = model_id,
    output_file = normalizePath(
      output_file,
      winslash = "/",
      mustWork = TRUE
    ),
    moderator = moderator,
    sample_filter = sample_filter,
    n = as.integer(
      model$summaries$Observations[1L]
    ),
    estimator = "MLR",
    adjustment = adjustment,
    covariates = covariates,
    analysis_role = analysis_role,
    global_chi_square = global_test$chi_square,
    global_df = global_test$df,
    global_p_value = global_test$p_value
  ) |>
    dplyr::bind_cols(
      warning_summary
    )

  list(
    model = model_information,
    separate_tests = separate_tests |>
      dplyr::mutate(
        model_id = model_id,
        moderator = moderator,
        .before = 1L
      ),
    simple_slopes = simple_slopes |>
      dplyr::mutate(
        model_id = model_id,
        moderator = moderator,
        .before = 1L
      ),
    class_contrasts = class_contrasts |>
      dplyr::mutate(
        model_id = model_id,
        moderator = moderator,
        .before = 1L
      )
  )
}

##### DEFINE PANEL-B FORMATTING HELPERS #####

format_integer_or_dash <- function(x) {
  dplyr::if_else(
    is.na(x),
    "—",
    format(
      as.integer(
        round(x)
      ),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  )
}


format_fit_number_or_dash <- function(
    x,
    digits = 2L
) {
  dplyr::if_else(
    is.na(x),
    "—",
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    )
  )
}


format_decimal_or_dash <- function(
    x,
    digits = 3L
) {
  formatted_values <- sprintf(
    paste0(
      "%.",
      digits,
      "f"
    ),
    x
  )
  
  formatted_values <- sub(
    "^0\\.",
    ".",
    formatted_values
  )
  
  formatted_values <- sub(
    "^-0\\.",
    "-.",
    formatted_values
  )
  
  formatted_values[
    is.na(x)
  ] <- "—"
  
  formatted_values
}


format_p_or_dash <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "—",
    x < .001 ~ "< .001",
    TRUE ~ sub(
      "^0\\.",
      ".",
      sprintf(
        "%.3f",
        x
      )
    )
  )
}


format_smallest_class <- function(
    class_n,
    class_proportion
) {
  output <- rep(
    "—",
    length(class_n)
  )
  
  available_values <- (
    !is.na(class_n) &
      !is.na(class_proportion)
  )
  
  output[
    available_values
  ] <- sprintf(
    "%d (%.1f%%)",
    as.integer(
      round(
        class_n[
          available_values
        ]
      )
    ),
    100 *
      class_proportion[
        available_values
      ]
  )
  
  output
}

scale_table_widths <- function(
    widths,
    maximum_width = 10.50
) {
  
  total_width <- sum(
    widths
  )
  
  if (
    total_width > maximum_width
  ) {
    
    widths <- widths *
      maximum_width /
      total_width
  }
  
  widths
}

heading_paragraph_properties <- officer::fp_par(
  text.align = "left",
  line_spacing = 2,
  padding = 0,
  keep_with_next = TRUE
)

note_paragraph_properties <- officer::fp_par(
  text.align = "left",
  line_spacing = 1,
  padding = 0
)

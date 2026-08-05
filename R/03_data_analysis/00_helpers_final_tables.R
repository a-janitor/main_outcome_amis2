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
      i = nrow(data),
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
    
    landscape_section <- officer::block_section(
      officer::prop_section(
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
        ),
        type = "continuous"
      )
    )
    
    doc <- officer::body_end_block_section(
      doc,
      value = landscape_section
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





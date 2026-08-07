#-----------------------------------------------------------------------
##### CREATE FINAL FIGURES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R"
)

required_packages <- c(
  "MplusAutomation",
  "dplyr",
  "tibble",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install first: ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}


#-------------------------------------------------------------------------
##### FIGURE X: LATENT CHANGE FROM T2 TO T5 #####
#-------------------------------------------------------------------------

figure_number <- "X"


##### READ M8 OUTPUT #####

m8_output_file <- file.path(
  mplus_input_dir,
  "08_sdq_classical_lcs.out"
)

if (!file.exists(m8_output_file)) {
  stop(
    "The M8 output file is missing:\n",
    m8_output_file
  )
}

m8_model <- MplusAutomation::readModels(
  m8_output_file,
  quiet = TRUE
)

m8_parameters <- m8_model$parameters$unstandardized


##### EXTRACT LATENT CHANGE MEANS #####

change_mean_rows <- m8_parameters |>
  dplyr::filter(
    toupper(trimws(as.character(paramHeader))) == "MEANS",
    toupper(trimws(as.character(param))) %in% c(
      "D_EXT",
      "D_EMO"
    )
  )

if (
  nrow(change_mean_rows) != 2 ||
  dplyr::n_distinct(
    toupper(trimws(as.character(change_mean_rows$param)))
  ) != 2
) {
  stop(
    paste(
      "The latent change means for d_ext and d_emo",
      "could not be identified uniquely in M8."
    )
  )
}

change_means <- change_mean_rows |>
  dplyr::transmute(
    outcome = dplyr::recode(
      toupper(trimws(as.character(param))),
      "D_EXT" = "Externalizing problems",
      "D_EMO" = "Emotional problems"
    ),
    estimate = as.numeric(est),
    se = as.numeric(se),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  )


##### PREPARE TRAJECTORY DATA #####

trajectory_data <- dplyr::bind_rows(
  change_means |>
    dplyr::transmute(
      outcome,
      time = 0,
      estimate = 0,
      ci_lower = 0,
      ci_upper = 0
    ),
  
  change_means |>
    dplyr::transmute(
      outcome,
      time = 1,
      estimate,
      ci_lower,
      ci_upper
    )
) |>
  dplyr::mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Externalizing problems",
        "Emotional problems"
      )
    )
  ) |>
  dplyr::arrange(
    outcome,
    time
  )


##### CREATE FIGURE #####

m8_figure <- ggplot2::ggplot(
  trajectory_data,
  ggplot2::aes(
    x = time,
    y = estimate,
    color = outcome,
    fill = outcome,
    group = outcome
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.45,
    color = "grey55"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    alpha = 0.18,
    color = NA,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    linewidth = 1.15
  ) +
  ggplot2::geom_point(
    size = 3
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 1),
    labels = c(
      "BASELINE T1",
      "FOLLOW-UP T2"
    ),
    expand = ggplot2::expansion(
      mult = c(0.05, 0.05)
    )
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25)
  ) +
  ggplot2::coord_cartesian(
    ylim = c(-1, 1)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Externalizing problems" = "#0072B2",
      "Emotional problems" = "#D55E00"
    ),
    labels = c(
      "EXTERNALIZING PROBLEMS",
      "EMOTIONAL PROBLEMS"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Externalizing problems" = "#0072B2",
      "Emotional problems" = "#D55E00"
    ),
    labels = c(
      "EXTERNALIZING PROBLEMS",
      "EMOTIONAL PROBLEMS"
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "ESTIMATED LATENT CHANGE",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_classic(
    base_size = 11,
    base_family = "Times New Roman"
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      face = "bold"
    ),
    axis.title.y = ggplot2::element_text(
      face = "bold",
      margin = ggplot2::margin(r = 8)
    ),
    legend.position = "inside",
    legend.position.inside = c(0.72, 0.88),
    legend.justification = c(0.5, 0.5),
    legend.background = ggplot2::element_rect(
      fill = scales::alpha("white", 0.75),
      color = NA
    ),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 12,
      b = 8,
      l = 8
    )
  )



##### SAVE FIGURE #####

dir.create(
  man_figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_output_file <- file.path(
  man_figure_dir,
  paste0(
    "Figure_",
    figure_number,
    "_SDQ_latent_change_T2_T5.tiff"
  )
)

ggplot2::ggsave(
  filename = figure_output_file,
  plot = m8_figure,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

if (interactive()) {
  print(m8_figure)
}

#--------------------------------------------------------------
###### PLOT LATENT TRAJECTORY CLASSES ###################
#--------------------------------------------------------------

##### DEFINE TRAJECTORY GRID FOR M18 CONFIDENCE BANDS ####

trajectory_grid_points_m18 <- 81L

trajectory_time_grid_m18 <- seq(
  from = min(
    developmental_time_scores
  ),
  to = max(
    developmental_time_scores
  ),
  length.out = trajectory_grid_points_m18
)

stopifnot(
  length(trajectory_time_grid_m18) ==
    trajectory_grid_points_m18,
  all(is.finite(trajectory_time_grid_m18))
)

#-----------------------------------------------------------------------
##### SELECT CLASSIFICATION VARIANT #####
#-----------------------------------------------------------------------

# Available options:
#   "original" = original three-class LTC solution
#   "mo"       = non-maltreated reference group plus three LTC classes

classification_variant <- "mo"

if (!classification_variant %in% c("original", "mo")) {
  stop(
    "classification_variant must be either 'original' or 'mo'."
  )
}


#-----------------------------------------------------------------------
##### CHECK PACKAGES AND PROJECT OBJECTS #####
#-----------------------------------------------------------------------

required_packages <- c(
  "MplusAutomation",
  "dplyr",
  "tidyr",
  "tibble",
  "purrr",
  "ggplot2",
  "ggrepel"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install first: ",
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}

required_objects <- c(
  "mplus_input_dir",
  "mplus_results_dir",
  "mplus_results_dir_maltreatment"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Run the project setup first. Missing objects: ",
    paste(
      missing_objects,
      collapse = ", "
    )
  )
}


#-----------------------------------------------------------------------
##### DYNAMIC CLASSIFICATION SETTINGS #####
#-----------------------------------------------------------------------

if (classification_variant == "original") {

  file_suffix <- ""
  figure_suffix <- ""

  class_numbers <- 1:3

  class_labels <- c(
    "Low and stable",
    "Elevated and declining",
    "High early burden with later rebound"
  )

  reference_label <- class_labels[1]

  m22_candidates <- c(
    "m22_lcs_with_ltc_classes_aget2_sex.out",
    "m22_lcs_with_ltc_classes_age_sex.out"
  )

  m24a_candidates <- c(
    "m24a_lcs_classes_prs_eau.out"
  )

  m25_candidates <- c(
    "m25_lcs_classes_hair_cortisol.out"
  )

} else {

  file_suffix <- "_mo"
  figure_suffix <- "_mo"

  class_numbers <- 1:4

  class_labels <- c(
    "Non-maltreated",
    "Moderate/early-increasing burden",
    "Elevated/declining burden",
    "High/rebound burden"
  )

  reference_label <- class_labels[1]

  m22_candidates <- c(
    "m22_lcs_with_ltc_classes_age_sex_mo.out",
    "m22_lcs_with_ltc_classes_aget2_sex_mo.out"
  )

  m24a_candidates <- c(
    "m24a_lcs_classes_prs_eau_mo.out"
  )

  m25_candidates <- c(
    "m25_lcs_classes_hair_cortisol_mo.out"
  )
}

stopifnot(
  length(class_numbers) == length(class_labels),
  identical(class_numbers, seq_along(class_labels))
)


#-----------------------------------------------------------------------
##### RESOLVE MODEL OUTPUT FILES #####
#-----------------------------------------------------------------------

resolve_existing_file <- function(
    directory,
    candidates,
    label
) {

  candidate_paths <- file.path(
    directory,
    candidates
  )

  existing_paths <- candidate_paths[
    file.exists(candidate_paths)
  ]

  if (length(existing_paths) == 0) {
    stop(
      label,
      " output not found. Checked:\n",
      paste(
        candidate_paths,
        collapse = "\n"
      )
    )
  }

  if (length(existing_paths) > 1) {
    warning(
      "Multiple ",
      label,
      " outputs found. Using the first one:\n",
      existing_paths[1]
    )
  }

  existing_paths[1]
}

m22_out <- resolve_existing_file(
  directory = mplus_input_dir,
  candidates = m22_candidates,
  label = "M22"
)

m24a_out <- resolve_existing_file(
  directory = mplus_input_dir,
  candidates = m24a_candidates,
  label = "M24a"
)

m25_out <- resolve_existing_file(
  directory = mplus_input_dir,
  candidates = m25_candidates,
  label = "M25"
)


#-----------------------------------------------------------------------
##### OUTPUT DIRECTORIES #####
#-----------------------------------------------------------------------

if (!exists("figures_manuscript_dir")) {
  figures_manuscript_dir <- file.path(
    mplus_results_dir,
    "08_figures",
    "manuscript"
  )
}

if (!exists("figures_appendix_dir")) {
  figures_appendix_dir <- file.path(
    mplus_results_dir,
    "08_figures",
    "appendix"
  )
}

dir.create(
  figures_manuscript_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figures_appendix_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


#-----------------------------------------------------------------------
##### HELPER FUNCTIONS #####
#-----------------------------------------------------------------------

clean_text <- function(x) {
  toupper(
    gsub(
      "[^A-Za-z0-9]+",
      "",
      trimws(
        as.character(x)
      )
    )
  )
}

read_unstandardized_parameters <- function(path) {

  model <- MplusAutomation::readModels(
    path,
    what = c(
      "parameters",
      "summaries",
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

  parameters <- tibble::as_tibble(
    model$parameters$unstandardized
  )

  names(parameters) <- tolower(
    names(parameters)
  )

  parameters |>
    dplyr::mutate(
      header_clean = clean_text(
        .data$paramheader
      ),
      param_clean = clean_text(
        .data$param
      )
    )
}

extract_new_parameter <- function(
    parameters,
    parameter_name
) {

  matched <- parameters |>
    dplyr::filter(
      grepl(
        "NEW|ADDITIONAL",
        .data$header_clean
      ),
      .data$param_clean ==
        clean_text(parameter_name)
    )

  if (nrow(matched) != 1) {
    stop(
      "Expected exactly one new parameter named ",
      parameter_name,
      " but found ",
      nrow(matched),
      "."
    )
  }

  matched |>
    dplyr::slice(1) |>
    dplyr::transmute(
      estimate = as.numeric(.data$est),
      se = as.numeric(.data$se),
      p = as.numeric(.data$pval)
    )
}

extract_regression <- function(
    parameters,
    header,
    predictor
) {

  matched <- parameters |>
    dplyr::filter(
      .data$header_clean ==
        clean_text(header),
      .data$param_clean ==
        clean_text(predictor)
    )

  if (nrow(matched) != 1) {
    stop(
      "Expected exactly one regression parameter for ",
      header,
      " / ",
      predictor,
      " but found ",
      nrow(matched),
      "."
    )
  }

  list(
    estimate = as.numeric(
      matched$est[1]
    ),
    se = as.numeric(
      matched$se[1]
    ),
    p = as.numeric(
      matched$pval[1]
    )
  )
}

save_figure <- function(
    plot,
    stem,
    width,
    height,
    output_dir
) {

  png_path <- file.path(
    output_dir,
    paste0(
      stem,
      figure_suffix,
      ".png"
    )
  )

  pdf_path <- file.path(
    output_dir,
    paste0(
      stem,
      figure_suffix,
      ".pdf"
    )
  )

  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 600,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf
  )

  c(
    PNG = png_path,
    PDF = pdf_path
  )
}


#-----------------------------------------------------------------------
##### VISUAL SETTINGS #####
#-----------------------------------------------------------------------

if (classification_variant == "original") {

  class_colours <- c(
    "Low and stable" = "#1B9E77",
    "Elevated and declining" = "#D95F02",
    "High early burden with later rebound" = "#355C9A"
  )

  class_linetypes <- c(
    "Low and stable" = "solid",
    "Elevated and declining" = "longdash",
    "High early burden with later rebound" = "dotdash"
  )

  class_shapes <- c(
    "Low and stable" = 16,
    "Elevated and declining" = 17,
    "High early burden with later rebound" = 15
  )

} else {

  class_colours <- c(
    "Non-maltreated" = "#666666",
    "Moderate/early-increasing burden" = "#1B9E77",
    "Elevated/declining burden" = "#D95F02",
    "High/rebound burden" = "#355C9A"
  )

  class_linetypes <- c(
    "Non-maltreated" = "solid",
    "Moderate/early-increasing burden" = "dashed",
    "Elevated/declining burden" = "longdash",
    "High/rebound burden" = "dotdash"
  )

  class_shapes <- c(
    "Non-maltreated" = 16,
    "Moderate/early-increasing burden" = 18,
    "Elevated/declining burden" = 17,
    "High/rebound burden" = 15
  )
}

stopifnot(
  identical(
    names(class_colours),
    class_labels
  ),
  identical(
    names(class_linetypes),
    class_labels
  ),
  identical(
    names(class_shapes),
    class_labels
  )
)

base_theme <- ggplot2::theme_classic(
  base_family = "Arial",
  base_size = 12
) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(
      size = 14,
      face = "bold",
      colour = "black"
    ),
    axis.text.x = ggplot2::element_text(
      size = 12,
      face = "bold",
      colour = "black"
    ),
    axis.text.y = ggplot2::element_text(
      size = 13,
      colour = "black"
    ),
    axis.line = ggplot2::element_line(
      colour = "black",
      linewidth = 0.75
    ),
    axis.ticks = ggplot2::element_line(
      colour = "black",
      linewidth = 0.65
    ),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = 14,
      face = "bold",
      colour = "black",
      margin = ggplot2::margin(
        b = 10
      )
    ),
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey88",
      linewidth = 0.40
    ),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 20,
      b = 10,
      l = 10
    )
  )


#-----------------------------------------------------------------------
##### READ PARAMETERS #####
#-----------------------------------------------------------------------

m22_parameters <- read_unstandardized_parameters(
  m22_out
)

m24a_parameters <- read_unstandardized_parameters(
  m24a_out
)

m25_parameters <- read_unstandardized_parameters(
  m25_out
)


#-----------------------------------------------------------------------
##### FIGURE 1: ADJUSTED CLASS-SPECIFIC TRAJECTORIES #####
#-----------------------------------------------------------------------

trajectory_key <- tidyr::crossing(
  Outcome = c(
    "Externalizing problems",
    "Emotional problems"
  ),
  Class_number = class_numbers
) |>
  dplyr::mutate(
    Class = class_labels[
      .data$Class_number
    ],
    Baseline_parameter = dplyr::case_when(
      .data$Outcome == "Externalizing problems" ~
        paste0(
          "EX2_C",
          .data$Class_number
        ),
      .data$Outcome == "Emotional problems" ~
        paste0(
          "EM2_C",
          .data$Class_number
        )
    ),
    Change_parameter = dplyr::case_when(
      .data$Outcome == "Externalizing problems" ~
        paste0(
          "DEX_C",
          .data$Class_number
        ),
      .data$Outcome == "Emotional problems" ~
        paste0(
          "DEM_C",
          .data$Class_number
        )
    )
  )

trajectory_estimates <- purrr::pmap_dfr(
  trajectory_key,
  function(
    Outcome,
    Class_number,
    Class,
    Baseline_parameter,
    Change_parameter
  ) {

    baseline <- extract_new_parameter(
      m22_parameters,
      Baseline_parameter
    )

    change <- extract_new_parameter(
      m22_parameters,
      Change_parameter
    )

    tibble::tibble(
      Outcome = Outcome,
      Class_number = Class_number,
      Class = Class,
      T2 = baseline$estimate,
      T5 = baseline$estimate +
        change$estimate
    )
  }
)

trajectory_data <- trajectory_estimates |>
  dplyr::select(
    .data$Outcome,
    .data$Class_number,
    .data$Class,
    T2,
    T5
  ) |>
  tidyr::pivot_longer(
    cols = c(
      "T2",
      "T5"
    ),
    names_to = "Time",
    values_to = "Estimate"
  ) |>
  dplyr::mutate(
    Time = factor(
      .data$Time,
      levels = c(
        "T2",
        "T5"
      )
    ),
    Class = factor(
      .data$Class,
      levels = class_labels
    ),
    Outcome = factor(
      .data$Outcome,
      levels = c(
        "Externalizing problems",
        "Emotional problems"
      )
    )
  )

trajectory_labels <- trajectory_data |>
  dplyr::filter(
    .data$Time == "T5"
  ) |>
  dplyr::mutate(
    label = dplyr::case_when(
      as.character(.data$Class) ==
        "High early burden with later rebound" ~
        "High early burden\nwith later rebound",
      as.character(.data$Class) ==
        "Moderate/early-increasing burden" ~
        "Moderate/early-\nincreasing burden",
      TRUE ~
        as.character(.data$Class)
    )
  )

trajectory_y_min <- floor(
  min(
    trajectory_data$Estimate,
    na.rm = TRUE
  ) * 2
) / 2

trajectory_y_max <- ceiling(
  max(
    trajectory_data$Estimate,
    na.rm = TRUE
  ) * 2
) / 2

figure_1 <- ggplot2::ggplot(
  trajectory_data,
  ggplot2::aes(
    x = .data$Time,
    y = .data$Estimate,
    group = interaction(
      .data$Outcome,
      .data$Class
    ),
    colour = .data$Class,
    linetype = .data$Class,
    shape = .data$Class
  )
) +
  ggplot2::geom_line(
    linewidth = 1.70,
    lineend = "round"
  ) +
  ggplot2::geom_point(
    size = 4.0,
    stroke = 0.90
  ) +
  ggrepel::geom_text_repel(
    data = trajectory_labels,
    ggplot2::aes(
      label = .data$label
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 0.18,
    size = if (
      classification_variant == "mo"
    ) {
      3.1
    } else {
      3.5
    },
    fontface = "bold",
    family = "Arial",
    segment.colour = "grey55",
    segment.size = 0.35,
    box.padding = 0.25,
    point.padding = 0.15,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(
      Outcome
    ),
    nrow = 1,
    scales = "fixed",
    labeller = ggplot2::labeller(
      Outcome = c(
        "Externalizing problems" = "EXTERNALIZING",
        "Emotional problems" = "EMOTIONAL PROBLEMS"
      )
    )
  ) +
  ggplot2::scale_colour_manual(
    values = class_colours
  ) +
  ggplot2::scale_linetype_manual(
    values = class_linetypes
  ) +
  ggplot2::scale_shape_manual(
    values = class_shapes
  ) +
  ggplot2::scale_x_discrete(
    labels = c(
      "T2" = "BASELINE (T2)",
      "T5" = "FOLLOW-UP (T5)"
    ),
    expand = ggplot2::expansion(
      add = c(
        0.10,
        if (
          classification_variant == "mo"
        ) {
          0.72
        } else {
          0.55
        }
      )
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      trajectory_y_min,
      trajectory_y_max
    ),
    breaks = seq(
      trajectory_y_min,
      trajectory_y_max,
      by = 0.5
    ),
    expand = ggplot2::expansion(
      mult = c(
        0.04,
        0.06
      )
    )
  ) +
  ggplot2::coord_cartesian(
    clip = "off"
  ) +
  ggplot2::labs(
    x = "ASSESSMENT",
    y = "ADJUSTED LATENT SCORE"
  ) +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing = grid::unit(
      2.2,
      "cm"
    )
  )

figure_1_paths <- save_figure(
  plot = figure_1,
  stem = "Figure_1_M22_direct_labelled_trajectories",
  width = if (
    classification_variant == "mo"
  ) {
    13.5
  } else {
    12.5
  },
  height = 5.8,
  output_dir = figures_manuscript_dir
)


#-----------------------------------------------------------------------
##### FIGURE 2: FOREST PLOT OF ADJUSTED CLASS CONTRASTS #####
#-----------------------------------------------------------------------

nonreference_classes <- class_numbers[
  class_numbers != 1
]

contrast_key <- tidyr::crossing(
  Outcome = c(
    "Baseline externalizing",
    "Baseline emotional problems",
    "Change in externalizing",
    "Change in emotional problems"
  ),
  Class_number = nonreference_classes
) |>
  dplyr::mutate(
    Contrast = paste0(
      class_labels[
        .data$Class_number
      ],
      " vs ",
      reference_label
    ),
    Header = dplyr::case_when(
      .data$Outcome == "Baseline externalizing" ~
        "EXT2 ON",
      .data$Outcome == "Baseline emotional problems" ~
        "EMO2 ON",
      .data$Outcome == "Change in externalizing" ~
        "DEXT ON",
      .data$Outcome == "Change in emotional problems" ~
        "DEMO ON"
    ),
    Predictor = paste0(
      "C",
      .data$Class_number
    )
  )

forest_data <- purrr::pmap_dfr(
  contrast_key,
  function(
    Outcome,
    Class_number,
    Contrast,
    Header,
    Predictor
  ) {

    result <- extract_regression(
      parameters = m22_parameters,
      header = Header,
      predictor = Predictor
    )

    tibble::tibble(
      Outcome = Outcome,
      Class_number = Class_number,
      Contrast = Contrast,
      estimate = result$estimate,
      se = result$se,
      p = result$p,
      lower = result$estimate -
        1.96 * result$se,
      upper = result$estimate +
        1.96 * result$se
    )
  }
) |>
  dplyr::mutate(
    Outcome = factor(
      .data$Outcome,
      levels = c(
        "Baseline externalizing",
        "Baseline emotional problems",
        "Change in externalizing",
        "Change in emotional problems"
      )
    ),
    Contrast = factor(
      .data$Contrast,
      levels = paste0(
        class_labels[
          nonreference_classes
        ],
        " vs ",
        reference_label
      )
    )
  )

contrast_colours <- stats::setNames(
  unname(
    class_colours[
      class_labels[
        nonreference_classes
      ]
    ]
  ),
  paste0(
    class_labels[
      nonreference_classes
    ],
    " vs ",
    reference_label
  )
)

figure_2 <- ggplot2::ggplot(
  forest_data,
  ggplot2::aes(
    x = .data$estimate,
    y = .data$Contrast,
    colour = .data$Contrast
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.65,
    colour = "grey45"
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = .data$lower,
      xmax = .data$upper
    ),
    height = 0.16,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(
    size = 3.6
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(
      Outcome
    ),
    ncol = 2,
    scales = "free_x"
  ) +
  ggplot2::scale_colour_manual(
    values = contrast_colours
  ) +
  ggplot2::labs(
    x = "UNSTANDARDIZED ESTIMATE WITH 95% CI",
    y = NULL
  ) +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_text(
      size = if (
        classification_variant == "mo"
      ) {
        9.2
      } else {
        10.5
      },
      colour = "black"
    ),
    panel.spacing = grid::unit(
      1.3,
      "cm"
    )
  )

figure_2_paths <- save_figure(
  plot = figure_2,
  stem = "Figure_2_M22_class_contrasts_forest",
  width = if (
    classification_variant == "mo"
  ) {
    12.5
  } else {
    11
  },
  height = if (
    classification_variant == "mo"
  ) {
    8.0
  } else {
    7.2
  },
  output_dir = figures_manuscript_dir
)


#-----------------------------------------------------------------------
##### FIGURE A1: PRI ASSOCIATIONS WITH BASELINE OUTCOMES #####
#-----------------------------------------------------------------------

prs_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Baseline externalizing", "EXT2 ON",
  "Baseline emotional problems", "EMO2 ON"
)

prs_coefficients <- purrr::pmap_dfr(
  prs_key,
  function(
    Outcome,
    Header
  ) {

    estimate <- extract_regression(
      m24a_parameters,
      Header,
      "PRS_EAU"
    )

    tibble::tibble(
      Outcome = Outcome,
      beta = estimate$estimate,
      se = estimate$se,
      p = estimate$p
    )
  }
)

prs_plot_data <- tidyr::crossing(
  prs_coefficients,
  PRS = seq(
    -2,
    2,
    by = 0.05
  )
) |>
  dplyr::mutate(
    Predicted_difference =
      .data$beta * .data$PRS,
    lower_raw =
      (.data$beta - 1.96 * .data$se) *
      .data$PRS,
    upper_raw =
      (.data$beta + 1.96 * .data$se) *
      .data$PRS,
    lower = pmin(
      .data$lower_raw,
      .data$upper_raw
    ),
    upper = pmax(
      .data$lower_raw,
      .data$upper_raw
    )
  )

figure_a1 <- ggplot2::ggplot(
  prs_plot_data,
  ggplot2::aes(
    x = .data$PRS,
    y = .data$Predicted_difference
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.55,
    colour = "grey55"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$lower,
      ymax = .data$upper
    ),
    alpha = 0.18,
    fill = "#355C9A"
  ) +
  ggplot2::geom_line(
    linewidth = 1.50,
    colour = "#355C9A"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(
      Outcome
    ),
    nrow = 1,
    scales = "fixed"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(
      -2,
      -1,
      0,
      1,
      2
    ),
    labels = c(
      "-2 SD",
      "-1 SD",
      "MEAN",
      "+1 SD",
      "+2 SD"
    )
  ) +
  ggplot2::labs(
    x = "EUROPEAN-ANCESTRY DEPRESSION PRI",
    y = "EXPECTED DIFFERENCE IN LATENT SCORE"
  ) +
  base_theme +
  ggplot2::theme(
    panel.spacing = grid::unit(
      1.5,
      "cm"
    )
  )

figure_a1_paths <- save_figure(
  plot = figure_a1,
  stem = "Figure_A1_PRS_baseline_associations",
  width = 10.5,
  height = 5.4,
  output_dir = figures_appendix_dir
)


#-----------------------------------------------------------------------
##### FIGURE A2: HCC ASSOCIATIONS WITH LATENT CHANGE #####
#-----------------------------------------------------------------------

hcc_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Change in externalizing", "DEXT ON",
  "Change in emotional problems", "DEMO ON"
)

hcc_coefficients <- purrr::pmap_dfr(
  hcc_key,
  function(
    Outcome,
    Header
  ) {

    estimate <- extract_regression(
      m25_parameters,
      Header,
      "C2P1_Z"
    )

    tibble::tibble(
      Outcome = Outcome,
      beta = estimate$estimate,
      se = estimate$se,
      p = estimate$p
    )
  }
)

hcc_plot_data <- tidyr::crossing(
  hcc_coefficients,
  HCC = seq(
    -2,
    2,
    by = 0.05
  )
) |>
  dplyr::mutate(
    Predicted_difference =
      .data$beta * .data$HCC,
    lower_raw =
      (.data$beta - 1.96 * .data$se) *
      .data$HCC,
    upper_raw =
      (.data$beta + 1.96 * .data$se) *
      .data$HCC,
    lower = pmin(
      .data$lower_raw,
      .data$upper_raw
    ),
    upper = pmax(
      .data$lower_raw,
      .data$upper_raw
    )
  )

figure_a2 <- ggplot2::ggplot(
  hcc_plot_data,
  ggplot2::aes(
    x = .data$HCC,
    y = .data$Predicted_difference
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth = 0.55,
    colour = "grey55"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$lower,
      ymax = .data$upper
    ),
    alpha = 0.18,
    fill = "#8C510A"
  ) +
  ggplot2::geom_line(
    linewidth = 1.50,
    colour = "#8C510A"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(
      Outcome
    ),
    nrow = 1,
    scales = "fixed"
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(
      -2,
      -1,
      0,
      1,
      2
    ),
    labels = c(
      "-2 SD",
      "-1 SD",
      "MEAN",
      "+1 SD",
      "+2 SD"
    )
  ) +
  ggplot2::labs(
    x = "PROXIMAL-SEGMENT HAIR CORTISOL",
    y = "EXPECTED DIFFERENCE IN LATENT CHANGE"
  ) +
  base_theme +
  ggplot2::theme(
    panel.spacing = grid::unit(
      1.5,
      "cm"
    )
  )

figure_a2_paths <- save_figure(
  plot = figure_a2,
  stem = "Figure_A2_HCC_change_associations",
  width = 10.5,
  height = 5.4,
  output_dir = figures_appendix_dir
)


#-----------------------------------------------------------------------
##### DOCUMENT FILES USED #####
#-----------------------------------------------------------------------

files_used <- tibble::tibble(
  classification_variant =
    classification_variant,
  model = c(
    "M22",
    "M24a",
    "M25"
  ),
  output_file = c(
    m22_out,
    m24a_out,
    m25_out
  )
)

files_used_path <- file.path(
  figures_manuscript_dir,
  paste0(
    "Figure_model_files_used",
    figure_suffix,
    ".csv"
  )
)

utils::write.csv(
  files_used,
  files_used_path,
  row.names = FALSE
)


#-----------------------------------------------------------------------
##### DISPLAY AND REPORT OUTPUTS #####
#-----------------------------------------------------------------------

print(
  figure_1
)

print(
  figure_2
)

print(
  figure_a1
)

print(
  figure_a2
)

cat(
  "\nFigures created successfully.",
  "\nClassification variant: ",
  classification_variant,
  "\n",
  "\nMplus outputs used:",
  "\nM22: ", m22_out,
  "\nM24a: ", m24a_out,
  "\nM25: ", m25_out,
  "\n",
  "\nFigure 1:",
  "\n", paste(
    figure_1_paths,
    collapse = "\n"
  ),
  "\n",
  "\nFigure 2:",
  "\n", paste(
    figure_2_paths,
    collapse = "\n"
  ),
  "\n",
  "\nFigure A1:",
  "\n", paste(
    figure_a1_paths,
    collapse = "\n"
  ),
  "\n",
  "\nFigure A2:",
  "\n", paste(
    figure_a2_paths,
    collapse = "\n"
  ),
  "\n",
  "\nFile documentation:",
  "\n", files_used_path,
  "\n",
  sep = ""
)

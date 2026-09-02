#-----------------------------------------------------------------------
##### CREATE FINAL FIGURES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R"
)

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_helper_functions.R"
)


required_packages <- c(
  "MplusAutomation",
  "dplyr",
  "tibble",
  "ggplot2",
  "ggtext"
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

#-------------------------------------------------------------------------
##### FIGURE X: MALTREATMENT BURDEN TRAJECTORY CLASSES #####
#-------------------------------------------------------------------------

figure_number <- "X"


##### READ M18 OUTPUT ####

m18_output_file <- file.path(
  mplus_input_dir,
  "18_mt_burden_quadratic_3class.out"
)

if (!file.exists(m18_output_file)) {
  stop(
    "The M18 output file is missing:\n",
    m18_output_file
  )
}

m18_output_lines <- readLines(
  m18_output_file,
  warn = FALSE
)

m18_growth_parameter_indices <-
  extract_growth_mean_parameter_indices(
    m18_output_lines
  )

m18_tech3_covariance_matrix <-
  extract_tech3_covariance_matrix(
    m18_output_lines
  )

m18_model <- MplusAutomation::readModels(
  m18_output_file,
  quiet = TRUE
)

m18_parameters <-
  m18_model$parameters$unstandardized

if (
  is.null(m18_parameters) ||
  nrow(m18_parameters) == 0
) {
  stop(
    "No unstandardized parameters could be read from M18."
  )
}


##### STANDARDIZE PARAMETER IDENTIFIERS ####

m18_parameters$.parameter <- toupper(
  trimws(
    as.character(
      m18_parameters$param
    )
  )
)

m18_parameters$.header <- gsub(
  "[^A-Z0-9]+",
  "",
  toupper(
    trimws(
      as.character(
        m18_parameters$paramHeader
      )
    )
  )
)


##### IDENTIFY LATENT-CLASS COLUMN ####

latent_class_column <- names(
  m18_parameters
)[
  gsub(
    "[^A-Z0-9]+",
    "",
    toupper(
      names(
        m18_parameters
      )
    )
  ) == "LATENTCLASS"
]

if (length(latent_class_column) != 1) {
  stop(
    paste(
      "The latent-class column could not be identified",
      "uniquely in the M18 parameters."
    )
  )
}

m18_parameters$.mplus_class <- as.integer(
  gsub(
    "[^0-9]+",
    "",
    as.character(
      m18_parameters[[latent_class_column]]
    )
  )
)


##### EXTRACT CLASS-SPECIFIC GROWTH MEANS ####

m18_growth_mean_rows <- m18_parameters |>
  dplyr::filter(
    .header == "MEANS",
    .parameter %in% c(
      "BUR_I",
      "BUR_S",
      "BUR_Q"
    ),
    .mplus_class %in% 1:3
  )

if (
  nrow(m18_growth_mean_rows) != 9 ||
  any(
    table(
      m18_growth_mean_rows$.mplus_class,
      m18_growth_mean_rows$.parameter
    ) != 1
  )
) {
  stop(
    paste(
      "The nine class-specific growth-factor means",
      "could not be identified uniquely in M18."
    )
  )
}

m18_growth_mean_se_check <- m18_growth_mean_rows |>
  dplyr::transmute(
    .mplus_class,
    .parameter,
    reported_se = as.numeric(se)
  ) |>
  dplyr::left_join(
    m18_growth_parameter_indices,
    by = c(
      ".mplus_class",
      ".parameter"
    )
  ) |>
  dplyr::mutate(
    tech3_se = sqrt(
      diag(
        m18_tech3_covariance_matrix
      )[tech3_index]
    )
  )

if (
  any(!is.finite(m18_growth_mean_se_check$tech3_se)) ||
  any(
    abs(
      m18_growth_mean_se_check$reported_se -
      m18_growth_mean_se_check$tech3_se
    ) > 0.002
  )
) {
  stop(
    paste(
      "The TECH1-to-TECH3 parameter mapping failed its standard-error check.",
      "The M18 output structure may have changed."
    )
  )
}

m18_growth_means <- m18_growth_mean_rows |>
  dplyr::group_by(
    .mplus_class
  ) |>
  dplyr::summarise(
    bur_i = as.numeric(
      est[
        .parameter == "BUR_I"
      ]
    ),
    bur_s = as.numeric(
      est[
        .parameter == "BUR_S"
      ]
    ),
    bur_q = as.numeric(
      est[
        .parameter == "BUR_Q"
      ]
    ),
    .groups = "drop"
  )


##### DEFINE CLASS LABELS AND SIZES ####

m18_class_lookup <- tibble::tibble(
  .mplus_class = 1:3,
  
  class_name = c(
    "Moderate/early-increasing burden",
    "Elevated/declining burden",
    "High/rebound burden"
  ),
  
  class_n = c(
    223L,
    42L,
    38L
  ),
  
  class_percent = c(
    73.6,
    13.9,
    12.5
  )
) |>
  dplyr::mutate(
    class_label = paste0(
      class_name,
      " (n = ",
      class_n,
      "; ",
      formatC(
        class_percent,
        format = "f",
        digits = 1
      ),
      "%)"
    )
  )


##### EXTRACT DEVELOPMENTAL TIME SCORES ####

developmental_period_lookup <- tibble::tibble(
  .parameter = c(
    "ZIND_SA",
    "ZIND_KK",
    "ZIND_VSA",
    "ZIND_FSZ",
    "ZIND_SSZ",
    "ZIND_JA",
    "ZIND_JEA"
  ),
  
  period = c(
    "SA",
    "KK",
    "VSA",
    "FSZ",
    "SSZ",
    "JA",
    "JEA"
  ),
  
  period_order = seq_len(
    7
  )
)

m18_time_score_rows <- m18_parameters |>
  dplyr::mutate(
    .header_clean = gsub(
      "[^A-Z0-9]",
      "",
      toupper(
        .header
      )
    ),
    est_numeric = suppressWarnings(
      as.numeric(
        est
      )
    )
  ) |>
  dplyr::filter(
    .header_clean == "BURS",
    .parameter %in%
      developmental_period_lookup$.parameter,
    is.finite(
      est_numeric
    )
  ) |>
  dplyr::group_by(
    .parameter
  ) |>
  dplyr::summarise(
    time_score = dplyr::first(
      est_numeric
    ),
    number_of_values = dplyr::n_distinct(
      est_numeric
    ),
    .groups = "drop"
  )

developmental_period_data <- developmental_period_lookup |>
  dplyr::left_join(
    m18_time_score_rows,
    by = ".parameter"
  ) |>
  dplyr::arrange(
    period_order
  )

if (
  any(
    !is.finite(
      developmental_period_data$time_score
    )
  ) ||
  any(
    developmental_period_data$number_of_values != 1L
  )
) {
  stop(
    paste(
      "At least one developmental time score is missing",
      "or was not identified uniquely."
    )
  )
}


##### CALCULATE MODEL-IMPLIED TRAJECTORIES AND 95% CIs ####

trajectory_grid_points_m18 <- 81L

minimum_time_score <- min(
  developmental_period_data$time_score
)

maximum_time_score <- max(
  developmental_period_data$time_score
)

calculate_m18_trajectory_values <- function(
    time_scores
) {
  trajectory_parts <- lapply(
    m18_growth_means$.mplus_class,
    function(class_number) {
      class_growth_means <- m18_growth_means |>
        dplyr::filter(
          .mplus_class == class_number
        )
      
      class_parameter_indices <-
        m18_growth_parameter_indices |>
        dplyr::filter(
          .mplus_class == class_number
        ) |>
        dplyr::mutate(
          parameter_order = match(
            .parameter,
            c(
              "BUR_I",
              "BUR_S",
              "BUR_Q"
            )
          )
        ) |>
        dplyr::arrange(
          parameter_order
        )
      
      if (
        nrow(class_growth_means) != 1L ||
        nrow(class_parameter_indices) != 3L ||
        any(!is.finite(class_parameter_indices$parameter_order))
      ) {
        stop(
          "Growth means or TECH3 indices are incomplete for class ",
          class_number,
          "."
        )
      }
      
      beta <- c(
        class_growth_means$bur_i,
        class_growth_means$bur_s,
        class_growth_means$bur_q
      )
      
      design_matrix <- cbind(
        1,
        time_scores,
        time_scores^2
      )
      
      class_covariance_matrix <-
        m18_tech3_covariance_matrix[
          class_parameter_indices$tech3_index,
          class_parameter_indices$tech3_index,
          drop = FALSE
        ]
      
      trajectory_variance <- rowSums(
        (
          design_matrix %*%
            class_covariance_matrix
        ) * design_matrix
      )
      
      if (any(trajectory_variance < -1e-10)) {
        stop(
          "A negative Delta-method trajectory variance was obtained for class ",
          class_number,
          "."
        )
      }
      
      trajectory_variance <- pmax(
        trajectory_variance,
        0
      )
      
      trajectory_se <- sqrt(
        trajectory_variance
      )
      
      trajectory_estimate <- as.vector(
        design_matrix %*% beta
      )
      
      tibble::tibble(
        .mplus_class = class_number,
        time_score = time_scores,
        estimate = trajectory_estimate,
        se = trajectory_se,
        ci_lower = trajectory_estimate -
          stats::qnorm(0.975) * trajectory_se,
        ci_upper = trajectory_estimate +
          stats::qnorm(0.975) * trajectory_se
      )
    }
  )
  
  dplyr::bind_rows(
    trajectory_parts
  )
}


m18_trajectory_grid <- tibble::tibble(
  time_score = seq(
    from = minimum_time_score,
    to = maximum_time_score,
    length.out = trajectory_grid_points_m18
  )
)

m18_trajectory_data <-
  calculate_m18_trajectory_values(
    m18_trajectory_grid$time_score
  ) |>
  dplyr::left_join(
    m18_class_lookup,
    by = ".mplus_class"
  ) |>
  dplyr::mutate(
    class_label = factor(
      class_label,
      levels =
        m18_class_lookup$class_label
    )
  ) |>
  dplyr::arrange(
    class_label,
    time_score
  )

if (
  nrow(m18_trajectory_data) !=
  3L * trajectory_grid_points_m18 ||
  any(
    table(
      m18_trajectory_data$.mplus_class
    ) != trajectory_grid_points_m18
  )
) {
  stop(
    "The M18 trajectory grid could not be constructed correctly."
  )
}


##### CALCULATE ESTIMATES AT THE SEVEN PERIOD MIDPOINTS ####

m18_period_estimates <-
  calculate_m18_trajectory_values(
    developmental_period_data$time_score
  ) |>
  dplyr::left_join(
    developmental_period_data,
    by = "time_score"
  ) |>
  dplyr::left_join(
    m18_class_lookup,
    by = ".mplus_class"
  ) |>
  dplyr::mutate(
    class_label = factor(
      class_label,
      levels =
        m18_class_lookup$class_label
    )
  ) |>
  dplyr::arrange(
    class_label,
    period_order
  )


##### CHECK PLOTTED VALUES ####

m18_period_estimates |>
  dplyr::select(
    class_name,
    period,
    time_score,
    estimate,
    se,
    ci_lower,
    ci_upper
  ) |>
  print(
    n = Inf
  )


##### DEFINE FIGURE COLORS AND LINE TYPES ####

m18_class_colors <- stats::setNames(
  c(
    "#0072B2",
    "#009E73",
    "#D55E00"
  ),
  m18_class_lookup$class_label
)

m18_class_linetypes <- stats::setNames(
  rep(
    "solid",
    nrow(m18_class_lookup)
  ),
  m18_class_lookup$class_label
)


##### DEFINE CLASS ORDER AND LEGEND LABELS ####

m18_class_order <- c(
  "Moderate/early-increasing burden (n = 223; 73.6%)",
  "Elevated/declining burden (n = 42; 13.9%)",
  "High/rebound burden (n = 38; 12.5%)"
)

m18_class_legend_labels <- stats::setNames(
  parse(
    text = c(
      '"Moderate/early-increasing burden"~"("*italic(n)~"="~223*"; 73.6%)"',
      '"Elevated/declining burden"~"("*italic(n)~"="~42*"; 13.9%)"',
      '"High/rebound burden"~"("*italic(n)~"="~38*"; 12.5%)"'
    )
  ),
  m18_class_order
)

m18_figure_note <- stringr::str_wrap(
  paste(
    "Note. Maltreatment burden scores are composites of standardized",
    "indicators of subtype count, frequency, and severity.",
    "Negative values indicate burden below the mean of the reference",
    "sample used for standardization and do not represent negative",
    "maltreatment exposure. Shaded areas represent pointwise 95%",
    "confidence intervals for the estimated class-specific mean trajectories."
  ),
  width = 135
)

##### DEFINE DEVELOPMENTAL PERIOD LABELS ####

developmental_period_labels <- c(
  "Infancy",
  "Toddlerhood",
  "Preschool age",
  "Early school age",
  "Late school age",
  "Adolescence",
  "Young adulthood"
)


##### VERIFY PLOT DEFINITIONS ####

stopifnot(
  identical(
    m18_class_order,
    names(m18_class_colors)
  ),
  identical(
    m18_class_order,
    names(m18_class_linetypes)
  ),
  sum(m18_class_lookup$class_n) == 303L,
  abs(
    sum(m18_class_lookup$class_percent) - 100
  ) < 0.1,
  all(
    m18_class_order %in%
      unique(
        as.character(
          m18_trajectory_data$class_label
        )
      )
  ),
  length(developmental_period_labels) ==
    nrow(developmental_period_data)
)


##### CREATE FIGURE ####

m18_figure <- ggplot2::ggplot(
  m18_trajectory_data,
  ggplot2::aes(
    x = time_score,
    y = estimate,
    color = class_label,
    fill = class_label,
    linetype = class_label,
    group = class_label
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
    alpha = 0.14,
    color = NA,
    linetype = 0,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    linewidth = 1.15,
    show.legend = TRUE
  ) +
  ggplot2::geom_point(
    data = m18_period_estimates,
    mapping = ggplot2::aes(
      x = time_score,
      y = estimate,
      color = class_label,
      group = class_label
    ),
    inherit.aes = FALSE,
    size = 2.7,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = developmental_period_data$time_score,
    labels = developmental_period_labels,
    expand = ggplot2::expansion(
      mult = c(
        0.025,
        0.025
      )
    ),
    guide = ggplot2::guide_axis(
      angle = 35
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      -2,
      NA
    ),
    breaks = seq(
      -2,
      10,
      by = 2
    ),
    expand = ggplot2::expansion(
      mult = c(
        0,
        0.06
      )
    )
  ) +
  ggplot2::scale_color_manual(
    values = m18_class_colors,
    breaks = m18_class_order,
    labels = unname(
      m18_class_legend_labels[
        m18_class_order
      ]
    ),
    drop = FALSE
  ) +
  ggplot2::scale_fill_manual(
    values = m18_class_colors,
    breaks = m18_class_order,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::scale_linetype_manual(
    values = m18_class_linetypes,
    breaks = m18_class_order,
    labels = unname(
      m18_class_legend_labels[
        m18_class_order
      ]
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "DEVELOPMENTAL PERIOD",
    y = "ESTIMATED MALTREATMENT BURDEN",
    color = NULL,
    fill = NULL,
    linetype = NULL,
    caption = m18_figure_note
  ) +
  ggplot2::guides(
    fill = "none",
    color = ggplot2::guide_legend(
      override.aes = list(
        linewidth = 1.15,
        alpha = 1
      )
    ),
    linetype = ggplot2::guide_legend(
      override.aes = list(
        linewidth = 1.15,
        alpha = 1
      )
    )
  ) +
  ggplot2::theme_classic(
    base_size = 11,
    base_family = "Times New Roman"
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      face = "bold",
      size = 9,
      hjust = 1,
      vjust = 1
    ),
    axis.title.x = ggplot2::element_text(
      face = "bold",
      margin = ggplot2::margin(
        t = 8
      )
    ),
    axis.title.y = ggplot2::element_text(
      face = "bold",
      margin = ggplot2::margin(
        r = 8
      )
    ),
    legend.position = "inside",
    legend.position.inside = c(
      0.62,
      0.88
    ),
    legend.justification = c(
      0.5,
      0.5
    ),
    legend.text = ggplot2::element_text(
      size = 8.5
    ),
    legend.background = ggplot2::element_rect(
      fill = scales::alpha(
        "white",
        0.85
      ),
      color = NA
    ),
    legend.key.width = grid::unit(
      25,
      "pt"
    ),
    plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9,
      face = "plain",
      lineheight = 1,
      margin = ggplot2::margin(
        t = 10
      )
    ),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 12,
      b = 8,
      l = 8
    )
  )


##### DISPLAY FIGURE ####

if (interactive()) {
  print(
    m18_figure
  )
}


##### SAVE FIGURE ####

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
    "_maltreatment_burden_trajectory_classes.tiff"
  )
)

ggplot2::ggsave(
  filename = figure_output_file,
  plot = m18_figure,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

message(
  "Saved Figure ",
  figure_number,
  ": ",
  figure_output_file
)

#-------------------------------------------------------------------------
##### FIGURE 1: M20 CLASS-SPECIFIC PSYCHOPATHOLOGY TRAJECTORIES #####
#-------------------------------------------------------------------------

##### READ M20 OUTPUT #####

m20_output_file <- file.path(
  mplus_input_dir,
  "20_sdq_lcs_class_effects_mo.out"
)

if (!file.exists(m20_output_file)) {
  stop(
    "The M20 output file is missing:\n",
    m20_output_file
  )
}

m20_model <- MplusAutomation::readModels(
  m20_output_file,
  quiet = TRUE
)

if (
  length(m20_model$errors) > 0L
) {
  stop(
    "Mplus errors were found in M20:\n",
    paste(
      unlist(m20_model$errors),
      collapse = "\n"
    )
  )
}

m20_parameters <- tibble::as_tibble(
  m20_model$parameters$unstandardized
) |>
  dplyr::mutate(
    header_clean = toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(
          as.character(.data$paramHeader)
        )
      )
    ),
    parameter_clean = toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(
          as.character(.data$param)
        )
      )
    )
  )


##### DEFINE PARAMETER EXTRACTION #####

extract_m20_parameter <- function(
    header,
    parameter
) {
  
  target_header <- toupper(
    gsub(
      "[^A-Za-z0-9]+",
      "",
      header
    )
  )
  
  target_parameter <- toupper(
    gsub(
      "[^A-Za-z0-9]+",
      "",
      parameter
    )
  )
  
  parameter_row <- m20_parameters |>
    dplyr::filter(
      .data$header_clean == .env$target_header,
      .data$parameter_clean == .env$target_parameter
    )
  
  if (nrow(parameter_row) != 1L) {
    stop(
      "Expected exactly one M20 parameter for ",
      header,
      " / ",
      parameter,
      "; found ",
      nrow(parameter_row),
      "."
    )
  }
  
  as.numeric(
    parameter_row$est[[1L]]
  )
}


extract_m20_new_parameter <- function(
    parameter
) {
  
  target_parameter <- toupper(
    gsub(
      "[^A-Za-z0-9]+",
      "",
      parameter
    )
  )
  
  parameter_row <- m20_parameters |>
    dplyr::filter(
      grepl(
        "NEW|ADDITIONAL",
        .data$header_clean
      ),
      .data$parameter_clean ==
        .env$target_parameter
    )
  
  if (nrow(parameter_row) != 1L) {
    stop(
      "Expected exactly one M20 MODEL CONSTRAINT parameter for ",
      parameter,
      "; found ",
      nrow(parameter_row),
      "."
    )
  }
  
  estimate <- as.numeric(
    parameter_row$est[[1L]]
  )
  
  standard_error <- as.numeric(
    parameter_row$se[[1L]]
  )
  
  tibble::tibble(
    Estimate = estimate,
    SE = standard_error,
    CI_lower = estimate -
      stats::qnorm(0.975) * standard_error,
    CI_upper = estimate +
      stats::qnorm(0.975) * standard_error
  )
}


##### DEFINE CLASSES #####

m20_class_lookup <- tibble::tibble(
  Class_number = 1:4,
  
  Class = c(
    "Non-maltreated",
    "Moderate/early-increasing burden",
    "Elevated/declining burden",
    "High/rebound burden"
  ),
  
  Class_n = c(
    281L,
    223L,
    42L,
    38L
  )
)

m20_class_colours <- stats::setNames(
  c(
    "#333333",
    "#0072B2",
    "#009E73",
    "#D55E00"
  ),
  m20_class_lookup$Class
)

m20_class_linetypes <- stats::setNames(
  c(
    "solid",
    "solid",
    "solid",
    "solid"
  ),
  m20_class_lookup$Class
)

m20_class_shapes <- stats::setNames(
  c(
    16,
    17,
    15,
    18
  ),
  m20_class_lookup$Class
)


##### EXTRACT CLASS-SPECIFIC LATENT SCORES #####

m20_trajectory_key <- tidyr::crossing(
  Outcome = c(
    "Externalizing problems",
    "Emotional problems"
  ),
  Class_number = 1:4,
  Assessment = c(
    "T2",
    "T5"
  )
) |>
  dplyr::left_join(
    m20_class_lookup,
    by = "Class_number"
  ) |>
  dplyr::mutate(
    Parameter = dplyr::case_when(
      .data$Outcome ==
        "Externalizing problems" &
        .data$Assessment == "T2" ~
        paste0(
          "EX2_C",
          .data$Class_number
        ),
      
      .data$Outcome ==
        "Externalizing problems" &
        .data$Assessment == "T5" ~
        paste0(
          "EX5_C",
          .data$Class_number
        ),
      
      .data$Outcome ==
        "Emotional problems" &
        .data$Assessment == "T2" ~
        paste0(
          "EM2_C",
          .data$Class_number
        ),
      
      .data$Outcome ==
        "Emotional problems" &
        .data$Assessment == "T5" ~
        paste0(
          "EM5_C",
          .data$Class_number
        )
    )
  )


##### PREPARE FIGURE DATA #####

m20_trajectory_data <- purrr::pmap_dfr(
  m20_trajectory_key,
  
  function(
    Outcome,
    Class_number,
    Assessment,
    Class,
    Class_n,
    Parameter
  ) {
    
    parameter_result <-
      extract_m20_new_parameter(
        Parameter
      )
    
    tibble::tibble(
      Outcome = Outcome,
      Class_number = Class_number,
      Class = Class,
      Class_n = Class_n,
      Assessment = Assessment,
      Parameter = Parameter,
      Estimate = parameter_result$Estimate,
      SE = parameter_result$SE,
      CI_lower = parameter_result$CI_lower,
      CI_upper = parameter_result$CI_upper
    )
  }
) |>
  dplyr::mutate(
    Assessment = factor(
      .data$Assessment,
      levels = c(
        "T2",
        "T5"
      )
    ),
    
    Assessment_numeric = dplyr::if_else(
      as.character(.data$Assessment) == "T2",
      1,
      2
    ),
    
    Class = factor(
      .data$Class,
      levels = m20_class_lookup$Class
    ),
    
    Outcome = factor(
      .data$Outcome,
      levels = c(
        "Externalizing problems",
        "Emotional problems"
      )
    )
  ) |>
  dplyr::arrange(
    .data$Outcome,
    .data$Class_number,
    .data$Assessment
  )

m20_trajectory_labels <- m20_trajectory_data |>
  dplyr::filter(
    .data$Assessment == "T5"
  ) |>
  dplyr::mutate(
    label = dplyr::case_when(
      as.character(.data$Class) ==
        "Moderate/early-increasing burden" ~
        "Moderate/early-\nincreasing burden",
      
      as.character(.data$Class) ==
        "Elevated/declining burden" ~
        "Elevated/declining\nburden",
      
      as.character(.data$Class) ==
        "High/rebound burden" ~
        "High/rebound\nburden",
      
      TRUE ~
        as.character(.data$Class)
    )
  )

m20_y_min <- floor(
  min(
    m20_trajectory_data$CI_lower,
    na.rm = TRUE
  ) * 2
) / 2

m20_y_max <- ceiling(
  max(
    m20_trajectory_data$CI_upper,
    na.rm = TRUE
  ) * 2
) / 2

##### CREATE FIGURE NOTE ######

m20_figure_note_text <- paste(
  "Note.",
  "Shaded areas connect the pointwise 95% confidence intervals",
  "at T2 and T5 based on delta-method standard errors.",
  "The shading between assessments is shown for visual presentation",
  "and does not represent a continuously estimated trajectory.",
  "Follow-up latent scores were calculated as the sum of the",
  "estimated T2 latent level and latent change.",
  "The T2 latent means of the non-maltreated reference group",
  "were fixed to zero for model identification."
)

m20_figure_note <- paste(
  strwrap(
    m20_figure_note_text,
    width = 200
  ),
  collapse = "\n"
)


##### CREATE FIGURE #####

figure_1_m20 <- ggplot2::ggplot(
  m20_trajectory_data,
  ggplot2::aes(
    x = .data$Assessment_numeric,
    y = .data$Estimate,
    group = .data$Class,
    colour = .data$Class
  )
) +
  ggplot2::geom_segment(
    x = 1,
    xend = 2.05,
    y = 0,
    yend = 0,
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.50,
    colour = "grey60"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = .data$CI_lower,
      ymax = .data$CI_upper,
      fill = .data$Class
    ),
    alpha = 0.12,
    colour = NA,
    show.legend = FALSE
  ) +
  ggplot2::geom_line(
    linewidth = 1.50,
    linetype = "solid",
    lineend = "round"
  ) +
  ggplot2::geom_point(
    shape = 16,
    size = 3.8
  ) +
  ggrepel::geom_label_repel(
    data = m20_trajectory_labels,
    ggplot2::aes(
      label = .data$label
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 0.18,
    size = 3.1,
    fontface = "bold",
    family = "Arial",
    fill = "white",
    label.size = NA,
    label.padding = grid::unit(
      0.10,
      "lines"
    ),
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
        "Externalizing problems" =
          "EXTERNALIZING PROBLEMS",
        "Emotional problems" =
          "EMOTIONAL PROBLEMS"
      )
    )
  ) +
  ggplot2::scale_colour_manual(
    values = m20_class_colours
  ) +
  ggplot2::scale_fill_manual(
    values = m20_class_colours
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(
      1,
      2
    ),
    labels = c(
      "BASELINE (T2)",
      "FOLLOW-UP (T5)"
    ),
    expand = ggplot2::expansion(
      add = c(
        0.10,
        0.78
      )
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(
      m20_y_min,
      m20_y_max
    ),
    breaks = seq(
      m20_y_min,
      m20_y_max,
      by = 1
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
    y = "UNADJUSTED LATENT SCORE",
    caption = m20_figure_note
  ) +
  ggplot2::theme_classic(
    base_size = 11,
    base_family = "Arial"
  ) +
  ggplot2::theme(
    legend.position = "none",
    
    strip.background =
      ggplot2::element_blank(),
    
    strip.text =
      ggplot2::element_text(
        face = "bold",
        size = 11
      ),
    
    plot.caption.position = "panel",
    
    plot.caption = ggplot2::element_text(
      hjust = 0,
      size = 9,
      face = "plain",
      lineheight = 1.1,
      margin = ggplot2::margin(
        t = 10
      )
    ),
    axis.text =
      ggplot2::element_text(
        colour = "black"
      ),
    
    axis.text.x =
      ggplot2::element_text(
        face = "bold"
      ),
    
    axis.title =
      ggplot2::element_text(
        face = "bold"
      ),
    
    axis.title.x =
      ggplot2::element_text(
        margin = ggplot2::margin(
          t = 8
        )
      ),
    
    axis.title.y =
      ggplot2::element_text(
        margin = ggplot2::margin(
          r = 8
        )
      ),
    
    panel.spacing = grid::unit(
      2.2,
      "cm"
    ),
    
    plot.margin = ggplot2::margin(
      t = 10,
      r = 125,
      b = 8,
      l = 8
    )
  )


##### DISPLAY FIGURE #####

if (interactive()) {
  print(
    figure_1_m20
  )
}


##### SAVE FIGURE #####

dir.create(
  man_figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_1_m20_file <- file.path(
  man_figure_dir,
  "Figure_1_M20_class_specific_trajectories.tiff"
)

ggplot2::ggsave(
  filename = figure_1_m20_file,
  plot = figure_1_m20,
  width = m20_figure_width,
  height = 5.8,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

message(
  "Saved M20 class-specific trajectory figure:\n",
  figure_1_m20_file
)

#-----------------------------------------------------------------------
#-------------------------- UPDATED UNTIL HERE -------------------------
#-----------------------------------------------------------------------

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
    linetype = .data$Class
  )
) +
  ggplot2::geom_line(
    linewidth = 1.70,
    lineend = "round"
  ) +
  ggplot2::geom_point(
    size = 16,
    stroke = 3.80
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

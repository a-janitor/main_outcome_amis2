#-----------------------------------------------------------------------
##### CREATE FINAL FIGURES: MAIN OUTCOME PAPER #####
#-----------------------------------------------------------------------

source(
  "C:/Users/keil/Documents/main_outcome_amis2/R/03_data_analysis/00_setup_standardized.R"
)

# Combined figure workflow for the MAIN OUTCOME PAPER
# Run the project setup first.

required_packages <- c(
  "MplusAutomation", "dplyr", "tidyr", "tibble",
  "purrr", "ggplot2", "ggrepel"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Install first: ", paste(missing_packages, collapse = ", "))
}

required_objects <- c("mplus_input_dir", "mplus_results_dir_maltreatment")
missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_objects) > 0) {
  stop("Run the project setup first. Missing: ", paste(missing_objects, collapse = ", "))
}

m22_out <- file.path(mplus_input_dir, "m22_lcs_with_ltc_classes_aget2_sex.out")
m24a_out <- file.path(mplus_input_dir, "m24a_lcs_classes_prs_eau.out")
m25_out <- file.path(mplus_input_dir, "m25_lcs_classes_hair_cortisol.out")

required_files <- c(m22_out, m24a_out, m25_out)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing Mplus outputs:\n", paste(missing_files, collapse = "\n"))
}

if (!exists("figures_manuscript_dir")) {
  figures_manuscript_dir <- file.path(mplus_results_dir, "08_figures", "manuscript")
}
if (!exists("figures_appendix_dir")) {
  figures_appendix_dir <- file.path(mplus_results_dir, "08_figures", "appendix")
}
dir.create(figures_manuscript_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_appendix_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- figures_manuscript_dir

clean_text <- function(x) {
  toupper(gsub("[^A-Za-z0-9]+", "", trimws(as.character(x))))
}

read_unstandardized_parameters <- function(path) {
  model <- MplusAutomation::readModels(
    path,
    what = c("parameters", "summaries", "warn_err"),
    quiet = TRUE
  )
  parameters <- tibble::as_tibble(model$parameters$unstandardized)
  names(parameters) <- tolower(names(parameters))
  parameters |>
    dplyr::mutate(
      header_clean = clean_text(.data$paramheader),
      param_clean = clean_text(.data$param)
    )
}

extract_new_parameter <- function(parameters, parameter_name) {
  matched <- parameters |>
    dplyr::filter(
      grepl("NEW|ADDITIONAL", .data$header_clean),
      .data$param_clean == clean_text(parameter_name)
    )
  if (nrow(matched) != 1) {
    stop("Expected exactly one new parameter named ", parameter_name,
         " but found ", nrow(matched), ".")
  }
  matched |>
    dplyr::slice(1) |>
    dplyr::transmute(estimate = .data$est, se = .data$se, p = .data$pval)
}

extract_regression <- function(
    parameters,
    header,
    predictor
) {
  
  matched <- parameters |>
    dplyr::filter(
      .data$header_clean == clean_text(header),
      .data$param_clean == clean_text(predictor)
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

save_figure <- function(plot, stem, width, height, output_dir = figures_manuscript_dir) {
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(png_path, plot, width = width, height = height,
                  units = "in", dpi = 600, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width, height = height,
                  units = "in", device = grDevices::cairo_pdf)
  c(PNG = png_path, PDF = pdf_path)
}

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

base_theme <- ggplot2::theme_classic(base_family = "Arial", base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(size = 14, face = "bold", colour = "black"),
    axis.text.x = ggplot2::element_text(size = 12, face = "bold", colour = "black"),
    axis.text.y = ggplot2::element_text(size = 13, colour = "black"),
    axis.line = ggplot2::element_line(colour = "black", linewidth = 0.75),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.65),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 14, face = "bold", colour = "black",
                                       margin = ggplot2::margin(b = 10)),
    panel.grid.major.y = ggplot2::element_line(colour = "grey88", linewidth = 0.40),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
  )

m22_parameters <- read_unstandardized_parameters(m22_out)
m24a_parameters <- read_unstandardized_parameters(m24a_out)
m25_parameters <- read_unstandardized_parameters(m25_out)

# Figure 1: direct-labelled class trajectories
trajectory_key <- tibble::tribble(
  ~Outcome, ~Class, ~Baseline_parameter, ~Change_parameter,
  "Externalizing problems", "Low and stable", "EX2_C1", "DEX_C1",
  "Externalizing problems", "Elevated and declining", "EX2_C2", "DEX_C2",
  "Externalizing problems", "High early burden with later rebound", "EX2_C3", "DEX_C3",
  "Emotional problems", "Low and stable", "EM2_C1", "DEM_C1",
  "Emotional problems", "Elevated and declining", "EM2_C2", "DEM_C2",
  "Emotional problems", "High early burden with later rebound", "EM2_C3", "DEM_C3"
)

trajectory_estimates <- purrr::pmap_dfr(
  trajectory_key,
  function(Outcome, Class, Baseline_parameter, Change_parameter) {
    baseline <- extract_new_parameter(m22_parameters, Baseline_parameter)
    change <- extract_new_parameter(m22_parameters, Change_parameter)
    tibble::tibble(
      Outcome = Outcome,
      Class = Class,
      T2 = baseline$estimate,
      T5 = baseline$estimate + change$estimate
    )
  }
)

trajectory_data <- trajectory_estimates |>
  tidyr::pivot_longer(c("T2", "T5"), names_to = "Time", values_to = "Estimate") |>
  dplyr::mutate(
    Time = factor(.data$Time, levels = c("T2", "T5")),
    Class = factor(.data$Class, levels = names(class_colours)),
    Outcome = factor(.data$Outcome,
                     levels = c("Externalizing problems", "Emotional problems"))
  )

trajectory_labels <- trajectory_data |>
  dplyr::filter(.data$Time == "T5") |>
  dplyr::mutate(
    label = dplyr::recode(
      as.character(.data$Class),
      "High early burden with later rebound" = "High early burden\nwith later rebound"
    )
  )

trajectory_y_min <- floor(min(trajectory_data$Estimate, na.rm = TRUE) * 2) / 2
trajectory_y_max <- ceiling(max(trajectory_data$Estimate, na.rm = TRUE) * 2) / 2

figure_1 <- ggplot2::ggplot(
  trajectory_data,
  ggplot2::aes(
    x = .data$Time,
    y = .data$Estimate,
    group = interaction(.data$Outcome, .data$Class),
    colour = .data$Class,
    linetype = .data$Class,
    shape = .data$Class
  )
) +
  ggplot2::geom_line(linewidth = 1.70, lineend = "round") +
  ggplot2::geom_point(size = 4.0, stroke = 0.90) +
  ggrepel::geom_text_repel(
    data = trajectory_labels,
    ggplot2::aes(label = .data$label),
    direction = "y",
    hjust = 0,
    nudge_x = 0.18,
    size = 3.5,
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
    ggplot2::vars(Outcome),
    nrow = 1,
    scales = "fixed",
    labeller = ggplot2::labeller(
      Outcome = c(
        "Externalizing problems" = "EXTERNALIZING",
        "Emotional problems" = "EMOTIONAL PROBLEMS"
      )
    )
  ) +
  ggplot2::scale_colour_manual(values = class_colours) +
  ggplot2::scale_linetype_manual(values = class_linetypes) +
  ggplot2::scale_shape_manual(values = class_shapes) +
  ggplot2::scale_x_discrete(
    labels = c("T2" = "BASELINE (T2)", "T5" = "FOLLOW-UP (T5)"),
    expand = ggplot2::expansion(add = c(0.10, 0.55))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(trajectory_y_min, trajectory_y_max),
    breaks = seq(trajectory_y_min, trajectory_y_max, by = 0.5),
    expand = ggplot2::expansion(mult = c(0.04, 0.06))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(x = "ASSESSMENT", y = "ADJUSTED LATENT SCORE") +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    panel.spacing = grid::unit(2.2, "cm")
  )

figure_1_paths <- save_figure(
  figure_1,
  "Figure_1_M22_direct_labelled_trajectories",
  width = 12.5,
  height = 5.8
)

# Figure 2: forest plot of adjusted class contrasts
contrast_key <- tibble::tribble(
  ~Outcome, ~Contrast, ~Header, ~Predictor,
  "Baseline externalizing", "Elevated and declining vs low and stable", "EXT2 ON", "C2",
  "Baseline externalizing", "High early burden with later rebound vs low and stable", "EXT2 ON", "C3",
  "Baseline emotional problems", "Elevated and declining vs low and stable", "EMO2 ON", "C2",
  "Baseline emotional problems", "High early burden with later rebound vs low and stable", "EMO2 ON", "C3",
  "Change in externalizing", "Elevated and declining vs low and stable", "DEXT ON", "C2",
  "Change in externalizing", "High early burden with later rebound vs low and stable", "DEXT ON", "C3",
  "Change in emotional problems", "Elevated and declining vs low and stable", "DEMO ON", "C2",
  "Change in emotional problems", "High early burden with later rebound vs low and stable", "DEMO ON", "C3"
)

forest_data <- purrr::pmap_dfr(
  contrast_key,
  function(
    Outcome,
    Contrast,
    Header,
    Predictor
  ) {
    
    matched <- m22_parameters |>
      dplyr::filter(
        .data$header_clean == clean_text(Header),
        .data$param_clean == clean_text(Predictor)
      )
    
    if (nrow(matched) != 1) {
      stop(
        "Expected exactly one parameter for ",
        Header,
        " / ",
        Predictor,
        " but found ",
        nrow(matched),
        "."
      )
    }
    
    estimate_value <- as.numeric(
      matched$est[[1]]
    )
    
    se_value <- as.numeric(
      matched$se[[1]]
    )
    
    p_value <- as.numeric(
      matched$pval[[1]]
    )
    
    tibble::tibble(
      Outcome = Outcome,
      Contrast = Contrast,
      estimate = estimate_value,
      se = se_value,
      p = p_value,
      lower = estimate_value - 1.96 * se_value,
      upper = estimate_value + 1.96 * se_value
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
      levels = c(
        "Elevated and declining vs low and stable",
        "High early burden with later rebound vs low and stable"
      )
    )
  )

contrast_colours <- c(
  "Elevated and declining vs low and stable" = "#D95F02",
  "High early burden with later rebound vs low and stable" = "#355C9A"
)

figure_2 <- ggplot2::ggplot(
  forest_data,
  ggplot2::aes(x = .data$estimate, y = .data$Contrast, colour = .data$Contrast)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                      linewidth = 0.65, colour = "grey45") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
    height = 0.16,
    linewidth = 0.85
  ) +
  ggplot2::geom_point(size = 3.6) +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), ncol = 2, scales = "free_x") +
  ggplot2::scale_colour_manual(values = contrast_colours) +
  ggplot2::labs(x = "UNSTANDARDIZED ESTIMATE WITH 95% CI", y = NULL) +
  base_theme +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_text(size = 10.5, colour = "black"),
    panel.spacing = grid::unit(1.3, "cm")
  )

figure_2_paths <- save_figure(
  figure_2,
  "Figure_2_M22_class_contrasts_forest",
  width = 11,
  height = 7.2
)

# Figure A1: continuous PRS associations with baseline outcomes
prs_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Baseline externalizing", "EXT2 ON",
  "Baseline emotional problems", "EMO2 ON"
)

prs_coefficients <- purrr::pmap_dfr(
  prs_key,
  function(Outcome, Header) {
    estimate <- extract_regression(m24a_parameters, Header, "PRS_EAU")
    tibble::tibble(Outcome = Outcome, beta = estimate$estimate,
                   se = estimate$se, p = estimate$p)
  }
)

prs_plot_data <- tidyr::crossing(
  prs_coefficients,
  PRS = seq(-2, 2, by = 0.05)
) |>
  dplyr::mutate(
    Predicted_difference = .data$beta * .data$PRS,
    lower_raw = (.data$beta - 1.96 * .data$se) * .data$PRS,
    upper_raw = (.data$beta + 1.96 * .data$se) * .data$PRS,
    lower = pmin(.data$lower_raw, .data$upper_raw),
    upper = pmax(.data$lower_raw, .data$upper_raw)
  )

figure_a1 <- ggplot2::ggplot(
  prs_plot_data,
  ggplot2::aes(x = .data$PRS, y = .data$Predicted_difference)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.55, colour = "grey55") +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    alpha = 0.18,
    fill = "#355C9A"
  ) +
  ggplot2::geom_line(linewidth = 1.50, colour = "#355C9A") +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), nrow = 1, scales = "fixed") +
  ggplot2::scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-2 SD", "-1 SD", "MEAN", "+1 SD", "+2 SD")
  ) +
  ggplot2::labs(
    x = "EUROPEAN-ANCESTRY DEPRESSION PRI",
    y = "EXPECTED DIFFERENCE IN LATENT SCORE"
  ) +
  base_theme +
  ggplot2::theme(panel.spacing = grid::unit(1.5, "cm"))

figure_a1_paths <- save_figure(
  figure_a1,
  "Figure_A1_PRS_baseline_associations",
  width = 10.5,
  height = 5.4,
  output_dir = figures_appendix_dir
)

# Figure A2: continuous HCC associations with latent change
hcc_key <- tibble::tribble(
  ~Outcome, ~Header,
  "Change in externalizing", "DEXT ON",
  "Change in emotional problems", "DEMO ON"
)

hcc_coefficients <- purrr::pmap_dfr(
  hcc_key,
  function(Outcome, Header) {
    estimate <- extract_regression(m25_parameters, Header, "C2P1_Z")
    tibble::tibble(Outcome = Outcome, beta = estimate$estimate,
                   se = estimate$se, p = estimate$p)
  }
)

hcc_plot_data <- tidyr::crossing(
  hcc_coefficients,
  HCC = seq(-2, 2, by = 0.05)
) |>
  dplyr::mutate(
    Predicted_difference = .data$beta * .data$HCC,
    lower_raw = (.data$beta - 1.96 * .data$se) * .data$HCC,
    upper_raw = (.data$beta + 1.96 * .data$se) * .data$HCC,
    lower = pmin(.data$lower_raw, .data$upper_raw),
    upper = pmax(.data$lower_raw, .data$upper_raw)
  )

figure_a2 <- ggplot2::ggplot(
  hcc_plot_data,
  ggplot2::aes(x = .data$HCC, y = .data$Predicted_difference)
) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.55, colour = "grey55") +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
    alpha = 0.18,
    fill = "#8C510A"
  ) +
  ggplot2::geom_line(linewidth = 1.50, colour = "#8C510A") +
  ggplot2::facet_wrap(ggplot2::vars(Outcome), nrow = 1, scales = "fixed") +
  ggplot2::scale_x_continuous(
    breaks = c(-2, -1, 0, 1, 2),
    labels = c("-2 SD", "-1 SD", "MEAN", "+1 SD", "+2 SD")
  ) +
  ggplot2::labs(
    x = "PROXIMAL-SEGMENT HAIR CORTISOL",
    y = "EXPECTED DIFFERENCE IN LATENT CHANGE"
  ) +
  base_theme +
  ggplot2::theme(panel.spacing = grid::unit(1.5, "cm"))

figure_a2_paths <- save_figure(
  figure_a2,
  "Figure_A2_HCC_change_associations",
  width = 10.5,
  height = 5.4,
  output_dir = figures_appendix_dir
)

print(figure_1)
print(figure_2)
print(figure_a1)
print(figure_a2)

cat(
  "\nFigures created successfully.\n\n",
  "Figure 1:\n", paste(figure_1_paths, collapse = "\n"),
  "\n\nFigure 2:\n", paste(figure_2_paths, collapse = "\n"),
  "\n\nFigure A1:\n", paste(figure_a1_paths, collapse = "\n"),
  "\n\nFigure A2:\n", paste(figure_a2_paths, collapse = "\n"),
  "\n",
  sep = ""
)

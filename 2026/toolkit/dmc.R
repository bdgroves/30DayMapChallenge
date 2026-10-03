# dmc.R — the #30DayMapChallenge 2026 house style for ggplot2 (same as toolkit/dmc.py)
#
#   source("../toolkit/dmc.R")
#   ggplot(...) + ... + theme_dmc() + dmc_labs(28, subtitle = "...", source = "USGS")
#   dmc_save(28, plot, size = "square")

dmc <- list(
  ink = "#1c1a16", parchment = "#f5f0e8", cream = "#ede8dc", white = "#fdfaf4",
  stone = "#7a7268", ash = "#6b6560", mist = "#d6d0c4", gold = "#c8922a",
  lava = "#c0392b", sage = "#6f8a6a", lake = "#3f6f8a", night = "#14171c"
)
dmc_categorical <- c(dmc$lava, dmc$lake, dmc$gold, dmc$sage, "#8a5a83", dmc$ash)
dmc_sizes <- list(square = c(8, 8), portrait = c(8, 10), wide = c(10.667, 6))

.dmc_dir <- local({
  f <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  if (is.null(f)) normalizePath("../toolkit") else dirname(f)
})

if (requireNamespace("showtext", quietly = TRUE)) {
  fonts <- file.path(.dmc_dir, "fonts")
  sysfonts::font_add("Playfair Display", file.path(fonts, "PlayfairDisplay-Black.ttf"))
  sysfonts::font_add("Libre Baskerville", file.path(fonts, "LibreBaskerville-Regular.ttf"),
                     italic = file.path(fonts, "LibreBaskerville-Italic.ttf"),
                     bold = file.path(fonts, "LibreBaskerville-Bold.ttf"))
  sysfonts::font_add("DM Mono", file.path(fonts, "DMMono-Regular.ttf"))
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
}

dmc_day <- function(day) {
  plan <- yaml::read_yaml(file.path(.dmc_dir, "..", "days.yml"))
  Filter(function(d) d$day == day, plan$days)[[1]]
}

theme_dmc <- function(base_size = 11, dark = FALSE) {
  paper <- if (dark) dmc$night else dmc$parchment
  ink <- if (dark) dmc$white else dmc$ink
  ggplot2::theme_void(base_size = base_size, base_family = "Libre Baskerville") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = paper, colour = NA),
      plot.title = ggplot2::element_text(family = "Playfair Display", size = base_size * 2.6,
                                         colour = ink, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(colour = dmc$stone, lineheight = 1.3,
                                            margin = ggplot2::margin(b = 12)),
      plot.caption = ggplot2::element_text(family = "DM Mono", size = base_size * 0.65,
                                           colour = dmc$stone, hjust = 0),
      plot.tag = ggplot2::element_text(family = "DM Mono", size = base_size * 0.8, colour = dmc$lava),
      plot.tag.position = c(0, 1.02),
      plot.title.position = "plot", plot.caption.position = "plot",
      legend.text = ggplot2::element_text(colour = ink), legend.title = ggplot2::element_text(colour = ink),
      plot.margin = ggplot2::margin(28, 24, 18, 24)
    )
}

dmc_labs <- function(day, subtitle = NULL, source = NULL, title = NULL) {
  d <- dmc_day(day)
  ggplot2::labs(
    tag = sprintf("#30DAYMAPCHALLENGE  ·  DAY %02d  ·  %s", day, toupper(d$theme)),
    title = if (is.null(title)) d$title else title,
    subtitle = subtitle,
    caption = paste0(if (!is.null(source)) paste0("DATA  ", source, "     ") else "",
                     "BROOKS GROVES  ·  BROOKSGROVES.COM")
  )
}

dmc_save <- function(day, plot, size = "square", alt = NULL, name = "map") {
  folder <- Sys.glob(file.path(.dmc_dir, "..", sprintf("day-%02d-*", day)))[1]
  dir.create(file.path(folder, "out"), showWarnings = FALSE)
  s <- dmc_sizes[[size]]
  path <- file.path(folder, "out", paste0(name, ".png"))
  ggplot2::ggsave(path, plot, width = s[1], height = s[2], dpi = 300, bg = dmc$parchment)
  if (!is.null(alt)) writeLines(alt, file.path(folder, "out", "alt.txt"))
  message("saved ", path)
  invisible(path)
}

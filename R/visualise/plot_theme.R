
#color pallete 
blue_deep <- "#14507e"; 
blue_med <- "#1f6fb2"; 
blue_light <- "#9dc3e6" 
red_main <- "#d1495b"; 
red_deep <- "#b0303f"; 
slate <- "#9aa7b4";
tint_blue <- "#e8f1f8"; 
tint_red <- "#DB7F8B"


concordance_colours <- c(Concordant = blue_med,
                         Inconclusive=slate, 
                         Discordante = red_main)

alpha_default <- 0.05
k_default <- 3
jeffreys_bf <- c(100, 30, 10, 3, 1, 1/3, 1/10, 1/30, 1/100)

theme_reanalysis <- function(base = 12) {
  theme_minimal(base_size = base) + 
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"),
          legend.position = "bottom")
}

save_fig <- function(plot, path, w = 8, h = 5.5) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(path, plot = plot, width = w, height = h, dpi = 300)
}







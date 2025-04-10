box::use(
  ggplot2[...]
)


box::use(
  app/constant[x_labels,seq_values,max_GDP]
)

#' @export
get_seq <- function() {
  seq_vals <- seq_values
  if (length(seq_vals) < 8) {
    seq_vals <- c(seq_vals, max_GDP)
  }
  return(seq_vals)
}

#' @export
seq_vals <- get_seq()


#' @export
plot_bar_chart <- function(df) {
  # Create palette for consistency with the map
  num_colors <- min(9, max(3, length(seq_vals) - 1))
  p <- ggplot(df, 
              aes(group_gdp, share, fill = group_gdp)) +
    geom_col() +
    geom_hline(yintercept = 0) +
    geom_text(
      aes(y = y_text, label = label),
      color = "black",  # Changed to a single color instead of x_labels
      size = 4,
      hjust = 0.5
    ) +
    coord_flip() +
    scale_fill_brewer(palette = "YlGnBu", direction = 1) +
    guides(fill = "none") +
    labs(
      title = "",
      x = NULL,
      y = NULL
    ) +
    theme_void() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

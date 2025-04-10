box::use(
  dplyr[left_join, mutate, group_by, summarise, ungroup, filter, select, rename, arrange, if_else],
  sf[st_drop_geometry, st_as_sf, st_centroid, st_coordinates],
  rnaturalearth[ne_states],
  stats[runif, na.omit, setNames],
  magrittr[`%>%`],
  utils[head],
  ggplot2[...],
  stats[quantile],
  RColorBrewer[brewer.pal]
)

#' @export
create_gdp_labels <- function(data, year = NULL) {
  # Filter data by year if specified
  if (!is.null(year) && "Year" %in% names(data)) {
    data_for_year <- data[data$Year == year, ]
  } else {
    data_for_year <- data
    message("Filter year dont work")
  }
  
  seq <- seq(min(data_for_year$GDP), max(data_for_year$GDP), mean(data_for_year$GDP) / 2)
  
  if (length(seq) < 8) {
    seq <- c(seq, max(data_for_year$GDP))
  }
  
  x_labels <- sapply(1:(length(seq) - 1), function(i) {
    if (i == length(seq) - 1) {
      paste0(format(round(seq[i], 0), big.mark = ","), " \u00A5", " to more")
    } else {
      paste0(format(round(seq[i], 0), big.mark = ","), " \u00A5", " to ", 
             format(round(seq[i+1], 0), big.mark = ","), " \u00A5")
    }
  })
  return(x_labels)
}

#' @export
func_toptier <- function(df, year) {
  # Create the filtered dataframe
  filtered_df <- df |> 
    filter(Year == year) |> 
    rename("Province" = "name") 
  
  total_gdp <- sum(filtered_df$GDP, na.rm = TRUE)
  
  # Create the top_tiers dataframe
  top_tiers <- filtered_df |>
    mutate(
      group_gdp = findInterval(GDP, 
                               seq(min(filtered_df$GDP, na.rm = TRUE), max(filtered_df$GDP, na.rm = TRUE), mean(filtered_df$GDP, na.rm = TRUE)/2), 
                               left.open = FALSE),
      gdp_percent = (GDP / total_gdp) * 100,
      province_label = paste0(Province, " (", round(gdp_percent, 1), "%)")
    ) |>
    filter(group_gdp >= max(group_gdp, na.rm = TRUE) - 1) |>
    arrange(desc(GDP))
  
  # Calculate centroids for top_tiers
  top_tiers <- top_tiers |>
    mutate(centroid = st_centroid(geometry),
           lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2]) |> 
    st_drop_geometry()

  # Return both dataframes in a list
  return(list(
    filtered_df = filtered_df,
    top_tiers = top_tiers
  ))
}

#' @export
func_barchart <- function(df, year) {
  filtered_df <- df |> 
    filter(Year == year)|> 
    rename("Province" = "name") 
  
  bar_chart <- filtered_df |> 
  st_drop_geometry() |> 
  mutate(
    group_gdp = findInterval(GDP, 
                             seq(min(filtered_df$GDP),max(filtered_df$GDP),mean(filtered_df$GDP)/2), 
                             left.open = FALSE),
    group_gdp = factor(group_gdp)
  ) |> 
  group_by(group_gdp) |> 
  summarise(score = sum(Population, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(share = score / sum(score) * 100) |> 
  na.omit()

  bar_chart <- bar_chart |> 
  mutate(
    y_text = if_else(share < 5, share + 3, share - 3),
    label = paste0(round(share, 1), "%")
  )
  
  return(bar_chart)
}

#' @export
plot_bar_chart <- function(df,label_gdp_group) {
  
  ggplot(df, aes(group_gdp, share, fill = group_gdp)) +  # Use group_gdp for fill (discrete)
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_text(
      aes(y = y_text, 
          label = label),
      size = 3.5,
      color = "black"
    ) +
    coord_flip() +
    scale_x_discrete(labels = label_gdp_group) +
    scale_fill_brewer(palette = "YlGnBu") +  # Now works with discrete group_gdp
    labs(
      title = "Population Share by GDP Group",
      subtitle = "Distribution by economic groups",
      caption = "Values shown as percentages",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "darkgrey"),
      plot.caption = element_text(size = 8, color = "darkgrey"),
      axis.text.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

#' @export
plot_gdp_boxplot <- function(data) {
  
  plot_data <- as.data.frame(data)
  plot_data$geometry <- NULL
  
  ylgnbu_colors <- brewer.pal(9, "YlGnBu")
  jitter_color <- ylgnbu_colors[7]  
  violin_color <- ylgnbu_colors[2]
  boxplot_fill <- ylgnbu_colors[4]  # Lighter color from the palette for jitter
  
  # Calculate percentage of provinces in top 10%
  top_threshold <- quantile(plot_data$GDP, 0.9, na.rm = TRUE)
  top_percent <- mean(plot_data$GDP >= top_threshold, na.rm = TRUE) * 100
  
  if(!is.null(plot_data$Year)) {
    subtitle_text <- sprintf("Year: %d | %.1f%% provinces have GDP above ¥%s (top 10%% threshold)", 
                             unique(plot_data$Year), top_percent, format(round(top_threshold), big.mark = ","))
  } else {
    subtitle_text <- sprintf("All Years | %.1f%% provinces have GDP above ¥%s (top 10%% threshold)", 
                             top_percent, format(round(top_threshold), big.mark = ","))
  }
  
  # Count data points below/equal to threshold and above threshold
  count_below <- sum(plot_data$GDP <= top_threshold, na.rm = TRUE)
  count_above <- sum(plot_data$GDP > top_threshold, na.rm = TRUE)
  total_count <- nrow(plot_data)
  
  count_text <- sprintf("Below threshold: %d provinces (%.1f%%) | Above threshold: %d provinces (%.1f%%)",
                        count_below, (count_below/total_count)*100,
                        count_above, (count_above/total_count)*100)
  
  p <- ggplot(plot_data, aes(x = factor(Year), y = GDP)) +
    geom_jitter(width = 0.25, alpha = 0.6, color = jitter_color, size = 2) +
    geom_violin(fill = violin_color, alpha = 0.3, trim = FALSE) +
    geom_boxplot(fill = boxplot_fill, alpha = 0.4, outlier.shape = NA) + # Hide boxplot outliers since we show the points
    geom_hline(yintercept = top_threshold, linetype = "dashed", 
               color = ylgnbu_colors[9], size = 1, alpha = 0.8) +
    # Add threshold label
    annotate("text", x = 0.6, y = top_threshold * 1.05, 
             label = "Top 10% Threshold", hjust = 0, 
             color = ylgnbu_colors[8], fontface = "bold", size = 3.5) +
    # Add count annotation for below threshold
    annotate("text", x = 1.5, y = min(plot_data$GDP, na.rm = TRUE) * 0.9,
             label = sprintf("≤ 90%%: %d provinces", count_below),
             hjust = 0, color = "black", fontface = "bold", size = 4,alpha = 0.8) +
    # Add count annotation for above threshold
    annotate("text", x = 1.5, y = max(plot_data$GDP, na.rm = TRUE) * 0.95,
             label = sprintf("10%%: %d provinces", count_above),
             hjust = 0, color = "black", fontface = "bold", size = 4,alpha = 0.8) +
    coord_flip() +  # Make it horizontal
    labs(
      title = "GDP Distribution by Year",
      subtitle = subtitle_text,
      x = NULL,
      y = "GDP (¥)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "darkgrey"),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(
      labels = scales::comma
    )
  
  return(p)
}

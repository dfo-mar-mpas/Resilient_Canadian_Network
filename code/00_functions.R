## companion functions 

weighted.median <- function(x, w, na.rm = FALSE) {
  # Input validation
  if (length(x) != length(w)) {
    stop("'x' and 'w' must have the same length")
  }
  
  # Handle NAs
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  
  # Check for negative weights
  if (any(w < 0)) {
    stop("'w' must be non-negative")
  }
  
  # Check if all weights are zero
  if (all(w == 0)) {
    return(NaN)
  }
  
  # Sort data and weights
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  # Calculate cumulative weights
  w <- w / sum(w)  # Normalize weights to sum to 1
  cum_w <- cumsum(w)
  
  # Find where cumulative weight crosses 0.5
  median_index <- min(which(cum_w >= 0.5))
  
  # If exactly 0.5, take the average of the two middle values
  if (cum_w[median_index] == 0.5 && median_index < length(x)) {
    return((x[median_index] + x[median_index + 1]) / 2)
  } else {
    return(x[median_index])
  }
}


create_all_region_radar_plots_enhanced <- function(summary_df, 
                                                   ncol = 3,
                                                   colors = c("blue", "red"),
                                                   sort_by = "vulnerability",  # "none", "vulnerability", "region"
                                                   decreasing = TRUE,
                                                   save_plot = FALSE,
                                                   file_name = NULL,
                                                   width = 14, 
                                                   height = 10) {
  # Load required packages
  if (!require(fmsb)) install.packages("fmsb")
  library(fmsb)
  library(dplyr)
  
  # Get regions and sort them if requested
  if (sort_by == "vulnerability") {
    # Sort by RCP 8.5 vulnerability by default
    region_order <- summary_df %>%
      filter(rcp == "RCP 8.5") %>%
      arrange(if(decreasing) desc(med_vuln) else med_vuln) %>%
      pull(region)
    
    # Match order with unique regions
    all_regions <- unique(summary_df$region)
    regions <- all_regions[match(region_order, all_regions)]
    
    # Add any missing regions
    regions <- c(regions, all_regions[!all_regions %in% regions])
  } else if (sort_by == "region") {
    # Sort alphabetically
    regions <- sort(unique(summary_df$region))
  } else {
    # Use original order
    regions <- unique(summary_df$region)
  }
  
  n_regions <- length(regions)
  
  # Calculate grid layout
  nrow <- ceiling(n_regions / ncol)
  
  # Calculate max value across all regions for consistent scaling
  all_values <- summary_df %>%
    select(med_expos, med_sens, med_adcap, med_vuln) %>%
    unlist()
  
  max_val <- max(all_values, na.rm = TRUE) * 1.1
  min_val <- 0
  
  # Create axis labels - FIXED
  axis_labels <- round(seq(0, max_val, length.out = 5), 2)
  
  # Create color with transparency
  rcp26_color <- adjustcolor(colors[1], alpha.f = 0.7)
  rcp85_color <- adjustcolor(colors[2], alpha.f = 0.7)
  
  # Set up plotting device
  if (save_plot) {
    if (is.null(file_name)) {
      file_name <- "all_bioregions_radar_plots.pdf"
    }
    pdf(file_name, width = width, height = height)
  } else {
    # For screen display
    if (dev.cur() == 1) {
      dev.new(width = width, height = height, noRStudioGD = TRUE)
    }
  }
  
  # Set up the plotting grid
  par(mfrow = c(nrow, ncol), mar = c(1, 1, 3, 1), oma = c(2, 0, 4, 0))
  
  # Loop through each region and create a radar chart
  for (i in 1:n_regions) {
    region_name <- regions[i]
    
    # Extract data for this region
    region_data <- summary_df %>%
      filter(region == region_name)
    
    # Get data for each RCP scenario
    rcp26_data <- region_data %>% 
      filter(rcp == "RCP 2.6") %>% 
      select(med_expos, med_sens, med_adcap, med_vuln)
    
    rcp85_data <- region_data %>% 
      filter(rcp == "RCP 8.5") %>% 
      select(med_expos, med_sens, med_adcap, med_vuln)
    
    # Check if we have data for both scenarios
    if (nrow(rcp26_data) == 0 || nrow(rcp85_data) == 0) {
      # Draw a blank plot with a message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, paste("Missing data for", region_name))
      next
    }
    
    # Create radar data matrix and convert to dataframe
    radar_data <- data.frame(
      row.names = c("max", "min", "RCP 2.6", "RCP 8.5"),
      "Exposure" = c(max_val, min_val, rcp26_data$med_expos, rcp85_data$med_expos),
      "Sensitivity" = c(max_val, min_val, rcp26_data$med_sens, rcp85_data$med_sens),
      "Adapt.Cap" = c(max_val, min_val, rcp26_data$med_adcap, rcp85_data$med_adcap),
      "Vulnerability" = c(max_val, min_val, rcp26_data$med_vuln, rcp85_data$med_vuln)
    )
    
    # Add rank if sorting by vulnerability
    title_text <- region_name
    if (sort_by == "vulnerability") {
      rank_num <- which(region_order == region_name)
      title_text <- paste0(rank_num, ". ", region_name)
    }
    
    # Create the radar chart
    radarchart(
      radar_data,
      title = title_text,
      pcol = c(rcp26_color, rcp85_color),
      pfcol = adjustcolor(c(rcp26_color, rcp85_color), alpha.f = 0.4),
      plwd = 2,
      plty = c(1, 1),
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey30",
      caxislabels = axis_labels,  # FIXED: Use predefined labels
      cglwd = 0.8,
      vlcex = 0.7,
      axistype = 1
    )
    
    # Add legend to first plot only
    if (i == 1) {
      legend(
        "topright",
        legend = c("RCP 2.6", "RCP 8.5"),
        col = c(rcp26_color, rcp85_color),
        lty = 1,
        lwd = 2,
        pch = 15,
        pt.cex = 1.2,
        bty = "n",
        cex = 0.7
      )
    }
  }
  
  # Add title to the entire figure
  mtext("Climate Vulnerability Components by Bioregion", 
        side = 3, line = 1, outer = TRUE, cex = 1.5)
  
  # Add subtitle based on sorting method
  if (sort_by == "vulnerability") {
    subtitle <- "Regions ranked by vulnerability under RCP 8.5 scenario"
    mtext(subtitle, side = 3, line = -0.5, outer = TRUE, cex = 0.9)
  }
  
  # Add explanation of components
  mtext("Components: Exposure, Sensitivity, Adaptive Capacity, and Vulnerability", 
        side = 1, line = 0, outer = TRUE, cex = 0.8)
  
  # Close plot if saving
  if (save_plot) {
    dev.off()
    message(paste("Plot saved to", file_name))
  }
}

dhsc_colour <-
  function(...) {
    dhsc_colours <-
      c(
        "DHSC teal" = "#00ad93",
        "Red" = "#cc092f",
        "Orange" = "#e57200",
        "Burgundy" = "#8b2346",
        "Sky blue" = "#34b6e4",
        "Green" = "#2eb135",
        "Yellow" = "#ecac00",
        "Fuchsia" = "#cd66cc",
        "Purple" = "#512698",
        "Dark grey" = "#616265",
        "Blue" = "#0063be"
      )
    
    dhsc_colours <- toupper(dhsc_colours)
    
    cols <- c(...)
    
    if (is.null(cols))
      return (dhsc_colours)
    
    dhsc_colours[cols]
    
  }




dhsc_palette <- function(palette = "main", ...) {
  dhsc_palettes <- list(`main` = dhsc_colour(
   1:11
  ))
  
  dhsc_palettes[[palette]]
}

dhsc_palette("main")

palette_gen <- function(palette = "main",
                        direction = 1) {
  function(n) {
    if (n > length(dhsc_palette(palette)))
      warning("Not enough colors in this palette!")
    
    else {
      all_colors <- dhsc_palette(palette)
      
      all_colors <- unname(unlist(all_colors))
      
      all_colors <-
        if (direction >= 0)
          all_colors
      else
        rev(all_colors)
      
      color_list <- all_colors[1:n]
      
    }
  }
}


scale_fill_dhsc <- function(palette = "main", direction = 1, ...){
ggplot2::discrete_scale(aesthetics = "fill",
                        scale_name = "dhsc",
                        palette_gen(palette, direction),
                        ...)
  
}



scale_colour_dhsc <- function(palette = "main", direction = 1, ...){
  ggplot2::discrete_scale(aesthetics = "colour",
                          scale_name = "dhsc",
                          palette_gen(palette, direction),
                          ...)
  
}

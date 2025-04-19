# geometric_interpolation.R
# ----------------------------------------
# Function to perform geometric interpolation of multiple attributes
# from a source polygon layer to a target polygon layer,
# weighted by the proportion of overlapping area.
#
# Particularly useful for interpolating count data between territorial
# units redrawn over time.
#
# Ensures that for each source unit the sum of weights = 1.
#
# Requirements:
#   - sf
#   - dplyr

#' Geometric interpolation of numeric attributes from one sf to another
#'
#' @param source_sf       sf object with original polygons and attributes
#' @param target_sf       sf object defining the new polygons
#' @param interp_cols     character vector of column names in source_sf to interpolate
#' @param output_names    character vector of names for the interpolated columns
#' @param source_id       name of the unique ID field in source_sf
#' @param target_id       name of the unique ID field in target_sf

geometric_interpolation <- function(
    source_sf, target_sf,
    interp_cols, output_names,
    source_id, target_id
    ) {
  # Rename ID fields 
  source_sf <- source_sf %>% rename(src = all_of(source_id))
  target_sf <- target_sf %>% rename(tgt = all_of(target_id))
  
  # Intersection and filter polygons
  inter <- st_intersection(source_sf, target_sf) %>%
    filter(st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  
  # Compute area fractions
  inter <- inter %>%
    mutate(
      src_area = st_area(source_sf)[match(src, source_sf$src)],
      int_area = st_area(geometry),
      frac     = as.numeric(int_area / src_area)
      ) %>%
    group_by(src) %>%
    mutate(frac = frac / sum(frac, na.rm = TRUE)) %>%
    ungroup()
  
  # Compute weighted sums and aggregate
  result <- inter %>%
    mutate(across(all_of(interp_cols), as.numeric)) %>%
    mutate(across(all_of(interp_cols), ~ .x * frac, .names = "w_{.col}")) %>%
    group_by(tgt) %>%
    summarise(
      across(starts_with("w_"), sum, na.rm = TRUE),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    
    rename_with(~ output_names, starts_with("w_")) %>%
    mutate(across(all_of(output_names), ~ as.integer(round(.x)))) %>%
    
    mutate(!!target_id := tgt) %>%
    select(all_of(target_id), all_of(output_names), geometry)
  
  result
  }

# Example:
# vlc_secc2015 <- st_read("data/vlc_secc2015.shp")
# vlc_secc2022 <- st_read("data/vlc_secc2022.shp")
# out <- geometric_interpolation(
#   vlc_secc2015, vlc_secc2022,
#   interp_cols = c("POPTOTAL2015"),
#   output_names = c("pop2015"),
#   source_id = "CUSEC", target_id = "CUSEC"
# )
# st_write(out, "output/interpolated.shp")
#

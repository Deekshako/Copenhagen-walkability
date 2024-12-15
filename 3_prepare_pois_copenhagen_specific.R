## #######################################################################################
##
## Process parks data from WFS URL and convert geometries to points
##
## Created for Copenhagen by Deeksha, Yichen, and Chaitali.Methodology adapted from Nathaniel Henry
## Purpose: Download and process parks data from a WFS service
##
## Note: The script fetches park data from a WFS service, processes geometries, and 
##   generates points from the park boundaries.
##
## #######################################################################################

## SET DEFAULT CONFIG PATH
DEFAULT_CONFIG_FILEPATH <- "E:/IAAC/Thesis/Henry Spatial Analysis/Copenhagen walkability/city_walkability/analysis/config.yaml"

## Setup -------------------------------------------------------------------------------->

# Load all required packages
load_packages <- c('sf', 'httr', 'dplyr', 'units', 'glue', 'yaml')
lapply(load_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
})

# Load config.yaml
if(interactive()){
  config_filepath <- DEFAULT_CONFIG_FILEPATH
  message("Using default config location: ", config_filepath)
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument(
    "--config_filepath",
    type = 'character',
    help = "Full path to the config.yaml file containing run settings"
  )
  config_filepath <- parser$parse_args(commandArgs(trailingOnly = TRUE))$config_filepath
}
config <- yaml::read_yaml(config_filepath)
settings <- config$project_settings
dl_paths <- config$download_paths

# Create all output folders
for(dir in config$directories) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Load the bounding box for this analysis
bbox_file <- file.path(config$directories$prepared_data, 'extended_bbox.yaml')
bb <- yaml::read_yaml(bbox_file)
bb_sf <- sf::st_bbox(c(xmin = bb$xmin, ymin = bb$ymin, xmax = bb$xmax, ymax = bb$ymax), crs = 4326)

## 1. Download parks data from WFS URL ------------------------------------------------>

# Define the WFS URL
wfs_url <- config$download_paths$copenhagen_parks

# Function to retry fetching data
fetch_data_with_retries <- function(url, retries = 3, timeout = 30) {
  attempt <- 1
  while (attempt <= retries) {
    tryCatch({
      response <- httr::GET(url, config = httr::timeout(timeout))
      if (httr::status_code(response) == 200) {
        return(response)
      } else {
        warning(sprintf("Failed to download data (status code %d). Retrying %d/%d...", httr::status_code(response), attempt, retries))
      }
    }, error = function(e) {
      warning(sprintf("Error encountered: %s. Retrying %d/%d...", e$message, attempt, retries))
    })
    Sys.sleep(2)  # Wait before retrying
    attempt <- attempt + 1
  }
  stop("Failed to download data after several retries.")
}

# Fetch the parks data
response <- fetch_data_with_retries(wfs_url)

# Read the response content as a GeoJSON
geojson_content <- httr::content(response, as = "text")
parks_data <- sf::st_read(geojson_content)

# Transform to the desired CRS if necessary
parks_data <- sf::st_transform(parks_data, crs = settings$working_crs)

# Inspect parks data
print(nrow(parks_data))
print(head(parks_data))
print(sf::st_crs(parks_data))

# Calculate area and check
parks_data$area <- sf::st_area(parks_data$geometry)
parks_data$area_ft2 <- as.numeric(parks_data$area) * 10.7639
print(head(parks_data$area_ft2))

# Filter parks based on area
parks_subset <- parks_data[parks_data$area_ft2 > settings$park_min_size_sqft, ]
print(nrow(parks_subset))

# Inspect geometry types
print(sf::st_geometry_type(parks_subset))

# Check for invalid geometries
invalid_geometries <- sf::st_is_valid(parks_subset$geometry)
print(sum(!invalid_geometries))  # Should be 0 if all geometries are valid

# Process parks to generate points
park_points_list <- lapply(seq_len(nrow(parks_subset)), function(row_idx) {
  park_name <- parks_subset$NAME[row_idx]
  park_polygons <- sf::st_cast(parks_subset$geometry[row_idx], to = 'POLYGON')
  
  print(paste("Processing park:", park_name))  # Debug: Check park names
  print(park_polygons)  # Debug: Check park polygons
  
  park_points <- lapply(seq_along(park_polygons), function(poly_idx) {
    poly <- park_polygons[poly_idx]
    if (sf::st_is_empty(poly)) {
      print(paste("Empty polygon for park:", park_name))
      return(NULL)
    }
    
    perimeter <- sf::st_cast(poly, to = 'LINESTRING')
    sample_points <- sf::st_line_sample(
      x = perimeter,
      density = min(as.numeric(sf::st_length(perimeter)) / 4, 500),
      type = 'regular'
    ) |> sf::st_cast(to = 'POINT')
    
    poly_centroid <- sf::st_centroid(poly)
    centroid_intersects_poly <- sf::st_intersects(poly_centroid, poly, sparse = F)[1, 1]
    if (centroid_intersects_poly) sample_points <- rbind(sample_points, poly_centroid)
    
    # Debug: Ensure no duplicate points
    print(paste("Number of sample points:", nrow(sample_points)))
    print(sample_points)
    
    # Return sample points if they are valid
    if (!is.null(sample_points) && nrow(sample_points) > 0) {
      return(sample_points)
    } else {
      return(NULL)
    }
  })
  
  # Combine and remove duplicates
  combined_points <- do.call(rbind, Filter(Negate(is.null), park_points))
  if (!is.null(combined_points) && nrow(combined_points) > 0) {
    # Ensure combined_points is an sf object
    combined_points_sf <- sf::st_sf(name = park_name, geometry = combined_points)
    combined_points_sf <- combined_points_sf[!duplicated(sf::st_coordinates(combined_points_sf)), ]
    return(combined_points_sf)
  } else {
    return(NULL)
  }
})

# Check the length and content of `park_points_list`
print(length(park_points_list))
print(sapply(park_points_list, function(x) if (!is.null(x)) nrow(x) else 0))

# Filter out NULL entries
park_points_list <- Filter(Negate(is.null), park_points_list)

# Validate combined data
if (length(park_points_list) > 0) {
  all_park_points <- do.call(rbind, park_points_list)
  if (inherits(all_park_points, "sf")) {
    all_park_points <- sf::st_transform(all_park_points, crs = 4326)
  } else {
    stop("Error: all_park_points is not an sf object.")
  }
  print(class(all_park_points))
  print(nrow(all_park_points))
  
  # Save the park points
  sf::st_write(all_park_points, glue::glue("{config$directories$prepared_data}/parks_points.geojson"))
} else {
  stop("Error: park_points_list is empty or contains only NULL objects.")
}

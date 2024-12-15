# Install and load required packages
if (!require("osmdata")) install.packages("osmdata")
if (!require("data.table")) install.packages("data.table")
if (!require("glue")) install.packages("glue")
if (!require("yaml")) install.packages("yaml")
if (!require("knitr")) install.packages("knitr")

library(osmdata)
library(data.table)
library(glue)
library(yaml)
library(knitr)

## SET DEFAULT CONFIG PATH
DEFAULT_CONFIG_FILEPATH <- "E:/IAAC/Thesis/Henry Spatial Analysis/Copenhagen walkability/city_walkability/analysis/config.yaml"

## Setup -------------------------------------------------------------------------------->

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

# Create all folders
for(dir in config$directories) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Load bounding box from YAML file
bbox_file <- file.path(config$directories$prepared_data, 'extended_bbox.yaml')
bb <- yaml::read_yaml(bbox_file)

# Print the raw bounding box values for debugging
print("Raw bounding box values:")
print(bb)

# Convert bounding box to numeric vector
bbox <- c(as.numeric(bb$xmin), as.numeric(bb$ymin), as.numeric(bb$xmax), as.numeric(bb$ymax))

# Print the converted bounding box for debugging
print("Converted bounding box:")
print(bbox)

# Check if bbox is valid
if (length(bbox) != 4 || !all(is.numeric(bbox))) {
  stop("Bounding box must be a numeric vector of length 4.")
}

# Define function to query OSM using osmdata
query_osmdata <- function(tags) {
  # Check if tags is in the correct format
  if (length(tags) != 2) {
    stop("Tags must be a vector of length 2: c('key', 'value').")
  }
  
  # Create OSM query
  bbox_query <- opq(bbox = bbox) %>%
    add_osm_feature(key = tags[1], value = tags[2])
  
  # Perform the query
  osm_data <- tryCatch({
    osmdata_sf(bbox_query)
  }, error = function(e) {
    message("Error querying OSM data: ", e$message)
    return(NULL)
  })
  
  # Check for data and return results
  if (is.null(osm_data) || nrow(osm_data$osm_points) == 0) {
    message("No data returned for tags: ", paste(tags, collapse = ", "))
    return(data.table())
  }
  
  return(data.table(
    name = osm_data$osm_points$name,
    lon = osm_data$osm_points$lon,
    lat = osm_data$osm_points$lat,
    address = osm_data$osm_points$addr,
    type = tags[2]
  ))
}

##  2. Download POIs based on the config, format as a single table, and save ------------>

destination_types <- names(config$osm_destination_queries)

destinations_table <- lapply(destination_types, function(d_type) {
  message("Querying OpenStreetMap for ", d_type, "...")
  
  osm_tags <- config$osm_destination_queries[[d_type]]
  
  # Ensure osm_tags is a vector of length 1 and split it into key-value pairs
  if (length(osm_tags) != 1) {
    stop("Each query in osm_destination_queries must be a vector of length 1 containing key-value pairs.")
  }
  
  tag_split <- strsplit(osm_tags[[1]], "=")[[1]]
  
  if (length(tag_split) != 2) {
    stop("Invalid tag format. It should be 'key=value'.")
  }
  
  formatted_locations <- query_osmdata(tag_split)
  
  formatted_locations[, type := d_type]
  return(formatted_locations)
}) |> rbindlist()

# Summarize results
message("Summary of results:")
knitr::kable(destinations_table[, .(Count = .N), by = .(`Destination type` = type)])

# Save table
data.table::fwrite(
  destinations_table,
  file = file.path(config$directories$prepared_data, 'destinations_osm.csv')
)

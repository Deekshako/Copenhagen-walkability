## #######################################################################################
## #######################################################################################
## #######################################################################################
## #######################################################################################

## SET DEFAULT CONFIG PATH
DEFAULT_CONFIG_FILEPATH <- "E:/IAAC/Thesis/Henry Spatial Analysis/Copenhagen walkability/city_walkability/analysis/config.yaml"

## Setup -------------------------------------------------------------------------------->

# Load all required packages
load_packages <- c(
  'argparse', 'elevatr', 'glue', 'sf', 'terra', 'httr', 'jsonlite', 'units', 'yaml'
)
load_packages |> lapply(library, character.only = T) |> invisible()

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
for(dir in config$directories) dir.create(dir, recursive = TRUE)


## 1. Load census-like blocks for the study area -------------------------->

# Set the coordinate reference system that will be used for all spatial data
working_crs <- sf::st_crs(x = settings$working_crs)
# Simple function to return the polygons to unprojected lat-long
to_latlong <- function(obj) sf::st_transform(obj, crs = sf::st_crs(4326))

# Load local shapefile for Copenhagen blocks
blocks_sf_path <- "E:/IAAC/Thesis/Henry Spatial Analysis/Copenhagen walkability/karre/karre.shp"  # Path to your shapefile
blocks_sf <- tryCatch({
  sf::st_read(blocks_sf_path)
}, error = function(e) {
  stop("Failed to read shapefile: ", e$message)
})

# Transform to working CRS
blocks_sf <- sf::st_transform(blocks_sf, crs = working_crs)

# Ensure required columns are present
id_column <- 'id'
area_column <- 'areal_m2'

missing_columns <- setdiff(c(id_column, area_column), colnames(blocks_sf))
if(length(missing_columns) > 0) stop("Missing columns in shapefile: ", paste(missing_columns, collapse = ", "))

# Rename columns if necessary
colnames(blocks_sf)[colnames(blocks_sf) == id_column] <- 'GEOID'
colnames(blocks_sf)[colnames(blocks_sf) == area_column] <- 'block_area'

# Drop blocks with small areas
blocks_sf$block_area <- sf::st_area(blocks_sf$geometry)
small_blocks <- which(units::drop_units(blocks_sf$block_area) <= settings$min_block_size)
if(length(small_blocks) > 0){
  message("Dropping ", length(small_blocks), " blocks smaller than the area cutoff.")
  blocks_sf <- blocks_sf[-small_blocks, ]
}

# Repair geometry
if(any(!sf::st_is_valid(blocks_sf))) blocks_sf <- sf::st_make_valid(blocks_sf)

# Simplify the blocks for faster processing
blocks_sf_simple <- sf::st_simplify(
  blocks_sf,
  preserveTopology = TRUE,
  dTolerance = settings$simplify_tolerance
)

# Save detailed and simplified blocks shapefile
tryCatch({
  sf::st_write(
    obj = blocks_sf[, c('GEOID', 'block_area')] |> to_latlong(),
    dsn = file.path(config$directories$prepared_data, "prepared_blocks.shp"),
    append = FALSE
  )
  
  sf::st_write(
    obj = blocks_sf_simple[, c('GEOID', 'block_area')] |> to_latlong(),
    dsn = file.path(config$directories$prepared_data, "prepared_blocks_simplified.shp"),
    append = FALSE
  )
}, error = function(e) {
  stop("Failed to save shapefiles: ", e$message)
})

# Plot simplified block outlines 
tryCatch({
  pdf(
    file.path(config$directories$prepared_data, "simplified_blocks.pdf"),
    height = 10,
    width = 10
  )
  plot(blocks_sf_simple$geometry |> to_latlong(), lwd = 0.2)
  dev.off()
}, error = function(e) {
  stop("Failed to plot simplified blocks: ", e$message)
})

message(
  "Finished processing input blocks. After processing, ", nrow(blocks_sf),
  " blocks remain."
)


## 2. Download OSM data and crop to the study area -------------------------------------->

# Get buffered bounding box of the study area
bb_sf <- blocks_sf |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_buffer(dist = settings$buffer, endCapStyle = "SQUARE") |>
  sf::st_as_sf() |>
  to_latlong()

# Extract bounding box as named vector
bb <- sf::st_bbox(bb_sf)

# Map the bounding box coordinates to the expected names
bb_named <- list(
  north = bb["ymax"],
  south = bb["ymin"],
  east = bb["xmax"],
  west = bb["xmin"]
)

# Save bounding box to YAML file
yaml::write_yaml(
  bb_named,
  file = file.path(config$directories$prepared_data, 'extended_bbox.yaml')
)

# Download OSM data for Copenhagen
osm_data_url <- config$download_paths$osm_extract
osm_data_path <- file.path(config$directories$raw_data, 'osm_extract_full.pbf')

message("Downloading OSM data from ", osm_data_url)

retry_count <- 5
timeout_seconds <- 600  # Increase timeout to 10 minutes

# Function to download file using curl
download_with_curl <- function(url, destfile, timeout_seconds) {
  system(glue::glue("curl --max-time {timeout_seconds} --retry 3 --retry-delay 5 --retry-max-time {timeout_seconds} --output {destfile} {url}"))
}

# Function to download file using wget
download_with_wget <- function(url, destfile, timeout_seconds) {
  system(glue::glue("wget --timeout={timeout_seconds} --tries=5 --continue --output-document={destfile} {url}"))
}

tryCatch({
  if (system("which curl", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
    download_with_curl(osm_data_url, osm_data_path, timeout_seconds)
  } else if (system("which wget", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
    download_with_wget(osm_data_url, osm_data_path, timeout_seconds)
  } else {
    stop("Neither curl nor wget is available for downloading files.")
  }
  
  message("OSM data downloaded successfully.")
}, error = function(e) {
  message("Failed to download OSM data.")
  message("Error details: ", e$message)
  stop("Download failed.")
})


## 3. Download elevation raster (DEM) for the study area ------------------------>

# Check if the coordinates are numeric and format the URL
if (!is.numeric(bb_named$north) || !is.numeric(bb_named$west)) {
  stop("Bounding box coordinates must be numeric.")
}

# Generate the correct URL for the elevation raster
elevation_raster_url <- sprintf(
  "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/n%f_w%f.tif",
  bb_named$north, bb_named$west
)
elevation_raster_path <- file.path(config$directories$raw_data, 'elevation_raster.tif')

# If you have OpenTopography API key, use it to download elevation data
if (Sys.getenv("OPENTOPO_API_KEY") != "") {
  message("Downloading elevation raster using OpenTopography API")
  
  elevation_data <- tryCatch({
    elevatr::get_elev_raster(
      lon = mean(c(bb_named$west, bb_named$east)),
      lat = mean(c(bb_named$south, bb_named$north)),
      z = 12,  # Adjust zoom level as needed
      prj = sf::st_crs(4326)$proj4string
    )
  }, error = function(e) {
    stop("Failed to download elevation raster: ", e$message)
  })
  
  # Save the raster
  tryCatch({
    terra::writeRaster(elevation_data, elevation_raster_path, overwrite = TRUE)
    message("Elevation raster downloaded and saved successfully.")
  }, error = function(e) {
    stop("Failed to save elevation raster: ", e$message)
  })
  
} else {
  message("No OpenTopography API key found. Downloading elevation raster from S3.")
  
  # Function to download file using curl
  download_with_curl <- function(url, destfile, timeout_seconds) {
    system(glue::glue("curl --max-time {timeout_seconds} --retry 3 --retry-delay 5 --retry-max-time {timeout_seconds} --output {destfile} {url}"))
  }
  
  # Function to download file using wget
  download_with_wget <- function(url, destfile, timeout_seconds) {
    system(glue::glue("wget --timeout={timeout_seconds} --tries=5 --continue --output-document={destfile} {url}"))
  }
  
  tryCatch({
    if (system("which curl", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      download_with_curl(elevation_raster_url, elevation_raster_path, timeout_seconds)
    } else if (system("which wget", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0) {
      download_with_wget(elevation_raster_url, elevation_raster_path, timeout_seconds)
    } else {
      stop("Neither curl nor wget is available for downloading files.")
    }
    
    message("Elevation raster downloaded successfully from S3.")
  }, error = function(e) {
    message("Failed to download elevation raster.")
    message("Error details: ", e$message)
    stop("Download failed.")
  })
}

## Finished -------------------------------------------------------------------------------->

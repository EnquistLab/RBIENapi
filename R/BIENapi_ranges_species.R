#'Download range maps for given species.
#'
#'BIENapi_ranges_species extracts range maps for the specified species.
#' @param species A single species.
#' @param include_id Logical. Should the range_id be appended to the file name?  Needed to save multiple maps per species.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Range maps for specified species.
#' @examples \dontrun{
#'
#'library(maps) #a convenient source of maps
#'
#'temp_dir <- file.path(tempdir(), "BIEN_temp")
#'
#'#Download ranges
#'BIENapi_ranges_species(species = "Abies_amabilis",
#'                  directory = temp_dir)#saves ranges to a temporary directory
#'
#'#Reading files
#'
#'Acer_poly <- st_read(dsn = temp_dir,
#'                      layer = "Abies_amabilis_117684")
#'
#'#Plotting files
#'plot(Abies_poly[,1])#plots the range, but doesn't mean much without any reference
#'map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#'plot(Abies_poly,col="forest green",add=TRUE) #adds the range of Abies lasiocarpa to the map
#'
#'#Getting data from the files
#'Abies_poly %>%
#'  st_drop_geometry()
#'
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
BIENapi_ranges_species <- function(species,directory=NULL,include_id=TRUE){

# make sure there are no spaces in the species names
  species <- gsub(" ","_",species)

# record original working directory,change to specified directory if given

  if(is.null(directory)){
    directory <- getwd()
  }

# make the URL
  url <- paste0("https://biendata.org/api/species-ranges/data?species=",species)


# Download the data from the API
  req <- GET(url = url)

# convert to raw - this throws an error
  raw_content <- rawToChar(req$content)

# convert raw content from JSON to R object
  df <- fromJSON(raw_content)


  # check for messages

  if("message" %in% names(df)){

    message(df$message)
    return(invisible(NULL))

  }

  df <- df$data

  for(l in 1:length(df$species)){

    Species <- df$species[l]

    sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                           wkt = "geometry",
                           crs = "epsg:4326")


    #save output
    if(include_id){

      suppressWarnings(st_write(obj = sp_range,
                                dsn = file.path(directory),
                                layer = paste(df$species[l],"_",df$gid[l],sep=""),
                                driver = "ESRI Shapefile",
                                append = FALSE,
                                quiet=TRUE))

    }else{

      suppressWarnings(st_write(obj = sp_range,
                                dsn = file.path(directory),
                                layer = paste(df$species[l]),
                                driver = "ESRI Shapefile",
                                append = FALSE,
                                quiet=TRUE))

    }


  }#for species in df loop

  return(invisible(NULL))



}#end fx





#'Extract occurrence data for specified species from BIEN
#'
#'BIENapi_occurrence_species downloads occurrence records for specific species from the BIEN database.
#' @param species A single species. Genus and species should be separated by a space. Genus should be capitalized.
#' @return Dataframe containing occurrence records for the specified species.
#' @examples \dontrun{
#' BIENapi_occurrence_species("Abies amabilis")
#' }
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
BIENapi_occurrence_species <- function(species,
                                    ...){

  # replace underscores

    species <- gsub(pattern = "_",replacement = " ",x = species)

    species <- gsub(pattern = " ",replacement = "%20",x = species)

  # make the URL

  url <- paste0("https://biendata.org/api/occurrences/records?species=",species)

  # Download the data from the API
  req <- GET(url = url)

  # convert to raw - this throws an error
  raw_content <- rawToChar(req$content)

  # convert raw content from JSON to R object
  df <- fromJSON(raw_content)

  # check for messages (which occur if things go wrong)

    if("message" %in% names(df)){

      message(df$message)
      return(invisible(NULL))

    }

  df <- df$data


  # return output
  return(df)

}


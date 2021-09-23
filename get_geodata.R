# Packages -----------------------------------------------------------------------------
require(httr)
require(stringr)

# Functions ----------------------------------------------------------------------------
check_geolevel <- function(geolevel, available_geolevels) {
  
  if (!geolevel %in% available_geolevels) stop("Please select valid 'geolevel'.")
  
}
check_api_call <- function(call_res) {
  
  if (httr::http_error(call_res)) stop("The API does not respond properly. Do you have an internet connection and an open proxy?")
  
}
call_api_geodata <- function() {
  
  # Call
  res <- httr::GET("https://opendata.swiss/api/3/action/package_show?id=geodaten-zu-den-eidgenoessischen-abstimmungsvorlagen")
  
  # Check
  check_api_call(res)
  
  # Return
  return(res)
  
}
get_geodata <- function(geolevel = "municipality", latest = T, verbose = F, call_res) {
  
  # Check input
  check_geolevel(geolevel, available_geolevels = c("national", "canton", "district", "municipality", "zh_counting_districts", "lakes"))
  
  # Call geodata api
  if (missing(call_res)) call_res <- call_api_geodata()
  cnt <- httr::content(call_res)
  
  # Get info
  if (latest) {
    
    gdInfo <- cnt[["result"]][["resources"]][[2]][["title"]]
    gdUrl <- cnt[["result"]][["resources"]][[2]][["download_url"]]
    gdLayers <- sf::st_layers(gdUrl)[1][["name"]]
    
  } else {
    
    gdInfo <- cnt[["result"]][["resources"]][[1]][["title"]] 
    gdUrl <- cnt[["result"]][["resources"]][[1]][["download_url"]]
    gdLayers <- sf::st_layers(gdUrl)[1][["name"]]
    
  }
  if (verbose) cat(paste0(gdInfo[!gdInfo == ""], collapse = "\n"), "\n\n")
  
  # Load geodata and join votes
  if (geolevel == "municipality") {
    
    # Load
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "voge_")], quiet = T)
    
    # Mutate if variable vogenr exists
    if ("vogenr" %in% names(gd)) {
      
      gd <- gd %>% 
        dplyr::mutate(id = vogenr) %>% 
        dplyr::select(-vogenr) 
      
    }
    if ("vogeId" %in% names(gd)) {
      
      gd <- gd %>% 
        dplyr::mutate(id = vogeId) %>% 
        dplyr::select(-vogeId) 
      
    }
    
    # Adjust variable mun_id
    gd <- gd %>% 
      dplyr::rename(mun_id = id) %>% 
      dplyr::mutate(mun_id = as.character(mun_id)) %>% 
      dplyr::select(mun_id, geometry)
    
  }
  if (geolevel == "district") {
    
    # Load
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "bezk_")], quiet = T)
    
    # Mutate if variable vogenr exists
    if ("bezkId" %in% names(gd)) {
      
      gd <- gd %>% 
        dplyr::mutate(id = bezkId) %>% 
        dplyr::select(-bezkId) 
      
    }
    
    # Adjust variable district_id
    gd <- gd %>%
      dplyr::rename(district_id = id) %>%
      dplyr::mutate(district_id = as.character(district_id)) %>% 
      dplyr::select(district_id, geometry)
    
  }
  if (geolevel == "canton") {
    
    # Load
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "kant_")], quiet = T) %>%  
      dplyr::rename(canton_id = id) %>% 
      dplyr::rename(canton_name = name) %>% 
      dplyr::mutate(canton_id = as.character(canton_id)) %>% 
      dplyr::select(canton_id, geometry)
    
  }
  if (geolevel == "zh_counting_districts") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "zaelhlkreise_")], quiet = T) %>% 
      dplyr::rename(mun_id = id) %>% 
      dplyr::rename(mun_name = name) %>% 
      dplyr::mutate(mun_id = as.character(mun_id)) %>% 
      dplyr::select(mun_id, geometry)
    
  }
  if (geolevel == "lakes") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "seen_")], quiet = T) %>% 
      dplyr::select(id, geometry)
    
  }
  if (geolevel == "national") {
    
    gd <- sf::st_read(gdUrl, layer = gdLayers[stringr::str_detect(gdLayers, "suis_")], quiet = T) %>% 
      dplyr::select(id, geometry)
    
  }
  
  # Return
  return(gd)
  
}

# This function first searches in the cache dirs for the figure or movie before 
# rendering it
app_caching <- function(cache, type = "rds", file_name, width, height) {
  
  # check if file exists
  if (!fs::file_exists(fs::path(cache_dir(type), file_name, ext = type))) {
   
    # defuse
    cache <- rlang::enquo(cache)
    
    # check
    if (!rlang::quo_is_call(cache)) {
      stop("A function call needs to be supplied.", call. = FALSE)
    }
                          
    # execute
    out <- rlang::eval_tidy(cache)
    
    # cache file
    method_selector(file_name, out, type, width, height)
    
  } 
  
  # always return something (either `dateframe` or paths for images and videos)
  method_selector(file_name, type = type)
  
}

# select method
method_selector <- function(file_name, file_out = NULL, type = "rds",  
                            width = NULL, height = NULL) {
  
  pt <- fs::path(cache_dir(type), file_name, ext = type)
  
  if (is.null(file_out)) {
    
    switch(
      type,    
      # return data frame
      rds = readRDS(pt),
      # return path of image or video
      png = pt,
      mkv = fs::path("vid", basename(pt))
    )
    
  } else {


    switch(
      type, 
      rds = saveRDS(file_out, pt),
      png = ggplot2::ggsave(
        basename(pt),
        plot = file_out, 
        path = dirname(pt),
        width = width,
        height = height,
        dpi = 72,
        units = "px"
      ),
      mkv = gganimate::anim_save(
        basename(pt), 
        fps = 3,
        animation = gganimate::animate(
          file_out,        
          renderer = gganimate::av_renderer(basename(pt)),
          width = width,
          height = height
        ), 
        path = dirname(pt),
        width = width,
        height = height
      )
    )
  }
}


# introduce filenaming convention
file_namer <- function(type, prefix, taxa, method = "count", trans = "unprocessed", 
                       viz = NULL, y = NULL, x = NULL) {
  
  stopifnot(prefix %in% c("raw", "engineering", "training", "validation"))
  if (type != "rds") stopifnot(viz %in% c("spatial", "xy"))
  if (type != "rds") stopifnot(all(!is.null(viz), !is.null(y), !is.null(x)))
  
  # core names
  nm <- paste(prefix, taxa, method, trans, sep = "_")
  
  # extend if needed
  if (type != "rds") {
    paste(nm, viz, y, x, sep = "_")
  } else {
    nm
  }

}
  
# make sure that caching dirs exist  
cache_dir <- function(type = "rds") {
  
  # path to package
  pkg_path <- fs::path_package("transferice")
  
  # if directories don't exist then create them
  # data 
  if (type == "rds") {
    
    out <- fs::path(pkg_path, "appdir", "cache")
    
    if (!fs::dir_exists(out)) {
      fs::dir_create(out)
    }
  }

  # browser()
  # images
  if (type == "png") {
    
    out <- fs::path(pkg_path, "www", "img")
    
    if (!fs::dir_exists(out)) {
      fs::dir_create(out)
    }
  }
  
  # videos
  if (type == "mkv") {
    
    out <- fs::path(pkg_path, "www", "vid")
    
    if (!fs::dir_exists(out)) {
      fs::dir_create(out)
    }
  }

  out
}
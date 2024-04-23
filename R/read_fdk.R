read_fdk <- function(site, path, pattern = "DD"){
  print(site)
  filn <- list.files(path = path)
  filn <- filn[which(stringr::str_detect(filn, pattern = site))]
  filn <- filn[which(stringr::str_detect(filn, pattern = pattern))]
  readr::read_csv(paste0(path, "/", filn))
}

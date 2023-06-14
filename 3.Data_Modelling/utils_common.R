# depends on ggplot2
# depends on tmap
# depends on constants.R

log_msg <- function(obj) {
  cat(as.character(obj), "\n")
}

create_parent_dir <- function(path) {
  dir_path <- dirname(path)
  if (nchar(dir_path) > 0) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
  }
}

start_plot <- function(file_name, ...) {
  create_parent_dir(file_name)
  log_msg(file_name)
  png(file_name, ...)
}

save_plot <- function() {
  if (SAVE_PLOTS) dev.off()
}

save_ggplot <- function(ggplot_out,
                        file_name,
                        width = GGPLOT_WIDTH,
                        height = GGPLOT_HEIGHT,
                        ...) {
  create_parent_dir(file_name)
  log_msg(file_name)
  ggplot2::ggsave(
    file_name,
    ggplot_out,
    width = width,
    height = height,
    ...
  )
}

save_tmap <- function(tmap_out,
                      file_name,
                      width = TMAP_WIDTH,
                      height = TMAP_HEIGHT,
                      ...) {
  create_parent_dir(file_name)
  log_msg(file_name)
  tmap::tmap_save(tmap_out, file_name, width = width, height = height, ...)
}

save_tex <- function(
  x,
  file_name,
  include.rownames = FALSE,
  comment = FALSE,
  timestamp = FALSE,
  ...
) {
  create_parent_dir(file_name)
  log_msg(file_name)
  print(
    x,
    file = file_name,
    include.rownames = include.rownames,
    comment = comment,
    timestamp = timestamp,
    ...
  )
}

save_tex_formatted <- function(x, file_name, hline_after_extra = NULL, ...) {
  save_tex(
    x,
    file_name,
    hline.after = c(-1, -1, 0, nrow(x), hline_after_extra),
    ...
  )
}

save_csv <- function(x, file_name) {
  create_parent_dir(file_name)
  log_msg(file_name)
  readr::write_csv(x, file = file_name)
}

save_rds <- function(x, file_name) {
  create_parent_dir(file_name)
  log_msg(file_name)
  saveRDS(x, file = file_name)
}

save_output <- function(..., file_name) {
  create_parent_dir(file_name)
  log_msg(file_name)
  capture.output(..., file = file_name)
}

save_st <- function(..., file_name) {
  create_parent_dir(file_name)
  log_msg(file_name)
  sf::st_write(..., dsn = file_name)
}

remove_punct <- function(x) {
  str_replace_all(x, "[^[:alnum:]]", "")
}

remove_spatial <- function(x) {
  x <- tibble::as_tibble(x)
  if ("geometry" %in% names(x)) {
    x[["geometry"]] <- NULL
  }
  x
}

replace_all_pairs <- function(strings, old, new) {
  purrr::reduce2(
    old,
    new,
    function(strings_curr, old, new) {
      stringr::str_replace_all(
        strings_curr,
        stringr::fixed(old),
        new
      )
    },
    .init = strings
  )
}


n_digits <- function(x) {

  max(
    stringr::str_length(
      stringr::str_replace(
        x,
        ".*\\.(\\d+)",
        "\\1"
      )
    )
  )

}


date_overlap <- function(x, y) {

  x <- cbind(apply(rbind(x), 1L, min), apply(rbind(x), 1L,
                                             max))
  y <- cbind(apply(rbind(y), 1L, min), apply(rbind(y), 1L,
                                             max))
  maxdim <- max(nrow(x), nrow(y))
  x <- x[rep(1L:nrow(x), length.out = maxdim), , drop = FALSE]
  y <- y[rep(1L:nrow(y), length.out = maxdim), , drop = FALSE]
  d1 <- x[, 2L]
  idx <- x[, 2L] > y[, 2L]
  d1[idx] <- y[idx, 2L]
  d2 <- y[, 1L]
  idx <- x[, 1L] > y[, 1L]
  d2[idx] <- x[idx, 1L]
  d <- d1 - d2
  d[d <= 0L] <- 0L
  unname(d)

}


activity_by_date <- function(df) {

  # create sequence(s) of date(s); sometimes there are overlapping
  # start and end dates due to batches being uploaded
  seq.Date(as.Date(df$start_date), as.Date(df$end_date), by = 'day') %>%
    tibble::as_tibble() %>%
    dplyr::rename(date_active = value) %>%
    dplyr::arrange(date_active)

}


add_year <- function(...) {

  if(as.numeric(stringr::str_sub(min_date, 2, 3)) > as.numeric(stringr::str_sub(max_date, 2, 3))) {

    1

  } else {

    0

  }

}

#' @import data.table
NULL

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param data Path to the input file
#' @param datafield Path to the input file
#' @param datafield_name Path to the input file
#' @param keep.time Path to the input file
#' @param keep.date Path to the input file
#' @return A matrix of the infile
#' @export
extract.time <- function(data, datafield, datafield_name, keep.time = TRUE, keep.date=TRUE){
  if (keep.time == TRUE | keep.date == TRUE){
    raw_datafields <- colnames(data)[which(startsWith(colnames(data), datafield))]
    nr_cols <- length(raw_datafields)
    my_cols <- c(sprintf(paste0(datafield_name, "_", "datetime", "_%1d"), seq(1, nr_cols)))
    colnames(data)[which(startsWith(colnames(data), datafield))] <- my_cols	
    for(x in 1:nr_cols) {
      x.col <- my_cols[x]
      if (keep.date == TRUE & keep.time == TRUE) {
      x.date <- paste0(datafield_name, "_", "date", "_", x)
      x.time <- paste0(datafield_name, "_", "time", "_", x)
      } else if (keep.date == TRUE & keep.time == FALSE) {
          x.date <- paste0(datafield_name, "_", "date", "_", x)
          x.time <- NA
      } else if (keep.date == FALSE & keep.time == TRUE) {
          x.date <- NA
          x.time <- paste0(datafield_name, "_", "time", "_", x)
      }
      data <- tidyr::separate(data = data,
        col = x.col,
        into = c(x.date, x.time),
        sep = "T",
        remove = TRUE)

      x.month <- paste0(datafield_name, "_", "month")
      set(data)[, (paste0(x.month, "_", x)) := month(as.Date(data[[x.date]]))]
    }
    set(data, , grep(paste0(datafield_name, "_date"), colnames(data)), NULL)
    x.month.cols <- c(sprintf(paste0(datafield_name, "_", "month", "_%1d"), seq(1, nr_cols)))
    for(col in x.month.cols) {
      set(data, j = col, value = as.numeric(data[[col]]))
    }
    data[[x.month]] <- rowMeans(data[, x.month.cols, with = F], na.rm=TRUE)
    for(y.col in x.month.cols) {print(paste0("Nr NA in ", y.col, ":", nrow(data[is.na(data[[y.col]])])))}
    data[[x.month]] <- round(data[[x.month]], digits = 0)
    set(data, , (x.month.cols), NULL)
    return(data)

  } else {
      stop("keep.time and keep.date can't be both FALSE at the same time.")
    }
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param data Path to the input file
#' @param datafield Path to the input file
#' @param datafield_name Path to the input file
#' @param my_exclusions Path to the input file
#' @param my_cols Path to the input file
#' @param exclude Path to the input file
#' @return A matrix of the infile
#' @export
exclude.id <- function(data, datafield, datafield_name, my_exclusions, my_cols, exclude = TRUE) {
  nr_cols <- length(colnames(data)[which(startsWith(colnames(data), datafield))])
  my_cols <- c(sprintf(paste0(datafield_name, "_%1d"), seq(1, nr_cols)))
  df_cols <- colnames(data)[which(startsWith(colnames(data), datafield))]
  data[, (my_cols) := lapply(.SD, function(x) ifelse(x %in% my_exclusions, 1, 0)), .SDcols = df_cols]
  data[[datafield_name]] <- rowSums(data[, my_cols, with = F], na.rm=TRUE)
  data[[datafield_name]] <- ifelse(data[[datafield_name]] > 0, 1, 0)
  data[, (my_cols) := NULL]
  if (exclude) {
    data <- data[data[[datafield_name]] == 0, ]
    data[, c(datafield_name) := NULL]
  }
  return(data)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param data Path to the input file
#' @param datafield Path to the input file
#' @param datafield_name Path to the input file
#' @param log log(value + 1)
#' @return A matrix of the infile
#' @export
get.means <- function(data, datafield, datafield_name, log = FALSE) {
  nr_cols <- length(colnames(data)[which(startsWith(colnames(data), datafield))])
  my_cols <- c(sprintf(paste0(datafield_name, "_%1d"), seq(1, nr_cols)))
  colnames(data)[which(startsWith(colnames(data), datafield))] <- my_cols
  data[[datafield_name]] <- rowSums(data[, my_cols, with =F], na.rm=TRUE)
  if(log) data[, (datafield_name) := lapply(.SD, function(x) log(x + 1)), .SDcols = datafield_name]
  set(data, , my_cols, NULL)
  return(data)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#' 
#' @param data Path to the input file
#' @param datafield Path to the input file
#' @param datafield_name Path to the input file
#' @param cols.rm Path to the input file
#' @return A matrix of the infile
#' @export
last.nm <- function(data, datafield, datafield_name, cols.rm = TRUE) {
  res <- NULL
  my_cols <- grep(datafield, names(data), value= TRUE)
  x <- data[, my_cols, with = F]
  x[, res := NA_character_]
  wh = x[, .I]
  for (v in (length(x)-1):1){
    if (!length(wh)) break
    set(x, j="res", i=wh, v = x[[v]][wh])
    wh = wh[is.na(x$res[wh])]
  }
  data[, eval(datafield_name) := x$res]
  if(cols.rm) {
    return(data[, c(my_cols) := NULL])
  } else return(data)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#' 
#' @importFrom stats median mad
#'
#' @param data Path to the input file
#' @param column Path to the input file
#' @param cutoff Path to the input file
#' @param remove Path to the input file
#' @return A matrix of the infile
#' @export
mad.cutoff <- function(data, column, cutoff = 2, remove = TRUE) {
  if (cutoff <= 0) stop("Cutoff must be an unsigned positive numeric.")
  if (!class(data)[1] %in% c("data.table", "data.frame")) stop("data must be a data.frame or simliar object.")
  if (!column %in%  colnames(data)) stop("column must be a valid column of data")
  x <- data[[column]]
  ov <- x[which((abs(x - median(x)) / mad(x)) > cutoff)]
  or <- which(data[[column]] %in% ov)
  if (remove == TRUE) {
    return(data[!or, ])
  } else {
    return(or)
  }
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#' 
#' @importFrom stats sd 
#'
#' @param data Path to the input file
#' @param column Path to the input file
#' @param sd_const Path to the input file
#' @param remove Path to the input file
#' @return A matrix of the infile
#' @export
sd.cutoff <- function(data, column, sd_const = 2, remove = TRUE) {
  if (sd_const <= 0) stop("Cutoff must be an unsigned positive numeric.")
  if (!class(data)[1] %in% c("data.table", "data.frame")) stop("data must be a data.frame or simliar object.")
  if (!column %in%  colnames(data)) stop("column must be a valid column of data")
  x <- data[[column]]
  ov <- x[which(abs(x - mean(x)) > sd_const*sd(x))]
  or <- which(data[[column]] %in% ov)
  if (remove == TRUE) {
    return(data[!or, ])
  } else {
    return(or)
  }
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#' 
#' @importFrom stats shapiro.test
#'
#' @param x Path to the input file
#' @param t Path to the input file
#' @param s Path to the input file
#' @param iter Path to the input file
#' @return A matrix of the infile
#' @export
shapiro.dt <- function(x, t = c(NA), s = 5000, iter = 100) {
  w <- c()
  p <- c()
  for(i in 1:iter) {
    tv <- x[sample(length(x), s)]
    st <- shapiro.test(tv)
    w <- c(w, st$statistic)
    p <- c(p, st$p.value)
  }
  w <- mean(w)
  p <- mean(p)
  dt <- data.table(
    Trait = t,
    W = w,
    P = p,
    iterations = iter
  )
  return(dt)
}


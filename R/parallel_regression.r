#' @import data.table
NULL

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
linreg.ancon <- function(x, y, data, covars) {
  reg <- lm(formula = paste0(y, " ~ ", "`", x, "` + ", covars), data = data)
  dt <- data.table(
    Exposure = x,
    BETA = summary(reg)$coef[2, 1],
    SE = summary(reg)$coef[2, 2],
    P = summary(reg)$coef[2, 4],
    F_stat = summary(reg)$fstatistic[1]
  )
  return(dt)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
logreg.ancon <- function(x, y, data, covars) {
  reg <- glm(formula = paste0(y, " ~ ", "`", x, "` + ", covars), data = data, family = binomial)
  dt <- data.table(
    Exposure = x,
    Outcome = "breast_cancer",
    BETA = summary(reg)$coef[2, 1],
    SE = summary(reg)$coef[2, 2],
    P = summary(reg)$coef[2, 4]
  )
  return(dt)
}

#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
parallel.reg <- function(data, x, y, reg_type = c("lm"), covars, num_cores) {
  if (reg_type == "lm") {
    reg_type = get("linreg.ancon")
  } else if (reg_type == "lg") {
    reg_type = get("logreg.ancon")
  } else stop("Incorrect reg_type selected.")
  dt_list <- list()
  for (i in 1:length(y)) {
    y <- y[i]
    print(paste0("Current trait: ", y))
    res <- rbindlist(
      pbmcapply::pbmclapply(
        X = x,
        FUN = reg_type,
        y = y,
        data = data,
        covars = covars,
        mc.style = "txt",
        mc.cores = num_cores
      )
    )
  res$Outcome <- y
  dt_list[[i]] <- res
  }
  return(rbindlist(dt_list))
}


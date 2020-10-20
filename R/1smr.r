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
w.score <- function(data, org_beta, org_cols = c(), org_trait = NA) {
  org_beta <- org_beta[Trait == org_trait, ..org_cols]
  snps <- org_beta[[1]]
  beta <- org_beta[[2]]
  data_ <- copy(data)
  for (j in snps) set(data_, j = j, value = data_[[j]] * abs(unlist(org_beta[j, 2])) )
  data[, paste0(wbc_trait, "_w_score") := (rowSums(data_[, ..snps]) / sum(abs(beta))) * length(snps) ] 
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
tsls.ancon <- function(x, y, data, covars, u_cols = c(), w_cols = c()) {
  mv_tsls_u <- ivreg(as.formula(paste(y, " ~ ", x, " + ", covars, " | ", u_cols, " + ", covars)), data = data)
  mv_tsls_w <- ivreg(as.formula(paste(y, " ~ ", x, " + ", covars, " | ", w_cols, " + ", covars)), data = data)
  paste0(names(wbc_list), "_u_score", collapse=" + ")
  dt <- data.table(
    Exposure = x,
    Outcome = "breast_cancer",
    BETA_u = summary(mv_tsls_u)$coef[2, 1],
    SE_u = summary(mv_tsls_u)$coef[2, 2],
    P_u = summary(mv_tsls_u)$coef[2, 4],
    BETA_w = summary(mv_tsls_w)$coef[2, 1],
    SE_w = summary(mv_tsls_w)$coef[2, 2],
    P_w = summary(mv_tsls_w)$coef[2, 4]
  )
  return(dt)
}


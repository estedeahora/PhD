#' PhD_eig_corrected
#'
#' This function corrects the eigenvalues of a Multiple Correspondence Analysis
#' (MCA) object according to the method proposed by Greenacre or Benzécri.
#'
#' @param res_mca An object of class MCA, typically obtained from the `MCA`
#'            function.
#' @param modo A character string indicating the correction method to use.
#'            Options are "Benzecri" or "Greenacre" (default).
#'
#' @return An object of class MCA with corrected eigenvalues.
#'
#' @export


PhD_eig_corrected <- function(res_mca, modo = "Greenacre"){
  clase <- class(res_mca)
  if(!any(c("MCA") %in% clase)){
    stop("'res_mca' debe ser clase MCA")
  }
  if(!modo %in% c("Benzecri", "Greenacre")){
    stop("'modo' debe ser 'Benzecri' o 'Greenacre')")
  }

  eig_orig <- res_mca$eig

  Q <- res_mca[["call"]][["quali"]] |> length()  # Q/K: Número de variables
  J <- res_mca[["var"]][["coord"]] |> nrow()     # J: Número de categorías

  # Calcular eigenvalues actualizados
  lam <- eig_orig[ , 1]
  lam_sel <- lam[lam > 1/ Q]
  eig_corr <- (Q / (Q -1))^2 * (lam_sel - 1/Q)^2

  # Calcular porcentaje
  if(modo == "Benzecri"){
    per <- eig_corr/sum(eig_corr) * 100
  }else if(modo == "Greenacre"){
    eig_tot <- (Q/(Q - 1)) * (sum(lam**2) - ((J - Q)/Q^2))
    per <- eig_corr/eig_tot * 100
  }

  # Armar tabla con valores actualizados
  eig_new <- data.frame(eig = eig_corr,
                        per = per,
                        cum = cumsum(per) )
  # eig_new["cum"] <- cumsum(eig_new$per)

  rownames(eig_new) <- paste0("dim ", 1:nrow(eig_new))
  colnames(eig_new) <- colnames(eig_orig)

  eig_new <- as.matrix(eig_new)

  # Actualizar res_mca
  res_mca$eig_original <- eig_orig
  res_mca$eig <- eig_new

  return(res_mca)
}

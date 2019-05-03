#' @title Visualization (plot) of the results of modified Bland Altman-analysis
#'
#' @description \code{plot.blandxtr} visualizes (plot) the results of
#' modified Bland Altman-analysis performed with \code{blandxtr}
#'
#' @author Inga Koenemund \email{inga.koenemund@@web.de}
#'
#' @param x list with results from \code{blandxtr}
#' @param type which plot to produce (1: analysis results,
#' 2: modified analysis results, 3: list with all plots available)
#' @param ... other arguments not used by this method
#'
#' @return A list with the following elements (ggplot2-objects) is returned
#' (for type=3)
#' \itemize{
#'  \item{\code{plot_res}} {Bland Altman-plot showing unmodified
#'  analysis results}
#'  \item{\code{plot_res_mod}} {Bland Altman-plot showing modified
#'  analysis results}
#'  \item{\code{qq_ind_means}} {QQ-plot of individual means}
#'  \item{\code{qq_resid}} {QQ-plot of individual residuals}
#'  \item{\code{plot_res_means}} {Plot of residuals vs mean}
#'  \item{\code{plot_res_id}} {Plot of residuals vs ID}
#'  }
#'  type = 1 only \code{plot_res} or type = 2 only \code{plot_res_mod} will
#'  be returned.
#'
#' @method plot blandxtr
#'
#' @export

plot.blandxtr <- function (x, type, ...) {

  # ---------------------------
  # check input

  if (missing(type)) {
    type <- 1
    warning("Variable `type` is missing. Setting type to 1 (Bland Altman-plot
      showing unmodified analysis results will be produced.).")
  } else if (type != 1 & type != 2 & type != 3) {
    type <- 1
    warning("Variable `type` has been given an invalid value.
      Setting type to 1 (Bland Altman-plot showing
      unmodified analysis results will be produced.).")
  }
  # ---------------------------

  # Bland Altman-plot
  # black/white
    plot_res <- function(){
      plot_res <- ggplot2::ggplot(data = x$bv$output_measurements) +
        ggplot2::geom_point(mapping = aes(x = m_ij, y = d_ij)) +
        # Add labels at points (without overlap) (increases runtime!)
        # geom_text_repel(label=x$bv$output_measurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
        # Add a horizontal line at y = 0
        ggplot2::geom_hline(aes(yintercept=0, linetype = "zeroline"), size=1) +
        # Add a horizontal line at y = mean of all differences (d)
        ggplot2::geom_hline(aes(yintercept=x$bv$d, linetype = "bias"), size=1) +
        # Add horizontal lines at limits of agreement
        ggplot2::geom_hline(aes(yintercept=x$loa$loa_l, linetype = "limit of agreement"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa$loa_u, linetype = "limit of agreement"), size=0.5) +
        # Add horizontal lines at 95%-CI of limits of agreement
        ggplot2::geom_hline(aes(yintercept=x$loa_mover$ci_l_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover$ci_u_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover$ci_l_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover$ci_u_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)") +
        scale_linetype_manual(name="Legend: ", breaks = c("zeroline", "bias", "limit of agreement", "CI of LoA"),
          values = c("zeroline" = 1, "bias" = 2, "limit of agreement" = 3, "CI of LoA" = 4)) +
        ggplot2::theme(legend.direction = "horizontal", legend.position = "bottom",
          legend.key.size = unit(3, "lines"), legend.background=element_blank(),
          legend.key=element_blank())
    }

  # ----------------------------------
  # Bland Altman-plot (modified analysis)
  # black/white
    plot_res_mod <- function(){
      plot_res_mod <- ggplot2::ggplot(data = x$bv$output_measurements) +
        ggplot2::geom_point(mapping = aes(x = m_ij, y = d_ij)) +
        # Add labels at points (without overlap) (increases runtime!)
        # geom_text_repel(label=x$bv$output_measurements$subject, mapping = aes(x = m_ij, y = d_ij), size = 2) +
        # Add a horizontal line at y = 0
        ggplot2::geom_hline(aes(yintercept=0, linetype = "zeroline"), size=1) +
        # Add a horizontal line at y = mean of all differences (d)
        ggplot2::geom_hline(aes(yintercept=x$bv$d, linetype = "bias"), size=1) +
        # Add horizontal lines at limits of agreement
        ggplot2::geom_hline(aes(yintercept=x$loa_mod$loa_l, linetype = "limit of agreement"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mod$loa_u, linetype = "limit of agreement"), size=0.5) +
        # Add horizontal lines at 95%-CI of limits of agreement
        ggplot2::geom_hline(aes(yintercept=x$loa_mover_mod$ci_l_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover_mod$ci_u_loa_l_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover_mod$ci_l_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::geom_hline(aes(yintercept=x$loa_mover_mod$ci_u_loa_u_mover, linetype = "CI of LoA"), size=0.5) +
        ggplot2::labs(x="mean value of X_ij and Y_ij", y="difference (X_ij - Y_ij)") +
        scale_linetype_manual(name="Legend: ", breaks = c("zeroline", "bias", "limit of agreement", "CI of LoA"), values = c("zeroline" = 1, "bias" = 2, "limit of agreement" = 3, "CI of LoA" = 4)) +
        ggplot2::theme(legend.direction = "horizontal", legend.position = "bottom",
          legend.key.size = unit(3, "lines"), legend.background=element_blank(),
          legend.key=element_blank())
    }

  # ----------------------------------
  # QQ plot of individual means

    plot_qq_ind_means <- function(){
      p <- ggplot2::ggplot (data = x$bv$output_subjects, aes(sample=d_i))
      qq_ind_means <- p + ggplot2::stat_qq(col="black") + ggplot2::stat_qq_line(col="black")
    }

  # ----------------------------------
  # QQ plot of residuals

    plot_qq_resid <- function(){
      p <- ggplot2::ggplot (data = x$bv$output_measurements, aes(sample=r_ij))
      qq_resid <- p + ggplot2::stat_qq(col="black") + ggplot2::stat_qq_line(col="black")
    }

  # ----------------------------------
  # plot of residuals vs mean

    plot_res_means <- function(){
      plot_res_means <- ggplot2::ggplot(data = x$bv$output_measurements) +
        ggplot2::geom_point(mapping = aes(x = m_ij, y = r_ij)) +
        # # Add a horizontal line at y = 1.96
        # ggplot2::geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
        # # Add a horizontal line at y = -1.96
        # ggplot2::geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
        ggplot2::labs(x="mean value of X_ij and Y_ij", y="residual of X_ij and Y_ij")
    }

  # ----------------------------------
  # plot of residuals vs ID
    plot_res_id <- function(){
      plot_res_id <- ggplot2::ggplot(data = x$bv$output_measurements) +
        ggplot2::geom_point(mapping = aes(x = subject, y = r_ij)) +
        # # Add a horizontal line at y = 1.96
        # ggplot2::geom_hline(aes(yintercept=1.96), size=1, linetype = "dashed") +
        # # Add a horizontal line at y = -1.96
        # ggplot2::geom_hline(aes(yintercept=-1.96), size=1, linetype = "dashed") +
        ggplot2::labs(x="ID", y="residual of X_ij and Y_ij")
    }

  # ----------------------------------
  # generate plots depending on 'type'

  if (type == 1) {
    plot_res <- plot_res()
    return(
      list(
        plot_res = plot_res
      )
    )
  } else if (type == 2) {
    plot_res_mod <- plot_res_mod()
    return(
      list(
        plot_res_mod = plot_res_mod
      )
    )
  } else {
    plot_res <- plot_res()
    plot_res_mod <- plot_res_mod()
    qq_ind_means <- plot_qq_ind_means()
    qq_resid <- plot_qq_resid()
    plot_res_means <- plot_res_means()
    plot_res_id <- plot_res_id()

    return(
      list(
        plot_res = plot_res,
        plot_res_mod = plot_res_mod,
        qq_ind_means = qq_ind_means,
        qq_resid = qq_resid,
        plot_res_means = plot_res_means,
        plot_res_id = plot_res_id
      )
    )
  }

}

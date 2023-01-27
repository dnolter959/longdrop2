#' Plot point estimates and CI by K using longdrop object
#'
#' @param longdrop
#'
#' @return
#' @export
#'
#' @examples
plot.longdrop = function(longdrop){
  k  = sort(as.numeric(hash::keys(longdrop$results)))
  extract_hash_val = function(k, val) {longdrop$results[[toString(k)]][[val]]}

  p  = unlist(lapply(k, extract_hash_val, "p-value"))
  pe = unlist(lapply(k, extract_hash_val, "point_estimate"))
  se = unlist(lapply(k, extract_hash_val, "standard-error"))
  t  = unlist(lapply(k, extract_hash_val, "t-statistic"))

  plot_data = data.frame(k, p, pe, se, t)

  ggplot2::ggplot(plot_data, aes(x = k, y = pe)) +
    geom_line(color = "#F8766D") +
    geom_point(color = "#F8766D") +
    geom_ribbon(aes(
      ymin = pe - 1.96*se,
      ymax = pe + 1.96*se),
      color = "#F8766D",
      fill = "#F8766D",
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, lty=2) +
    labs(
      title = "Treatment Effect by K",
      x = "k",
      y = "Treatment Effect",
    )
}

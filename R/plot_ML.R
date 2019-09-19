#' Plot the fitted Debiased ML treatment effects.
#'
#' @description A function to plot the estimated treatment effects.
#' @param x The estimated ML_Treatment_effects object to plot
#' @param coloured.same Whether to use the same colour for all plots, defaults to TRUE.
#' @param palette An optional custom colour/vector of palette colour codes to be used for plotting.
#' @param ... Additional arguments (currently ignored).
#' @details The plot samples a random colour from a predefined colourblind palette by default.
#'          You may use a custom colour/colour palette for plotting by supplying the palette argument.
#'          If you supply a palette, the colour will be used in the order in which they are specified.
#'          Palette lengths longer than the number of plots will be ignored.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#'
#'     ols_example <- double_ML(X, Y, W, method = c("ols"),
#'                              show.progress = FALSE,
#'                              k.fld = 4,
#'                              simulations = 50)
#'
#'    plot_ML(ols_example)
#'
#'
#'
#'
#' }
#'



plot_ML <- function( x,
                                       coloured.same = TRUE,
                                       palette = NULL,
                                       ...)
{
######################################################################################
# Appease R CMD Check
######################################################################################
   Treatment_Effect <- NULL
######################################################################################

   if(is.null(palette) && coloured.same == TRUE)
   {
      make_palette <- c( "#999999", "#E69F00", "#56B4E9",
                         "#009E73", "#F0E442", "#0072B2",
                         "#D55E00", "#CC79A7")
      that_one <- sample( make_palette, 1 )
   }

   else if(!is.null(palette) && length(palette) == 1)
   {
     palette <- rep( palette, 5 )
   }
   else if(!is.null(palette) && length(palette) >= 5 )
   {
    warning("Dropping additional palette colours and using colours 1-5.")
   }
   else if(!is.null(palette) && length(palette) != 5 && length(palette) != 1 && !(length(palette) >= 5))
   {
      user_in <- readline("The custom palette is neither single colour, nor full length. Drop custom palette in favour of default? Y/N \n")
      if(user_in == "N")
      {
      user_drop <- readline("Use first custom colour for all plots? Y/N \n")
        if(user_drop == "Y")
        {
         warning("Using first custom colour for all plots.")
           palette <- rep( palette, 5)
        }
        else
        {
          stop("Stopping the plot, specify valid custom colour palette or use the default one.")
        }
      }
   }

  Simul <-  x[["Simulation results"]][["Simulation"]]
  Het_Eff <- x[["Average Heterogenous Effect + Std_err"]][,1]
  Std_Err_Het_Eff <- x[["Average Heterogenous Effect + Std_err"]][,2]
  Y_mom_cond <- x[["Moment Condition (All Estimates)"]][["All Moment Conditions"]][,1]
  W_mom_cond <- x[["Moment Condition (All Estimates)"]][["All Moment Conditions"]][,2]

  obj_list <- list( Simul, Het_Eff, Std_Err_Het_Eff, Y_mom_cond, W_mom_cond )
  names(obj_list) <- c("Simulated mean treatment effects","Simulated Heterogenous effects",
                       "Simulated Heterogenous effect",
                       "Simulation Moment conditions",
                       "Simulation Moment conditions")
  object_x_titles <- c("treatment effects","Heterogenous effect",
                       "Standard errors","Y",
                       "W")

  plottables <- list()

for(i in 1:length(obj_list))
   {
   if(!is.null(palette) && length(palette) >= 5)
   {
      that_one <- palette[i]
   }
   if( coloured.same == FALSE && is.null(palette) )
   {
      that_one <- sample( make_palette, 1 )
   }

   df <- data.frame(obj_list[[i]])
   colnames(df) <- "Treatment_Effect"

   plottables[[i]] <- ggplot2::ggplot(df, ggplot2::aes( x = Treatment_Effect)) +
      ggplot2::geom_density( fill = that_one, alpha = 0.7 ) +
      ggplot2::geom_vline(ggplot2::aes( xintercept = mean( Treatment_Effect )),
                          color = "black", linetype="dashed", size = 1) +
      ggplot2::labs( title = names(obj_list)[i],
                     x = object_x_titles[i],
                     y = "Density") +
      ggplot2::theme(
         axis.text.y = ggplot2::element_blank(),
         axis.ticks.y = ggplot2::element_blank(),
         axis.title.x = ggplot2::element_text( size = 14),
         title = ggplot2::element_text( size = 14))

}

return(plottables)
}


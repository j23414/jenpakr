#! /usr/bin/env Rscript

library(magrittr)

#' Converts HI Titer values to generalized mean titer (GMT)
#' @param hi_titers HI Titer values, assumes all titers are above 0
#' @return The GMT titer = log(HI titer/ 10, 2)
#' @examples
#' HI_to_GMT(c(1:10*100))
#' @export
HI_to_GMT <- function(hi_titers){
  gmt <- log(hi_titers/10, 2)
  return(gmt)
}

#' Calculate and plot the correlation of a given dataset
#' @param df Dataframe containing HI titer values, each column is a different strain
#' @param corr Correlation method "spearman" "pearson" "kendall", spearman is default
#' @param xlevels Can order or limit the levels on x axis (not done yet)
#' @param ylevels Can order or limit the levels on y axis (not done yet)
#' @return The ggplot of the correlation heatmap
#' @export
corrplot <- function(df, corr="spearman", xlevels=NULL, ylevels=NULL){
  # Calculate correlation and prep data for plotting
  cdf <- df %>% cor(., method=corr) %>% reshape2::melt(.) %>%
    dplyr::mutate(
      Strain1=Var1,
      Strain2=Var2,
      Corr=value,
      Var1=NULL,
      Var2=NULL,
      value=NULL
    )
  
  # Plot correlation
  p<-cdf %>% 
    ggplot2::ggplot(. , ggplot2::aes(x=Strain1, y=Strain2, fill=Corr))+
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label=round(Corr, digits = 1)), size = 3) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0)) +
    ggplot2::scale_fill_gradient2(low="#0000FF", mid = "#FFFFFF", midpoint=0, high = "#FF0000") +
    ggplot2::labs(title=paste(corr, " correlation", sep=""),
         x="",y="", fill="")
  return(p)
}

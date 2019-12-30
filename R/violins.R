#! /usr/bin/env Rscript

library(magrittr)

#' Generate violin plots of a given dataset
#' @param df Dataframe containing HI titer values, each column is a different strain
#' @param ids contain the annotation columns, not to be melted
#' @return The ggplot of the violins
#' @export
violinplot <- function(df, ids=NULL){
  # Prep data for plotting
  cdf <- df %>% reshape2::melt(.) %>%  #reshape2::melt(., id=ids)
    dplyr::mutate(
      Strain = variable,
      HITiter = value,
      variable = NULL,
      value = NULL
    )
  
  # Plot violins
  maxtiter = max(cdf$HITiter)+1
  mintiter = min(0, cdf$HITiter[cdf$HITiter != -Inf])
  p<-cdf %>% 
    ggplot2::ggplot(. ,ggplot2::aes(x = Strain,
                           y = HITiter, 
                           fill = Strain)) +
    ggplot2::geom_violin(ggplot2::aes(color=Strain))+
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "darkgray")+
    ggplot2::geom_boxplot(width = 0.2)+
    ggplot2::geom_jitter(width = 0.2, size = 0.6) +
    ggplot2::facet_grid(~Strain, space = "free", scale = "free")+
    ggplot2::theme_minimal()+
    ggplot2::theme(strip.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(colour = "black",fill="NA"),
          legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(breaks=c(mintiter:maxtiter), limits=c(mintiter, maxtiter)) %>%
    ggplot2::labs(x="")
    #labs(y="GMT")
  return(p)
}

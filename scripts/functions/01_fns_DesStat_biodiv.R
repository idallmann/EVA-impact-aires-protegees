library(ARTofR)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##        FUNCTIONS FOR DESCRIPTIVE STATISTICS RELATED TO BIODIVERSITY      ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------------------- FOREST-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Plotting the evolution of forest cover for a given PA
stat_treeloss_id = function(df, id, name_pa, treatment_yr)
{
  df_id = df %>%
    subset(wdpaid == id)
  
  #Evolution of tree loss 
  fig_evo = ggplot(data = df_id,
                   aes(x=years,y=loss)) %>%
    + geom_line(color = "#3182BD") %>%
    + geom_point(color = "#3182BD") %>%
    #Add a vertical line for treatment year
    + geom_vline(xintercept = treatment_yr, color = "#FB6A4A" ) %>%
    + labs(title = paste("Evolution du couvert forestier -", name_pa),
           x = "Année",
           y = "Perte de couvert forestier (%)",
           caption = "La perte de couvert forestier est estimée par-rapport à l'année précédente.\nL'année d'octroi est marquée par une ligne verticale") %>%
    + theme(legend.position = "bottom",
            legend.key = element_rect(fill = "white"),
            plot.title = element_text(size = 14, face = "bold"), 
            axis.text.x = element_text(angle = 0,size=9, hjust = .5, vjust = .6),
            panel.background = element_rect(fill = 'white', colour = 'white', 
                                            linewidth = 0.5, linetype = 'solid'),
            panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
            panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
            plot.caption = element_text(color = 'grey50', size = 10, face = 'plain', hjust = 0))
  
  #Evolution of tree loss by intervals of five years
  fig_evo_5yr = ggplot(data = df_id,
                       aes(x=years_regroup,y=moy_5)) %>%
    # + geom_line(color = "#3182BD") %>%
    # + geom_point(color = "#3182BD") %>%
    + geom_bar(fill = "#3182BD", stat = "identity") %>%
    #Add a vertical line for treatment year
    #+ geom_vline(xintercept = treatment_yr, color = "#FB6A4A" ) %>%
    + labs(title = paste("Evolution du couvert forestier (5 ans) -", name_pa),
           x = "Année",
           y = "Perte moyenne de couvert forestier (%)",
           caption = paste("Année d'octroi du financement :", treatment_yr)) %>%
    + theme(legend.position = "bottom",
            legend.key = element_rect(fill = "white"),
            plot.title = element_text(size = 14, face = "bold"), 
            axis.text.x = element_text(angle = 0,size=9, hjust = .5, vjust = .6),
            panel.background = element_rect(fill = 'white', colour = 'white', 
                                            linewidth = 0.5, linetype = 'solid'),
            panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
            panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
            plot.caption = element_text(color = 'grey50', size = 10, face = 'plain', hjust = 0))
  
  #Store figures in a list and return it
  list_fig = list(fig_evo, fig_evo_5yr)
  return(list_fig)
    
}

stat_treeloss_reg = function(df, reg, name_reg) 
{
  df_reg = df %>% 
    subset(drct == reg) %>%
    subset(!is.na(loss_region))
  
  fig_evo_reg = ggplot(data = df_reg,
                       aes(x = years, y = loss_region)) %>%
    + geom_line(color = "#3182BD") %>%
    + geom_point(color = "#3182BD") %>%
    + labs(title = "Evolution moyenne du couvert forestier au sein des AP par année",
           subtitle = name_reg,
           x = "Année",
           y = "Perte de couvert forestier (%)",
           caption = "La perte de couvert forestier est estimée par-rapport à l'année précédente") %>%
    + theme(legend.position = "bottom",
            legend.key = element_rect(fill = "white"),
            plot.title = element_text(size = 14, face = "bold"), 
            plot.subtitle = element_text(face = "bold"),
            axis.text.x = element_text(angle = 0,size=9, hjust = .5, vjust = .6),
            panel.background = element_rect(fill = 'white', colour = 'white', 
                                            linewidth = 0.5, linetype = 'solid'),
            panel.grid.major = element_line(colour = 'grey90', linetype = 'solid'),
            panel.grid.minor = element_line(colour = 'grey90', linetype = 'solid'),
            plot.caption = element_text(color = 'grey50', size = 10, face = 'plain', hjust = 0))
  
  return(fig_evo_reg)
}

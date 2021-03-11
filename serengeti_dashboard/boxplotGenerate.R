
source('helpers.R')

boxplotCreate <- function(
  ss_data,
  species_input,
  standing_input,
  resting_input,
  moving_input,
  eating_input,
  interacting_input,
  babies_input,
  habitat_input,
  date_input,
  y_input) {
  
  ss_data_filtered <- filterSerengetiData(
    ss_data,
    species_input,
    standing_input,
    resting_input,
    moving_input,
    eating_input,
    interacting_input,
    babies_input,
    habitat_input,
    date_input) 
  
  p <- ss_data_filtered %>%
    ggplot(aes_string(x = 'Species', y = y_input)) +
    geom_boxplot(aes(color = Species)) +
    scale_color_manual(values = cbPalette) +
    theme_minimal() +
    scale_x_discrete(labels = label_wrap(10)) +
    theme(axis.title = element_text(face = "bold", size = 24),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(face = "bold", size = 24),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  return(p)
}

#============================================================================



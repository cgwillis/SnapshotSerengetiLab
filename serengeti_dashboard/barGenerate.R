
source('helpers.R')

barplotCreate <- function(
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
  x_input,
  y_input) {
  
  summary_data <- filterSerengetiData(
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
  
  summary_data <- summary_data %>%
    group_by(Species, !!(as.symbol(x_input))) %>%
    dplyr::count(name = "Count") %>%
    ungroup() %>%
    group_by(Species) %>%
    complete(!!(as.symbol(x_input)), fill = list(Count = 0)) %>%
    mutate(Frequency = round(Count/sum(Count), 3))
  
  p <- summary_data %>% 
    ggplot(aes_string(x = x_input, y = y_input)) +
    geom_col(aes(fill = Species), position = position_dodge()) +
    geom_text(aes(group = Species, label = paste0("N = ", Count, "\nFreq = ", round(Frequency, 2))),
              vjust = -0.25, size = 4, fontface = 'bold', position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = cbPalette) +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0, .3))) +
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
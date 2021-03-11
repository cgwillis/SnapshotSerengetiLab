
source('helpers.R')

histoplotCreate <- function(
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
  x_input) {
  
  filterData <- filterSerengetiData(
    ss_data,
    species_input,
    standing_input,
    resting_input,
    moving_input,
    eating_input,
    interacting_input,
    babies_input,
    habitat_input,
    date_input
  )
  
  
  group_mean_data <- filterData %>% 
  group_by(Species) %>%
  dplyr::summarise(group_mean = mean(!!(as.symbol(x_input))))
  
  p <- filterData  %>%
      ggplot(aes_string(x = x_input)) +
      geom_histogram(aes(color = Species), fill = "white") +
      geom_vline(data = group_mean_data, 
                 aes(xintercept = group_mean, color = Species), linetype = "dashed") +
      scale_color_manual(values = cbPalette) +
      theme_minimal() +
      labs(y = "Count",
           caption = "Dashed vertical lines show means") +
      theme(axis.title = element_text(face = "bold", size = 24),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 15, l = 0)),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            legend.position = "none",
            plot.margin = unit(c(1,1,1,1), "cm"),
            plot.caption = element_text(hjust = 0, face = "italic", size = 16),
            plot.caption.position = "plot",
            strip.background = element_rect(fill = "grey90"),
            strip.text = element_text(size = 20, face = "bold")) +
      facet_wrap(~ Species, ncol = 1)
  
  return(p)
}
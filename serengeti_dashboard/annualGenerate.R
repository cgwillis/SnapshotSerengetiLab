
source('helpers.R')

annualplotCreate <- function(
  ss_data,
  species_input,
  standing_input,
  resting_input,
  moving_input,
  eating_input,
  interacting_input,
  babies_input,
  habitat_input,
  year_input,
  y_input
) {

  summary_data <- ss_data %>%
    filter(Species %in% species_input) %>%
    filter(Standing %in% standing_input) %>%
    filter(Resting %in% resting_input) %>%
    filter(Moving %in% moving_input) %>%
    filter(Eating %in% eating_input) %>%
    filter(Interacting %in% interacting_input) %>%
    filter(Babies %in% babies_input) %>%
    filter(Habitat %in% habitat_input) %>%
    filter(Year == year_input)
  
  summary_data <- summary_data %>%
    mutate(Month=format(Date, '%m')) %>%
    mutate(Month=as.numeric(Month))
  
  monthIndices <- seq(1, 12)
  speciesMonths <- data.frame(species_input, monthIndices)
  colnames(speciesMonths) <- c('Species', 'Month')
  speciesMonths <- speciesMonths %>%
    expand(Species, Month)
  
  summary_data <- summary_data %>%
    group_by(Species, Month, Year) %>%
    dplyr::count(name="Count") %>%
    ungroup() %>%
    group_by(Species) %>%
    full_join(speciesMonths) %>%
    complete(fill = list(Count = 0)) %>%
    mutate(Frequency = round(Count/sum(Count), 3))
  
  
  p <- summary_data %>% 
    ggplot(aes_string(x = 'Month', y = y_input)) +
    geom_col(aes(fill = Species), position = position_dodge()) +
    scale_fill_manual(values = cbPalette) +
    theme_minimal() +
    scale_x_continuous(breaks=seq(1, 12), ) +
    theme(axis.title = element_text(face = "bold", size = 14),
          #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text.x = element_text(size=8),
          #axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size=12),
          legend.text = element_text(size = 12),
          legend.title = element_text(face = "bold", size = 12),
          plot.margin = unit(c(1,1,1,1), "cm"),
          panel.spacing.y = unit(1, "lines")) +
    facet_grid(rows=vars(Species), scales='free_y')
  
  return(p)
}
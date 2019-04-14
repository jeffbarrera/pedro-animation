library(plyr)
library(dplyr)
library(tidyr)
library(gganimate)

# read in dataset
setwd('~/Documents/Personal/pedro-animation')
cumulative <- read.csv('data/cumulative-papers.csv')

# clean up category names
colnames(cumulative) <- c('Year', 'Musculoskeletal', 'Cardiorespiratory', 'Neurology', 'Gerontology', 'Continence & Women\'s Health', 'Paediatrics', 'Orthopaedics', 'Oncology', 'Sports', 'Ergonomics & Occupational Health')

# reshape dataset to one row per category per year
cum.long <- gather(cumulative, category, num_papers, 'Musculoskeletal':'Ergonomics & Occupational Health', factor_key=TRUE)

# for each year, rank the categories by number of papers
ranked.cats <- cum.long %>%
		group_by(Year) %>%
		mutate(rank=rank(num_papers))

# build graph
graph <- ggplot(ranked.cats, aes(rank, group = category, fill = as.factor(category), color = as.factor(category))) +
	geom_tile(aes(y = num_papers/2, height = num_papers, width = 0.9), color = NA) +
	coord_flip(clip = "off", expand = FALSE) +
	geom_text(aes(y = 0, label = paste(category, " ")), vjust = 0.2, hjust = 1) +
	scale_y_continuous(labels = scales::comma) +
	guides(color = FALSE, fill = FALSE) +
	labs(title='{closest_state}', x = "", y = "Cumulative number of records indexed on PEDro") +
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,6, "cm")) +
	transition_states(Year, transition_length = 4, state_length = 0, wrap = FALSE) +
	ease_aes('cubic-in-out')

animate(graph, fps = 30, duration = 15, width = 1000, height = 600)
anim_save('graph3.gif', path='~/Documents/Personal/pedro-animation')
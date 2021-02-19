library(tidyverse)

write_csv(freed_slaves, "freed.csv")
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
write_csv(freed_slaves, "freed.csv")
freed_slaves <- read_csv("freed.csv")

freed_slaves <- freed_slaves %>%
  pivot_longer(2:3, names_to = "Status", values_to = "Percentage")

theme <- theme(
  plot.title = element_text(hjust = .5),
  panel.background = element_rect( fill = NA),
  plot.background = element_rect(fill = "#dfd2c6",
                                 color = "#dfd2c6"),
  #panel.grid.major = element_blank(),
  #panel.grid.major.y = element_line(color = "#0e0e0e"),
  #panel.grid.minor = element_blank(),
  legend.position = "none"
)

ggplot(freed_slaves) +
  geom_area(aes(Year, Percentage, fill = Status)) +
  labs(title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES.\n\nPROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE.\n\n DONE BY ATLANTA UNIVERSITY.") +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c("#238455", "#0e0e0e")) +
  scale_x_continuous(position = "top", expand = c(0, 1), breaks = c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870)) +
  scale_y_continuous(expand = c(0, 0), breaks = NULL) +
  coord_cartesian(clip = "off") +
  geom_text(aes(1791, 93), label = "8%") +
  geom_text(aes(1801, 90), label = "11%") +
  geom_text(aes(1811, 88), label = "13.5%") +
  geom_text(aes(1821, 88), label = "13%") +
  geom_text(aes(1831, 87.5), label = "14%") +
  geom_text(aes(1841, 88), label = "13%") +
  geom_text(aes(1851, 89.5), label = "12%") +
  geom_text(aes(1861, 90), label = "11%") +
  geom_text(aes(1868, 90), label = "100%") +
  geom_text(aes(1830, 65), color = "#daccbd", size = 12, label = "SLAVES \nESCLAVES") +
  theme

ggsave("freed_slaves.png", height = 12, width = 8)


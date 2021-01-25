# TidyTuesday 2021 Week 4 
# 2019 Kenya Census

# Start: 12:30pm
# Stop: 3:00
# Start:6:15
# Stop: :6:45
# Start: 12:45
# Stop: 2:00
# Start: 4:30
# End: 5:00

# Total time: 4 hours and 45 minutes

#libraries


library(sf)
library(rKenyaCensus)
library(cowplot)
library(raster)
library(rnaturalearth)
library(tidyverse)


#data

ag <- read_csv("original/V4_T2.20_Agriculture.csv")
shop <- read_csv("original/V4_T2.34_Online_Shopping.csv")
counties <- st_read("original/kenya-county/Shapefile/ke_county.shp")

# Africa map data

africa = ne_countries(continent = "Africa")
kenya = raster::getData('GADM', country = "KEN", level = 1)
crs(kenya) = crs(africa)
kenya_sf = st_as_sf(kenya)
africa_sf = st_as_sf(africa)

#quick check of shapefiles
plot(counties)


# grab just the counties
ag_counties <- ag %>%
  filter(AdminArea == "County")
shop_counties <- shop %>%
  filter(AdminArea == "County")



# join ag, shopping, and counties datasets

ag_shop <- left_join(ag_counties, shop_counties, by = "County") %>%
  #trim variables
  dplyr::select(County, Population = Total.y, Farming, OnlineShopping = SearchedOnline_Total, LivestockProduction) %>%
  #ensure I'm using same base population, over 15 year old
  mutate(Farming_Perc = Farming/Population, OnlineShopping_Perc = OnlineShopping/Population, Livestock_Perc = LivestockProduction/Population)

counties <- counties %>% mutate(County = toupper(county)) %>%
  mutate(County = as.character(County)) %>%
  mutate(County = replace(County, County == "ELGEYO-MARAKWET", "ELGEYO/MARAKWET")) %>%
  mutate(County = replace(County, County == "NAIROBI", "NAIROBI CITY")) %>%
  mutate(County = replace(County, County == "TAITA TAVETA", "TAITA/TAVETA"))

counties$County <- counties[County=="ELGEYO-MARAKWET"] <- "ELGEYO/MARAKWET"
sort(ag_shop$County)
sort(counties$County)

df <- left_join(counties, ag_shop, by = c("County" = "County"))

class(df)

# analysis

# What is the relationship between farming and internet shopping in Kenyan counties?
ggplot(df) +
  geom_point(aes(OnlineShopping_Perc, Farming, size = Population)) +
  geom_smooth(aes(OnlineShopping_Perc, Farming))

# There seems to be a slight negative correllation between online shopping and farming but it's not as strong as I would have expected. 
# It's also largely driven by Nairobi. I wonder where those places on the left that have very low online shopping and farming are located?

ggplot(df) +
  geom_sf(aes(fill = Farming_Perc))

# So most of the farming tends to happen in the southwest and center

ggplot(df) +
  geom_sf(aes(fill = OnlineShopping_Perc))

#As expected, most of the online shopping is happening in Nairobi and the center, but there is also a lot more online shopping in the southeast. 
# The northwest (Somali territory) has very little of both, which sort of makes sense. 

ggplot(df) +
  geom_sf(aes(fill = Livestock_Perc))

# A little more livestock production going on in northwest

# It's important to get a sense where the people are too

ggplot(df) +
  geom_sf(aes(fill = Population))

st_write(df, "processed/kenya_df.shp")


# plot

kenya.color = ifelse(africa_sf$sovereignt == "Kenya", "#2D708EFF", "grey30")

(kenya_map <- ggplot(africa_sf) +
  geom_sf(fill = kenya.color, color = "grey70") +
  theme(plot.background = element_rect(fill = "gray90", color = NA),
        panel.background = element_rect(fill = "gray90"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_blank(),
        axis.ticks = element_blank()))

#geom_hline(aes(yintercept =  mean(Farming_Perc)), color = "#2D708EFF", alpha = .5) +
#geom_vline(aes(xintercept = mean(OnlineShopping_Perc)), , color = "#2D708EFF", alpha = .5) +

(scatter <- ggplot(df) +
  geom_point(aes(OnlineShopping_Perc, Farming_Perc, size = Population), color = "#2D708EFF", alpha = .5) +
 # geom_point(aes(OnlineShopping_Perc, Farming_Perc, size = Population), shape = 1, colour = "black") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(labels = scales::comma) +
  #theme_bw() +
  theme(plot.title = element_text(size = 24),
        plot.background = element_rect(fill = "gray90", color = "gray90", size = 2),
        panel.background = element_rect(fill = "gray90"),
        #panel.grid = element_rect(fill = "gray90", color = "gray90"),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),
        legend.key = element_rect(fill = "transparent"),
        plot.margin = margin(20,20,20,20),
        axis.title.y = element_text(margin = margin(r=20), size = 15),
        axis.title.x = element_text(margin = margin(t=20), size = 15),
        legend.position = "top") +
  labs(title = "Who's on the farm and who's on Amazon?",
       subtitle = "Population farming vs online shopping in Kenya counties, 2019 census", 
       y = "% of population over 15 engaged in farming",
       x = "% of population over 15 who has shopped online",
       size = "County population",
       caption = "Visual: Orion Wilcox | #TidyTuesday | Source: rKenyaCensus") +
  scale_color_viridis_c(option = "viridis") +
  geom_text(aes(x = .135, y = .025), label = "Nairobi") +
 # geom_text(aes(x = .015, y = .08), label = "Mostly counties\n in the northwest") +
  #geom_text(aes(x = .042, y = .11), label = "Averge percent ever\n shopped online\n (3%)") +
  #geom_text(aes(x = .13, y = .26), label = "Averge percent farmers\n (25%)") +
  guides(color = "none"))

ggsave("scatter.png")

(farming <- ggplot(df) +
  geom_sf(aes(fill = Farming_Perc), color = "transparent") +
  theme(plot.background = element_rect(fill = "gray90", color = NA),
        panel.background = element_rect(fill = "gray90"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis_c(option = "viridis", labels = scales::percent) +
  labs(fill = "Percent farming"))

ggsave("farming.png")

(online <- ggplot(df) +
  geom_sf(aes(fill = OnlineShopping_Perc), color = "transparent") +
  theme(plot.background = element_rect(fill = "gray90", color = NA),
        panel.background = element_rect(fill = "gray90"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis_c(option = "viridis", labels = scales::percent) +
  labs(fill = "Percent ever\n shopped online"))

ggsave("online.png")

(charts <- plot_grid(scatter, plot_grid(farming, online, ncol=1, align = "hv"), ncol=2, rel_widths = c(2.5,1)) +
   theme(panel.background = element_rect(fill = "gray90"),
         panel.border = element_blank(),
        plot.margin = margin(20,20,20,20)))

ggsave("comboII.png", width = 12, height = 8)

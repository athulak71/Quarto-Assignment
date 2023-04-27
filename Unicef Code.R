#-----------------------------------------------
#Required Packages

# install.packages("stringr", "ggplot2","maps", "gridExtra","plotly","scales",
#                  "RColorBrewer","ggiraph","tidyr","dplyr","Hmisc","forcats","ggthemes","ggrepel","mapproj)

#--------------------------------------------
#Importing Libraries
library(readxl)
library(ggplot2)
library(maps)
# library(tidyverse)
library(gridExtra)
library(plotly)
library("scales") 
library("stringr") 
library("Hmisc") 
library("forcats") 
library("ggthemes") 
library(RColorBrewer)
library(ggiraph)
library(tidyr)
library(dplyr)
library(ggrepel)

#------------------------------------------------------------------------
#' ---
#' title: " UNICEF Assignment Story"
#' subtitle: "UNICEF data set Indicator 2 merged with metadata"
#' date: "27 April 2023"
#' output: "html_document"
#' ---
#' # Story

#' The visualisations provided below offer a range of insights into various aspects of countries worldwide. 
#' From economic indicators such as GDP and inflation rates to health-related issues such as Vitamin A deficiency, these visualisations 
#' help to paint a picture of different countries' conditions. Moreover, by presenting data in an easily understandable format, 
#' these visualisations can help policymakers and individuals make informed decisions and gain a deeper understanding of the challenges 
#' facing different nations.

#' The dataset helps to compare the GDP of the countries in the world over the period of time. The Time Series visualisation allows the 
#' viewer to easily see the relative size of the GDPs of the countries and the observation values of Vitamin Deficiency. 

#' The bar graph displays the trend in Vitamin A deficiency. The chart shows the stacked version by GDP and observation Values for Vitamin deficiency. 
#' This visualisation helps the viewer understand the prevalence of Vitamin A deficiency across different time frame and how it has changed 
#' over time along with the GDP values in 10 thousand for respective year.

#' In conclusion, the visualisations presented below provide valuable insights into the conditions of different countries worldwide. 
#' They highlight the wide range of challenges these nations face, from economic issues such as inflation and GDP growth and health issues such as 
#' Vitamin A deficiency. By providing a clear and concise view of  the data, these visualisations help to facilitate better decision-making 
#' and a deeper understanding of the issues facing different nations. 
#' These visualisations are essential for policymakers and individuals, providing valuable insights into the world's diverse and complex conditions.

#-----------------------------------------------------------------------
#Importing Files

indict_1_grpd <- read.csv("/cloud/project/unichef_data_indicator_1_grpd.csv", header = TRUE)
View(indict_1_grpd)

indict_1_grpd$population_million <- indict_1_grpd$Population/ 1000000

indict_1_grpd$gdp_in_10k <- indict_1_grpd$GDP/ 10000

indict_1_grpd$obs_val_in_10k <- indict_1_grpd$obs_true_val / 10000
#--------------------------------------------
#Time Series Code
time_s <- select(indict_1_grpd, time_period, population_million, obs_true_val, GDP)
View(time_s)

time_s_grp <- time_s %>% group_by(time_period) %>% 
  summarise(avg_population_million= mean(population_million),
            avg_obs_true_val = mean(obs_true_val),
            avg_gdp = mean(GDP),
            .groups = 'drop')
View(time_s_grp)

f_obs_val<- ggplot(data = time_s_grp, aes(x = time_period, y = avg_obs_true_val))+ geom_line(color = "black", size = 2) +
  theme(panel.background = element_rect(fill='grey'))+ 
  scale_x_continuous(breaks=seq(2000,2020, by = 2))+ 
  ggtitle("Time Series by observation value of Vitamin Deficency") +
  xlab("year") + ylab("obs_val")

f_gdp<- ggplot(data = time_s_grp, aes(x = time_period, y = avg_gdp))+ geom_line(color = "black", size = 2)+
  theme(panel.background = element_rect(fill='grey'))+
  scale_x_continuous(breaks=seq(2000,2020, by = 2))+ 
  ggtitle("Time Series by GDP of Vitamin Deficency") +
  xlab("year") + ylab("gdp")
grid.arrange(f_obs_val,f_gdp, ncol = 1)

#------------------------------------------------------------------
#Bar Plot

indict_1_grp_new <- indict_1_grpd %>% group_by(time_period) %>% 
  summarise(mean_obs_true_val = mean(obs_true_val),
            mean_gdp = mean(GDP),
            .groups = 'drop')
View(indict_1_grp_new)


fig_1 <- plot_ly(indict_1_grp_new, x = ~time_period, y = ~mean_obs_true_val, type = 'bar', name = 'obs_val')
fig_1 <- fig_1 %>% add_trace(y = ~mean_gdp, name = 'gdp in 10k')
fig_1 <- fig_1 %>% layout(title = 'Year by Obs Value & Gdp of Vitamin Deficiency', plot_bgcolor = "#e5ecf6", 
                          xaxis = list(title = 'Year'),yaxis = list(title = 'Values'), barmode = 'stack')%>% 
  layout(xaxis = list(categoryorder = "total descending"))

fig_1
#---------------------------------------------------------------------
#Scatter Plot

indict_1_grp_sctr <- indict_1_grpd %>% group_by(country) %>% 
  summarise(mean_obs_true_val = mean(obs_true_val),
            mean_gdp = mean(GDP),
            mean_population_million = mean(population_million),
            .groups = 'drop')
View(indict_1_grp_sctr)

fit_model <- lm(mean_obs_true_val ~ mean_gdp, data = indict_1_grp_sctr)

indict_1_grp_sctr %>% 
  plot_ly(x = ~mean_gdp) %>% 
  add_markers(y = ~mean_obs_true_val) %>% 
  add_lines(x = ~mean_gdp, y = fitted(fit_model))%>%
  layout(showlegend = F)%>%
  layout(title = 'Scatter plot of Mean GDP and Mean Observation Values', plot_bgcolor = "#e5ecf6")

#----------------------------------------------------------------------------------------------
#World Map by obs value and countries

wd <- map_data('world') %>% fortify
View(wd)

wrld_data_df <- select(indict_1_grpd, region = country, "obs_value" = obs_true_val)
View(wrld_data_df)

world_df_1 <- left_join(wrld_data_df, wd, by = "region")
View(world_df_1)

fig_map <- ggplot() +
  geom_map(data = wd, map = wd,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = wrld_data_df, map=wd, 
           aes(fill=obs_value,map_id=region),
           colour="#7f7f7f", size=0.5) + geom_text_repel() +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Obs Value", title="World Map", x="", y="") + 
  theme_bw() 

fig_map



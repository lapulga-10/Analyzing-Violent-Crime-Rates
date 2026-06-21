# Packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Standard Normal --------------------------------------------------------------
x = seq(-20,20,0.1)

standard_normal = data.frame(x = x, y = dnorm(x,sd = 10))

standard_normal_plot = standard_normal %>%
  ggplot(aes(x,y)) +
  geom_line(linewidth = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 30),
        axis.text.y = element_text(color = "grey20", size = 30),  
        axis.title.x = element_text(color = "grey20", size = 30),
        axis.title.y = element_text(color = "grey20", size = 30)) +
  labs(x = "", y = "")

ggsave(plot = standard_normal_plot, filename = "ImagesForPresentation/standard_normal_prior.png")


# Normal -----------------------------------------------------------------------

x2 = seq(-20,20,0.1)
normal = data.frame(x = x2, y = dnorm(x2, mean = -2.75,sd = 10))

normal_plot = normal %>%
  ggplot(aes(x2,y)) +
  geom_line(linewidth = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 30),
        axis.text.y = element_text(color = "grey20", size = 30),  
        axis.title.x = element_text(color = "grey20", size = 30),
        axis.title.y = element_text(color = "grey20", size = 30)) +
  labs(x = "", y = "")

ggsave(plot = normal_plot, filename = "ImagesForPresentation/normal_prior.png")

# Gamma ------------------------------------------------------------------------
gamma = data.frame(x = x+17.5, y = dgamma(x = x+17.5, shape = 2, rate = 1))

gamma_plot = gamma %>%
  ggplot(aes(x,y)) +
  geom_line(linewidth = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 30),
        axis.text.y = element_text(color = "grey20", size = 30),  
        axis.title.x = element_text(color = "grey20", size = 30),
        axis.title.y = element_text(color = "grey20", size = 30)) +
  labs(x = "", y = "")

ggsave(plot = gamma_plot, filename = "ImagesForPresentation/gamma_prior.png")

# Normal2 ----------------------------------------------------------------------
x1 = seq(-30,30,0.1)
normal2 = data.frame(x = x1, y = dnorm(x1, mean = 0, sd = 10))

normal2_plot = normal2 %>%
  ggplot(aes(x,y)) +
  geom_line(linewidth = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 20),
        axis.text.y = element_text(color = "grey20", size = 20),  
        axis.title.x = element_text(color = "grey20", size = 20),
        axis.title.y = element_text(color = "grey20", size = 20)) +
  labs(x = "", y = "")

ggsave(plot = normal2_plot, filename = "ImagesForPresentation/normal2_prior.png")

# Normal3 ----------------------------------------------------------------------
normal3 = data.frame(x = x1, y = dnorm(x1, mean = 0, sd = 5))

normal3_plot = normal3 %>%
  ggplot(aes(x,y)) +
  geom_line(linewidth = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 20),
        axis.text.y = element_text(color = "grey20", size = 20),  
        axis.title.x = element_text(color = "grey20", size = 20),
        axis.title.y = element_text(color = "grey20", size = 20)) +
  labs(x = "", y = "")

ggsave(plot = normal3_plot, filename = "ImagesForPresentation/normal3_prior.png")


# Dataset
################################################################################

source("loadData.R")

# Histogram --------------------------------------------------------------------
histogram_crimeRate = data %>%
  ggplot(aes(x = ViolentCrimesPerPop)) +
  geom_histogram(binwidth = 0.01) +
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 40),
        axis.text.y = element_text(color = "grey20", size = 40),  
        axis.title.x = element_text(color = "grey20", size = 40),
        axis.title.y = element_text(color = "grey20", size = 40)) +
  labs(x = "Normalized Crime Rate", y = "Frequency")

ggsave(plot = histogram_crimeRate, filename = "ImagesForPresentation/histogram_crimeRate.png")

# Predictor vs. Target ---------------------------------------------------------
epsilon <- 1e-9
logit_transform <- function(y) log(y / (1 - y))

data_modified <- data %>% 
  select(ViolentCrimesPerPop,PctKids2Par,PctIlleg,PctFam2Par,racePctWhite,PctYoungKids2Par) %>%
  mutate(y_logit = logit_transform(pmax(pmin(ViolentCrimesPerPop, 1 - epsilon), epsilon)))

data_long <- data_modified %>%
  pivot_longer(cols = c(PctKids2Par,PctIlleg,PctFam2Par,racePctWhite,PctYoungKids2Par),
               names_to = "Predictor", 
               values_to = "X_value")


# Scatter plots of predictors vs y
scatter_plot_x_y <- ggplot(data_long, aes(x = X_value, y = ViolentCrimesPerPop)) +
  geom_point(alpha = 0.5, color = "red", size = 1) +
  facet_wrap(~Predictor, scales = "free_x") + 
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 32,
                                   angle = 45, vjust = 0.7, hjust=0.7),
        axis.text.y = element_text(color = "grey20", size = 32),  
        axis.title.x = element_text(color = "grey20", size = 32),
        axis.title.y = element_text(color = "grey20", size = 32),
        strip.text = element_text(size=32, color = 'black')) +
  scale_x_continuous(breaks = seq(min(data_long$X_value), max(data_long$X_value), length.out = 3)) + 
  scale_y_continuous(breaks = seq(0, 1, length.out = 3)) +  
  labs(x = "Predictor Value", y = "Normalized Crime Rate")

ggsave(plot = scatter_plot_x_y, filename = "ImagesForPresentation/scatter_plot_x_y.png")



# Scatter plots of predictors vs logit(y)
scatter_plot_x_logity <- ggplot(data_long, aes(x = X_value, y = y_logit)) +
  geom_point(alpha = 0.5, color = "blue", size = 1) +
  facet_wrap(~Predictor, scales = "free_x") + 
  theme_bw() +
  theme(axis.text.x = element_text(color = "grey20", size = 32,
                                   angle = 45, vjust = 0.7, hjust=0.7),
        axis.text.y = element_text(color = "grey20", size = 32),  
        axis.title.x = element_text(color = "grey20", size = 32),
        axis.title.y = element_text(color = "grey20", size = 32),
        strip.text = element_text(size=32, color = 'black')) +
  scale_x_continuous(breaks = seq(min(data_long$X_value), max(data_long$X_value), length.out = 3)) + 
  scale_y_continuous(breaks = c(-15,0,15)) +  
  labs(x = "Predictor Value", y = "logit(Normalized Crime Rate)")

ggsave(plot = scatter_plot_x_logity, filename = "ImagesForPresentation/scatter_plot_x_logity.png")


library(tidyverse)
library(stargazer)

data <- read_csv("your file here", 
                 col_types = cols(
                   `Ease of doing business rank (1=most business-friendly regulations) [IC.BUS.EASE.XQ]` = col_double(),
                   `Tax revenue (% of GDP) [GC.TAX.TOTL.GD.ZS]` = col_double(),
                   `GDP per capita (current US$) [NY.GDP.PCAP.CD]` = col_double()))
data <- data %>%
  select(-(starts_with("Time")), -(`Country Code`))

names(data) <- sub(" \\[.*", "", names(data))

data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `GDP per capita (current US$)`)) +
  geom_point() +
  geom_smooth(method = "lm")

data %>%
  ggplot(aes(`Ease of doing business rank (1=most business-friendly regulations)`, 
             `Tax revenue (% of GDP)`)) +
  geom_point() +
  geom_smooth(method = "lm")

reg <- lm(formula, data)

summary(reg)

data %>%
  ggplot(aes(`Tax revenue (% of GDP)`, 
             `GDP per capita (current US$)`, 
             alpha = `Ease of doing business rank (1=most business-friendly regulations)`)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = FALSE) +
  ylim(0, 150000) +
  labs(title = "GDP per capita and Tax Revenue (2019)",
       subtitle = "World Bank data - Europe and Central Asia",
       alpha = "Ease of doing\nbusiness") +
  theme(legend.position = c(.85, .75),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

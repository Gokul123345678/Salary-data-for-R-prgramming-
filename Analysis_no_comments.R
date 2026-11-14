library(tidyverse)

salary <- read_csv("salary_data.csv")
glimpse(salary)
sum(is.na(salary))

eu_af <- salary %>%
  filter(continent_name %in% c("Europe", "Africa")) %>%
  mutate(continent_name = factor(continent_name,
                                 levels = c("Europe", "Africa")))

desc <- eu_af %>%
  group_by(continent_name) %>%
  summarise(n      = n(),
            mean   = mean(average_salary),
            median = median(average_salary),
            sd     = sd(average_salary),
            min    = min(average_salary),
            max    = max(average_salary))
print(desc)

p_box <- ggplot(eu_af, aes(x = continent_name, y = average_salary,
                           fill = continent_name)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white") +
  labs(title = "Average monthly salary in European and African countries",
       x = "Continent",
       y = "Average monthly salary (USD)",
       fill = "Continent") +
  theme_minimal()
print(p_box)
ggsave("boxplot_salary.png", p_box, width = 7, height = 5, dpi = 300)

p_hist <- ggplot(eu_af, aes(x = average_salary, fill = continent_name)) +
  geom_histogram(bins = 15, colour = "white") +
  facet_wrap(~ continent_name, ncol = 1) +
  labs(title = "Distribution of average monthly salary by continent",
       x = "Average monthly salary (USD)",
       y = "Number of countries",
       fill = "Continent") +
  theme_minimal()
print(p_hist)
ggsave("histogram_salary.png", p_hist, width = 7, height = 6, dpi = 300)

shapiro.test(eu_af$average_salary[eu_af$continent_name == "Europe"])
shapiro.test(eu_af$average_salary[eu_af$continent_name == "Africa"])

wilcox.test(average_salary ~ continent_name, data = eu_af,
            alternative = "two.sided", exact = FALSE)

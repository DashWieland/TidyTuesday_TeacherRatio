library(tidyverse)
library(plotly)
library(countrycode)

student_ratio <- readr::read_csv(here::here("/pupilteacherratio.csv"))

student_ratio <- janitor::clean_names(student_ratio)

### This section of code borrowed from Will Chase
### See https://www.williamrchase.com/post/custom-fonts-and-plot-quality-with-ggplot-on-windows/

### Get various country codes from the countrycode pkg 
### This will help us group by continent later
codes <- 
  codelist %>%
  select(iso3c, country.name.en, region, continent)

student_reduced <- 
  student_ratio %>%
  group_by(indicator, location) %>%
  # Join to country codes 
  left_join(., codes, by = c("location" = "iso3c")) %>%
  # Get rid of anything that isn't a continent
  filter(!is.na(continent)) %>%
  select(indicator, location, country, time, value, continent) %>%
  ungroup() %>%
  # From the TidyTuesday clean up code - making the indicators readable
  mutate(indicator = str_remove(indicator, "Pupil-teacher ratio in"),
         indicator = str_remove(indicator, "(headcount basis)"),
         indicator = str_remove(indicator, "\\(\\)"),
         indicator = str_trim(indicator),
         indicator = stringr::str_to_title(indicator))

### Recode the indicator as a factor
student_reduced$indicator <- fct_relevel(student_reduced$indicator, "Tertiary Education", "Post-Secondary Non-Tertiary Education", 
                                      "Upper Secondary Education", "Secondary Education", "Lower Secondary Education",
                                      "Primary Education", "Pre-Primary Education")
### Thanks Will :) 

### This is a mess of a pipe, but stick with me
student_sec_prim <- student_reduced %>%
  # We're comparing primary and secondary schools, so filter to just those
  filter(indicator %in% c("Secondary Education", "Primary Education")) %>%
  # Split the indicator column into two columns - primary and secondary rates
  spread(key = indicator, value = value) %>%
  rename(secondary_ratio = "Secondary Education",
         primary_ratio = "Primary Education") %>%
  # Round year down to the decade
  mutate(decade = time - time %% 10) %>%
  group_by(country, continent, decade) %>%
  # Get the mean rate by decade for primary and secondary schools
  summarise(mean_secondary = round(mean(secondary_ratio, na.rm = TRUE), digits = 1),
            mean_primary = round(mean(primary_ratio, na.rm = TRUE), digits = 1)) %>%
  # Get rid of missing data and records earlier than 2000
  filter(!is.na(mean_secondary),
         !is.na(mean_primary),
         decade >=2000)

### Add a column explaining the ratio of secondary to primary ratios for 
### the tooltip in the plot
student_sec_prim$ratio_text=paste("Primary teachers had", student_sec_prim$prim_sec_ratio, "times <br>as many students as secondary teachers.")

p <- ggplot(data = student_sec_prim,
            aes(x = mean_primary,
                y = mean_secondary,
                color = continent,
                text = paste(ratio_text,"<br>",
                             "Country:", country, "<br>",
                             "Mean Primary P-T Ratio:", mean_primary, "<br>",
                             "Mean Secondary P-T Ratio:", mean_secondary))) +
  geom_point(alpha = .75) +
  xlim(0,100) +
  ylim(0,100) + 
  geom_abline(intercept = 0, slope = 1, color = "#93ACB5") +
  annotate("text",
           x = 35,
           y = 80,
           size = 2,
           label = "Points on this side of the line are countries with
more pupils-per-teacher in secondary schools
than in primary schools") +
  annotate("text",
         x = 77, 
         y = 15, 
         size = 2,
         label = "Points on this side
of the line are countries
with more pupils-per-
teacher in primary schools
than in secondary schools") +
  theme_minimal() +
  labs(
    title = "Are primary school teachers<br> burdened with larger classes?",
    x = "Mean Primary School Pupil-Teacher Ratio",
    y = "Mean Secondary School Pupil-Teacher Ratio",
    colour = "Continent",
    subtitle = "Primary and Secondary pupil-student ratio by country and continent 2000 - 2016") + 
  theme(legend.text = element_text(size = 6),
    plot.title = element_text(face = "bold",
                                  size = 8),
        axis.title = element_text(face = "bold",
                                  size = 6),
        legend.title = element_text(face = "bold",
                                    size = 8), 
        axis.text = element_text(face = "bold",
                                 size = 6),
        axis.ticks = element_line(linetype = "blank"))

### ggplot to plotly
interactive_plot <- plotly::ggplotly(p, tooltip=c("text"))

### Remove padding for Twitter
interactive_plot$sizingPolicy$padding <- "0"

### Save as html widget 
htmlwidgets::saveWidget(
  interactive_plot, "pupil_teacher_ratio.html", libdir = "lib",
  title = "Pupil-Teacher Ratios - Interactive",
  selfcontained = FALSE
)






  


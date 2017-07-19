# Ocean Health Index British Columbia: /prep/spp_ico

<!--This folder describes the methods used to prepare data for _GOALNAME_ for the OHIBC assessment.

More information about this goal is available [here](http://ohi-science.org/goals/#artisanal-fishing-opportunities).

-->

## Data management and citation info

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!

## Directory information

The directories in this goal directory include metadata, R/Rmd scripts, intermediate data processing, and layer outputs for each assessement year (i.e., the year the assessment was conducted, for OHIBC currently only 2017) or scenario (e.g. Howe Sound OHIBC assessment).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

## IUCN vs NatureServe conservation status ranks

* __Rmd file:__ https://github.com/OHI-Science/ohibc/blob/master/prep/spp_ico/iucn_to_ns.Rmd 
* __HTML file:__ https://rawgit.com/OHI-Science/ohibc/master/prep/spp_ico/iucn_to_ns.html

### Summary:

{r compare Natureserve to IUCN2, echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'center', fig.width = 6, fig.height = 4}

mdl <- lm(status_gl_score ~ category_score, data = spp_natureserve)

ns_v_iucn <- ggplot(data = spp_natureserve, aes(x = category_score, y = status_gl_score)) +
  theme(text = element_text(family = 'Helvetica', color = 'gray30', size = 9),
        plot.title = element_text(size = rel(1.2), face = 'bold')) +
  geom_point(position = position_jitter(w = 0.03, h = 0.03)) + 
  geom_abline(slope = mdl$coefficients[2], intercept = mdl$coefficients[1], color = 'red') +
  coord_equal() +
  labs(title = 'IUCN vs NatureServe global',
       x = 'IUCN category score (0 = LC, 1 = EX)',
       y = 'Natureserve global score (0 = LC, 1 = EX)')

print(ns_v_iucn)

knitr::kable(summary(mdl)$coef, digits = 4,
             caption = ' of IUCN vs global Natureserve')

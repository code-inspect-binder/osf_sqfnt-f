library(patchwork)
library(waffle)


### Goal 2 ###
# figure 4a
sdview = data.frame(
  view = c(
    'Stylistic (79.2%) ',
    'Substantive (8.3%)',
    'Both (5.6%)',
    'Unclear (6.9%)'
  ),
  count = c(57, 6, 4, 5),
  Percentage = scales::percent(c(57 / 72, 6 / 72, 4 /
                                   72, 5 / 72))
)
waffle1 = waffle(sdview, rows = 4) +
  labs(title = "a. The conceptualization of social desirability scales (72 studies)",
       x = '1 square = 1 study') +
  scale_y_reverse()+
  theme(plot.title = ggtext::element_markdown(),
    legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 15))

# figure 4b
scuse = data.frame(
  view = c('Controlled for social desirability (83.3%) ', 'Did not control for social desirability (16.7%)'),
  count = c(40, 8)
)
waffle2 = waffle(scuse,
                 rows = 4,
                 colors = c("#ffcf20ff", "#3a5e8cff")) +
  labs(title = "b. Statistical control of social desirability when using SCS as predictor (48 studies)",
       x = '1 square = 1 study') +
  scale_y_reverse()+
  theme(plot.title = ggtext::element_markdown(),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 15))

# figure 4
fig4 = waffle1 / waffle2

ggsave('fig4.tiff', width = 10, height = 6)

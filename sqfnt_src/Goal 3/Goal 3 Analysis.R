############# Goal 3 ##############
#library(jcolors)
library(dplyr)
library(ggplot2)
library(tidyr)
#library(simstudy)
library(yhat)
#library(matlib)
library(WebPower)
#library(reshape2)
library(patchwork)
library(ggpattern)

# Simulation-based approach
commonality_sim = function(rx1y, rx2y, rx1x2){
  # construct correlation matrix
  C <-
    matrix(
      c(1, rx1x2, rx1y, rx1x2, 1, rx2y, rx1y, rx2y, 1),
      nrow = 3)
  
  # generate fake data
  df <-
    genCorData(
      10000,
      mu = c(0, 0, 0),
      sigma = c(1, 1, 1),
      corMatrix = C,
      cnames = c('X1', 'X2', 'Y')
    )
  
  # check correlations
  cormat = cor(df[, c('X1', 'X2', 'Y')])
  
  # commonality analysis
  commality.coef = commonalityCoefficients(dataMatrix = df,
                                       dv = 'Y',
                                       ivlist = list('X1', 'X2'))$CC
  list('cormat' = cormat, 'commonality.coef' = as.vector(commality.coef[,1]))
}


commonality_sim2 = function(rx1y, rx2y, rx1x2){
  # construct correlation matrix
  C <-
    matrix(
      c(1, rx1x2, rx1y, rx1x2, 1, rx2y, rx1y, rx2y, 1),
      nrow = 3)
  
  # generate fake data
  df <-
    genCorData(
      10000,
      mu = c(0, 0, 0),
      sigma = c(1, 1, 1),
      corMatrix = C,
      cnames = c('X1', 'X2', 'Y')
    )
  
  # check correlations
  cormat = cor(df[, c('X1', 'X2', 'Y')])
  
  # commonality analysis
  commonalityCoefficients(dataMatrix = df,
                                           dv = 'Y',
                                           ivlist = list('X1', 'X2'))
}


# Exact-number based approach
commonality_exact = function(rx1y, rx2y, rx1x2) {
  # calculated r squared
  # https://en.wikipedia.org/wiki/Multiple_correlation
  
  #yx = c(rx1y, rx2y)
  #xx = matrix(c(1, rx1x2, rx1x2, 1), nrow = 2)
  #rsq = t(yx) %*% inv(xx) %*% yx
  rsq = (rx1y^2+rx2y^2 - (2*rx1y*rx2y*rx1x2))/(1-rx1x2^2)
  
  # calculate shared and unique variance
  x1 = rsq - rx2y^2
  x2 = rsq - rx1y^2
  x1_x2 = rsq - x1 - x2
  
  c(x1, x2, x1_x2, rsq)
  }


semipartial_corr = function(r12, r13, r23) {
  # 1 - DV, 2 - IV1, 3 - IV2
  r12_3 = (r12 - r13*r23)/sqrt(1-r23**2)
  r12_3
}

# test run
rx1y = .34 # correlation between SC and DV
rx2y =  .34 # correlation between SD and DV
rx1x2 = .51 # correlation between SC and SD

# simulation results
result_sim = commonality_sim(rx1y, rx2y, rx1x2)
result_sim$commonality.coef

# exact results
result_exact = commonality_exact(rx1y, rx2y, rx1x2)
result_exact

## to verify the results using the simulated corrs, un-comment below.
#rx1y = result_sim$cormat[1,3]
#rx2y = result_sim$cormat[2,3]
#rx1x2 = result_sim$cormat[1,2]
#commonality_exact(rx1y, rx2y, rx1x2)


# specify correlations
v_x1y = seq(.2, .4, by = 0.01)
v_x2y = c(.05, .2, .35) 
v_x1x2 = c(.41)

# create empty df
df <- data.frame(
  `rx1y` = numeric(),
  `rx2y` = numeric(),
  `rx1x2` = numeric(),
  `rx1y_x2` = numeric(),
  `rx2y_x1` = numeric(),
  `x1` = numeric(),
  `x2` = numeric(),
  `Common` = numeric(),
  `Total` = numeric()
)

# loop through all possible values
for (i in v_x1y) {
  for (k in v_x2y){
    for (j in v_x1x2){
      row = round(c(i, k, j, semipartial_corr(i,k,j), semipartial_corr(k, i, j), commonality_exact(i, k, j)), 4)
      #row = round(c(i, k, j, semipartial_corr(i,k,j), semipartial_corr(k, i, j), commonality_sim(i, k, j)$commonality.coef), 4)
      df[nrow(df) + 1,] = row
    }
  }
} 

# calculate percentage of explained variance
df = df %>% mutate(x1_pct = x1/Total, x2_pct=x2/Total, Common_pct=Common/Total)

#proportion graph
p1 = df %>% 
  #filter(rx1y_x2 >= 0 & rx2y_x1 >= 0) %>% # get rid of suppression cases?
  gather(key, value, x1, x2, Common) %>%
  mutate(
    facet_title1 = paste('r(SC, SD) = ', rx1x2),
    facet_title2 = paste('r(SD, DV) = ', rx2y),
    key = recode(
      key,
      x1 = 'Unique to SC (middle)',
      x2 = 'Unique to SD (top)',
      Common = 'Common to SC and SD (bottom)'),
    key = factor(key, levels=c("Unique to SD (top)", "Unique to SC (middle)","Common to SC and SD (bottom)"))) %>%
  ggplot(aes(
    x = rx1y,
    y = value,
    fill = key,
    group = key
  )) +
  geom_area(aes(fill = key), color = 'black', size= 0.5, alpha = 0.6, position = 'stack') + # use identity for non-stack
  geom_segment(aes(x = .2 , y = 0, xend = .4, yend = 0), size = 0.5) +
  facet_wrap(facet_title1 ~ facet_title2) +
  labs(
    title = 'b. Commonality Analysis',
    x = 'r(SC, DV)',
    y = '% of Variance of DV',
    color = 'Variance Partition',
    fill = 'Variance Partition'
  ) +
  scale_fill_brewer(palette = "Set1",direction=-1)+
  scale_y_continuous(
    labels = scales::percent,
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~.*1, name="Proportion of Total Variance", labels = scales::percent, scales::pretty_breaks(n = 6)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  coord_cartesian(ylim = c(-0.01,.22))+
  theme_bw() + theme(legend.position = 'top')

p1



# Power Analysis
pwr_corr = function(r, alpha=.05, power=0.8){
  if (r > 0){
  round(wp.correlation(r=r, alpha = alpha, power = power, alternative = 'two.sided')$n)
  } else {NA}
}

pwr_reg = function(f2, p1=2, p2=1, alpha=0.05, power=0.8){
  if (f2 > 0){
    round(wp.regression(p1=p1,p2=p2,f2=f2,alpha=alpha, power=power)$n)}
  else {NA}
}


# compute cohen's f
df = df %>%
  mutate(f2_cor = rx1y**2/(1-rx1y**2),
         f2_reg = x1/(1-Total)) 

df$N_cor = mapply(pwr_reg, f2 = df$f2_cor, p1=1, p2=0)
df$N_reg = mapply(pwr_reg, f2=df$f2_reg)


p2 = df %>% 
  mutate(
    facet_title1 = paste('r(SC, SD) = ', rx1x2),
    facet_title2 = paste('r(SD, DV) = ', rx2y)) %>%
  ggplot(aes(x=rx1y,y=N_reg)) + 
  facet_wrap(facet_title1 ~ facet_title2, scales='free')+
  geom_line(size=1, color='blue',alpha=0.6) +
  geom_line(data = df, aes(x = rx1y, y = N_cor), color = 'black', size = 1, lty='dashed')+
  labs(title = 'Power Analysis on the unique effect of SC while controlling for SD', x = 'r(SC, DV)', y = 'Required N') +
  coord_cartesian(ylim = c(0, 1000)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_bw() 
p2


venn =  png::readPNG('venn diagram.png') %>% grid::rasterGrob()

p3 = qplot(1:10, 1:10, geom="blank") +
  annotation_custom(venn, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  theme_void() +
  labs(title='a. Venn Diagram')

fig5 = p3 + p1 + plot_layout(widths = c(1, 2))
fig5

ggsave(plot = fig5, filename = 'fig5.tiff',width = 10, height = 4.5, units = 'in',dpi = 400)

# ggsave(plot = p2, filename = 'fig6.png',width = 9, height = 4.5, units = 'in',dpi = 400)

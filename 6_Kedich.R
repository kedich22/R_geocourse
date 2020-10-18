library(tidyverse)
library(ggthemes)
library(soilDB)
library(viridis)
library(RColorBrewer)

#Загрузка данных по почвам
soils = c('cecil', 'altavista', 'lloyd', 'wickham')
soil_ser = fetchOSD(soils, extended = TRUE)

#---------------------------------------------------------------------------------------
#Преобразования входных данных
position <- soil_ser$hillpos %>% 
  select(-7,-8) %>% 
  pivot_longer(2:6) %>% 
  mutate(position = factor(name, ordered = T, levels = rev(unique(name))))

#Построение графика
ggplot(position, mapping = aes(y = series, x = value, fill = position)) +
  geom_col(color = 'black', size = 0.2, position = 'fill') +
  scale_fill_viridis_d(direction = -1) +
  ggtitle('Empirical probability for hillslope position') +
  xlab('Probability') +
  ylab('soilseries') +
  labs(fill = 'Hillslope position')

#---------------------------------------------------------------------------------------
#Запись необходимых данных в переменную
parent_mat = soil_ser$pmorigin

#Построение графика
ggplot(parent_mat, mapping = aes(x = '', y = P, fill = pmorigin)) +
  geom_col(colour = 'black') +
  scale_y_continuous(breaks = seq(0, 0.95, 0.05), labels = seq(0, 0.95, 0.05)) +
  coord_polar(theta = 'y') +
  facet_wrap(~series) +
  scale_fill_manual(values = c(brewer.pal(8, 'Set3'), brewer.pal(8, 'Set1'))) + 
  labs(fill = 'Origin') +
  ggtitle('Empirical probability for parent materal origin') +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8)) +
  guides(fill = guide_legend(keyheight = 0.9, keywidth = 0.9)) 
#При сохранении в нормльном разрешении получается график как на примере

#---------------------------------------------------------------------------------------
#Построение графика осадков
#Преобразование данных, фильтр ненужных строк
temp = soil_ser$climate.monthly %>% 
  filter(!str_detect(variable, 'Potential'))

#Построение графика
ggplot(temp, mapping = aes(x = month, y = q50, color = series, group = series)) +
  geom_line(size = 1) +
  geom_point() +
  ylab('mm') +
  xlab("Month") +
  ggtitle('Precipitation') +
  labs(color = 'Series') +
  scale_x_discrete(labels = month.abb)

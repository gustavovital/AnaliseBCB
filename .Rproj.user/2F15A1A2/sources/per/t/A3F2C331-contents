# Script para a animação do desemprego no Brasil de acordo com a PNADC
# 
# Autor: gustavovital@id.uff.br

# pacotes necessarios ====

require(tidyverse)
require(gganimate)
require(sidrar)
require(ggthemes)
require(extrafont)

# lendo a base de dados ====

API = '/t/4093/n1/all/n2/all/v/4099/p/all/c2/6794/d/v4099%201'
desemprego <- get_sidra(api = API)

# manibulação dos dados ====

desemprego <- desemprego[, c(4,8,13)]
Trimestre <- seq(as.Date('2012-01-01'), length.out = 31, by = 'quarter')

desemprego$Trimestre <- Trimestre 
colnames(desemprego) <- c('REG', 'TRI', 'VAL') 

desemprego_brasil <- 
  desemprego %>% 
  filter(REG == 'Brasil')

# animação ====

des <- ggplot(desemprego_brasil, aes(x = TRI, y = VAL)) +
  geom_area(fill = 'dodgerblue2', alpha = .3) +
  geom_line(colour = 'dodgerblue3',
            size = 1.5,
            alpha = .9) +
  geom_segment(aes(xend=max(TRI), yend = VAL), linetype=2, colour='dodgerblue4') +
  geom_point(size = 1.7, colour='dodgerblue4') +
  geom_text(aes(x = max(TRI)+.1, label = sprintf("%.1f", VAL)), hjust=0, colour = 'dodgerblue4') +
  labs(title = 'Evolução do desemprego no Brasil',
       subtitle = 'Período de 2012 - Atual',
       caption = 'Fonte: PNADC. Elaboração própria',
       x = '', y = 'Valores em (%)') +
  theme_economist() +
  theme(plot.title = element_text(size = 17, family = 'Times'),
        plot.subtitle = element_text(size = 13, family = 'Times'),
        plot.caption = element_text(size = 12, family = 'Times')) +

  transition_reveal(TRI)
  
# salvando como gif ====

gif_desemprego <- animate(des, fps = 10, width = 700)
magick::image_write(gif_desemprego, path="gif_desemprego.gif")

# script para grafico geral de inlação no brasil ---- 
# Autor: @gustavoovital

# Pacotes necessários ----

library(sidrar)
library(tidyverse)
library(tm)
library(grid)

# começando... ----

api = '/t/1419/n1/all/v/63/p/all/c315/7169,7170,7283,7448,7451,7485,7488,7522,7541,7542,7547,7558,7625,7627,7641,7685,7686,7690,7692,7695,7704,7711,7730,7731,7732,7738,7747,7759,7790,7792,12422,12427,12428,12429,12430,107668,107671,107672,107673,107677,107688/d/v63%202'

ipca <- get_sidra(api = api)[, c(8,10,13)]
columns <- removeNumbers(removePunctuation(ipca$`Geral, grupo, subgrupo, item e subitem`[1:41]))

# limpando os dados ----

ipca_clean <- ipca %>% 
  pivot_wider(names_from = `Geral, grupo, subgrupo, item e subitem`, values_from = Valor)

colnames(ipca_clean) <- c('Data', columns)
ipca_clean$Data <- seq(as.Date('2012-01-01'), length.out = nrow(ipca_clean), by = 'm')

# ipca_clean %>%
#   glimpse()

# manipulando os dados ----

acum <- function(serie) {
  
  acum <- c()
  acum[1] <- 1 + serie[1]/100
  
  for(indice in 2:length(serie)) {
    acum[indice] <- (acum[indice - 1])*(1 + serie[indice]/100) 
  }
  acum <- (acum - 1)
  return(acum)  
}

# selecionando ----

# baixa ----

plot(acum(ipca_clean$`Tv som e informática`))
plot(acum(ipca_clean$Televisor))
plot(acum(ipca_clean$Microcomputador)) # talvez
plot(acum(ipca_clean$`Ingresso para jogo`))
plot(acum(ipca_clean$`Telefone fixo`))
plot(acum(ipca_clean$`Índice geral`))

# alta ----


ggplot(ipca_clean, aes(x = Data)) +
  
  
  # geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2020-01-01'), y=0, yend=0), size = 1, colour = 'gray80') +
  # geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2020-01-01'), y=-.3, yend=-.3), size = 1, colour = 'gray80') +
  # geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2020-01-01'), y=0.3, yend=0.3), size = 1, colour = 'gray80') +
  # geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2020-01-01'), y=0.9, yend=0.9), size = 1, colour = 'gray80') +

  
  geom_line(aes(y = acum(`Aluguel residencial`)), size = 3, colour = 'tomato3', alpha = .6) +
  geom_line(aes(y = acum(`Transporte público`)), size = 3, colour = 'tomato2', alpha = .6) +
  geom_line(aes(y = acum(`Televisor`)), size = 3, colour = 'dodgerblue2', alpha = .6) +
  geom_line(aes(y = acum(`Microcomputador`)), size = 3, colour = 'dodgerblue3', alpha = .6) +
  geom_line(aes(y = acum(`Vestuário`)), size = 3, colour = 'dodgerblue4', alpha = .6) +
  geom_line(aes(y = acum(`Automóvel novo`)), size = 3, colour = 'darkblue', alpha = .6) +
  geom_line(aes(y = acum(`Hospitalização e cirurgia`)), size = 3, colour = 'red', alpha = .6) +
  geom_line(aes(y = acum(`Plano de saúde`)), size = 3, colour = 'tomato4', alpha = .6) +
  geom_line(aes(y = acum(`Ingresso para jogo`)), size = 3, colour = 'dodgerblue4', alpha = .6) +
  geom_line(aes(y = acum(`Alimentação e bebidas`)), size = 3, colour = 'darkred', alpha = .6) +
  geom_line(aes(y = acum(`Livro`)), size = 3, colour = 'blue', alpha = .6) +
  geom_line(aes(y = acum(`Telefone celular`)), size = 3, colour = 'darkblue', alpha = .6) +
  geom_line(aes(y = acum(`Carnes`)), size = 3, colour = 'red', alpha = .6) +
  geom_line(aes(y = acum(`Cigarro`)), size = 3, colour = 'darkred', alpha = .6) +
  geom_line(aes(y = acum(`Cursos regulares`)), size = 3, colour = 'red', alpha = .6) +
  
  scale_y_continuous(limits = c(-.40,1.30),
                     breaks = seq(-.30,1.30,.20),labels = scales::percent) +
  
  annotation_custom(grob = textGrob('Aluguel residencial', gp=gpar(fontsize=18, col="tomato3", fontface="bold")),  xmin = as.Date('2020-03-01'), 
                    ymin = tail(acum(ipca_clean$`Aluguel residencial`), 1), ymax = tail(acum(ipca_clean$`Aluguel residencial`), 1),
                    xmax = as.Date('2022-03-01')) +
  
  
  annotation_custom(grob = textGrob('Transporte público', gp=gpar(fontsize=18, col="tomato2", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Transporte público`), 1), ymax = tail(acum(ipca_clean$`Transporte público`), 1),
                    xmax = as.Date('2023-03-01')) +
  
  
  annotation_custom(grob = textGrob('Televisor', gp=gpar(fontsize=18, col="dodgerblue2", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Televisor`), 1), ymax = tail(acum(ipca_clean$`Televisor`), 1),
                    xmax = as.Date('2022-02-01')) +
  
  
  annotation_custom(grob = textGrob('Microcomputador', gp=gpar(fontsize=18, col="dodgerblue3", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Microcomputador`), 1), ymax = tail(acum(ipca_clean$`Microcomputador`), 1),
                    xmax = as.Date('2023-01-01')) +
  
  
  annotation_custom(grob = textGrob('Vestuário', gp=gpar(fontsize=18, col="dodgerblue4", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Vestuário`), 1), ymax = tail(acum(ipca_clean$`Vestuário`), 1),
                    xmax = as.Date('2022-03-01')) +
  
  
  annotation_custom(grob = textGrob('Automóvel novo', gp=gpar(fontsize=18, col="darkblue", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Automóvel novo`), 1), ymax = tail(acum(ipca_clean$`Automóvel novo`), 1),
                    xmax = as.Date('2022-12-01')) +
  
  
  annotation_custom(grob = textGrob('Hospitalização e cirurgia', gp=gpar(fontsize=16, col="red", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Hospitalização e cirurgia`), 1), ymax = tail(acum(ipca_clean$`Hospitalização e cirurgia`), 1),
                    xmax = as.Date('2023-07-01')) +
  
  
  annotation_custom(grob = textGrob('Plano de saúde', gp=gpar(fontsize=18, col="tomato4", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Plano de saúde`), 1), ymax = tail(acum(ipca_clean$`Plano de saúde`), 1),
                    xmax = as.Date('2022-10-01')) +
  
  
  annotation_custom(grob = textGrob('Ingresso para jogo', gp=gpar(fontsize=18, col="dodgerblue4", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Ingresso para jogo`), 1), ymax = tail(acum(ipca_clean$`Ingresso para jogo`), 1),
                    xmax = as.Date('2023-02-01')) +
  
  annotation_custom(grob = textGrob('Alimentação e bebidas', gp=gpar(fontsize=18, col="tomato3", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Alimentação e bebidas`), 1), ymax = tail(acum(ipca_clean$`Alimentação e bebidas`), 1),
                    xmax = as.Date('2023-07-01')) +
  
  annotation_custom(grob = textGrob('Livro', gp=gpar(fontsize=18, col="blue", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Livro`), 1), ymax = tail(acum(ipca_clean$`Livro`), 1),
                    as.Date('2021-09-01')) +
  
  annotation_custom(grob = textGrob('Telefone celular', gp=gpar(fontsize=18, col="darkblue", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Telefone celular`), 1), ymax = tail(acum(ipca_clean$`Telefone celular`), 1),
                    xmax = as.Date('2022-11-01')) +
  
  
  annotation_custom(grob = textGrob('Carnes', gp=gpar(fontsize=18, col="red", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Carnes`), 1), ymax = tail(acum(ipca_clean$`Carnes`), 1),
                    xmax = as.Date('2021-12-01')) +
  
  
  annotation_custom(grob = textGrob('Cigarro', gp=gpar(fontsize=18, col="darkred", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Cigarro`), 1), ymax = tail(acum(ipca_clean$`Cigarro`), 1),
                    xmax = as.Date('2021-12-01')) +
  
  annotation_custom(grob = textGrob('Cursos regulares', gp=gpar(fontsize=18, col="red", fontface="bold")),  xmin = as.Date('2019-03-01'), 
                    ymin = tail(acum(ipca_clean$`Cursos regulares`), 1), ymax = tail(acum(ipca_clean$`Cursos regulares`), 1),
                    as.Date('2023-01-01')) +
  
  labs(title = expression(bold('Mudanças de Preços')~'(2012-2020)'),
       subtitle = 'Com base no Índice de Preços ao Consumidor Amplo acumulado',
       x = NULL, y = NULL,
       caption = 'Fonte: SIDRA/IBGE\nElaboração: @gustavoovital') +

  
  #geom_vline(xintercept = as.Date('2020-03-01')) +
  
  # geom_hline(yintercept = tail(acum(ipca_clean$`Índice geral`), 1), linetype = 'solid', colour = 'black', size = 3) +
  geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2020-01-01'),y=tail(acum(ipca_clean$`Índice geral`), 1),
                   yend=tail(acum(ipca_clean$`Índice geral`), 1)), size = 4) +
  geom_vline(xintercept = as.Date('2012-01-01'), size = 1, colour = 'gray50') +
  scale_x_date(limits = c(as.Date('2012-01-01'), as.Date('2022-01-01')), date_breaks = '3 year', labels=scales::date_format("%Y")) +
  
  geom_segment(aes(x=as.Date('2012-01-01'), xend=as.Date('2016-01-01'),y=tail(acum(ipca_clean$`Índice geral`), 1)  + .03,
                   yend=tail(acum(ipca_clean$`Índice geral`), 1)+ .03), size =14) +
  
  annotation_custom(grob = textGrob(paste(sep = '', 'Inflação Acumulada ','(', round(100*tail(acum(ipca_clean$`Índice geral`), 2), 2), ')', '%' ),
                                    gp=gpar(fontsize=23, col="white", fontface="bold")),  xmin = as.Date('2013-01-01'), 
                    ymin = tail(acum(ipca_clean$`Índice geral`), 1) + .03, ymax = tail(acum(ipca_clean$`Índice geral`), 1) + .03,
                    as.Date('2015-01-01')) +


  theme_minimal() +
  
  theme(plot.title = element_text(size = 45, colour = 'gray30'),
        plot.subtitle = element_text(size = 30, colour = 'gray50'),
        plot.caption = element_text(size = 25, colour = 'gray30'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 20, colour = 'gray50', hjust = 2, angle = 30),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 20))



  # [1] "Data"                                  "Índice geral"                          "Alimentação e bebidas"                 "Carnes"                               
# [5] "Aluguel residencial"                   "Taxa de água e esgoto"                 "Energia elétrica residencial"          "Mobiliário"                           
# [9] "Eletrodomésticos e equipamentos"       "Tv som e informática"                  "Televisor"                             "Microcomputador"                      
# [13] "Vestuário"                             "Transportes"                           "Transporte público"                    "Automóvel novo"                       
# [17] "Médico"                                "Dentista"                              "Serviços laboratoriais e hospitalares" "Hospitalização e cirurgia"            
# [21] "Plano de saúde"                        "Perfume"                               "Artigos de maquiagem"                  "Recreação"                            
# [25] "Cinema"                                "Ingresso para jogo"                    "Brinquedo"                             "Motel"                                
# [29] "Cigarro"                               "Telefone fixo"                         "Telefone celular"                      "Recreação fumo e fotografia"          
# [33] "Cursos regulares"                      "Atividades físicas"                    "Telefone com internet  pacote"         "TV por assinatura com internet"       
# [37] "Jogos de azar"                         "Ensino fundamental"                    "Ensino médio"                          "Ensino superior"                      
# [41] "Livro"                                 "Acesso à internet" 

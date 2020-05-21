# Script para a leitura e criação de um corpus em formato VCorpus para a manipulação
# e análise textual das atas do BCB. O tamanho amostral é de 140 atas, pegando os períodos
# de 2003 - 2018o. Assim, a análise textual e NLP será feita com base das gestões de 
# Meirelles; Tombini; e Goldfajn.

# Data: 08/01/2020
# Autor: Gustavo de Oliveira Vital

rm(list = ls())

# Pacotes necessários ----

library(pdftools)
library(tidyverse)

# Criando uma função split para remoção de \n ----

split_n <-
  function(caracter){
    split <- str_c(unlist(caracter %>%
                            str_split("\n")), collapse = " ")
  }

# Leitura dos PDF's e criação e um corpus "sujo" ----

texts <- c()

for(ata in 1:length(list.files("Atas"))){
  texts[ata] <- paste("Atas/", list.files("Atas")[ata], sep = "") %>% 
    pdf_text() %>% 
    split_n()
}

# Criando um corpus "sujo" ----

corpus <-
  tibble(doc_id = as.numeric(gsub("([0-9]+).*$", "\\1", list.files("Atas"))),
         text = texts)

corpus <- corpus %>% 
  arrange(doc_id)

rm(ata, texts, split_n)
saveRDS(corpus, 'corpus.rds')

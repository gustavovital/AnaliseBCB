# Script relacionado a primeira parte (corpus clean, para a contagem de palavras simples),
# a partir deste script, removo as palavras desnecessárias para a compreensão essencial das
# expressões das atas do COPOM. 

# Data: 08/01/2020
# Autor: Gustavo de Oliveira Vital

rm(list = ls())

# Base de Dados ----

corpus <- readRDS('corpus.rds')

# Pacotes necessários e source ----

library(tidyverse)
library(tidytext)
library(qdap)
library(wordcloud)

source('Sources/main.R')

# Analise de palavras ----

corpus %>% 
  unnest_tokens(Palavra, text, token = "ngrams", n = 1) %>% 
  mutate(Palavra = if_else(Palavra %in% c("price", "prices"), "price", Palavra)) %>% 
  mutate(Palavra = if_else(Palavra %in% c("increase", "increased"), "increase", Palavra)) %>% 
  mutate(Palavra = if_else(Palavra %in% c("market", "markets"), "market", Palavra)) %>% 
  
  filter(!Palavra %in% StopWords) %>%
  count(Palavra, sort = TRUE) %>% 
  arrange(desc(n)) -> vetor_palavras


# Analise de termos ----

corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('Termo1', 'Termo2'), sep = ' ') %>% 
  
  mutate(Termo1 = if_else(Termo1 %in% c("price", "prices"), "price", Termo1)) %>% 
  mutate(Termo2 = if_else(Termo2 %in% c("price", "prices"), "price", Termo2)) %>% 
  
  filter(!Termo1 %in% StopWords) %>%
  filter(!Termo2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(Termos = paste(Termo1, Termo2)) %>% 
  count(Termos, sort = TRUE) %>% 
  select(Termos, n) -> vetor_termos

# Nuvens de Palavras ----

wordcloud(words = vetor_palavras$Palavra, freq = vetor_palavras$n, min.freq = 1,  
          max.words=200, random.order=FALSE, rot.per=0.10,   
          colors=viridis::inferno(16, direction = -1))

# Analise de palavras ----

analise_palavras <- TermDocumentMatrix(corpus_clean)
analise_palavras_matrix <- as.matrix(analise_palavras)

# Meirelles ----

palavras_meirelles <- analise_palavras_matrix[, 1:76] %>% 
  rowSums() %>% 
  sort(decreasing = TRUE) %>% 
  head(200)

palavras_meirelles <- tibble(word = names(palavras_meirelles),
                            n = palavras_meirelles)

# Tombini ----

palavras_tombini <- analise_palavras_matrix[, 77:121] %>% 
  rowSums() %>% 
  sort(decreasing = TRUE) %>% 
  head(200)

palavras_tombini <- tibble(word = names(palavras_tombini),
                             n = palavras_tombini)

# Goldfajn ----

palavras_goldfajn <- analise_palavras_matrix[, 122:140] %>% 
  rowSums() %>% 
  sort(decreasing = TRUE) %>% 
  head(200)

palavras_goldfajn <- tibble(word = names(palavras_goldfajn),
                             n = palavras_goldfajn)

# organizando e nuvens de palavras ----

palavras_meirelles$periodo <- 'Meirelles'
palavras_tombini$periodo <- 'Tombini'
palavras_goldfajn$periodo <- 'Goldfajn'

df_palavras <- rbind(palavras_meirelles, palavras_tombini, palavras_goldfajn)
df_palavras$periodo <- factor(df_palavras$periodo, levels = c('Meirelles', 'Tombini', 'Goldfajn'))
df_palavras$N <- rep(c(rep(200, 5), rep(120, 45), rep(100, 150)), 3)

# vetor de tamanho ----

ggplot(df_palavras, aes(label = word, size = N)) +
  geom_text_wordcloud(colour = 'gray35') +
  labs(title = 'Núvens de Palavras para os Períodos de Gestão do BCB', subtitle = 'De 2003-2018. Meirelles - Goldfajn',
       caption = 'Fonte: BCB\nElaboração: @gustavoovital') +
  facet_wrap(~ periodo, scales = 'free') +
  theme_solarized_2() +
  theme(text = element_text(family = 'Lato'),
        plot.title = element_text(size = 25),
        plot.title.position = 'plot',
        plot.subtitle = element_text(size = 22),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 20),
        plot.caption = element_text(size = 15))



# Fontes ----

# [1] "Amiri"                       "Amiri Quran"                 "Amiri Quran Colored"         "Arimo"                      
# [5] "Baekmuk Batang"              "Baekmuk Dotum"               "Baekmuk Gulim"               "Baekmuk Headline"           
# [9] "AR PL SungtiL GB"            "Caladea"                     "Carlito"                     "Comfortaa"                  
# [13] "Comfortaa Light"             "Cousine"                     "DejaVu Math TeX Gyre"        "DejaVu Sans"                
# [17] "DejaVu Sans Light"           "DejaVu Sans Condensed"       "DejaVu Sans Mono"            "DejaVu Serif"               
# [21] "DejaVu Serif Condensed"      "Droid Sans Fallback"         "EB Garamond 08"              "EB Garamond 12 All SC"      
# [25] "EB Garamond 12"              "EB Garamond Initials Fill1"  "EB Garamond Initials Fill2"  "EB Garamond SC 08"          
# [29] "EB Garamond SC 12"           "FontAwesome"                 "FreeMono"                    "FreeSans"                   
# [33] "FreeSerif"                   "AR PL KaitiM GB"             "Gentium"                     "GentiumAlt"                 
# [37] "Gentium Basic"               "Gentium Book Basic"          "Gentium Plus"                "Gentium Plus Compact"       
# [41] "GLYPHICONS Halflings"        "Go"                          "Go Medium"                   "Go Mono"                    
# [45] "Go Smallcaps"                "IPAexGothic"                 "IPAexMincho"                 "IPAGothic"                  
# [49] "IPAMincho"                   "IPAPGothic"                  "IPAPMincho"                  "Junicode"                   
# [53] "Lato Black"                  "Lato"                        "Lato Hairline"               "Lato Heavy"                 
# [57] "Lato Light"                  "Lato Medium"                 "Lato Semibold"               "Lato Thin"                  
# [61] "Liberation Mono"             "Liberation Sans"             "Liberation Serif"            "Noto Kufi Arabic"           
# [65] "Noto Mono"                   "Noto Music"                  "Noto Naskh Arabic"           "Noto Naskh Arabic UI"       
# [69] "Noto Nastaliq Urdu"          "Noto Sans"                   "Noto Sans Adlam"             "Noto Sans Adlam Unjoined"   
# [73] "Noto Sans AnatoHiero"        "Noto Sans Arabic"            "Noto Sans Arabic UI"         "Noto Sans Armenian"         
# [77] "Noto Sans Avestan"           "Noto Sans Bamum"             "Noto Sans Bassa Vah"         "Noto Sans Batak"            
# [81] "Noto Sans Bengali"           "Noto Sans Bengali UI"        "Noto Sans Bhaiksuki"         "Noto Sans Brahmi"           
# [85] "Noto Sans Buginese"          "Noto Sans Buhid"             "Noto Sans CanAborig"         "Noto Sans Carian"           
# [89] "Noto Sans CaucAlban"         "Noto Sans Chakma"            "Noto Sans Cham"              "Noto Sans Cherokee"         
# [93] "Noto Sans Coptic"            "Noto Sans Cuneiform"         "Noto Sans Cypriot"           "Noto Sans Deseret"          
# [97] "Noto Sans Devanagari"        "Noto Sans Devanagari UI"     "Noto Sans Display"           "Noto Sans Duployan"         
# [101] "Noto Sans EgyptHiero"        "Noto Sans Elbasan"           "Noto Sans Ethiopic"          "Noto Sans Georgian"         
# [105] "Noto Sans Glagolitic"        "Noto Sans Gothic"            "Noto Sans Grantha"           "Noto Sans Gujarati"         
# [109] "Noto Sans Gujarati UI"       "Noto Sans Gurmukhi"          "Noto Sans Gurmukhi UI"       "Noto Sans Hanunoo"          
# [113] "Noto Sans Hatran"            "Noto Sans Hebrew"            "Noto Sans ImpAramaic"        "Noto Sans InsPahlavi"       
# [117] "Noto Sans InsParthi"         "Noto Sans Javanese"          "Noto Sans Kaithi"            "Noto Sans Kannada"          
# [121] "Noto Sans Kannada UI"        "Noto Sans Kayah Li"          "Noto Sans Kharoshthi"        "Noto Sans Khmer"            
# [125] "Noto Sans Khmer UI"          "Noto Sans Khudawadi"         "Noto Sans Lao"               "Noto Sans Lao UI"           
# [129] "Noto Sans Lepcha"            "Noto Sans Limbu"             "Noto Sans Linear A"          "Noto Sans Linear B"         
# [133] "Noto Sans Lisu"              "Noto Sans Lycian"            "Noto Sans Lydian"            "Noto Sans Mahajani"         
# [137] "Noto Sans Malayalam"         "Noto Sans Malayalam UI"      "Noto Sans Mandaic"           "Noto Sans Manichaean"       
# [141] "Noto Sans Marchen"           "Noto Sans MeeteiMayek"       "Noto Sans Mende Kikakui"     "Noto Sans Meroitic"         
# [145] "Noto Sans Miao"              "Noto Sans Modi"              "Noto Sans Mongolian"         "Noto Sans Mono"             
# [149] "Noto Sans Mro"               "Noto Sans Multani"           "Noto Sans Myanmar"           "Noto Sans Myanmar UI"       
# [153] "Noto Sans Nabataean"         "Noto Sans Newa"              "Noto Sans NewTaiLue"         "Noto Sans N'Ko"             
# [157] "Noto Sans Ogham"             "Noto Sans Ol Chiki"          "Noto Sans OldHung"           "Noto Sans Old Italic"       
# [161] "Noto Sans OldNorArab"        "Noto Sans Old Permic"        "Noto Sans OldPersian"        "Noto Sans OldSouArab"       
# [165] "Noto Sans Old Turkic"        "Noto Sans Oriya"             "Noto Sans Oriya UI"          "Noto Sans Osage"            
# [169] "Noto Sans Osmanya"           "Noto Sans Pahawh Hmong"      "Noto Sans Palmyrene"         "Noto Sans PauCinHau"        
# [173] "Noto Sans PhagsPa"           "Noto Sans Phoenician"        "Noto Sans PsaPahlavi"        "Noto Sans Rejang"           
# [177] "Noto Sans Runic"             "Noto Sans Samaritan"         "Noto Sans Saurashtra"        "Noto Sans Sharada"          
# [181] "Noto Sans Shavian"           "Noto Sans Sinhala"           "Noto Sans Sinhala UI"        "Noto Sans SoraSomp"         
# [185] "Noto Sans Sundanese"         "Noto Sans Syloti Nagri"      "Noto Sans Symbols"           "Noto Sans Symbols2"         
# [189] "Noto Sans Syriac"            "Noto Sans Syriac Eastern"    "Noto Sans Syriac Estrangela" "Noto Sans Syriac Western"   
# [193] "Noto Sans Tagalog"           "Noto Sans Tagbanwa"          "Noto Sans Tai Le"            "Noto Sans Tai Tham"         
# [197] "Noto Sans Tai Viet"          "Noto Sans Takri"             "Noto Sans Tamil"             "Noto Sans Tamil UI"         
# [201] "Noto Sans Telugu"            "Noto Sans Telugu UI"         "Noto Sans Thaana"            "Noto Sans Thai"             
# [205] "Noto Sans Thai UI"           "Noto Sans Tibetan"           "Noto Sans Tifinagh"          "Noto Sans Tirhuta"          
# [209] "Noto Sans Ugaritic"          "Noto Sans Vai"               "Noto Sans WarangCiti"        "Noto Sans Yi"               
# [213] "Noto Serif"                  "Noto Serif Ahom"             "Noto Serif Armenian"         "Noto Serif Balinese"        
# [217] "Noto Serif Bengali"          "Noto Serif Devanagari"       "Noto Serif Display"          "Noto Serif Ethiopic"        
# [221] "Noto Serif Georgian"         "Noto Serif Gujarati"         "Noto Serif Gurmukhi"         "Noto Serif Hebrew"          
# [225] "Noto Serif Kannada"          "Noto Serif Khmer"            "Noto Serif Lao"              "Noto Serif Malayalam"       
# [229] "Noto Serif Myanmar"          "Noto Serif Sinhala"          "Noto Serif Tamil"            "NotoSerifTamilSlanted"      
# [233] "Noto Serif Telugu"           "Noto Serif Thai"             "Noto Serif Tibetan"          "Open Sans"                  
# [237] "Open Sans Condensed"         "Open Sans Condensed Light"   "Open Sans Extrabold"         "Open Sans Light"            
# [241] "Open Sans Semibold"          "OpenSymbol"                  "Padauk"                      "Padauk Book"                
# [245] "Quicksand"                   "Quicksand Light"             "Quicksand Medium"            "Roboto Black"               
# [249] "Roboto"                      "Roboto Light"                "Roboto Medium"               "Roboto Thin"                
# [253] "Roboto Condensed"            "Roboto Condensed Light"      "Roboto Condensed Medium"     "AR PL Mingti2L Big5"        
# [257] "Tinos"                       "UnBatang"                    "UnDinaru"                    "UnDotum"                    
# [261] "UnGraphic"                   "UnGungseo"                   "UnJamoBatang"                "UnJamoDotum"                
# [265] "UnJamoNovel"                 "UnJamoSora"                  "UnPen"                       "UnPenheulim"                
# [269] "UnPilgi"                     "UnPilgia"                    "UnShinmun"                   "UnTaza"                     
# [273] "UnVada"                      "UnYetgul"                    "AR PL KaitiM Big5"  


#Modificar o nome da primeira coluna

pacotes<-c("tidyverse","readxl")

install.packages(pacotes)
library(tidyverse)
library(readxl)
lapply(pacotes,require, character.only = TRUE)




bergamia <- read.csv("KW_bergamia.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
holistix <- read.csv("KW_holistix.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
herbia <- read.csv("KW_herbia.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
feitobrasil <- read.csv("KW_feitobrasilcosmeticos.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
elementomineral <- read.csv("KW_elementomineral.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
ekilibre <- read.csv("KW_ekilibre.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
boninatural <- read.csv("KW_boninatural.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
bhava_loja <- read.csv("KW_bhava_loja.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
souvie <- read.csv("KW_souvie.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
simple_organic <- read.csv("KW_simpleorganic", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
simple_organic <- read.csv("KW_simpleorganic.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
simbioze_amazonica <- read.csv("KW_simbiozeamazonica.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
sallve <- read.csv("KW_sallve.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
quintal_cosmeticos <- read.csv("KW_quintalcosmeticos.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
oshadhi <- read.csv("KW_oshadhi.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
lolacosmetics <- read.csv("KW_lolacosmetics.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
lolacosmetics_loja <- read.csv("KW_lolacosmetics_loja.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
nesh_cosmeticos <- read.csv("KW_neshcosmeticos.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
livealoe <- read.csv("KW_livealoe.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)
khor_cosmeticos <- read.csv("KW_khorcosmeticos.csv", sep=";", encoding = "UTF-8", skipNul = TRUE, quote = "", header = TRUE,)


Consolidado <- rbind(khor_cosmeticos, livealoe, lolacosmetics, lolacosmetics_loja, nesh_cosmeticos, oshadhi, quintal_cosmeticos, sallve, simbioze_amazonica, simple_organic, souvie)
View(Consolidado)

names(Consolidado)
Consolidado <- rename(Consolidado, Keywords=X.U.FEFF.Keyword)

map(Consolidado$Keywords,unique)
Consolidado <- Consolidado %>% distinct(Keywords, .keep_all = TRUE)

Hidratantes_Cremes <- Consolidado %>%select(Keywords, categoria_kw)%>%
    filter(str_detect(Keywords, "hidra|hidratante|creme|loç|locao"))%>%
    rename(merge=categoria_kw) %>%
    mutate(merge, merge="Hidratantes")

Skincare <- Consolidado %>%select(Keywords, categoria_kw)%>%
  filter(str_detect(Keywords, "facial|skin|serum|sérum|rosto|esfol|acne|espinha|cravo"))%>%
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Skincare")

Desodorante <- Consolidado %>%select(Keywords, categoria_kw) %>%
    filter(str_detect(Keywords, "desodo"))%>% 
    rename(merge=categoria_kw) %>% 
    mutate(merge, merge="Desodorante")

Argilas <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "argila"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Argilas")

Oleos <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "óleo|oleo|oil|oléo"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Óleos")

Protetor_Solar <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "solar"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Protetor Solar")

Hidrolatos <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "hidrolato"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Hidrolato")

Cabelo <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "shamp|condi|xampu|cabelo"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Shampoo")

Sabonetes <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "sabon"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Sabonetes")

Barba <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "barba"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Barba")

Veganos <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "vegan"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Veganos")

Clareadores <- Consolidado %>%select(Keywords, categoria_kw) %>%
  filter(str_detect(Keywords, "clare|mancha"))%>% 
  rename(merge=categoria_kw) %>% 
  mutate(merge, merge="Clareadores")



Categorias <- rbind(Hidratantes_Cremes, Skincare,
                    Desodorante, Argilas, Oleos,
                    Protetor_Solar, Hidrolatos,
                    Cabelo,Sabonetes,Barba, Veganos, Clareadores)

Consolidado_novo <- left_join(Consolidado, Categorias,
                    by = "Keywords") %>% 
                    select(Keywords, merge, everything()) %>%
                    rename(pesquisa_mensal = Avg..monthly.searches,
                           mudanca_3meses=Mudança.em.três.meses,
                           YOY=Mudança.YoY, categoria = merge) %>% ggplot(Consolidado_novo) + 
  geom_col(aes(x=categoria, y=pesquisa_mensal), fill="orange") + 
  labs(x = "Categorias",
       y = "Volume Mensal Pesquisas",
       title = "Análise KW Cosméticos")



    
names(Consolidado_novo)

sum(is.na(Consolidado_novo$merge))


write.csv(Consolidado_novo, "Consolidado_novo.csv")
categoria_kw <-c(1:17115)
Consolidado <- mutate(Consolidado, categoria_kw)

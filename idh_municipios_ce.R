##Carga de dados sobre índice GINI

library(tidyverse)
library("basedosdados")
library(cluster)
library(viridis)
library(colorspace)

# Defina o seu projeto no Google Cloud
set_billing_id("nice-diorama-306223")
# Para carregar o dado direto no R
query <-
"SELECT a.*,
        b.sigla_uf,
        b.nome
FROM `basedosdados.mundo_onu_adh.municipio` a
inner join `basedosdados.br_bd_diretorios_brasil.municipio` b
on a.id_municipio = b.id_municipio"


idh_municipios <- read_sql(query, page_size=1000)





####################Primeiras e segundas diferenças


###### Primeiras e e segundas diferenças relativas usando dplyr


diff_idh<-
  idh_municipios%>%
  group_by(id_municipio, sigla_uf, nome) %>%
  summarise( across(expectativa_vida:idhm, function(x){ (x -dplyr::lag(x))/dplyr::lag(x)},.names = "perc_{.col}" )) %>%
  ungroup()

idh_lag<- rep(c(0,1,2),times= NROW(diff_idh)/3)

diff_idh$idh_lag<- idh_lag



base_cluster_idh1<-
diff_idh %>%
  select(id_municipio, sigla_uf, nome, perc_idhm) %>%
  filter(idh_lag==1)


#Descomentar as linhas abaixo caso seja necessário rodar outros testes de silhueta
# set.seed(1972)
# sil_info<-
#   map_dbl(2:6, function(k){
#     print(k)
#     model_idh<- cluster::pam(base_cluster_idh1[2],k=k)
#     model_idh$silinfo$avg.width
#   })



set.seed(1972)

#calcula o modelo de cluster para três grupos
model_idh_diff1<- cluster::pam(base_cluster_idh1$perc_idhm,k=3)
base_cluster_idh1$grupo <-model_idh_diff1$clustering



base_cluster_idh1<-
base_cluster_idh1 %>%
  mutate( grupo = as.character(grupo),
          grupo = factor(grupo, levels=c("2","1","3"), labels = c("Baixa variação IDHM","Média variação IDHM","Alta variação IDHM")))

##########Gráfico dos clusters de variação de idh para o Ceará

pontos_ceara<-
  base_cluster_idh1 %>%
  filter(sigla_uf == "CE")

pontos_resto_brasil <-
  base_cluster_idh1 %>%
  filter(sigla_uf != "CE")


#Alterar para destacar o ponto de máxima e Fortaleza?
p<-
  pontos_ceara %>%
  ggplot(aes(x=grupo, y= perc_idhm*100))+
  geom_jitter(data= pontos_resto_brasil, fill= "lightgray",pch=21, color="#444444", alpha= 0.2)+
  geom_jitter(aes(fill= grupo),pch=21, color="#444444", alpha= 1)+
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  #scale_fill_viridis(discrete = TRUE)+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    legend.position = "bottom",
    axis.title.x = element_blank()
  ) +
  labs(
    y= "Variação percentual do IDHM",
    fill= "Classe de variação"
  )

png("clusters_1991_2000_ce.png", width=3200, height =2000, res= 400)

print(p)

dev.off()




########Preparação para ML
base_cluster_idh1_ml <-
  diff_idh %>%
  filter(idh_lag == 1) %>%
  select(-c(idh_lag,perc_indice_gini, perc_indice_theil,perc_indice_treil_trabalho)) %>%
  bind_cols(
    base_cluster_idh1 %>%
      select(grupo)
  ) %>%
  select(-perc_idhm)


base_cluster_idh1_ml <-base_cluster_idh1_ml[, sapply(base_cluster_idh1_ml, Negate(anyNA)), drop = FALSE]





library(caret)


control_dt <- trainControl(method="cv")

set.seed(1972)
dt_model <- train(grupo~., data=base_cluster_idh1_ml, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model$finalModel)

save(list=ls(), file="modelo_idh_ce_derivado_2000.Rdata")



municipios<-
  (diff_idh%>%
  filter(idh_lag ==1) %>%
  select(id_municipio))$id_municipio

base_cluster_idh1_ml$id_municipio<- municipios

##Base dos mapas

library(geobr)

estados<- geobr::read_state(year=2010)
ceara<- estados[estados$abbrev_state=="CE",]
municipios<- geobr::read_municipal_seat(year = 2010)
brasil <- geobr::read_country()
municipios_area<- geobr::read_municipality()

#mapa idh

df_mapa<-
  municipios %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )


df_mapa_area<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )




library(colorspace)

df_forca_limite_max<-
  df_mapa_area %>%
  mutate(idhm=max(idh_municipios$idhm[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area %>%
  mutate(idhm=min(idh_municipios$idhm[idh_municipios$sigla_uf=="CE"]))



p<-
df_mapa_area %>%
  ggplot() +
  geom_sf(data= df_forca_limite_min, aes(fill= idhm),color = "#444444", alpha= 1) +
  geom_sf(data= df_forca_limite_max,aes(fill= idhm),color = "#444444", alpha= 1) +
  geom_sf(aes(fill= idhm),color = "#444444", alpha= 1) +
  geom_sf(data = ceara, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=0.5)+
  labs( fill="IDHM") +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050")
  ) +
  facet_wrap(ano+grupo~.)

png("mapa_idh_ce_2000.png", width=3200, height =2300, res= 400)

print(p)

dev.off()


################ Gráficos selecionados

#Mapa da dispersão do índice de frequência escolar com idhm com wrap ano
# Observar esse texto da UOL
#https://noticias.uol.com.br/ultimas-noticias/agencia-estado/2013/07/29/melhora-do-idhm-educacao-e-puxado-por-fluxo-escolar.htm


df_mapa_area_freq_jovem<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 indice_frequencia_escolar),
        by= "id_municipio"
      )
  )


df_forca_limite_max<-
  df_mapa_area_freq_jovem %>%
  mutate(indice_frequencia_escolar=max(idh_municipios$indice_frequencia_escolar[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area_freq_jovem %>%
  mutate(indice_frequencia_escolar=min(idh_municipios$indice_frequencia_escolar[idh_municipios$sigla_uf=="CE"]))


p<-
df_mapa_area_freq_jovem %>%
  ggplot() +
  geom_sf(data= df_forca_limite_max,aes(fill= indice_frequencia_escolar),color = "#444444", alpha= 1) +
  geom_sf(data= df_forca_limite_min,aes(fill= indice_frequencia_escolar),color = "#444444", alpha= 1) +
  geom_sf(aes(fill= indice_frequencia_escolar),color = "#444444", alpha= 1) +
  geom_sf(data = ceara, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=0.5)+
  labs( fill=str_wrap("Índice*",10),
        caption = "* Índice de frequência escolar de população jovem") +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050")
  ) +
  facet_wrap(ano+grupo~.)



png("mapa_frequencia_pop_jovem_1_ce.png", width=3200, height =2000, res= 400)

print(p)

dev.off()





#Mapa da dispersão da do índice de frequência escolar 5 e 6 anos com idhm wrap ano


df_mapa_area_5_6<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 taxa_freq_5_6),
        by= "id_municipio"
      )
  )

df_forca_limite_max<-
  df_mapa_area_5_6 %>%
  mutate(taxa_freq_5_6=max(idh_municipios$taxa_freq_5_6[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area_5_6 %>%
  mutate(taxa_freq_5_6=min(idh_municipios$taxa_freq_5_6[idh_municipios$sigla_uf=="CE"]))


p<-
  df_mapa_area_5_6 %>%
  ggplot() +
  geom_sf(data= df_forca_limite_max, aes(fill= taxa_freq_5_6),color = "#444444", alpha= 1) +
  geom_sf(data= df_forca_limite_min, aes(fill= taxa_freq_5_6),color = "#444444", alpha= 1) +
  geom_sf(aes(fill= taxa_freq_5_6),color = "#444444", alpha= 1) +
  geom_sf(data = ceara, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=50)+
  labs( fill=str_wrap("Taxa *",10),
        caption = "* (%) da população de 5 a 6 anos de idade frequentando a escola") +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050")
  ) +
  facet_wrap(ano+grupo~.)



png("mapa_frequencia_5_6_ce.png", width=3200, height =2000, res= 400)

print(p)

dev.off()




#Gráfico da dispersão da do índice de fundamental com 18 anos ou mais com idhm wrap com ano + grupo
##Esses gráficos não entram no texto

df_mapa_area_18<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2000 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 taxa_fundamental_18_mais),
        by= "id_municipio"
      )
  )

p<-
  df_mapa_area_18 %>%
  ggplot() +
  geom_sf(aes(fill= taxa_fundamental_18_mais),color = "#444444", alpha= 1) +
  geom_sf(data = ceara, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=50)+
  labs( fill=str_wrap("(%) da população de 18 a 24 anos com fundamental completo",10), size=8) +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050")
  ) +
  facet_wrap(ano+grupo~.)


base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_fundamental_18_mais,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_fundamental_18_mais,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano+grupo~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Percentual da população de 18 a 24 anos com fundamental completo",
    y= "IDHM",
    fill= "Classe de variação"
  )






######################## Gráficos de apoio

library(viridis)

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             taxa_freq_5_6,
             idhm),
    by= "id_municipio"
  ) %>%
  #mutate(super_grupo= "super grupo") %>%
  ggplot() +
  geom_jitter(aes(x= taxa_freq_5_6*100,  y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_wrap(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "black")
  ) +
  labs(
    x= "Percentual da população de 5 a 6 anos de idade frequentando a escola",
    y= "IDHM",
    fill= "Classe de variação"
  )



#Gráfico da dispersão da do índice de frequência escolar com idhm com wrap ano+grupo

p<-
  base_cluster_idh1_ml %>%
  filter(sigla_uf == "CE") %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= indice_frequencia_escolar, y= idhm, fill= grupo),pch=21, color="#444444", alpha= 1) +
  facet_wrap(ano+grupo~.)+
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#505050")
  ) +
  ylim(0,1)+
  labs(
    x= "Índice de rrequência escolar de população jovem",
    y= "IDHM",
    fill= "Classe de variação"
  )




png("frequencia_pop_jovem_2_ce.png", width=3200, height =2300, res= 400)

print(p)

dev.off()

#Mapa da dispersão da do índice de frequência escolar com idhm com wrap ano+grupo


mid_point = median(df_mapa$indice_frequencia_escolar)

df_mapa %>%
  ggplot() +
  geom_sf(aes(color= indice_frequencia_escolar*100), size=0.1, alpha=0.5) +
  geom_sf(data = brasil, fill=NA) +
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_point*100)+
  labs( color="Frequência escolar", size=8) +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "black")
  ) +
  facet_wrap(ano+grupo~.)



p<-
  base_cluster_idh1_ml %>%
  filter(sigla_uf == "CE") %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= indice_frequencia_escolar, y= idhm, fill= grupo),pch=21, color="#444444", alpha= 1) +
  facet_wrap(ano~.)+
  scale_fill_discrete_qualitative(palette = "Dark 3")+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "#505050")

  ) +
  ylim(0,1)+
  labs(
    x= "Índice de frequência escolar de população jovem",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da variação percentual do índice de taxa_freq_1_5  com idhm

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= perc_taxa_freq_5_6, y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_grid(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percentual da população de 5 a 6 anos de idade frequentando escola entre 1991 e 2000",
    y= "IDHM",
    fill= "Classe de variação"
  )


#Gráfico da dispersão da variação percentual do índice de taxa_perc_18_mais  com idhm

base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar,
             idhm),
    by= "id_municipio"
  ) %>%
  ggplot() +
  geom_jitter(aes(x= perc_taxa_fundamental_18_mais, y= idhm, color= grupo, fill= grupo),pch=21, color="white", alpha= 0.5) +
  facet_grid(ano~.)+
  scale_fill_viridis(discrete = TRUE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percentual da população de 18 anos ou mais com ensino fundamental",
    y= "IDHM",
    fill= "Classe de variação"
  )

#Gráfico da dispersão da variação percentual do índice de frequência escola com idhm


dt_text_1<- data.frame(perc_indice_frequencia_escolar= 0, indice_frequencia_escolar= 0.9, ano = 1991)
dt_text_2<- data.frame(perc_indice_frequencia_escolar= 1.20, indice_frequencia_escolar= 0.75, ano = 1991)
dt_text_3<- data.frame(perc_indice_frequencia_escolar= 2.40, indice_frequencia_escolar= 0.6, ano = 1991)

library(viridis)
base_cluster_idh1_ml %>%
  inner_join(
    idh_municipios %>%
      dplyr::filter(ano == 1991 | ano== 2000) %>%
      mutate(ano = factor(ano)) %>%
      select(ano,
             id_municipio,
             indice_frequencia_escolar),
    by= "id_municipio"
  ) %>%
  ggplot(aes(x= perc_indice_frequencia_escolar*100, y= indice_frequencia_escolar)) +
  geom_rect(aes(xmin=0,xmax=120,ymin=0, ymax=1), fill="#A9A9A9")+
  geom_rect(aes(xmin=120,xmax=240,ymin=0, ymax=1), fill="#696969")+
  geom_rect(aes(xmin=240,xmax=3617,ymin=0, ymax=1), fill="#404040")+
  geom_jitter(aes( fill= grupo),pch=21, color="white", alpha= 0.5 ) +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(ano+grupo~.) +
  theme_light() +
  geom_text(data = dt_text_1,label = "82% das alterações baixas de IDHM",
            color= "white",hjust   = -0.01,)+
  geom_text(data = dt_text_2,label = "75% das alterações médias de IDHM",
            color= "white",hjust   = -0.01,)+
  geom_text(data = dt_text_3,label = "85% das alterações altas de IDHM",
            color= "white",hjust   = -0.01,)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    legend.position = "bottom"
  ) +
  labs(
    x= "Variação percenutal em frequência escolar da população jovem entre 1991 e 2000",
    y= "IDHM",
    fill= "Classe de variação de IDHM"
  )

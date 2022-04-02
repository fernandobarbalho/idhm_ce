#Gerar nova base para análise de cluster agora pegando o lab entre 2010 e 2000
base_cluster_idh2<-
  diff_idh %>%
  select(id_municipio,perc_idhm) %>%
  filter(idh_lag==2)


#Descomentar caso queira testar clusters com mais grupos
# set.seed(1972)
# sil_info_idh2<-
#   map_dbl(2:6, function(k){
#     print(k)
#     model_idh<- cluster::pam(base_cluster_idh2[2],k=k)
#     model_idh$silinfo$avg.width
#   })


set.seed(1972)
model_idh_diff2<- cluster::pam(base_cluster_idh2[2],k=3)
base_cluster_idh2$grupo <-model_idh_diff2$clustering


base_cluster_idh2<-
  base_cluster_idh2 %>%
  mutate( grupo = as.character(grupo),
          grupo = factor(grupo, levels=c("3","1","2"), labels = c("Baixa variação IDHM","Média variação IDHM","Alta variação IDHM")))



base_cluster_idh2_ml <-
  diff_idh %>%
  filter(idh_lag == 2) %>%
  select(-c(idh_lag,perc_indice_gini, perc_indice_theil,perc_indice_treil_trabalho)) %>%
  bind_cols(
    base_cluster_idh2 %>%
      select(grupo)
  ) %>%
  select(-perc_idhm)


base_cluster_idh2_ml <-base_cluster_idh2_ml[, sapply(base_cluster_idh2_ml, Negate(anyNA)), drop = FALSE]


library(caret)


control_dt <- trainControl(method="cv")

set.seed(1972)
dt_model_idh2 <- train(grupo~., data=base_cluster_idh2_ml, method="rpart",  trControl=control_dt)

library(rattle)
fancyRpartPlot(dt_model_idh2$finalModel)

save(list=ls(), file="modelo_idh_ce.Rdata")



#mapa idh

df_mapa_area_2010<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 2000 | ano == 2010 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )



library(colorspace)
df_forca_limite_max<-
  df_mapa_area_2010 %>%
  mutate(idhm=max(idh_municipios$idhm[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area_2010 %>%
  mutate(idhm=min(idh_municipios$idhm[idh_municipios$sigla_uf=="CE"]))



p<-
df_mapa_area_2010 %>%
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



png("mapa_idh_lag2_ce.png", width=3200, height =2300, res= 400)

print(p)

dev.off()



df_mapa_area_1991_2010<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 1991 | ano == 2010 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 idhm),
        by= "id_municipio"
      )
  )





library(colorspace)

p<-
  df_mapa_area_1991_2010 %>%
  ggplot() +
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



png("mapa_idh_1991-2010_ce.png", width=3200, height =2300, res= 400)

print(p)

dev.off()



################ Gráficos selecionados

#Mapa da dispersão do índice de frequência escolar com idhm com wrap ano
# Observar esse texto da UOL
#https://noticias.uol.com.br/ultimas-noticias/agencia-estado/2013/07/29/melhora-do-idhm-educacao-e-puxado-por-fluxo-escolar.htm


df_mapa_area_freq_jovem_2010<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 2000 | ano == 2010 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 indice_frequencia_escolar),
        by= "id_municipio"
      )
  )

df_forca_limite_max<-
  df_mapa_area_freq_jovem_2010 %>%
  mutate(indice_frequencia_escolar=max(idh_municipios$indice_frequencia_escolar[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area_freq_jovem_2010 %>%
  mutate(indice_frequencia_escolar=min(idh_municipios$indice_frequencia_escolar[idh_municipios$sigla_uf=="CE"]))


p<-
  df_mapa_area_freq_jovem_2010 %>%
  ggplot() +
  geom_sf(data= df_forca_limite_max, aes(fill= indice_frequencia_escolar),color = "#444444", alpha= 1) +
  geom_sf(data= df_forca_limite_min, aes(fill= indice_frequencia_escolar),color = "#444444", alpha= 1) +
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



png("mapa_frequencia_pop_jovem_1_ce_2010.png", width=3200, height =2000, res= 400)

print(p)

dev.off()





#Mapa da dispersão da do índice de frequência escolar 5 e 6 anos com idhm wrap ano



df_mapa_area_11_13<-
  municipios_area %>%
  filter(abbrev_state == "CE") %>%
  mutate(id_municipio = as.character(code_muni) ) %>%
  inner_join(
    base_cluster_idh1_ml %>%
      inner_join(
        idh_municipios %>%
          dplyr::filter(ano == 2000 | ano == 2010 ) %>%
          mutate(ano = factor(ano)) %>%
          select(ano,
                 id_municipio,
                 taxa_fundamental_11_13),
        by= "id_municipio"
      )
  )


df_forca_limite_max<-
  df_mapa_area_11_13 %>%
  mutate(taxa_fundamental_11_13=max(idh_municipios$taxa_fundamental_11_13[idh_municipios$sigla_uf=="CE"]))

df_forca_limite_min<-
  df_mapa_area_11_13 %>%
  mutate(taxa_fundamental_11_13=min(idh_municipios$taxa_fundamental_11_13[idh_municipios$sigla_uf=="CE"]))



p<-
  df_mapa_area_11_13 %>%
  ggplot() +
  geom_sf(data=df_forca_limite_max, aes(fill= taxa_fundamental_11_13),color = "#444444", alpha= 1) +
  geom_sf(data=df_forca_limite_min,aes(fill= taxa_fundamental_11_13),color = "#444444", alpha= 1) +
  geom_sf(aes(fill= taxa_fundamental_11_13),color = "#444444", alpha= 1) +
  geom_sf(data = ceara, fill=NA) +
  scale_fill_continuous_divergingx (palette = "RdYlBu",mid=50)+
  labs( fill=str_wrap("Taxa *",10),
        caption = "* (%) da população de 11 a 13 anos de idade nos anos finais do fundamental") +
  theme_light()  +
  theme(
    axis.text = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#505050")
  ) +
  facet_wrap(ano+grupo~.)



png("mapa_frequencia_11_13_ce.png", width=3200, height =2000, res= 400)

print(p)

dev.off()


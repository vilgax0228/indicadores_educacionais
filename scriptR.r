# Indicador 1:
#  - Nome: Razão entre escolas rurais com acesso à rede pública de esgoto e 
#  escolas rurais com acesso a esgoto por fossa séptica no Rio Grande do Sul.

#	 - Descrição: Este indicador mede a relação entre o número de escolas rurais 
#  do Rio Grande do Sul que possuem acesso à rede pública de esgoto e aquelas que 
#  possuem acesso a sistema de fossa séptica.

#	 - Fórmula:
#  Razão = Nº de escolas rurais com rede pública de esgoto/Nº de escolas rurais com fossa séptica

#  - Finalidade: Avaliar o padrão de acesso ao saneamento básico em escolas rurais 
#  do RS e identificar possíveis desigualdades de infraestrutura.







# importando os pacotes necessários 

library(readr)
library(dplyr)
library(naniar)
library(tidyr)


# importando a base de dados


base = read_csv2(file = "microdados_ed_basica_2024.csv",
                 locale = locale(encoding = "ISO-8859-1"),
                 na = c("88888"))

View(base)


# filtrando apenas as escolas do Rio Grande do Sul


base_rs = base |> 
  dplyr::filter(SG_UF == "RS")

View(base_rs)


# filtrando as variáveis que vão ser usadas no indicador 


base_rs <- base_rs |> 
  dplyr::select(CO_MUNICIPIO, NO_MUNICIPIO, CO_ENTIDADE,TP_DEPENDENCIA, TP_LOCALIZACAO, IN_ESGOTO_REDE_PUBLICA, IN_ESGOTO_FOSSA_SEPTICA
                ) |>
  dplyr::filter(TP_LOCALIZACAO == 2)

View(base_rs)


# recodificando as variáveis



base_rs <- base_rs |> 
  mutate(CO_MUNICIPIO = factor(CO_MUNICIPIO)) |> 
  
  mutate(IN_ESGOTO_REDE_PUBLICA = case_when(
    IN_ESGOTO_REDE_PUBLICA == 0 ~ "Não",
    IN_ESGOTO_REDE_PUBLICA == 1 ~ "Sim"
  )) |> 
  
  
  mutate(IN_ESGOTO_FOSSA_SEPTICA = case_when(
    IN_ESGOTO_FOSSA_SEPTICA == 0 ~ "Não",
    IN_ESGOTO_FOSSA_SEPTICA == 1 ~ "Sim"
  )) |> 
  
  
  mutate(TP_LOCALIZACAO = case_when(
    TP_LOCALIZACAO == 2 ~ "Rural"
  )) |> 
  
  
  
  mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~ "Federal",
    TP_DEPENDENCIA == 2 ~ "Estadual",
    TP_DEPENDENCIA == 3 ~ "Municipal",
    TP_DEPENDENCIA == 4 ~ "Privada"
  )) |> 
  
  
  mutate(CO_ENTIDADE = factor(CO_ENTIDADE))
  
  
View(base_rs)


# analisando dados faltantes


vis_miss(x = base_rs)
gg_miss_var(x = base_rs)
miss_var_summary(data = base_rs)



# contando escolas com esgoto rede pública


contagem_redep <- base_rs |> 
  dplyr :: summarise(sum(IN_ESGOTO_REDE_PUBLICA == "Sim",
                         na.rm = TRUE))
                     

contagem_redep


# contando as escolas com esgoto em fossas sépticas  


contagem_fossasep <- base_rs |> 
  dplyr :: summarise(sum(IN_ESGOTO_FOSSA_SEPTICA == "Sim",
                         na.rm = TRUE))


contagem_fossasep



# criando e visualizando o indicador razão entre escolas rurais com acesso a rede pública de esgoto e escolas rurais com  
# acesso a esgoto por fossa séptica



razao_esgoto = contagem_redep/contagem_fossasep

contagem_redep
contagem_fossasep
razao_esgoto
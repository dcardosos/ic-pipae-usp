library(dplyr)
library(ggplot2)
library(patchwork)

read_sql <- function(query){
  
  bigrquery::bq_project_query(
    x = 'rodoviar',
    query = query) |> 
    bigrquery::bq_table_download(bigint = "integer64")
  
}


query <- "
  SELECT * 
  FROM `rodoviar.br_dpfr_acidentes_rodoviarios.acidentes_por_ocorrencia`
  WHERE ano = 2021
"

df <- read_sql(query)



# tab 1 - Quantidade de acidentes nas rodovias federais e de vítimas (2021)
df |> 
  group_by(classificacao_acidente) |> 
  summarise(
    Acidentes = n(),
    `Veículos envolvidos` = sum(veiculos),
    Ilesos = sum(ilesos),
    Feridos = sum(feridos),
    Mortos = sum(mortos)) |> 
  rename(Categoria = classificacao_acidente) |> 
  janitor::adorn_totals()


# functions ---------------------------------------------------------------
order_dayofweek <- function(.tb, .col) {
  .tb |> 
    dplyr::mutate(dayofweek_index = dplyr::case_when(
      {{.col}} == 'domingo' ~ 1,
      {{.col}} == 'segunda' ~ 2,
      {{.col}} == 'terca' ~ 3,
      {{.col}} == 'quarta' ~ 4,
      {{.col}} == 'quinta' ~ 5,
      {{.col}} == 'sexta' ~ 6,
      {{.col}} == 'sabado' ~ 7)) |> 
    dplyr::arrange(dayofweek_index)
}
  

mean_death_by <- function(tb, .by){
    tb |> 
    dplyr::group_by({{.by}}) |> 
    dplyr::summarise(
      mean_mortos = mean(mortos),
      sum_mortos = sum(mortos),
      sd_mortos = sd(mortos),
      count_acid = dplyr::n()) |> 
    dplyr::arrange(-mean_mortos)
}

# by dia da semana --------------------------------------------------------

## count acidentes
mean_death_by(df, dia_semana) |>
  order_dayofweek(dia_semana) |> 
  ggplot(aes(reorder(dia_semana, dayofweek_index), count_acid)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count acidentes') +

## count mortos
mean_death_by(df, dia_semana) |>
  order_dayofweek(dia_semana) |> 
  ggplot(aes(reorder(dia_semana, dayofweek_index), sum_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count mortos') +

## mean mortos
mean_death_by(df, dia_semana) |>
  order_dayofweek(dia_semana) |> 
  ggplot(aes(reorder(dia_semana, dayofweek_index), mean_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'mean mortos') +

## sd mortos
mean_death_by(df, dia_semana) |>
  order_dayofweek(dia_semana) |> 
  ggplot(aes(reorder(dia_semana, dayofweek_index), sd_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'sd mortos')


# by uf -------------------------------------------------------------------

 
## count acidentes
mean_death_by(df, uf) |>
  ggplot(aes(reorder(uf, count_acid), count_acid)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count acidentes') +
  coord_flip() +
  
## count mortos
mean_death_by(df, uf) |>
  ggplot(aes(reorder(uf, sum_mortos), sum_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count mortos') +
  coord_flip() +
  
## mean mortos
mean_death_by(df, uf) |>
  ggplot(aes(reorder(uf, mean_mortos), mean_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'mean mortos') +
  coord_flip() +

## sd mortos
mean_death_by(df, uf) |>
  ggplot(aes(reorder(uf, sd_mortos), sd_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'sd mortos') +
  coord_flip()

  


# by br -------------------------------------------------------------------

## count acidentes
mean_death_by(df, br) |>
  dplyr::filter(!is.na(br)) |> 
  dplyr::top_n(n = 20, count_acid) |> 
  ggplot(aes(reorder(br, count_acid), count_acid)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count acidentes') +
  coord_flip() +
  
## count mortos
mean_death_by(df, br) |>
  dplyr::filter(!is.na(br)) |> 
  dplyr::top_n(n = 20, count_acid) |> 
  ggplot(aes(reorder(br, sum_mortos), sum_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count mortos') +
  coord_flip() +
  
## mean mortos
mean_death_by(df, br) |>
  dplyr::filter(!is.na(br)) |> 
  dplyr::top_n(n = 20, count_acid) |> 
  ggplot(aes(reorder(br, mean_mortos), mean_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'mean mortos') +
  coord_flip() +
  
## sd mortos
mean_death_by(df, br) |>
  dplyr::filter(!is.na(br)) |> 
  dplyr::top_n(n = 20, count_acid) |> 
  ggplot(aes(reorder(br, sd_mortos), sd_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'sd mortos') +
  coord_flip()

# by municipio ------------------------------------------------------------
## count acidentes
mean_death_by(df, municipio) |>
  dplyr::filter(!is.na(municipio)) |> 
  dplyr::top_n(n = 30, count_acid) |> 
  ggplot(aes(reorder(municipio, count_acid), count_acid)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count acidentes') +
  coord_flip() +
  
## count mortos
mean_death_by(df, municipio) |>
  dplyr::filter(!is.na(municipio)) |> 
  dplyr::top_n(n = 30, count_acid) |> 
  ggplot(aes(reorder(municipio, sum_mortos), sum_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'count mortos') +
  coord_flip() +
  
## mean mortos
mean_death_by(df, municipio) |>
  dplyr::filter(!is.na(municipio)) |> 
  dplyr::top_n(n = 30, count_acid) |> 
  ggplot(aes(reorder(municipio, mean_mortos), mean_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'mean mortos') +
  coord_flip() +
  
## sd mortos
mean_death_by(df, municipio) |>
  dplyr::filter(!is.na(municipio)) |> 
  dplyr::top_n(n = 30, count_acid) |> 
  ggplot(aes(reorder(municipio, sd_mortos), sd_mortos)) + 
  geom_col() +
  labs(x = NULL, y = NULL, title = 'sd mortos') +
  coord_flip()



# by causa acidente -------------------------------------------------------



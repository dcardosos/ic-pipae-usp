existentes <- readr::read_csv("acidentes_rodoviarios/acidentes_rodoviarios_2007_2018.csv")

novos <- purrr::map_dfr(
  c(
    'acidentes_rodoviarios/datatran2019.csv',
    'acidentes_rodoviarios/datatran2020.csv',
    'acidentes_rodoviarios/datatran2021.csv'),
  ~ readr::read_csv2(
    .x,
    col_types = readr::cols(latitude = "c"), 
    locale = readr::locale(encoding = "ISO-8859-2"))
)


da <- existentes |> 
  dplyr::filter(!is.na(uf)) |> 
  dplyr::rename(uop = delegacia) |> 
  dplyr::mutate(
    delegacia = NA, 
    latitude = as.character(latitude),
    hora = as.character(hora),
    minuto = as.character(minuto)) |> 
  dplyr::bind_rows(
     novos |> 
      dplyr::rename(data = data_inversa) |> 
      dplyr::mutate(
        ano = lubridate::year(data),
        hora = stringr::str_sub(horario, 1, 2),
        minuto = stringr::str_sub(horario, 4, 5),
        dia_semana = stringr::str_remove_all(dia_semana, "-feira")) |> 
      dplyr::select(-horario)) 


clean_sentences <- function(...){
  stringr::str_replace_all(...) |> 
    stringr::str_to_sentence()
}

dados <- da |> 
  dplyr::mutate(
    dplyr::across(
      .cols = c(causa_acidente, tipo_acidente, classificacao_acidente, fase_dia, 
                sentido_via, tracado_via, uso_solo),
      .fns = ~ purrr::reduce2(
        c("ă", "ę", "ŕ"), 
        c("ã", "ê", "à"),
        .init = .x,
        clean_sentences)),
    causa_acidente = dplyr::case_when(
      causa_acidente == "Avaria no pneu" ~ "Avarias e/ou desgaste excessivo no pneu",
      causa_acidente == "Carga Mal Acondicionada" ~ "Carga excessiva e/ou mal acondicionada",
      causa_acidente == "Defeito mecânico em veículo" ~ "Defeito mecânico no veículo",
      causa_acidente == "Deficiência do sistema de iluminação/sinalização" ~ "Deficiência ou não acionamento do sistema de iluminação/sinalização do veículo",
      causa_acidente == "Ingestão de álcool ou de substâncias psicoativas pelo pedestre" ~ "Ingestão de álcool e/ou substâncias psicoativas pelo pedestre",
      TRUE ~ causa_acidente),
    condicao_metereologica = dplyr::case_when(condicao_metereologica == "Ceu claro" ~ "Céu claro"),
    dplyr::across(c(hora, minuto), readr::parse_integer))



dados |> 
  readr::write_csv("acidentes_por_ocorrencia.csv")

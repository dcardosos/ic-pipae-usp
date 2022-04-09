library(dplyr)
library(ggplot2)
library(patchwork)

domicilio <- readr::read_csv2('tic_dados/dados/ticdom_2019_domicilios_base_de_microdados_v1.1.csv') 

# Quantos domicílios possuem computador (celular, tablet ou pc) e quantos a internet? 

domicilio |> 
  group_by(A1_AGREG) |> 
  count() |>
  ungroup() |> 
  mutate(
    `tem_computador?` = case_when(
      A1_AGREG == 0 ~ 'Tem',
      A1_AGREG == 1 ~ 'Não tem'),
    fraction = .data$n / sum(.data$n),
    ymax = cumsum(.data$n),
    ymin = c(0, head(ymax, n=-1)),
    label_position = (ymax + ymin) / 2) |> 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `tem_computador?`)) +
  geom_rect() +
  scale_fill_manual(values = c('#0e4238', '#1d9091'), name = 'Domicílio possui computador?') + 
  scale_color_manual(values = c('#0e4238', '#1d9091'), name = 'Domicílio possui computador?') +
  geom_text(x = 2, aes(y = label_position, label = n, color = `tem_computador?`), size = 6) +
  coord_polar(theta = 'y') +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(
    title = 'Quantidade de domicílios com acesso a um computador (amostral)',
    caption = 'Fonte: Núcleo da Informação e Coordenação do Ponto BR - NIC.br.') +
  
  domicilio |> 
  group_by(A4) |> 
  count() |>
  ungroup() |> 
  mutate(
    `tem_internet?` = case_when(
      A4 == 0 ~ 'Não tem',
      A4 == 1 ~ 'Tem',
      TRUE ~ 'Outra resposta')) |> 
  select(-A4) |> 
  group_by(`tem_internet?`) |> 
  mutate(n = sum(n)) |> 
  unique() |>
  ungroup() |> 
  mutate(
    fraction = .data$n / sum(.data$n),
    ymax = cumsum(.data$n),
    ymin = c(0, head(ymax, n=-1)),
    label_position = (ymax + ymin) / 2) |> 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `tem_internet?`)) +
  geom_rect() +
  scale_fill_manual(values = c('#0e4238', '#1d9091', '#a1e1e2'), name = 'Domicílio possui acesso à internet?') +
  scale_color_manual(values = c('#0e4238', '#1d9091', '#a1e1e2'), name = 'Domicílio possui acesso à internet?') +
  geom_text(x = 2, aes(y = label_position, label = n, color = `tem_internet?`), size = 6) +
  coord_polar(theta = 'y') +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(
    title = 'Quantidade de domicílios com acesso à internet (amostral)',
    caption = 'Fonte: Núcleo da Informação e Coordenação do Ponto BR - NIC.br.')

# Acesso a Internet por classe econômica
domicilio |> 
  group_by(CLASSE_CB2015) |> 
  count() |>
  ungroup() |> 
  mutate(classe_economica = case_when(
    CLASSE_CB2015 == 1 ~ 'A',
    CLASSE_CB2015 == 2 ~ 'B',
    CLASSE_CB2015 == 3 ~ 'C',
    CLASSE_CB2015 == 4 ~ 'DE')) |> 
  ggplot(aes(as.factor(classe_economica), n, fill = classe_economica)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n), size = 4, hjust= 0.5, vjust = 2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())




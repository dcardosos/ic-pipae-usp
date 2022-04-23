

read_sql <- function(query){
  
  bigrquery::bq_project_query(
    x = 'rodoviar',
    query = query) |> 
    bigrquery::bq_table_download(bigint = "integer64")
  
}


query <- "
  SELECT * FROM `rodoviar.br_dpfr_acidentes_rodoviarios.microdados`
"

df <- read_sql(query)

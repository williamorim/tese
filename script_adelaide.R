
# Pacotes ------------------------------------------------------------------

# Pacotes utilizados. Caso não tenha instalado na sua máquina,
# basta descomentar e rodar o código embaixo de cada library para instalação.

library(tidyverse)
# install.packages(tidyverse)

library(koffing)
# install.packages("devtools")
# devtools::install_github("atmoschem/koffing")


# Definindo estações e parâmetros ------------------------------------------

# As estações disponíveis podem ser acessadas pelo objeto abaixo.
View(cetesb_station_ids)

# Os parâmetros disponíveis podem ser acessados pelo objeto abaixo.
View(cetesb_param_ids)

# Salvar os ids das estações e parâmetros nos objetos abaixo.
# Nesse exemplo, estamos pegando informação de O3 e NO para a estação
# Mauá e Dieadema. 

stations_ids <- c(65, 92)
parameters_id <- c(63, 17)


# Funções ------------------------------------------------------------------

# Função para baixar os arquivos e não parar caso haja algum erro em alguma
# das tentativas. Substituir o login e senha com as suas informações de acesso
# ao sistema Qualar.

safe_scraper_cetesb <- possibly(
  function(station, parameter, start, end) {
    koffing::scraper_cetesb(
      station = station,
      parameter = parameter,
      start = start,
      end = end,
      login = "seu_login", # colocar seu login entre as aspas
      password = "sua_senha" # colocar sua senha entre as aspas
    )
  },
  otherwise = NULL
)

# Função que acessa o sistema e baixa os dados.
# Por padrão, ela vai baixar todos os dados de um parâmetro para uma estação
# e um ano inteiro.

# Os dados serão salvos em arquivos separados no diretório de trabalho da
# sua sessão.

get_cetesb_data <- function(station, parameter, year) {
  
  start <- str_c("01/01/", year)
  end <- str_c("31/12/", year)
  
  df <- safe_scraper_cetesb(station, parameter, start, end)
  
  if(!is.null(df)) {
    
    write_csv(
      x = df, 
      path = str_c(
        "df-", 
        parameter, "-", station, "-", year, ".csv"
      )
    )
    
    print(
      str_c(
        "Dados de ", parameter, " da estação ", 
        station, " de ", year, " baixados com sucesso."
      )
    )
  } else {
    print(
      str_c(
        "Não foi possível baixar dados de ", parameter, " da estação ", 
        station, " de ", year
      )
    )
  }
  
}


# Baixando os dados --------------------------------------------------------

# Definir os anos que você quer baixar.
# No exemplo, vamos baixar dados de 2016, 2017 e 2018
years <- 2016:2018

# Rodar código abaixo para baixar os dados
for(i in 1:length(years)) {
  for(j in 1:length(parameters_id))
    walk(
      stations_ids$id, 
      get_cetesb_data, 
      parameter = parameters_id[j], 
      year = years[i]
    )
}


# Juntando os arquivos -----------------------------------------------------

# Colocar o caminho para a pasta onde estão salvos os arquivos.

paths <- list.files(
  path = "pasta-onde-estao-salvos-os-arquivos-baixados", 
  full.names = TRUE
)
paths <- paths[str_detect(paths, ".csv") & file.size(paths) > 2000]

# Lendo e juntando todos os arquivos. Ignorar os warnings.

df <- map_dfr(
  paths, 
  read_csv,
  col_types = str_c(rep("c", 17), collapse = "")
)

# Limpeza da base final. Opcional.

df <- df %>%
  select(
    parameter = `Nome Parâmetro`,
    stationname = `Nome Estação`,
    date = Data,
    hour = Hora,
    mass_conc = `Média Horária`
  ) %>%
  mutate(
    mass_conc = str_replace(mass_conc, ",", "."),
    mass_conc = as.numeric(mass_conc),
    date = lubridate::dmy(date),
    hour = str_sub(hour, start = 1, end = 2),
    hour = as.numeric(hour),
    dayofweek = lubridate::wday(date, label = TRUE),
    mass_conc = ifelse(abs(mass_conc) == 9999999, NA, mass_conc),
    parameter = str_replace_all(parameter, " [(].*", "")
  ) %>% 
  spread(parameter, mass_conc)

# Salvando a base final em rds.

write_rds(
  df,
  path = "base-final.rds",
  compress = "gz"
)

# Funções de coleta de dados do IPCA via rbcb e sidrar

#' Coleta o IPCA mensal (variação percentual mês a mês)
#'
#' Busca a série 433 do Banco Central via pacote rbcb.
#' Retorna todos os dados disponíveis desde o início da série.
#'
#' @return Tibble com colunas: data (Date), ipca_mm (numeric)
coletar_ipca_mensal <- function() {
  dados <- rbcb::get_series(
    code     = 433,
    start_date = "1980-01-01",
    as       = "tibble"
  )

  dados |>
    dplyr::rename(ipca_mm = `433`) |>
    dplyr::mutate(data = as.Date(date)) |>
    dplyr::select(data, ipca_mm)
}


#' Coleta a meta de inflação anual definida pelo CMN
#'
#' Busca a série 13521 do Banco Central via pacote rbcb.
#' Retorna todos os dados disponíveis desde o início da série.
#'
#' @return Tibble com colunas: data (Date), meta_inflacao (numeric)
coletar_meta_inflacao <- function() {
  dados <- rbcb::get_series(
    code     = 13521,
    start_date = "1999-01-01",
    as       = "tibble"
  )

  dados |>
    dplyr::rename(meta_inflacao = `13521`) |>
    dplyr::mutate(data = as.Date(date)) |>
    dplyr::select(data, meta_inflacao)
}


#' Coleta o IPCA por grupos de despesa (variação e peso)
#'
#' Busca a tabela 7060 do SIDRA/IBGE via pacote sidrar.
#' Realiza duas chamadas separadas: variação mensal (variável 63)
#' e peso do grupo no índice (variável 66), cobrindo os 9 grupos
#' de despesa do IPCA. Ao final, une as duas chamadas em formato tidy.
#'
#' Grupos consultados (c315):
#'   7170 - Alimentação e bebidas
#'   7445 - Habitação
#'   7486 - Artigos de residência
#'   7558 - Vestuário
#'   7625 - Transportes
#'   7660 - Saúde e cuidados pessoais
#'   7712 - Despesas pessoais
#'   7766 - Educação
#'   7786 - Comunicação
#'
#' @return Tibble com colunas: data (Date), grupo (character),
#'         variacao (numeric), peso (numeric)
coletar_ipca_grupos <- function() {
  codigos_grupos <- c(7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786)
  c315_param     <- paste(codigos_grupos, collapse = ",")

  url_base <- paste0(
    "/t/7060/n1/all",
    "/v/{var}",
    "/p/all",
    "/c315/", c315_param
  )

  buscar_variavel <- function(var) {
    sidrar::get_sidra(
      api = gsub("\\{var\\}", var, url_base)
    ) |>
      tibble::as_tibble()
  }

  raw_variacao <- buscar_variavel(63)
  raw_peso     <- buscar_variavel(66)

  limpar <- function(raw, col_nome) {
    raw |>
      dplyr::select(
        periodo  = `Mês (Código)`,
        grupo    = `Geral, grupo, subgrupo, item e subitem`,
        valor    = Valor
      ) |>
      dplyr::mutate(
        data  = lubridate::ym(periodo),
        valor = as.numeric(valor)
      ) |>
      dplyr::select(data, grupo, valor) |>
      dplyr::rename(!!col_nome := valor)
  }

  variacao <- limpar(raw_variacao, "variacao")
  peso     <- limpar(raw_peso,     "peso")

  dplyr::left_join(variacao, peso, by = c("data", "grupo")) |>
    dplyr::arrange(data, grupo)
}

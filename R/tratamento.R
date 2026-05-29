# Funções de tratamento e transformação dos dados do IPCA

#' Calcula o IPCA acumulado em 12 meses (janela móvel)
#'
#' Aplica produto encadeado sobre os últimos 12 meses:
#' acumulado = prod(1 + x/100) - 1, expresso em percentual.
#' Retorna NA para as primeiras 11 observações, onde a janela
#' ainda não está completa.
#'
#' @param df Tibble com colunas: data (Date), ipca_mm (numeric)
#' @return O mesmo tibble acrescido da coluna ipca_12m (numeric)
calcular_acumulado_12m <- function(df) {
  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(
      ipca_12m = slider::slide_dbl(
        ipca_mm,
        .f     = \(x) (prod(1 + x / 100) - 1) * 100,
        .before = 11,
        .complete = TRUE
      )
    )
}


#' Calcula o IPCA acumulado no ano (reinicia em janeiro)
#'
#' Para cada mês, acumula todos os valores desde janeiro do mesmo ano.
#' Em janeiro o acumulado é igual à variação mensal do próprio mês.
#' Expresso em percentual.
#'
#' @param df Tibble com colunas: data (Date), ipca_mm (numeric)
#' @return O mesmo tibble acrescido da coluna ipca_ano (numeric)
calcular_acumulado_ano <- function(df) {
  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::group_by(ano) |>
    dplyr::mutate(
      ipca_ano = purrr::accumulate(
        ipca_mm,
        \(acc, x) (acc / 100 + 1) * (x / 100 + 1) * 100 - 100,
        .init = 0
      )[-1]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-ano)
}


#' Prepara os dados para o gráfico de sazonalidade mensal
#'
#' Filtra a série a partir de ano_inicio e cria colunas auxiliares
#' de mês e ano para empilhar os anos. O ano corrente recebe a flag
#' `ano_atual = TRUE` para destaque visual nos gráficos.
#'
#' @param df Tibble com colunas: data (Date), ipca_mm (numeric)
#' @param ano_inicio Inteiro; primeiro ano a incluir (padrão: 2015)
#' @return Tibble com colunas: data, ipca_mm, mes (int), ano (int),
#'         mes_abr (factor ordenado), ano_atual (logical)
preparar_sazonal <- function(df, ano_inicio = 2015) {
  ano_corrente <- lubridate::year(max(df$data, na.rm = TRUE))

  abreviacoes <- c(
    "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
    "Jul", "Ago", "Set", "Out", "Nov", "Dez"
  )

  df |>
    dplyr::filter(lubridate::year(data) >= ano_inicio) |>
    dplyr::mutate(
      mes      = lubridate::month(data),
      ano      = lubridate::year(data),
      mes_abr  = factor(abreviacoes[mes], levels = abreviacoes),
      ano_atual = ano == ano_corrente
    ) |>
    dplyr::arrange(data)
}


#' Prepara as contribuições de cada grupo para o IPCA cheio
#'
#' A contribuição de cada grupo é calculada como:
#'   contribuicao = variacao * peso / 100
#'
#' Isso reproduz, pela soma dos 9 grupos, a variação total do IPCA
#' no mês — usado como sanity check interno.
#'
#' Retorna uma lista com dois data frames:
#'   - historico: contribuições mensais de todos os grupos
#'   - mes_atual: recorte apenas do mês mais recente disponível
#'
#' @param df_grupos Tibble com colunas: data (Date), grupo (character),
#'        variacao (numeric), peso (numeric)
#' @return Lista com elementos `historico` (tibble) e `mes_atual` (tibble)
preparar_contribuicoes <- function(df_grupos) {
  historico <- df_grupos |>
    dplyr::arrange(data, grupo) |>
    dplyr::mutate(contribuicao = variacao * peso / 100)

  # sanity check: soma das contribuições deve aproximar o IPCA cheio
  soma_por_mes <- historico |>
    dplyr::group_by(data) |>
    dplyr::summarise(
      n_grupos      = dplyr::n(),
      soma_contrib  = sum(contribuicao, na.rm = TRUE),
      .groups       = "drop"
    )

  meses_incompletos <- soma_por_mes |>
    dplyr::filter(n_grupos != 9)

  if (nrow(meses_incompletos) > 0) {
    warning(
      nrow(meses_incompletos),
      " mês(es) com número de grupos diferente de 9. ",
      "Verifique os dados de entrada."
    )
  }

  data_atual <- max(historico$data, na.rm = TRUE)

  mes_atual <- historico |>
    dplyr::filter(data == data_atual) |>
    dplyr::arrange(dplyr::desc(contribuicao))

  list(
    historico = historico,
    mes_atual = mes_atual
  )
}


#' Expande a meta anual de inflação para frequência mensal
#'
#' A série 13521 fornece um valor por ano. Esta função expande a meta
#' para cada mês da série do IPCA, fazendo join pelo ano.
#' Meses sem meta correspondente recebem NA.
#'
#' @param df_ipca Tibble com coluna data (Date)
#' @param df_meta Tibble com colunas data (Date), meta_inflacao (numeric)
#' @return Tibble com colunas: data, ipca_mm (se presente), meta_inflacao
preparar_meta_mensal <- function(df_ipca, df_meta) {
  meta_anual <- df_meta |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::select(ano, meta_inflacao)

  df_ipca |>
    dplyr::mutate(ano = lubridate::year(data)) |>
    dplyr::left_join(meta_anual, by = "ano") |>
    dplyr::select(-ano)
}

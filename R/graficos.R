# Funções de visualização do IPCA com ggplot2

cor_primaria <- "#282f6b"
cor_laranja  <- "#d97706"
cor_verde    <- "#059669"
cor_cinza    <- "#6b7280"
cor_banda    <- "#05966920"

salvar_png_ <- function(p, nome, largura = 10, altura = 5.5) {
  dir.create("output", showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    filename = file.path("output", paste0(nome, ".png")),
    plot     = p,
    width    = largura,
    height   = altura,
    dpi      = 150
  )
  invisible(p)
}

tema_base <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold"),
      axis.title         = ggplot2::element_text(size = 9, color = cor_cinza),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_blank()
    )
}


#' Gráfico de barras do IPCA mensal (últimos 24 meses)
#'
#' Exibe a variação mensal do IPCA em barras verticais, com rótulo
#' numérico acima (ou abaixo) de cada barra. O eixo y é truncado ao
#' intervalo observado nos dados para evitar espaço em branco excessivo.
#'
#' @param df Tibble com colunas: data (Date), ipca_mm (numeric)
#' @return Objeto ggplot (salvo em output/grafico_ipca_mensal.png)
grafico_ipca_mensal <- function(df) {
  dados <- df |>
    dplyr::arrange(.data$data) |>
    dplyr::slice_tail(n = 24) |>
    dplyr::mutate(
      rotulo   = formatC(.data$ipca_mm, digits = 2, format = "f"),
      positivo = .data$ipca_mm >= 0
    )

  lim_y_min <- min(dados$ipca_mm, na.rm = TRUE)
  lim_y_max <- max(dados$ipca_mm, na.rm = TRUE)
  margem    <- (lim_y_max - lim_y_min) * 0.20

  p <- ggplot2::ggplot(
    dados,
    ggplot2::aes(x = .data$data, y = .data$ipca_mm)
  ) +
    ggplot2::geom_col(fill = cor_primaria, width = 20) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data$rotulo,
        vjust = ifelse(.data$positivo, -0.4, 1.3)
      ),
      size     = 2.8,
      color    = cor_primaria,
      fontface = "bold"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b\n%Y"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(lim_y_min - margem, lim_y_max + margem),
      labels = scales::label_number(decimal.mark = ",", suffix = "%")
    ) +
    ggplot2::labs(
      title = "IPCA — Variação Mensal",
      x     = NULL,
      y     = "Variação (% a.m.)"
    ) +
    tema_base()

  salvar_png_(p, "grafico_ipca_mensal")
}


#' Gráfico do IPCA acumulado em 12 meses com meta variável
#'
#' Plota a série de acumulado 12m junto à meta anual (expandida para
#' frequência mensal) e a banda de tolerância de ±1,5 p.p. em torno
#' da meta. Uma caixa de anotação exibe o último valor disponível
#' no canto superior esquerdo do painel.
#'
#' @param df      Tibble com colunas: data (Date), ipca_12m (numeric)
#' @param df_meta Tibble com colunas: data (Date), meta_inflacao (numeric)
#' @return Objeto ggplot (salvo em output/grafico_ipca_12m.png)
grafico_ipca_12m <- function(df, df_meta) {
  meta_mensal <- df_meta |>
    dplyr::mutate(
      meta_sup = .data$meta_inflacao + 1.5,
      meta_inf = .data$meta_inflacao - 1.5
    )

  dados <- df |>
    dplyr::filter(!is.na(.data$ipca_12m)) |>
    dplyr::left_join(meta_mensal, by = "data")

  ultimo       <- dplyr::slice_tail(dados, n = 1)
  ultimo_valor <- formatC(ultimo$ipca_12m, digits = 2, format = "f")
  ultimo_data  <- format(ultimo$data, "%b/%Y")
  anotacao     <- paste0(ultimo_valor, "%\n", ultimo_data)

  intervalo  <- as.numeric(max(dados$data) - min(dados$data))
  x_anotacao <- min(dados$data) + intervalo * 0.02

  p <- ggplot2::ggplot(dados, ggplot2::aes(x = .data$data)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$meta_inf,
        ymax = .data$meta_sup
      ),
      fill  = cor_banda,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$meta_inflacao, color = "Meta"),
      linetype  = "dashed",
      linewidth = 0.7,
      na.rm     = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$ipca_12m, color = "IPCA 12m"),
      linewidth = 1,
      na.rm     = TRUE
    ) +
    ggplot2::annotate(
      "label",
      x          = x_anotacao,
      y          = max(dados$ipca_12m, na.rm = TRUE),
      label      = anotacao,
      hjust      = 0,
      vjust      = 1,
      size       = 3,
      color      = cor_primaria,
      fill       = "white",
      label.size = 0.3
    ) +
    ggplot2::scale_color_manual(
      values = c("IPCA 12m" = cor_primaria, "Meta" = cor_verde)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%")
    ) +
    ggplot2::labs(
      title = "IPCA — Acumulado 12 Meses e Meta",
      x     = NULL,
      y     = "Variação acumulada (% a.a.)"
    ) +
    tema_base()

  salvar_png_(p, "grafico_ipca_12m")
}


#' Gráfico de sazonalidade: uma linha por ano
#'
#' Sobrepõe os perfis mensais de cada ano para revelar padrão sazonal.
#' O ano corrente é destacado com linha mais espessa e cor primária;
#' os demais anos recebem tonalidades de cinza escalonadas.
#'
#' @param df_saz Tibble retornado por preparar_sazonal(), com colunas:
#'   mes_abr (factor), ipca_mm (numeric), ano (int), ano_atual (logical)
#' @return Objeto ggplot (salvo em output/grafico_sazonal.png)
grafico_sazonal <- function(df_saz) {
  anos        <- sort(unique(df_saz$ano))
  anos_outros <- anos[anos != max(anos)]
  n_outros    <- length(anos_outros)
  cinzas      <- grDevices::colorRampPalette(
    c("#d1d5db", "#374151")
  )(n_outros)
  paleta <- stats::setNames(cinzas, as.character(anos_outros))
  paleta[as.character(max(anos))] <- cor_primaria

  p <- ggplot2::ggplot(
    df_saz,
    ggplot2::aes(
      x         = .data$mes_abr,
      y         = .data$ipca_mm,
      group     = as.factor(.data$ano),
      color     = as.factor(.data$ano),
      linewidth = .data$ano_atual
    )
  ) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::scale_linewidth_manual(
      values = c("FALSE" = 0.5, "TRUE" = 1.4),
      guide  = "none"
    ) +
    ggplot2::scale_color_manual(values = paleta, name = NULL) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark = ",", suffix = "%")
    ) +
    ggplot2::labs(
      title = "IPCA — Sazonalidade Mensal por Ano",
      x     = NULL,
      y     = "Variação (% a.m.)"
    ) +
    tema_base() +
    ggplot2::theme(
      legend.position    = "right",
      panel.grid.major.x = ggplot2::element_line(
        color     = "#e5e7eb",
        linewidth = 0.3
      )
    )

  salvar_png_(p, "grafico_sazonal")
}


#' Gráfico de contribuições dos grupos ao IPCA do mês
#'
#' Barras horizontais ordenadas da maior para a menor contribuição.
#' Cada barra representa a contribuição em pontos percentuais
#' (variacao * peso / 100). Rótulos com o valor em p.p. são
#' posicionados na ponta de cada barra.
#'
#' @param df Tibble com colunas: grupo (character), contribuicao (numeric).
#'   Tipicamente o elemento `mes_atual` retornado por preparar_contribuicoes().
#' @return Objeto ggplot (salvo em output/grafico_contribuicoes.png)
grafico_contribuicoes <- function(df) {
  dados <- df |>
    dplyr::mutate(
      grupo     = stringr::str_wrap(.data$grupo, width = 28),
      grupo     = forcats::fct_reorder(.data$grupo, .data$contribuicao),
      cor_barra = ifelse(.data$contribuicao >= 0, cor_primaria, cor_laranja),
      rotulo    = formatC(.data$contribuicao, digits = 2, format = "f")
    )

  p <- ggplot2::ggplot(
    dados,
    ggplot2::aes(x = .data$contribuicao, y = .data$grupo)
  ) +
    ggplot2::geom_col(fill = dados$cor_barra, width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(.data$rotulo, " p.p."),
        hjust = ifelse(.data$contribuicao >= 0, -0.1, 1.1)
      ),
      size  = 3,
      color = cor_cinza
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.05, 0.25)),
      labels = scales::label_number(
        decimal.mark = ",",
        suffix = " p.p."
      )
    ) +
    ggplot2::labs(
      title = "IPCA — Contribuição por Grupo no Mês",
      x     = "Contribuição (p.p.)",
      y     = NULL
    ) +
    tema_base() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        color     = "#e5e7eb",
        linewidth = 0.3
      ),
      panel.grid.major.y = ggplot2::element_blank()
    )

  salvar_png_(p, "grafico_contribuicoes")
}

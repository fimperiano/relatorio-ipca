# Relatório Mensal do IPCA

Relatório automatizado de análise da inflação ao consumidor (IPCA) gerado com [Quarto](https://quarto.org) e publicado via [Posit Connect Cloud](https://connect.posit.cloud).

## Sobre

O relatório consolida as principais dimensões do IPCA em quatro análises:

- **Variação mensal** — últimos 24 meses
- **Acumulado 12 meses** — frente à meta do CMN e banda de tolerância (±1,5 p.p.)
- **Sazonalidade** — perfil mensal comparado ao histórico desde 2015
- **Contribuições por grupo** — decomposição dos 9 grupos de despesa em pontos percentuais

## Fontes

| Dado | Fonte | Identificador |
|---|---|---|
| IPCA mensal | Banco Central do Brasil | Série 433 |
| Meta de inflação | Banco Central do Brasil | Série 13521 |
| IPCA por grupos | IBGE/SIDRA | Tabela 7060 |

## Estrutura

```
.
├── R/
│   ├── coleta.R       # Funções de coleta via rbcb e sidrar
│   ├── tratamento.R   # Transformações (acumulados, sazonalidade, contribuições)
│   └── graficos.R     # Gráficos ggplot2 → salva em output/
├── relatorio_ipca.qmd # Documento principal
├── _quarto.yml        # Configuração do projeto Quarto
└── output/            # PNGs gerados (não versionado)
```

## Como rodar

### Pré-requisitos

- R ≥ 4.3
- Quarto ≥ 1.4
- Pacotes R:

```r
install.packages(c(
  "dplyr", "ggplot2", "lubridate", "scales",
  "slider", "purrr", "tibble", "stringr",
  "forcats", "sidrar", "knitr"
))
remotes::install_github("wilsonfreitas/rbcb")
```

### Renderizar

```bash
quarto render relatorio_ipca.qmd
```

O HTML é gerado na raiz do projeto. Os gráficos são salvos em `output/` durante a renderização.

## Atualização mensal

O relatório busca automaticamente os dados mais recentes a cada renderização. Com `execute: freeze: auto` no `_quarto.yml`, basta re-renderizar após a divulgação mensal do IPCA pelo IBGE.

#' Teste exato de Fisher
#'
#' Realiza o teste exato de Fisher a partir de dois vetores categoricos, construindo uma tabela de contingencia.
#'
#' @param var1 Vetor categorico (fator ou caractere).
#' @param var2 Vetor categorico (fator ou caractere).
#' @param titulo Titulo do gráfico (string). Default: "Teste Exato de Fisher"
#' @param x Nome do eixo x no gráfico (string). Default: NULL (usa nome da variável).
#' @param y Nome do eixo y no gráfico (string). Default: "Proporcao"
#' @param mostrar_tabela Lógico. Se TRUE, exibe a tabela de contingência no console. Default: TRUE.
#' @param ajuda Lógico. Se TRUE, mostra explicação detalhada da função. Default: FALSE.
#'
#' @return Uma lista com:
#' \describe{
#'   \item{tabela}{Tabela de contingencia}
#'   \item{p.valor}{Valor-p do teste}
#' }
#'
#' @examples
#' Grupo_Pacientes <- c('A', 'A', 'B', 'B', 'A', 'A', 'B', 'B')
#' Doente <- c('Sim', 'Nao', 'Sim', 'Nao', 'Nao', 'Nao', 'Sim', 'Nao')
#' teste.fisher(Grupo_Pacientes, Doente)
#'
#' @export


teste.fisher <- function(var1, var2, titulo = "Teste Exato de Fisher",
                         x = NULL, y = "Proporcao", mostrar_tabela = TRUE, ajuda = FALSE) {

  if (ajuda || missing(var1) || missing(var2)) {
    cat(
      "Funcao teste.fisher()

Descricao:
  Realiza o Teste Exato de Fisher para avaliar se ha associacao significativa entre duas variaveis categoricas.

Quando usar:
  - Quando voce tem **duas variaveis categoricas** (fatores).
  - Quando a **tabela de contingencia e pequena (especialmente 2x2)**.
  - Quando ha **frequencias esperadas menores que 5** - situacao onde o teste qui-quadrado nao e confiavel.
  - E o teste preferido para amostras pequenas.

Diferenca entre Fisher e Qui-quadrado:
  - **Fisher** calcula a probabilidade exata das combinacoes observadas.
  - **Qui-quadrado** usa uma aproximacao baseada em distribuicoes teoricas, o que exige maiores tamanhos amostrais.

Limitacoes:
  - Para tabelas maiores que 2x2, o calculo exato pode ser **computacionalmente lento** - o R usa aproximacoes.

Argumentos:
  var1, var2     : variaveis categoricas
  titulo         : titulo do grafico
  x, y           : nomes dos eixos
  mostrar_tabela : exibe ou nao a tabela de contingencia
  ajuda          : se TRUE, mostra esta explicacao

Exemplo:
    Grupo_Pacientes <- c('A', 'A', 'B', 'B', 'A', 'A', 'B', 'B')
    Doente <- c('Sim', 'Nao', 'Sim', 'Nao', 'Nao', 'Nao', 'Sim', 'Nao')
    teste.fisher(Grupo_Pacientes, Doente)
")
    return(invisible(NULL))
  }

  pacotes <- c("ggplot2", "dplyr")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "nao esta instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }

  if (length(var1) != length(var2)) stop("As variaveis devem ter o mesmo comprimento.")

  nome1 <- deparse(substitute(var1))
  nome2 <- deparse(substitute(var2))
  nome1 <- sub(".*\\$", "", nome1)
  nome2 <- sub(".*\\$", "", nome2)

  if (is.null(x)) x <- nome1
  if (is.null(y)) y <- "Proporcao"

  tabela <- table(var1, var2)

  if (mostrar_tabela) {
    cat("Tabela de contingencia (observada):\n")
    print(tabela)
    cat("\n")
  }

  if (any(dim(tabela) > 2)) {
    message("A tabela tem dimensao maior que 2x2. O teste de Fisher pode ser computacionalmente pesado e o p-valor e uma aproximacao.")
  }

  teste <- stats::fisher.test(tabela)

  cat("Teste Exato de Fisher\n")
  cat("---------------------\n")
  cat("p-valor:", signif(teste$p.value, 4), "\n")

  df_plot <- data.frame(var1 = var1, var2 = var2)
  df_prop <- df_plot |>
    dplyr::group_by(var1, var2) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(var1) |>
    dplyr::mutate(prop = n / sum(n))

  g <- ggplot2::ggplot(df_prop, ggplot2::aes(x = var1, y = prop, fill = var2)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::labs(
      title = titulo,
      x = x,
      y = y,
      fill = nome2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  print(g)
  invisible(teste)
}

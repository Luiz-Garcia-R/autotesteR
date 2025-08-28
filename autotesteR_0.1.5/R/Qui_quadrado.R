#' Teste de qui-quadrado
#'
#' Aplica o teste qui-quadrado ou Fisher para associacao entre duas variaveis categoricas.
#'
#' @param var1 Vetor categorico (grupo 1).
#' @param var2 Vetor categorico (grupo 2).
#' @param titulo Titulo do grafico (string). Default: "Teste Qui-Quadrado".
#' @param x Rotulo do eixo x no grafico (string). Default: NULL (usa nome da variavel).
#' @param y Rotulo do eixo y no grafico (string). Default: "Proporcao".
#' @param mostrar_tabela Logico. Se TRUE, exibe a tabela de contingencia no console. Default: TRUE.
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Logico. Se TRUE, imprime mensagens sobre o teste e frequencias esperadas. Default: TRUE.
#'
#' @return Resultado do teste e tabela de contingencia.
#'
#' @examples
#' Grupo_Pacientes <- c(rep('A', 48), rep('B', 24))
#' Doente <- c(rep('A', 36), rep('B', 12), rep('A', 12), rep('B', 12))
#' teste.qui(Grupo_Pacientes, Doente)
#'
#' @export

teste.qui <- function(var1, var2, titulo = "Teste Qui-Quadrado",
                      x = NULL, y = "Proporcao", mostrar_tabela = TRUE,
                      ajuda = FALSE, verbose = TRUE) {

  if (ajuda || missing(var1) || missing(var2)) {
    message(
      "Funcao teste.qui()

Descricao:
  Realiza o Teste Qui-Quadrado de Pearson para avaliar associacao entre duas variaveis categoricas.

Quando usar:
  - Duas variaveis categoricas com amostras grandes
  - A tabela de contingencia pode ter mais de 2 categorias por variavel
  - Frequencias esperadas nas celulas devem ser >= 5

Assuncoes:
  - Observacoes independentes
  - Categorias mutuamente exclusivas

Limitacoes:
  - Nao recomendado para tabelas pequenas; use `teste.fisher()` nesses casos

Comparacao:
  - Qui-quadrado: aproximacao estatistica (boa para amostras grandes)
  - Fisher: p-valores exatos (melhor para amostras pequenas)

Argumentos:
  var1, var2     : variaveis categoricas
  titulo         : titulo do grafico
  x, y           : rotulos dos eixos
  mostrar_tabela : exibe tabela de contingencia
  ajuda          : mostra esta explicacao
  verbose        : imprime mensagens detalhadas

Exemplo:
  Grupo_Pacientes <- c(rep('A', 48), rep('B', 24))
  Doente <- c(rep('A', 36), rep('B', 12), rep('A', 12), rep('B', 12))
  teste.qui(Grupo_Pacientes, Doente)"
    )
    return(invisible(NULL))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("O pacote ggplot2 nao esta instalado. Instale com install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("O pacote dplyr nao esta instalado. Instale com install.packages('dplyr')")
  }

  if (length(var1) != length(var2)) stop("As variaveis devem ter o mesmo comprimento.")

  nome1 <- deparse(substitute(var1))
  nome2 <- deparse(substitute(var2))
  nome1 <- sub(".*\\$", "", nome1)
  nome2 <- sub(".*\\$", "", nome2)

  if (is.null(x)) x <- nome1
  if (is.null(y)) y <- "Proporcao"

  tabela <- table(var1, var2)

  if (verbose && mostrar_tabela) {
    message("Tabela de contingencia (observada):")
    print(tabela)
  }

  teste <- suppressWarnings(stats::chisq.test(tabela))

  freq_esperadas <- teste$expected
  if (verbose && any(freq_esperadas < 5)) {
    message("Algumas frequencias esperadas sao < 5. Considere usar o teste exato de Fisher para maior precisao.")
  }

  if (verbose) {
    message("Teste Qui-Quadrado de Pearson:")
    message("------------------------------")
    message("Estatistica X2: ", round(teste$statistic, 3))
    message("p-valor: ", signif(teste$p.value, 4))
    message("Graus de liberdade: ", teste$parameter)
  }

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

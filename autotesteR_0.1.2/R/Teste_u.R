#' Teste de Mann-Whitney
#'
#' Realiza o teste de Mann-Whitney para comparacao de dois grupos nao-parametricos.
#'
#' @param ... Dois vetores numericos independentes (ex: grupo1, grupo2).
#' @param titulo Titulo do grafico (string). Default: "Teste de Mann-Whitney".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#'
#' @importFrom stats sd
#' @return Resultado do teste e grafico.

#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' teste.u(x, y)

#' @export


teste.u <- function(..., titulo = "Teste de Mann-Whitney", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- list(...)

  if (ajuda || length(grupos) != 2) {
    cat("
Funcao teste.u()

Descricao:
  Realiza o teste de Mann-Whitney (ou Wilcoxon rank-sum) para comparar a mediana de dois grupos independentes.

Quando usar:
  - Quando os dados nao seguem distribuicao normal.
  - Quando se deseja comparar dois grupos independentes.
  - Quando se deseja uma alternativa nao parametrica ao teste t.

Argumentos:
  ...     : dois vetores numericos (ex: grupo1, grupo2)
  titulo  : titulo do grafico (padrao = 'Teste de Mann-Whitney')
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe essa explicacao

Exemplo:
    x <- c(1, 3, 5, 6)
    y <- c(7, 8, 9, 12)
    teste.u(x, y)
")
    return(invisible(NULL))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("O pacote ggplot2 nao esta instalado. Instale com install.packages('ggplot2')")
  }

  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser vetores numericos.")

  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  resultado <- stats::wilcox.test(grupos[[1]], grupos[[2]], exact = FALSE)
  pval <- resultado$p.value

  # Calculo de media e desvio padrao para cada grupo
  medias <- sapply(grupos, mean)
  desvios <- sapply(grupos, sd)

  # Imprime resumo no console com p-valor
  cat("Resumo dos grupos:\n")
  for (i in seq_along(nomes)) {
    cat(sprintf(" - %s: Media = %.2f, Desvio Padrao = %.2f\n", nomes[i], medias[i], desvios[i]))
  }
  cat(sprintf("p-valor do teste de Mann-Whitney: %.4g\n", pval))

  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", signif(pval, 3))
  }

  signif_label <- if (pval < 0.001) {
    "***"
  } else if (pval < 0.01) {
    "**"
  } else if (pval < 0.05) {
    "*"
  } else {
    ""
  }

  y_max <- max(valores, na.rm = TRUE)
  y_pos <- y_max + 0.1 * abs(y_max)

  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 6) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = p_label,
      x = x,
      y = y
    )

  print(g)
  invisible(resultado)
}





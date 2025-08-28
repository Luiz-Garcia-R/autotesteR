#' Teste de Student
#'
#' Realiza o teste t para comparacao de medias entre dois grupos,
#' com verificacoes de normalidade e homogeneidade e apresenta
#' resultado com interpretacao e grafico.
#'
#' @param ... Dois vetores numericos, correspondendo aos grupos para comparacao.
#' @param titulo Titulo do grafico (string). Default: "Teste t".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, mostra explicacao detalhada da funcao. Default: FALSE.
#'
#' @return Lista invisivel com:
#' \describe{
#'   \item{resumo}{Resumo estatistico (media e desvio padrao) por grupo}
#'   \item{resultado}{Resultado do teste t (objeto stats::htest)}
#'   \item{grafico}{Objeto ggplot2 com visualizacao dos grupos}
#' }
#' @export
#'
#' @examples
#' x <- rnorm(30, 10)
#' y <- rnorm(30, 12)
#' teste.t(x, y)
teste.t <- function(..., titulo = "Teste t", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- list(...)

  if (ajuda || length(grupos) != 2) {
    message("
Funcao teste.t()

Descricao:
  Realiza o teste t de Student para comparar as medias de dois grupos.

Quando usar:
  - Quando os dados seguem distribuicao aproximadamente normal.
  - Quando se deseja comparar a media de dois grupos independentes.
  - Ideal para amostras com tamanho moderado a grande (n > 30) ou quando a normalidade pode ser assumida.

Argumentos:
  ...     : dois vetores numericos (ex: grupo1, grupo2)
  titulo  : titulo do grafico (padrao = 'Teste t')
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe esta explicacao

Exemplo:
    x <- rnorm(30, 10)
    y <- rnorm(30, 12)
    teste.t(x, y)
")
    return(invisible(NULL))
  }

  # Verifica pacotes
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("O pacote ggplot2 nao esta instalado.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("O pacote dplyr nao esta instalado.")

  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser vetores numericos.")

  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  resultado <- stats::t.test(grupos[[1]], grupos[[2]])
  pval <- resultado$p.value

  # Resumo estatistico com dplyr
  resumo <- dados |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      Media = round(mean(valor), 2),
      Desvio_Padrao = round(sd(valor), 2),
      .groups = "drop"
    )

  message("\nResumo estatistico por grupo:")
  print(resumo)

  # Rotulo do p-valor
  p_label <- if (pval < 0.001) "p < 0.001" else paste0("p = ", signif(pval, 3))
  signif_label <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""

  # Posicao para anotacao
  y_pos <- max(valores, na.rm = TRUE) + 0.1 * diff(range(valores, na.rm = TRUE))

  # Criar grafico
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::annotate("text", x = mean(1:2), y = y_pos, label = signif_label, size = 6) +
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
  invisible(list(resumo = resumo, resultado = resultado, grafico = g))
}

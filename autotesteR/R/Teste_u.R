#' Teste de Mann-Whitney
#'
#' Realiza o teste de Mann-Whitney para comparacao de dois grupos nao-parametricos,
#' apresentando resultado com interpretacao e grafico.
#'
#' @param ... Dois vetores numericos independentes (ex: grupo1, grupo2).
#' @param titulo Titulo do grafico (string). Default: "Teste de Mann-Whitney".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Se TRUE, imprime mensagens detalhadas (default = TRUE)
#' @return Lista invisivel com:
#' \describe{
#'   \item{resumo}{Resumo estatistico (media e desvio padrao) por grupo}
#'   \item{resultado}{Resultado do teste (objeto stats::htest)}
#'   \item{grafico}{Objeto ggplot2 com visualizacao dos grupos}
#' }
#' @export
#'
#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' teste.u(x, y)

teste.u <- function(..., titulo = "Teste de Mann-Whitney", x = "Grupo", y = "Valor",
                    ajuda = FALSE, verbose = TRUE) {

  grupos <- list(...)

  # Mensagem de ajuda
  if (ajuda || length(grupos) != 2) {
    if (verbose) {
      message("
Funcao teste.u()

Descricao:
  Realiza o teste de Mann-Whitney (Wilcoxon rank-sum) para comparar a mediana de dois grupos independentes.

Quando usar:
  - Dados nao-normais ou ordinais.
  - Comparar dois grupos independentes.
  - Alternativa nao-parametrica ao teste t.

Argumentos:
  ...     : dois vetores numericos (ex: grupo1, grupo2)
  titulo  : titulo do grafico (padrao = 'Teste de Mann-Whitney')
  x, y    : nomes dos eixos
  ajuda   : se TRUE, exibe esta explicacao
  verbose : se TRUE, imprime mensagens detalhadas

Exemplo:
    x <- c(1, 3, 5, 6)
    y <- c(7, 8, 9, 12)
    teste.u(x, y)
")
    }
    return(invisible(NULL))
  }

  # Verifica pacotes
  pacotes <- c("ggplot2", "dplyr")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "nao esta instalado. Instale com install.packages('", pkg, "')", sep = " "))
    }
  }

  # Nomes dos grupos
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  # Validacao
  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser vetores numericos.")

  # Preparar dados
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  # Teste de Mann-Whitney
  resultado <- stats::wilcox.test(grupos[[1]], grupos[[2]], exact = FALSE)
  pval <- resultado$p.value

  # Resumo estatistico
  resumo <- dados |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      Media = round(mean(valor), 2),
      Desvio_Padrao = round(sd(valor), 2),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumo estatistico por grupo:")
    print(resumo)
  }

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


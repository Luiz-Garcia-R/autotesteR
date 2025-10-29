#' Teste de Mann-Whitney (U)
#'
#' Realiza o teste de Mann-Whitney (Wilcoxon rank-sum) para comparacao de dois grupos independentes,
#' com resumo estatistico e visualizacao grafica.
#'
#' @param ... Dois vetores numericos ou um data.frame com duas colunas numericas.
#' @param titulo Titulo do grafico. Default: "Teste de Mann-Whitney".
#' @param x Nome do eixo x no grafico. Default: "Grupo".
#' @param y Nome do eixo y no grafico. Default: "Valor".
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Se TRUE, imprime mensagens detalhadas. Default: TRUE.
#' @importFrom stats median
#'
#' @return Lista invisivel com:
#' \describe{
#'   \item{resumo}{Resumo estatistico por grupo}
#'   \item{resultado}{Resultado do teste (objeto htest)}
#'   \item{grafico}{Objeto ggplot2 com a visualizacao}
#' }
#' @export
#'
#' @examples
#' x <- c(1, 3, 5, 6)
#' y <- c(7, 8, 9, 12)
#' teste.u(x, y)
#'
#' dados <- data.frame(grupoA = x, grupoB = y)
#' teste.u(dados)
teste.u <- function(..., titulo = "Teste de Mann-Whitney", x = "Grupo", y = "Valor",
                    ajuda = FALSE, verbose = TRUE) {

  grupos <- list(...)

  # === Entrada via data.frame ===
  if (length(grupos) == 1 && is.data.frame(grupos[[1]])) {
    df <- grupos[[1]]
    if (ncol(df) != 2)
      stop("O data.frame deve conter exatamente duas colunas numericas.")
    if (!all(sapply(df, is.numeric)))
      stop("As duas colunas do data.frame devem ser numericas.")
    nomes <- colnames(df)
    grupos <- as.list(df)
  }

  # === Mensagem de ajuda ===
  if (ajuda) {
    if (verbose) {
      message("
Funcao teste.u()

Descricao:
  Realiza o teste de Mann-Whitney (Wilcoxon rank-sum) para comparar dois grupos independentes.

Quando usar:
  - Dados nao-normais ou ordinais.
  - Comparar dois grupos independentes.
  - Alternativa nao-parametrica ao teste t.

Exemplos:
  x <- c(1, 3, 5, 6)
  y <- c(7, 8, 9, 12)
  teste.u(x, y)

  dados <- data.frame(grupoA = x, grupoB = y)
  teste.u(dados)
")
    }
    return(invisible(NULL))
  }

  # === Verificacao de pacotes ===
  pacotes <- c("ggplot2", "dplyr")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote ", pkg, " nao esta instalado. Instale com install.packages('", pkg, "')"))
    }
  }

  # === Caso nao seja data.frame ===
  if (!exists("nomes")) {
    if (length(grupos) != 2) {
      stop("Forneca dois vetores numericos ou um data.frame com duas colunas.")
    }
    nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
    nomes <- sub("^.*\\$", "", nomes_raw)
  }

  # === Validacao ===
  if (!all(sapply(grupos, is.numeric))) {
    stop("Todos os grupos devem ser vetores numericos.")
  }

  # === Dados em formato longo ===
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  # === Teste Mann-Whitney ===
  resultado <- stats::wilcox.test(grupos[[1]], grupos[[2]], exact = FALSE)
  pval <- resultado$p.value

  # === Resumo estatistico ===
  resumo <- dados |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      Mediana = round(median(valor, na.rm = TRUE), 2),
      Media = round(mean(valor, na.rm = TRUE), 2),
      Desvio_Padrao = round(sd(valor, na.rm = TRUE), 2),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumo estatistico por grupo:")
    print(resumo)
  }

  # === Rotulo de p-valor ===
  p_label <- if (pval < 0.001) "p < 0.001" else paste0("p = ", signif(pval, 3))
  signif_label <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""

  # === Posicao da anotacao ===
  y_pos <- max(valores, na.rm = TRUE) + 0.1 * diff(range(valores, na.rm = TRUE))

  # === Grafico ===
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

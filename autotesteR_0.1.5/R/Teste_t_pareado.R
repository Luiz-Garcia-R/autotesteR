#' Teste de Student pareado
#'
#' Realiza o teste t pareado entre dois vetores numericos emparelhados (ex: antes e depois)
#' e gera grafico com as medias, desvios padrao e anotacao de significancia.
#'
#' @param ... Dois vetores numericos com o mesmo comprimento (ex: antes, depois)
#' @param titulo Titulo do grafico (default: "Teste t pareado")
#' @param x Nome do eixo x (default: "Grupo")
#' @param y Nome do eixo y (default: "Valor")
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao (default: FALSE)
#' @return Lista invisivel com: resumo (medias e desvios), resultado do t.test, grafico ggplot2
#' @export
teste.t.pareado <- function(..., titulo = "Teste t pareado", x = "Grupo", y = "Valor", ajuda = FALSE) {

  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (ajuda) {
    message("
Funcao teste.t.pareado()

Descricao:
  Realiza o teste t pareado para comparar dois conjuntos de medidas relacionadas.

Quando usar:
  - Comparar medidas antes e depois no mesmo sujeito.
  - Controlar variabilidade intra-individuo.
  - Comparacao de dois grupos dependentes.

Argumentos:
  ...     : dois vetores numericos do mesmo comprimento
  titulo  : titulo do grafico
  x, y    : nomes dos eixos
  ajuda   : se TRUE, exibe essa explicacao

Exemplo:
    antes <- c(100, 105, 98, 102)
    depois <- c(95, 100, 97, 99)
    teste.t.pareado(antes, depois)
")
    return(invisible(NULL))
  }

  # Validacao de entrada
  if (length(grupos) != 2 || !is.numeric(grupos[[1]]) || !is.numeric(grupos[[2]]) ||
      length(grupos[[1]]) != length(grupos[[2]])) {
    stop("Erro: forneca exatamente dois vetores numericos com o mesmo comprimento.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("O pacote ggplot2 nao esta instalado. Instale com install.packages('ggplot2')")
  }

  A <- grupos[[1]]
  B <- grupos[[2]]
  valores <- c(A, B)
  grupo <- factor(rep(nomes, each = length(A)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  resultado <- t.test(A, B, paired = TRUE)
  pval <- resultado$p.value

  # Calculo das medias e desvios
  resumo <- data.frame(
    Grupo = nomes,
    Media = c(mean(A), mean(B)),
    Desvio_Padrao = c(sd(A), sd(B))
  )

  p_label <- if (pval < 0.001) "p < 0.001" else paste0("p = ", signif(pval, 3))
  signif_label <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""

  message("Resumo:\n")
  print(resumo)
  message("P-valor do teste t pareado: ", p_label)

  # Posicao da anotacao
  y_pos <- max(valores, na.rm = TRUE) + 0.1 * diff(range(valores, na.rm = TRUE))

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
    ggplot2::labs(title = titulo, subtitle = p_label, x = x, y = y)

  print(g)

  invisible(list(resumo = resumo, resultado = resultado, grafico = g))
}

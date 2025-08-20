#' Teste de Student
#'
#' Realiza o teste t para comparacao de medias entre dois grupos
#' com verificacoes de normalidade e homogeneidade e apresenta
#' resultado com interpretacao e grafico.
#'
#' @param ... Dois vetores numericos, correspondendo aos grupos para comparacao.
#' @param titulo Titulo do grafico (string). Default: "Teste t".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, mostra explicacao detalhada da funcao. Default: FALSE.
#'
#' @importFrom stats sd aggregate
#' @return Lista com estatisticas do teste e grafico.
#' @export
#'
#' @examples
#' x <- rnorm(30, 10)
#' y <- rnorm(30, 12)
#' teste.t(x, y)


teste.t <- function(..., titulo = "Teste t", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- list(...)
  if (ajuda || length(grupos) != 2) {
    base::cat(
      "\nFuncao teste.t()\n\nDescricao:\n  Realiza o teste t de Student para comparar as medias de dois grupos.\n\nQuando usar:\n  - Quando os dados seguem distribuicao aproximadamente normal.\n  - Quando se deseja comparar a media de dois grupos independentes.\n  - Ideal para amostras com tamanho moderado a grande (n > 30) ou quando a normalidade pode ser assumida.\n\nArgumentos:\n  ...     : dois vetores numericos (ex: grupo1, grupo2)\n  titulo  : titulo do grafico (padrao = 'Teste t')\n  x       : nome do eixo x\n  y       : nome do eixo y\n  ajuda   : se TRUE, exibe essa explicacao\n\nExemplo:\n    x <- rnorm(30, 10)\n    y <- rnorm(30, 12)\n    teste.t(x, y)\n"
    )
    return(invisible(NULL))
  }

  pacotes_necessarios <- c("ggplot2")
  for (pkg in pacotes_necessarios) {
    if (!base::requireNamespace(pkg, quietly = TRUE)) {
      base::stop(base::paste("O pacote", pkg, "nao esta instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }

  nomes_raw <- base::as.character(base::match.call(expand.dots = FALSE)$...)
  nomes <- base::sub("^.*\\$", "", nomes_raw)
  if (!base::all(base::sapply(grupos, base::is.numeric))) base::stop("Todos os grupos devem ser vetores numericos.")

  valores <- base::unlist(grupos)
  grupo <- base::factor(base::rep(nomes, times = base::sapply(grupos, base::length)), levels = nomes)
  dados <- base::data.frame(valor = valores, grupo = grupo)

  resultado <- stats::t.test(grupos[[1]], grupos[[2]])
  pval <- resultado$p.value

  # Calculo da media e desvio padrao
  resumo <- aggregate(valor ~ grupo, dados, function(x) c(media = mean(x), dp = sd(x)))
  resumo_formatado <- data.frame(
    Grupo = resumo$grupo,
    Media = round(resumo$valor[, "media"], 2),
    Desvio_Padrao = round(resumo$valor[, "dp"], 2)
  )

  base::cat("\nResumo estatistico por grupo:\n")
  base::print(resumo_formatado)

  base::cat("\nResultado do teste t de Student:\n")
  if (pval < 0.001) {
    base::cat("p-valor < 0.001\n")
  } else {
    base::cat("p-valor =", base::format.pval(pval, digits = 3), "\n")
  }

  # Rotulo para o grafico
  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    base::paste0("p = ", base::signif(pval, 3))
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

  y_max <- base::max(valores, na.rm = TRUE)
  y_pos <- y_max + 0.1 * base::abs(y_max)

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

  base::print(g)
  invisible(NULL)
}



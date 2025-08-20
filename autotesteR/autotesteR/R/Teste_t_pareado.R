#' Teste de Student pareado
#'
#' Realiza o teste t pareado entre dois vetores numericos emparelhados (ex: antes e depois).
#'
#' @param ... Dois vetores numericos com o mesmo comprimento (ex: antes, depois).
#' @param titulo Titulo do grafico (string). Default: "Teste t pareado".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#'
#' @importFrom stats sd
#' @return Uma lista com:
#' \describe{
#'   \item{diferenca}{Media da diferenca entre pares}
#'   \item{p.valor}{Valor-p do teste}
#'   \item{grafico}{Objeto ggplot2 com a visualizacao, se solicitada}
#' }
#'
#' @examples
#' antes <- c(100, 105, 98, 102)
#' depois <- c(95, 100, 97, 99)
#' teste.t.pareado(antes, depois)
#'
#' @export


teste.t.pareado <- function(..., titulo = "Teste t pareado", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- base::list(...)
  nomes_raw <- base::as.character(base::match.call(expand.dots = FALSE)$...)
  nomes <- base::sub("^.*\\$", "", nomes_raw)

  if (
    ajuda ||
    base::length(grupos) != 2 ||
    !base::is.numeric(grupos[[1]]) ||
    !base::is.numeric(grupos[[2]]) ||
    base::length(grupos[[1]]) != base::length(grupos[[2]])
  ) {
    base::cat(
      "
Funcao teste.t.pareado()

Descricao:
  Realiza o teste t pareado para comparar dois conjuntos de medidas relacionadas.

Quando usar:
  - Quando os mesmos individuos sao medidos antes e depois de um tratamento.
  - Quando ha uma correspondencia natural entre os dois grupos (ex: gemeos, olhos esquerdo/direito, amostras do mesmo animal em condicoes diferentes).
  - Quando se deseja controlar a variabilidade intraindividuo.
  - O teste t pareado e mais sensivel que o teste t nao pareado quando os dados sao dependentes, pois reduz o erro associado a variabilidade entre sujeitos.

Diferenca entre t pareado e nao pareado:
  - Pareado: compara a **diferenca dentro dos pares**.
  - Nao pareado: compara **medias entre dois grupos independentes**.

Argumentos:
  ...     : dois vetores numericos com o mesmo comprimento
  titulo  : titulo do grafico
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe essa explicacao

Exemplo:
    antes <- c(100, 105, 98, 102)
    depois <- c(95, 100, 97, 99)
    teste.t.pareado(antes, depois)
"
    )
    return(base::invisible(NULL))
  }

  if (!base::requireNamespace("ggplot2", quietly = TRUE)) {
    base::stop("O pacote ggplot2 nao esta instalado. Instale com install.packages('ggplot2')")
  }

  A <- grupos[[1]]
  B <- grupos[[2]]

  valores <- base::c(A, B)
  grupo <- base::factor(base::rep(nomes, each = base::length(A)), levels = nomes)
  dados <- base::data.frame(valor = valores, grupo = grupo)

  resultado <- stats::t.test(A, B, paired = TRUE)
  pval <- resultado$p.value

  # Cálculo das médias e desvios padrão
  resumo <- base::data.frame(
    Grupo = nomes,
    Media = round(c(base::mean(A), base::mean(B)), 2),
    Desvio_Padrao = round(c(sd(A), sd(B)), 2)
  )

  # Cálculo do p-valor e texto
  pval <- resultado$p.value
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

  # Mostrar resumo com médias, desvios e p-valor
  base::print(resumo)
  base::message("P-valor do teste t pareado: ", p_label)

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



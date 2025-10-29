#' Teste de Student
#'
#' Realiza o teste t para comparacao de medias entre dois grupos,
#' com verificacoes de normalidade e homogeneidade e apresenta
#' resultado com interpretacao e grafico.
#'
#' @param ... Dois vetores numericos ou um data frame com exatamente duas colunas.
#' @param titulo Titulo do grafico (string). Default: "Teste t".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param ajuda Logico. Se TRUE, mostra explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Se TRUE, imprime mensagens detalhadas (default = TRUE)
#' @return Lista invisivel com resumo, resultado do teste t e grafico.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   controle = rnorm(30, 10),
#'   tratamento = rnorm(30, 12)
#' )
#' teste.t(df)
teste.t <- function(..., titulo = "Teste t", x = "Grupo", y = "Valor",
                    ajuda = FALSE, verbose = TRUE) {

  args <- list(...)

  # --- Ajuda rapida ---
  if (ajuda) {
    if (verbose) {
      message("
Funcao teste.t()

Descricao:
  Realiza o teste t de Student para comparar as medias de dois grupos independentes.

Pode receber:
  - Dois vetores numericos (ex: grupo1, grupo2)
  - Um data frame com exatamente duas colunas numericas.

Exemplo:
  df <- data.frame(
    controle = rnorm(30, 10),
    tratamento = rnorm(30, 12)
  )
  teste.t(df)
")
    }
    return(invisible(NULL))
  }

  # --- Detecta se foi passado um data.frame ---
  if (length(args) == 1 && is.data.frame(args[[1]])) {
    df <- args[[1]]
    if (ncol(df) != 2) stop("O data frame deve conter exatamente duas colunas numericas.")
    grupos <- as.list(df)
    nomes <- colnames(df)
  } else {
    grupos <- args
    if (length(grupos) != 2) stop("Forneca exatamente dois grupos ou um data frame com duas colunas.")
    nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
    nomes <- sub("^.*\\$", "", nomes_raw)
  }

  # --- Validacao ---
  if (!all(sapply(grupos, is.numeric))) {
    stop("Ambos os grupos devem ser numericos.")
  }

  if (any(sapply(grupos, function(g) sd(g, na.rm = TRUE) == 0))) {
    stop("Um dos grupos possui variancia zero (dados constantes). O teste t nao pode ser aplicado.")
  }

  # --- Pacotes necessarios ---
  pacotes <- c("ggplot2", "dplyr")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages('", pkg, "')"))
    }
  }

  # --- Monta dataframe long ---
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  # --- Teste t ---
  resultado <- stats::t.test(grupos[[1]], grupos[[2]])
  pval <- resultado$p.value

  # --- Resumo estatistico ---
  resumo <- dados |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      Media = round(mean(valor, na.rm = TRUE), 2),
      Desvio_Padrao = round(sd(valor, na.rm = TRUE), 2),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumo estatistico por grupo:")
    print(resumo)
    message("\nResultado do teste t:")
    message("----------------------")
    message("t = ", round(resultado$statistic, 3),
            ", gl = ", round(resultado$parameter, 2),
            ", p-valor = ", signif(pval, 4))
  }

  # --- Label de p-valor ---
  p_label <- if (pval < 0.001) "p < 0.001" else paste0("p = ", signif(pval, 3))
  signif_label <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""

  # --- Posicaoo para anotacao ---
  y_pos <- max(valores, na.rm = TRUE) + 0.1 * diff(range(valores, na.rm = TRUE))

  # --- Grafico ---
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
      x = "",
      y = y
    )

  print(g)

  invisible(list(resumo = resumo, resultado = resultado, grafico = g))
}

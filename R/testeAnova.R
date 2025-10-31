#' Teste de ANOVA com checagem automática de pressupostos
#'
#' Realiza ANOVA (e Tukey HSD) se os dados atenderem aos pressupostos de normalidade e homogeneidade.
#' Caso contrário, recomenda automaticamente o uso de Kruskal-Wallis/Dunn.
#'
#' @param ... Vetores ou um data.frame com >= 2 colunas.
#' @param titulo Título do gráfico.
#' @param x Rótulo do eixo X.
#' @param y Rótulo do eixo Y.
#' @param ajuda Se TRUE, mostra a ajuda.
#' @param verbose Se TRUE, mostra mensagens detalhadas.
#' @importFrom stats aov sd aggregate shapiro.test var.test
#' @return Objeto `aov` ou mensagem de recomendação.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   controle = rnorm(30, 10),
#'   tratamento = rnorm(30, 12),
#'   teste = rnorm(30, 11)
#' )
#' teste.anova(df)
teste.anova <- function(..., titulo = "ANOVA/Tukey HSD", x = "Eixo x", y = "Eixo y",
                        ajuda = FALSE, verbose = TRUE) {

  # --- Bloco de ajuda rapida ---
  if (ajuda || length(list(...)) == 0) {
    message("
Funcao teste.anova()

Descricao:
  Executa ANOVA entre grupos numericos, seguida do teste de Tukey HSD,
  caso os pressupostos de normalidade e homogeneidade sejam atendidos.
  Caso contrario, recomenda o uso de Kruskal-Wallis/Dunn.

Exemplo:
  df <- data.frame(
  controle = rnorm(30, 10),
  tratamento = rnorm(30, 12),
  teste = rnorm(30, 11)
  )

teste.anova(df)
")
    return(invisible(NULL))
  }

  # --- Checagem de pacotes necessarios ---
  pacotes_necessarios <- c("ggplot2", "multcompView", "car")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages('", pkg, "')"))
    }
  }

  # --- Detectar formato da entrada ---
  dots <- list(...)
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- dots[[1]]
    grupos <- as.list(df)
    nomes <- names(df)
  } else {
    grupos <- dots
    nomes <- sapply(substitute(list(...))[-1], deparse)
  }

  if (length(grupos) < 2) stop("Forneca pelo menos dois grupos (ou um data frame com >= 2 colunas).")
  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser numericos.")

  # --- Montagem dos dados em formato longo ---
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  # --- Verificacao de pressupostos ---
  aplicar_teste_normalidade <- function(x) {
    if (length(x) < 3) return(NA)
    stats::shapiro.test(x)$p.value
  }

  p_normal <- sapply(grupos, aplicar_teste_normalidade)
  normal <- all(p_normal > 0.05, na.rm = TRUE)

  p_levene <- tryCatch({
    if (length(grupos) > 2) {
      car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      stats::var.test(grupos[[1]], grupos[[2]])$p.value
    }
  }, error = function(e) NA)

  homogeneo <- !is.na(p_levene) && p_levene > 0.05

  # --- Diagnostico ---
  if (verbose) {
    message("\nVerificacao dos pressupostos:")
    print(data.frame(grupo = nomes, p_normal = signif(p_normal, 3)))
    message(sprintf("Homogeneidade de variancias: p = %.4f", p_levene))
  }

  # --- Caso os pressupostos nao sejam atendidos ---
  if (!normal || !homogeneo) {
    if (verbose) {
      message("\nPressupostos nao atendidos. ANOVA nao realizada.")
      message("Sugestao: utilize 'teste.kruskal()' para analise nao parametrica.\n")
    }
    return(invisible(list(
      normal = normal,
      p_normal = p_normal,
      homogeneo = homogeneo,
      p_levene = p_levene,
      recomendacao = "Kruskal-Wallis/Dunn"
    )))
  }

  # --- ANOVA e Tukey ---
  modelo <- stats::aov(valor ~ grupo, data = dados)
  p_anova <- summary(modelo)[[1]][["Pr(>F)"]][1]
  p_label <- if (p_anova < 0.001) "ANOVA: p < 0.001" else paste0("ANOVA: p = ", signif(p_anova, 3))

  tukey_res <- stats::TukeyHSD(modelo)
  letras <- multcompView::multcompLetters4(modelo, tukey_res)
  letras_df <- data.frame(grupo = names(letras$grupo$Letters),
                          letra = letras$grupo$Letters,
                          stringsAsFactors = FALSE)

  # --- Posicionamento das letras no grafico ---
  maximos <- stats::aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 0.05 * diff(range(dados$valor, na.rm = TRUE))

  # --- Plotagem ---
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                       size = 4, vjust = 0) +
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

  invisible(modelo)
}

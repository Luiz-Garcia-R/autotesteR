#' Teste de ANOVA com pos-teste de Tukey HSD
#'
#' Realiza analise de variancia (ANOVA) para comparar medias entre dois ou mais grupos,
#' seguida de teste post-hoc de Tukey HSD para identificar pares significativamente diferentes.
#' Exibe um grafico com boxplots e letras indicando significancia entre grupos.
#'
#' @param ... Dois ou mais vetores numericos representando os grupos a serem comparados.
#' @param titulo Titulo do grafico (padrao: "ANOVA/Tukey HSD").
#' @param x Rotulo do eixo x (padrao: "Eixo x").
#' @param y Rotulo do eixo y (padrao: "Eixo y").
#' @param ajuda Se TRUE, exibe a ajuda desta funcao.
#' @param verbose Se TRUE, imprime mensagens detalhadas (default = TRUE)
#' @importFrom stats sd aggregate
#'
#' @return Objeto `aov` da analise de variancia.
#'
#' @details
#' A ANOVA verifica se ha diferenca significativa entre as medias de tres ou mais grupos.
#' Caso o resultado seja significativo, o teste post-hoc de Tukey HSD identifica quais pares de grupos diferem.
#' O grafico gerado apresenta boxplots para cada grupo com letras indicativas de diferencas estatisticas.
#'
#' @examples
#' g1 <- rnorm(10, 5)
#' g2 <- rnorm(10, 7)
#' g3 <- rnorm(10, 6)
#' teste.anova(g1, g2, g3)
#'
#' @export


teste.anova <- function(..., titulo = "ANOVA/Tukey HSD", x = "Eixo x", y = "Eixo y", ajuda = FALSE, verbose = TRUE) {

  if (ajuda || length(list(...)) == 0) {
    message("
Funcao teste.anova()

Descricao:
  Realiza analise de variancia (ANOVA) entre dois ou mais grupos, seguida de teste post-hoc de Tukey HSD.
  Exibe o resultado grafico com letras de significancia.

Uso:
  teste.anova(grupo1, grupo2, ..., titulo = \"Titulo\", x = \"Eixo x\", y = \"Eixo y\")

Argumentos:
  grupo1, grupo2, ... Vetores numericos representando os grupos
  titulo              Titulo do grafico
  x, y                Rotulos dos eixos
  ajuda               Exibe esta mensagem de ajuda
  verbose             Se TRUE, exibe mensagens estatisticas

Exemplo:
  g1 <- rnorm(10, 5)
  g2 <- rnorm(10, 7)
  g3 <- rnorm(10, 9)
  teste.anova(g1, g2, g3)
")
    return(invisible(NULL))
  }

  pacotes_necessarios <- c("ggplot2", "multcompView")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "nao esta instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }

  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (length(grupos) < 2) stop("Forneca pelo menos dois grupos.")

  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = base::sapply(grupos, length)), levels = nomes)
  dados <- base::data.frame(valor = valores, grupo = grupo)

  # ANOVA
  modelo <- stats::aov(valor ~ grupo, data = dados)
  p_anova <- as.numeric(summary(modelo)[[1]][["Pr(>F)"]][1])
  p_label <- if (p_anova < 0.001) "ANOVA: p < 0.001" else paste0("ANOVA: p = ", base::signif(p_anova, 3))

  # Tukey HSD
  tukey_res <- stats::TukeyHSD(modelo)
  tukey_df <- base::as.data.frame(tukey_res[[1]])
  pares_signif <- base::subset(tukey_df, `p adj` < 0.05)

  # Medias e desvios padrao
  medias_dp <- stats::aggregate(valor ~ grupo, data = dados, function(x) c(media = mean(x), dp = sd(x)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")

  if (verbose) {
    message("Medias e desvios padrao por grupo:")
    print(medias_dp)

    if (base::nrow(pares_signif) == 0) {
      message("Nenhuma comparacao post-hoc significativa (p < 0.05).")
    } else {
      nomes_pares <- base::rownames(pares_signif)
      pvals <- pares_signif$`p adj`
      msg <- base::paste0("(", base::gsub("-", " vs ", nomes_pares), ", p = ", base::signif(pvals, 3), ")")
      message("Pares significativos (Tukey HSD):\n", base::paste(msg, collapse = "\n"))
    }
  }

  # Letras de significancia
  letras <- multcompView::multcompLetters4(modelo, tukey_res)
  letras_df <- base::data.frame(grupo = base::names(letras$grupo$Letters),
                                letra = letras$grupo$Letters)

  # Posicao das letras no grafico
  maximos <- stats::aggregate(valor ~ grupo, data = dados, max)
  letras_df <- base::merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 1

  # Grafico
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                       size = 4, vjust = 0) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = titulo,
      subtitle = p_label,
      x = "",
      y = y
    )

  print(g)

  base::invisible(modelo)
}



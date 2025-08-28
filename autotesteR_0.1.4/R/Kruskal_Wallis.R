#' Teste de Kruskal-Wallis com pos-teste de Dunn
#'
#' Realiza o teste de Kruskal-Wallis para comparacao de multiplos grupos independentes
#' quando a suposicao de normalidade nao e atendida, seguido do pos-teste de Dunn com ajuste de Bonferroni.
#' Exibe resultado numerico e grafico com letras de significancia.
#'
#' @param ... Vetores numericos representando os grupos a serem comparados.
#' @param titulo Titulo do grafico (padrao = "Kruskal-Wallis + Dunn").
#' @param x Nome do eixo x (padrao = "Grupo").
#' @param y Nome do eixo y (padrao = "Valor").
#' @param ajuda Se TRUE, exibe esta mensagem de ajuda.
#' @importFrom stats sd aggregate
#' @importFrom stats p.adjust
#' @return Objeto da classe `htest` resultante do teste de Kruskal-Wallis.
#'
#' @details
#' O teste de Kruskal-Wallis e uma alternativa nao parametrica a ANOVA para comparar a mediana de tres ou mais grupos independentes.
#' Quando o resultado e significativo, aplica-se o teste pos-hoc de Dunn com correcao de Bonferroni para identificar pares de grupos diferentes.
#'
#' @section Quando usar:
#' - Para dados que nao seguem distribuicao normal;
#' - Para variaveis ordinais ou continuas;
#' - Para comparar 3 ou mais grupos independentes.
#'
#' @examples
#' g1 <- c(5, 6, 7)
#' g2 <- c(8, 9, 10)
#' g3 <- c(2, 3, 4)
#' teste.kruskal(g1, g2, g3)
#'
#' @export


teste.kruskal <- function(..., titulo = "Kruskal-Wallis + Dunn", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (ajuda || length(grupos) < 2) {
    cat("
Funcao teste.kruskal()

Descricao:
  Teste de Kruskal-Wallis (ANOVA nao parametrica) com pos-teste de Dunn.
  Ideal para comparar >2 grupos independentes com dados nao-normais.

Quando usar:
  - Comparacao de 3+ grupos independentes
  - Dados ordinais ou continuos nao-normais
  - Alternativa a ANOVA sem normalidade

Assuncoes:
  - Observacoes independentes
  - Variavel dependente ordinal ou continua
  - Distribuicoes com a mesma forma (diferem na mediana)

Pos-teste:
  - Dunn com ajuste de Bonferroni

Limitacoes:
  - Menor poder que ANOVA se pressupostos forem atendidos
  - Detecta diferencas gerais, nao so na media

Exemplo:
  g1 <- c(5, 6, 7)
  g2 <- c(8, 9, 10)
  g3 <- c(2, 3, 4)
  teste.kruskal(g1, g2, g3)
")
    return(invisible(NULL))
  }

  # Verifica pacotes necessarios
  pacotes <- c("ggplot2", "FSA", "dplyr", "multcompView")
  lapply(pacotes, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  })

  # Prepara dados
  dados <- data.frame(
    valor = unlist(grupos),
    grupo = factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  )

  # Teste de Kruskal-Wallis
  kruskal <- stats::kruskal.test(valor ~ grupo, data = dados)
  p_kruskal <- kruskal$p.value
  p_label <- ifelse(p_kruskal < 0.001, "Kruskal-Wallis: p < 0.001", paste0("Kruskal-Wallis: p = ", signif(p_kruskal, 3)))

  # Medias e desvios padrao
  medias_dp <- aggregate(valor ~ grupo, data = dados, function(x) c(media = mean(x), dp = sd(x)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")
  message("Medias e desvios padrao por grupo:")
  print(medias_dp)

  # Pos-teste de Dunn
  dunn_df <- FSA::dunnTest(valor ~ grupo, data = dados, method = "bonferroni")$res
  pares_signif <- subset(dunn_df, P.adj < 0.05)

  if (nrow(pares_signif) == 0) {
    message("Nenhuma comparacao pos-hoc significativa (p < 0.05).")
  } else {
    msg <- paste0("(", pares_signif$Comparison, ", p = ", signif(pares_signif$P.adj, 3), ")")
    message("Pares significativos (Dunn, Bonferroni):\n", paste(msg, collapse = "\n"))
  }

  # Gera letras para os grupos
  comparacoes <- setNames(dunn_df$P.adj, gsub(" ", "", dunn_df$Comparison))
  letras_df <- multcompView::multcompLetters(comparacoes)$Letters |>
    utils::stack() |>
    dplyr::rename(grupo = ind, letra = values)

  # Prepara posicao das letras
  maximos <- stats::aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 0.05 * max(letras_df$valor, na.rm = TRUE)

  # Grafico
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::geom_text(data = letras_df, ggplot2::aes(label = letra), size = 4, vjust = 0) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = titulo, subtitle = p_label, x = x, y = y)

  print(g)
  invisible(kruskal)
}


utils::globalVariables(c("P.adj"))

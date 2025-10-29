#' Teste de Kruskal-Wallis com pos-teste de Dunn
#'
#' Realiza o teste de Kruskal-Wallis para comparacao de multiplos grupos independentes,
#' seguido do pos-teste de Dunn com correcao de Bonferroni.
#'
#' @param ... Vetores numericos ou um data frame com duas ou mais colunas (cada uma representando um grupo)
#' @param titulo Titulo do grafico (padrao = "Kruskal-Wallis + Dunn")
#' @param x Nome do eixo x (padrao = "Grupo")
#' @param y Nome do eixo y (padrao = "Valor")
#' @param ajuda Se TRUE, exibe mensagem de ajuda
#' @param verbose Se TRUE, imprime mensagens detalhadas (padrao = TRUE)
#' @return Objeto do teste de Kruskal-Wallis
#' @export

teste.kruskal <- function(..., titulo = "Kruskal-Wallis + Dunn", x = "Grupo", y = "Valor",
                          ajuda = FALSE, verbose = TRUE) {

  # Captura dos argumentos
  args <- list(...)

  # Permitir data frame como entrada
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    grupos <- lapply(args[[1]], function(col) col)
    nomes <- colnames(args[[1]])
  } else {
    grupos <- args
    nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
    nomes <- sub("^.*\\$", "", nomes_raw)
  }

  # Mensagem de ajuda
  if (ajuda || length(grupos) < 2) {
    message("
Funcao teste.kruskal()

Descricao:
  Teste de Kruskal-Wallis (ANOVA nao-parametrica) com pos-teste de Dunn.
  Ideal para comparar tres ou mais grupos independentes com dados nao-normais.

Exemplo:
  df <- data.frame(
    controle = c(5, 6, 7),
    tratamento = c(8, 9, 10),
    tratamento2 = c(2, 3, 4)
  )
  teste.kruskal(df)
")
    return(invisible(NULL))
  }

  # Pacotes necessarios
  pacotes <- c("ggplot2", "FSA", "dplyr", "multcompView")
  lapply(pacotes, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("O pacote '%s' nao esta instalado.", pkg), call. = FALSE)
    }
  })

  # Preparacao dos dados
  dados <- data.frame(
    valor = unlist(grupos),
    grupo = factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  )

  # Teste de Kruskal-Wallis
  kruskal <- stats::kruskal.test(valor ~ grupo, data = dados)
  p_kruskal <- kruskal$p.value
  p_label <- ifelse(p_kruskal < 0.001, "Kruskal-Wallis: p < 0.001",
                    paste0("Kruskal-Wallis: p = ", signif(p_kruskal, 3)))

  # Medias e desvios padrao
  medias_dp <- aggregate(valor ~ grupo, data = dados, function(x) c(media = mean(x), dp = sd(x)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")

  if (verbose) {
    sep <- paste0(rep("=", 50), collapse = "")
    message("Medias e desvios padrao por grupo:")
    message(sep)
    print(medias_dp)
    message(sep)
  }

  # Pos-teste de Dunn
  dunn_df <- FSA::dunnTest(valor ~ grupo, data = dados, method = "bonferroni")$res
  pares_signif <- subset(dunn_df, P.adj < 0.05)

  if (verbose) {
    if (nrow(pares_signif) == 0) {
      message("Nenhuma comparacao pos-hoc significativa (p < 0.05).")
    } else {
      msg <- paste0("(", pares_signif$Comparison, ", p = ", signif(pares_signif$P.adj, 3), ")")
      message("Pares significativos (Dunn, Bonferroni):")
      message(sep)
      message(paste(msg, collapse = "\n"))
      message(sep)
    }
  }

  # Letras de significancia
  comparacoes <- setNames(dunn_df$P.adj, gsub(" ", "", dunn_df$Comparison))
  letras_df <- multcompView::multcompLetters(comparacoes)$Letters
  letras_df <- data.frame(grupo = names(letras_df), letra = unname(letras_df))

  # Ajuste da posicao das letras
  maximos <- aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 0.05 * max(letras_df$valor, na.rm = TRUE)

  # Grafico
  g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    ggplot2::geom_text(data = letras_df, ggplot2::aes(label = letra), size = 4, vjust = 0) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = titulo, subtitle = p_label, x = "", y = y)

  print(g)
  invisible(kruskal)
}

utils::globalVariables(c("P.adj"))

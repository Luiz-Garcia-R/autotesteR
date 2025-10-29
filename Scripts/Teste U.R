teste.u <- function(..., titulo = "Teste de Mann-Whitney", x = "Grupo", y = "Valor", ajuda = FALSE) {
  grupos <- list(...)
  
  if (ajuda || length(grupos) != 2) {
    cat("
Função mann.whitney()

Descrição:
  Realiza o teste de Mann-Whitney (ou Wilcoxon rank-sum) para comparar a mediana de dois grupos independentes.

Quando usar:
  - Quando os dados não seguem distribuição normal.
  - Quando se deseja comparar dois grupos independentes.
  - Quando se deseja uma alternativa não paramétrica ao teste t.

Argumentos:
  ...     : dois vetores numéricos (ex: grupo1, grupo2)
  titulo  : título do gráfico (padrão = 'Teste de Mann-Whitney')
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe essa explicação

Exemplo:
  g1 <- rpois(12, 5)
  g2 <- rpois(12, 7)
  mann.whitney(g1, g2)
")
    return(invisible(NULL))
  }
  
  pacotes_necessarios <- c("ggplot2")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  
  library(ggplot2)
  
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser vetores numéricos.")
  
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)
  
  resultado <- wilcox.test(grupos[[1]], grupos[[2]], exact = FALSE)
  pval <- resultado$p.value
  
  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", signif(pval, 3))
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
  
  y_max <- max(valores, na.rm = TRUE)
  y_pos <- y_max + 0.1 * abs(y_max)
  
  g <- ggplot(dados, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 6) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = titulo,
      subtitle = p_label,
      x = x,
      y = y
    )
  
  print(g)
  return(resultado)
}
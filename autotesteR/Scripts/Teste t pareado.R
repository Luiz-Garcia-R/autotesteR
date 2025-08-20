teste.t.pareado <- function(..., titulo = "Teste t pareado", x = "Grupo", y = "Valor", ajuda = FALSE) {
  # Captura dos vetores e seus nomes
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
  if (ajuda || length(grupos) != 2 || !is.numeric(grupos[[1]]) || !is.numeric(grupos[[2]]) || length(grupos[[1]]) != length(grupos[[2]])) {
    cat("
Função teste.t.pareado()

Descrição:
  Realiza o teste t pareado para comparar dois conjuntos de medidas relacionadas.

Quando usar:
  - Quando os mesmos indivíduos são medidos antes e depois de um tratamento.
  - Quando há uma correspondência natural entre os dois grupos (ex: gêmeos, olhos esquerdo/direito, amostras do mesmo animal em condições diferentes).
  - Quando se deseja controlar a variabilidade intraindivíduo.
  - O teste t pareado é mais sensível que o teste t não pareado quando os dados são dependentes, pois reduz o erro associado à variabilidade entre sujeitos.

Diferença entre t pareado e não pareado:
  - Pareado: compara a **diferença dentro dos pares**.
  - Não pareado: compara **médias entre dois grupos independentes**.

Argumentos:
  ...     : dois vetores numéricos com o mesmo comprimento
  titulo  : título do gráfico
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe essa explicação

Exemplo:
  antes <- c(120, 130, 125, 118)
  depois <- c(115, 128, 123, 110)
  teste.t.pareado(antes, depois)
")
    return(invisible(NULL))
  }
  
  # Carregamento dos pacotes
  pacotes_necessarios <- c("ggplot2")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  
  library(ggplot2)
  
  A <- grupos[[1]]
  B <- grupos[[2]]
  
  # Dados em formato longo
  valores <- c(A, B)
  grupo <- factor(rep(nomes, each = length(A)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)
  
  # Teste t pareado
  resultado <- t.test(A, B, paired = TRUE)
  pval <- resultado$p.value
  
  # Texto do p-valor
  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", signif(pval, 3))
  }
  
  # Asteriscos de significância
  signif_label <- if (pval < 0.001) {
    "***"
  } else if (pval < 0.01) {
    "**"
  } else if (pval < 0.05) {
    "*"
  } else {
    ""
  }
  
  # Posição do rótulo no gráfico
  y_max <- max(valores, na.rm = TRUE)
  y_pos <- y_max + 0.1 * abs(y_max)
  
  # Gráfico
  g <- ggplot(dados, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 6) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = titulo,
      subtitle = p_label,
      x = x,
      y = y
    )
  
  print(g)
  return(resultado)
}


pre.teste <- function(..., alpha = 0.05, ajuda = FALSE) {
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
  if (ajuda || length(grupos) < 2) {
    cat("
Função pre.teste()

Descrição:
  Identifica automaticamente se os dados são numéricos ou categóricos e sugere o teste estatístico adequado.

Para dados numéricos:
  - Verifica normalidade (Shapiro, Anderson-Darling ou KS conforme n)
  - Verifica homogeneidade de variância
  - Sugere teste t/ANOVA ou Mann-Whitney/Kruskal-Wallis

Para dados categóricos:
  - Cria tabela de contingência
  - Avalia condições para teste do qui-quadrado ou teste exato de Fisher

Argumentos:
  ...   : dois ou mais vetores numéricos ou categóricos
  alpha : nível de significância (padrão = 0.05)
  ajuda : se TRUE, exibe esta explicação

Exemplo com valores numéricos:
  g1 <- c(1,2,3)
  g2 <- c(3,4,5)
  pre.teste(g1, g2)
  
Exemplo com valores categóricos:
  grupo1 <- c('A', 'A', 'B', 'B', 'A', 'B', 'A', 'B')
  grupo2 <- c('Sim', 'Não', 'Sim', 'Sim', 'Não', 'Não', 'Sim', 'Não')
  pre.teste(grupo1, grupo2)
")
    return(invisible(NULL))
  }
  
  # Pacotes
  pacotes <- c("car", "dplyr", "nortest", "ggpubr", "ggplot2", "RColorBrewer")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Instale o pacote:", pkg))
    }
  }
  lapply(pacotes, library, character.only = TRUE)
  
  # Detectar tipo de dado
  sao_numericos <- all(sapply(grupos, is.numeric))
  sao_categoricos <- all(sapply(grupos, function(x) is.factor(x) || is.character(x)))
  
  if (!sao_numericos && !sao_categoricos) {
    stop("Os dados devem ser todos numéricos ou todos categóricos.")
  }
  
  if (sao_numericos) {
    ## --- FLUXO PARA DADOS NUMÉRICOS ---
    valores <- unlist(grupos)
    grupo <- factor(rep(nomes, times = sapply(grupos, length)))
    dados <- data.frame(valor = valores, grupo = grupo)
    
    aplicar_teste_normalidade <- function(x) {
      n <- length(x)
      if (n <= 50) {
        p <- shapiro.test(x)$p.value
        metodo <- "Shapiro-Wilk"
      } else if (n <= 300) {
        p <- nortest::ad.test(x)$p.value
        metodo <- "Anderson-Darling"
      } else {
        ks_result <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
        p <- ks_result$p.value
        metodo <- "Kolmogorov-Smirnov"
      }
      list(p = p, metodo = metodo)
    }
    
    normalidade <- lapply(grupos, aplicar_teste_normalidade)
    p_values <- sapply(normalidade, function(x) x$p)
    metodos <- sapply(normalidade, function(x) x$metodo)
    
    normalidade_df <- data.frame(
      grupo = nomes,
      n = sapply(grupos, length),
      metodo = metodos,
      p_valor = signif(p_values, 3),
      normal = p_values > alpha
    )
    
    p_levene <- if (length(grupos) > 2) {
      car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      var.test(grupos[[1]], grupos[[2]])$p.value
    }
    homogeneo <- p_levene > alpha
    
    cat("Resultados dos testes de normalidade (baseado em n):\n\n")
    print(normalidade_df)
    
    cat("\n Homogeneidade de variâncias:\n")
    cat(ifelse(homogeneo,
               paste0("  ✅ Variâncias homogêneas (p = ", signif(p_levene, 3), ")\n"),
               paste0("  ⚠️ Variâncias heterogêneas (p = ", signif(p_levene, 3), ")\n")))
    
    cat("\n Recomendação de teste estatístico:\n")
    if (all(normalidade_df$normal)) {
      if (homogeneo) {
        cat(ifelse(length(grupos) == 2, "Use: teste t\n", "Use: ANOVA\n"))
      } else {
        cat(ifelse(length(grupos) == 2, "Use: Welch's t-test\n", "Use: ANOVA com correção de Welch ou Kruskal-Wallis\n"))
      }
    } else {
      cat(ifelse(length(grupos) == 2, "Use: teste de Mann-Whitney\n", "Use: teste de Kruskal-Wallis\n"))
    }
    
    return(invisible(list(normalidade = normalidade_df, p_levene = p_levene)))
  }
  
  if (sao_categoricos) {
    ## --- FLUXO PARA DADOS CATEGÓRICOS ---
    categorias <- lapply(grupos, as.factor)
    tabela <- table(categorias[[1]], categorias[[2]])
    
    cat("Tabela de Contingência:\n")
    print(tabela)
    
    cat("\n Recomendação de teste de associação:\n")
    if (any(tabela < 5)) {
      cat("⚠️ Há valores esperados < 5. Recomendado: Teste exato de Fisher.\n")
    } else {
      cat("✅ Frequências adequadas. Recomendado: Teste do Qui-quadrado.\n")
    }
    
    return(invisible(list(tabela = tabela)))
  }
}

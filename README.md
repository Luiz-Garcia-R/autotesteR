# autotestR

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Pacote R para testes estatísticos simplificados com gráficos automáticos e sugestões inteligentes de análise.**

O `autotestR` tem como objetivo simplificar análises estatísticas em R, oferecendo uma interface acessível para testes comuns e geração automática de gráficos, além de incluir uma função diagnóstica que sugere o teste estatístico mais apropriado para os dados fornecidos.

---

## Instalação

Você pode instalar a versão em desenvolvimento diretamente do GitHub:

```r
# Instale o devtools caso ainda não tenha
install.packages("devtools")

# Instale o autotestR a partir do GitHub
devtools::install_github("Luiz-Garcia-R/autotestR")

## Funcionalidades principais

✅ Teste t (independente e pareado)
✅ Teste de Mann-Whitney (U de Wilcoxon)
✅ Teste qui-quadrado e teste exato de Fisher
✅ ANOVA com pós-teste de Tukey HSD
✅ Teste de correlação de Pearson com gráfico automático
✅ Função diagnóstica que sugere o teste estatístico mais adequado
✅ Gráficos intuitivos já integrados nas funções


## Exemplos de uso

library(autotestR)

# Exemplo com teste t
grupo1 <- c(5.1, 5.3, 5.8, 6.0, 5.5)
grupo2 <- c(6.2, 6.5, 6.8, 7.1, 6.9)

resultado <- t_test(grupo1, grupo2)

# Resultado do teste
print(resultado)

# Gráfico gerado automaticamente
plot(resultado)


# Exemplo com a função diagnóstica
dados <- data.frame(
  grupo = rep(c("A", "B"), each = 10),
  valor = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
)

pre.teste(dados$valor ~ dados$grupo)


## Contato
👨‍🔬 Luiz Fernando Cardoso Garcia
📧 Email: luiz.cardoso@ufpr.br
💼 LinkedIn: luiz-garcia-7025a277


## Licença
Este projeto está licenciado sob a licença MIT – veja o arquivo LICENSE
 para mais detalhes.

Contribuições são bem-vindas!
Sinta-se à vontade para abrir issues ou enviar pull requests.

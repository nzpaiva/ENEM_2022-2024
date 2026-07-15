# ANEXO: CÓDIGOS EM R PARA A ANALISE DOS DADOS
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

#Estudo dos resultados de 2024, 2023, 2022 e 2021:
dat1_2024 <- read.csv2("Dados_utilizados\\RESULTADOS_2024.csv")

dat1_2023 <- read.csv2("Dados_utilizados\\MICRODADOS_ENEM_2023.csv")

dat1_2022 <- read.csv2("Dados_utilizados\\MICRODADOS_ENEM_2022.csv")

# Função para criar nota final para cada ano:
cria_nota_final <- function(dat1) {
  
  # Transformando todas as notas em dado tipo double:
  dat1$NU_NOTA_CN <- as.double(dat1$NU_NOTA_CN) #nota de ciências da natureza
  dat1$NU_NOTA_CH <- as.double(dat1$NU_NOTA_CH) #nota de ciências humanas
  dat1$NU_NOTA_LC <- as.double(dat1$NU_NOTA_LC) #nota de linguagens e códigos
  dat1$NU_NOTA_MT <- as.double(dat1$NU_NOTA_MT) #nota de matemática
  dat1$NU_NOTA_REDACAO <- as.double(dat1$NU_NOTA_REDACAO) #nota da redação
  
  # Criar coluna com notas finais
  dat1 <- dat1 |>
    mutate(nota_final = rowMeans(across(c(NU_NOTA_CN, 
                                          NU_NOTA_CH, 
                                          NU_NOTA_LC, 
                                          NU_NOTA_MT, 
                                          NU_NOTA_REDACAO))))
  return(dat1)
}

dat1_2022 <- cria_nota_final(dat1_2022)
dat1_2023 <- cria_nota_final(dat1_2023)
dat1_2024 <- cria_nota_final(dat1_2024)

  
  ###########################################################
  # 1. Distribuição das notas: 2024 a 2022 concordam
  ###########################################################
  
  dat1 <- dat1_2024

  #g1 <- 
  dat1 |>
  ggplot( aes(x = nota_final)) +
  geom_boxplot(fill = "lightblue") + 
  labs( title = "Boxplot das notas ENEM - 2024",
        x = "Notas",
        y = "") +
  theme_minimal() 
  ggsave("Gráficos_gerados\\box_plot_2024.png", width = 8, height = 6, dpi = 300)
  

  not_media <- mean(dat1$nota_final, na.rm = TRUE)
  not_sd <- sd(dat1$nota_final, na.rm = TRUE)
  
  #g2 <- 
  dat1 |> 
  ggplot( aes(x = nota_final)) +
    scale_color_manual(
      name = "Legenda",
      values = c("ajuste" = "black", "histograma" = "blue")) +
    geom_histogram(aes(y = after_stat(density), color = "histograma"), fill = "steelblue", alpha = 0.7) + 
    stat_function(aes(color = "ajuste"), fun = dnorm, args = list(mean = not_media, sd = not_sd), linewidth = 0.8) +
    labs( title = "Distribuição de notas ENEM - 2024",
          x = "Notas",
          y = "Densidade de probabilidade") +
    theme_minimal() 
  ggsave("Gráficos_gerados\\hist_2024.png", width = 8, height = 6, dpi = 300)
  
  #Qual distribuição melhor descreve essa distribuição?
  #Talvez a distribuição normal, sim, o ajuste é muito bom!
  
  not_norm <- (dat1$nota_final - not_media)/not_sd
  vet_p <- seq(0.0001, 0.9999, 0.0001)
  quan_norm <- qnorm(vet_p)
  quan_dis_notas <- quantile(not_norm, probs = vet_p, na.rm = TRUE)
  
  #calculo do coeficiente de determinação:
  r2 <- 1-(sum((quan_dis_notas-quan_norm)**2)/sum((quan_dis_notas-mean(quan_dis_notas))**2)) ; r2
  
  r2_text <- sprintf("R² = %.5f", r2)
  
  #g3 <- 
    ggplot(data.frame(quan_norm,quan_dis_notas), aes(x = quan_norm, y = quan_dis_notas)) +
    geom_line(aes(x = quan_norm, y = quan_norm, color = "esperado")) + 
    geom_point(aes(x = quan_norm, y = quan_dis_notas, color = "dados")) +
    scale_color_manual(
      name = "Legenda",
      values = c("esperado" = "black", "dados" = "blue")) +
    labs( title = "Gráfico QQ-plot para as distribuições de nota ENEM-2024",
          subtitle = r2_text,
          x = "Quantis da normal padrão", 
          y = "Quantis da amostra") +
    theme_minimal()
    ggsave("Gráficos_gerados\\qq_plot_2024.png", width = 8, height = 6, dpi = 300)
    
  ggarrange(g1, g2, g3,
            ncol = 1, nrow = 3,
            legend = "right")
  
  
  ggsave("Gráficos_gerados\\distribuição_2024.png", width = 8, height = 6, dpi = 300)
  
  # Criar tabela com coeficientes de determinação das edições anteriores:
  df_list <- list(
    dat1_2022,
    dat1_2023,
    dat1_2024
  )
  cof_r2 <- c()
  for (i in 1:3) {
    dat1 <- data.frame(df_list[i]) #seleciona o ano de estudo
    
    # Transformando todas as notas em dado tipo double:
    dat1$NU_NOTA_CN <- as.double(dat1$NU_NOTA_CN) #nota de ciências da natureza
    dat1$NU_NOTA_CH <- as.double(dat1$NU_NOTA_CH) #nota de ciências humanas
    dat1$NU_NOTA_LC <- as.double(dat1$NU_NOTA_LC) #nota de linguagens e códigos
    dat1$NU_NOTA_MT <- as.double(dat1$NU_NOTA_MT) #nota de matemática
    dat1$NU_NOTA_REDACAO <- as.double(dat1$NU_NOTA_REDACAO) #nota da redação
    
    # Criar coluna com notas finais
    dat1 <- dat1 |>
      mutate(nota_final = rowMeans(across(c(NU_NOTA_CN, 
                                            NU_NOTA_CH, 
                                            NU_NOTA_LC, 
                                            NU_NOTA_MT, 
                                            NU_NOTA_REDACAO))))
    
    
    not_media <- mean(dat1$nota_final, na.rm = TRUE)
    not_sd <- sd(dat1$nota_final, na.rm = TRUE)
    
    not_norm <- (dat1$nota_final - not_media)/not_sd
    vet_p <- seq(0.0001, 0.9999, 0.0001)
    quan_norm <- qnorm(vet_p)
    
    quan_dis_notas <- quantile(not_norm, probs = vet_p, na.rm = TRUE)
    r2 <- 1-(sum((quan_dis_notas-quan_norm)**2)/sum((quan_dis_notas-mean(quan_dis_notas))**2)) ; r2
    
    cof_r2 <- c(cof_r2, r2)
  }
  #Montar a tabela:
  pacman::p_load(gt)
  
  
  cof_r2_2 <- data.frame(Ano = c("2022","2023","2024"), R2 = cof_r2)
  
  cof_r2_2 |> gt() 
  
#Notas por localização da escola (1-urbana , 2-rural)
  name <- c("Urbana","Rural")
  not_loc <- aggregate(nota_final ~ TP_LOCALIZACAO_ESC , data = dat1, mean)
  y <- table(select(dat1,TP_LOCALIZACAO_ESC))
  y <- y/sum(y)
  not_loc <- mutate(not_loc, Proporção = y) ; not_loc
  
  
#Notas por dependencia administrativa: 
  not_dep <- aggregate(nota_final ~ TP_DEPENDENCIA_ADM_ESC , data = dat1, mean)
  y <- table(select(dat1,TP_DEPENDENCIA_ADM_ESC))
  y <- y/sum(y)
  not_dep <- mutate(not_dep, Proporção = y) ; not_dep
  
#Notas por situação de funcionamento da escola: 
#Todas as escolas estão em funcionamento em 2024. 2023 é diferente
  not_fun <- aggregate(nota_final ~ TP_SIT_FUNC_ESC , data = dat1, mean)
  y <- table(select(dat1,TP_SIT_FUNC_ESC))
  y <- y/sum(y)
  not_fun <- mutate(not_fun, Proporção = y) ; not_fun
  
#Proporções de situação da redação dos participantes: Não é interessante
  est_redacao <- table(select(dat1,TP_STATUS_REDACAO))
  est_redacao <- est_redacao/sum(est_redacao) ; est_redacao
  
  
  ####################################################################
  # 2. Calculo do coeficiente de correlaçao de pearson para todas as areas:
  # 2024 e 2023 concordam, 2022 tem algo diferente
  ####################################################################
  
  library(GGally)
  
  painel_disp <- function(data, mapping, ...) {
    ggplot(sample_frac(select(dat1, NU_NOTA_CN, 
                                    NU_NOTA_CH, 
                                    NU_NOTA_LC, 
                                    NU_NOTA_MT, 
                                    NU_NOTA_REDACAO), 0.01), mapping) +
      geom_point(alpha = 0.1, size = 0.1) +
      theme_bw()
  }
  painel_cor <- function(data, mapping, ...) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    r <- cor(x, y, use = "pairwise.complete.obs")
    
    ggally_text(
      label = sprintf("r = %.4f", r),
      xP = 0.5, yP = 0.5, size = 5
    )
  }
  painel_dens <- function(data, mapping, ...) {
    ggplot(data, mapping) +
      geom_density(fill = "lightblue", alpha = 0.5) +
      theme_bw()
  }
  
  dat1 <- dat1_2022 #seleciona o ano em estudo
  
  ggpairs(
    data = select(dat1, NU_NOTA_CN, 
                  NU_NOTA_CH, 
                  NU_NOTA_LC, 
                  NU_NOTA_MT, 
                  NU_NOTA_REDACAO),
    lower = list(continuous = painel_disp),
    upper = list(continuous = painel_cor),
    diag  = list(continuous = painel_dens),
    columnLabels = c("Ciências da Natureza",
                     "Ciências Humanas", 
                     "Linguagens e Códigos", 
                     "Matemática", 
                     "Redação"))
  
  ggsave("Gráficos_gerados\\correlacao_por_areas_2022.png", width = 8, height = 6, dpi = 300)
  
  
  ###########################################################################
  # 3. Analise linear multivariada
  # dados de 2022 para criar o modelo e dados de 2023 parea testa-lo
  ###########################################################################
  
  #função para criar as variaveis categoricas a serem usadas no modelo e na verificação
  prep_model <- function(dat1) {
    
    #retira os candidatos com notas finais com NA:
    dat1 <- dat1 |> filter( !is.na(nota_final))
    
    dat1 <- dat1 |> rename(estado_escola = SG_UF_ESC) 
    dat1 <- dat1 |> rename(dependencia_adm = TP_DEPENDENCIA_ADM_ESC) 
    dat1 <- dat1 |> rename(localizacao_escola = TP_LOCALIZACAO_ESC) 
    dat1 <- dat1 |> rename(faixa_etaria = TP_FAIXA_ETARIA) 
    dat1 <- dat1 |> rename(sexo = TP_SEXO) 
    dat1 <- dat1 |> rename(cor_raca = TP_COR_RACA) 
    dat1 <- dat1 |> rename(tipo_escola = TP_ESCOLA) 
    dat1 <- dat1 |> rename(escolaridade_pai = Q001) 
    dat1 <- dat1 |> rename(escolaridade_mae = Q002) 
    dat1 <- dat1 |> rename(ocupacao_pai = Q003) 
    dat1 <- dat1 |> rename(ocupacao_mae = Q004) 
    dat1 <- dat1 |> rename(pessoas_na_residencia = Q005) 
    dat1 <- dat1 |> rename(renda_familiar_mensal = Q006) 
    dat1 <- dat1 |> rename(possui_computador = Q024) 
    dat1 <- dat1 |> rename(possui_internet = Q025) 
    
    # Transformar as variaveis explicativas em fator
    
    dat1$estado_escola <- as.factor(dat1$estado_escola)
    dat1$estado_escola <- relevel(dat1$estado_escola, ref = "SP")
    dat1$estado_escola[dat1$estado_escola == ""] <- NA
    
    dat1$pessoas_na_residencia <- as.factor(dat1$pessoas_na_residencia)
    
    dat1$dependencia_adm <- as.factor(dat1$dependencia_adm)
    levels(dat1$dependencia_adm) <- c(": Federal", ": Estadual", ": Municipal", ": Privada")
    dat1$dependencia_adm <- relevel(dat1$dependencia_adm, ref = ": Privada")
    
    dat1$localizacao_escola <- as.factor(dat1$localizacao_escola)
    levels(dat1$localizacao_escola) <- c(": Urbana", ": Rural")
    
    dat1$faixa_etaria <- as.factor(dat1$faixa_etaria)
    levels(dat1$faixa_etaria) <- c(": Menor de 17 anos",
                                   ": 17 anos",
                                   ": 18 anos",
                                   ": 19 anos",
                                   ": 20 anos",
                                   ": 21 anos",
                                   ": 22 anos",
                                   ": 23 anos",
                                   ": 24 anos",
                                   ": 25 anos",
                                   ": Entre 26 e 30 anos",
                                   ": Entre 31 e 35 anos",
                                   ": Entre 36 e 40 anos",
                                   ": Entre 41 e 45 anos",
                                   ": Entre 46 e 50 anos",
                                   ": Entre 51 e 55 anos",
                                   ": Entre 56 e 60 anos",
                                   ": Entre 61 e 65 anos",
                                   ": Entre 66 e 70 anos",
                                   ": Maior de 70 anos")
    
    dat1$sexo <- as.factor(dat1$sexo)
    levels(dat1$sexo) <- c(": Feminino", ": Masculino")
    
    dat1$cor_raca <- as.factor(dat1$cor_raca)
    dat1$cor_raca[dat1$cor_raca == "6"] <- NA
    levels(dat1$cor_raca) <- c(": Não declarado",
                               ": Branca",
                               ": Preta",
                               ": Parda",
                               ": Amarela",
                               ": Indígena")
    
    dat1$tipo_escola <- as.factor(dat1$tipo_escola)
    dat1$tipo_escola[dat1$tipo_escola == "1"] <- NA
    levels(dat1$tipo_escola) <- c(": Não Respondeu",
                                  ": Pública",
                                  ": Privada")
    
    dat1$renda_familiar_mensal <- as.factor(dat1$renda_familiar_mensal)
    levels(dat1$renda_familiar_mensal) <- c(": Nenhuma Renda",
                                  ": Até R$ 1.212,00",
                                  ": De R$ 1.212,01 até R$ 1.818,00.",
                                  ": De R$ 1.818,01 até R$ 2.424,00.",
                                  ": De R$ 2.424,01 até R$ 3.030,00.",
                                  ": De R$ 3.030,01 até R$ 3.636,00.",
                                  ": De R$ 3.636,01 até R$ 4.848,00.",
                                  ": De R$ 4.848,01 até R$ 6.060,00.",
                                  ": De R$ 6.060,01 até R$ 7.272,00.",
                                  ": De R$ 7.272,01 até R$ 8.484,00.",
                                  ": De R$ 8.484,01 até R$ 9.696,00.",
                                  ": De R$ 9.696,01 até R$ 10.908,00.",
                                  ": De R$ 10.908,01 até R$ 12.120,00.",
                                  ": De R$ 12.120,01 até R$ 14.544,00.",
                                  ": De R$ 14.544,01 até R$ 18.180,00.",
                                  ": De R$ 18.180,01 até R$ 24.240,00.",
                                  ": Acima de R$ 24.240,00.")
    #dat1$renda_familiar_mensal <- relevel(dat1$renda_famiiar_mensal, ref = "Nenhuma Renda")
    
    dat1$escolaridade_pai <- as.factor(dat1$escolaridade_pai)
    dat1$escolaridade_pai[dat1$escolaridade_pai == "H"] <- NA
    
    dat1$escolaridade_mae <- as.factor(dat1$escolaridade_mae)
    dat1$escolaridade_mae[dat1$escolaridade_mae == "H"] <- NA
    
    dat1$ocupacao_pai <- as.factor(dat1$ocupacao_pai)
    dat1$ocupacao_pai[dat1$ocupacao_pai == "F"] <- NA
    
    dat1$ocupacao_mae <- as.factor(dat1$ocupacao_mae)
    dat1$ocupacao_mae[dat1$ocupacao_mae == "F"] <- NA
    
    dat1$possui_computador <- as.factor(dat1$possui_computador)
    levels(dat1$possui_computador) <- c(": Não.",
                                            ": Sim, um.",
                                            ": Sim, dois.",
                                            ": Sim, três.",
                                            ": Sim, quatro ou mais.")
    
    dat1$possui_internet <- as.factor(dat1$possui_internet)
    levels(dat1$possui_internet) <- c(": Não.", ": Sim.")
    
    return(dat1)
    
  }
  
  # Modelo de regressão linear multivariada aplicado em 2022:
  dat1_2022 <- prep_model(dat1_2022)
  
  modelo_2022 = lm(nota_final ~ estado_escola +
                 pessoas_na_residencia + 
                 dependencia_adm +
                 localizacao_escola + 
                 faixa_etaria + 
                 sexo + 
                 cor_raca + 
                 tipo_escola + 
                 renda_familiar_mensal + 
                 escolaridade_pai + 
                 escolaridade_mae +
                 ocupacao_pai +
                 ocupacao_mae +
                 possui_computador +
                 possui_internet, data = dat1_2022)
  
  summary(modelo_2022)
  
  #analise com a função ANOVA:
  anova(modelo_2022)
  #O desempenho dos alunos é fortemente influenciado por fatores estruturais da escola (dependência administrativa, tipo, localização) e fatores socioeconômicos (renda, escolaridade dos pais, raça). Variáveis individuais como sexo têm efeito menor, mas ainda significativo.
  
  # stepAIC: escolhe as variaveis mais releventes automaticamente:
  library(MASS)
  stepAIC(modelo_2022, direction = "both")
  
  #VIP: importância de variáveis
  install.packages("vip")
  library(vip)
  vip(modelo_2022)
  
  #usaremos o modelo de regressão de 2022 para tentar descrever os resultados de 2023:
  
  # prepara os dados de 2023 para serem inseridos no modelo:
  dat1_2023 <- prep_model(dat1_2023)
  
  # executa a previsão para o novo ano:
  # problemas com os novos niveis:
  nota_pred <- predict(modelo_2022, newdata = dat1_2023)
  
  # avalia se o modelo é consistente:
  mse <- mean((dat1_2023$nota_final - nota_pred)^2, na.rm = TRUE)
  ss_res <- sum((dat1_2023$nota_final - nota_pred)^2, na.rm = TRUE)
  ss_tot <- sum((dat1_2023$nota_final - mean(dat1_2023$nota_final, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res/ss_tot ; r2
  np <- sum(!is.na(nota_pred))/(length(nota_pred)) ; np
  
  
  ############################################################
  #   Distribuição das notas tem relação com o tamanho da escola?
  #   2024: SÓ seria aplicar a essa edição
  #   2023: não tem o id da escola
  #   2022: idem a 2023
  ############################################################
  #Dados do catalogo das escolas:
  dat2 <- read.csv("Dados_utilizados\\Análise - Tabela da lista das escolas - Detalhado.csv")
  
  not_media_por_escolas <- aggregate(nota_final ~ CO_ESCOLA, data = dat1, mean)
  
  dat3 <- data.frame(CO_ESCOLA = dat2$Código.INEP, porte_esc = dat2$Porte.da.Escola)
   
  # Anexar o tamanho das escolas:
  dat4 <- left_join(not_media_por_escolas, dat3)
  
  #mas as escolas com menos de 10 participantes tem o seu id substituido por uma mascara?
  #o que pode ser inferido de uma escola com menos de 10 participantes?
  #12% das escolas não foram encontradas nessa base de dados!
  #pode ser que a base de dados do censo das escola esteja incompleta

  rot <- c(
    "Dados faltantes",
    "Até 50 matrículas de escolarização",
    "Entre 51 e 200 matrículas de escolarização",
    "Entre 201 e 500 matrículas de escolarização",
    "Entre 501 e 1000 matrículas de escolarização",
    "Mais de 1000 matrículas de escolarização"
           )
  
  glob <- list(dat4[is.na(dat4$porte_esc),],
    dat4[dat4$porte_esc == "Até 50 matrículas de escolarização",],
    dat4[dat4$porte_esc == "Entre 51 e 200 matrículas de escolarização",],
    dat4[dat4$porte_esc == "Entre 201 e 500 matrículas de escolarização",],
    dat4[dat4$porte_esc == "Entre 501 e 1000 matrículas de escolarização",],
    dat4[dat4$porte_esc == "Mais de 1000 matrículas de escolarização",])
  
  
  max_len <- max(length(glob[1:6]))
  
  df <- data.frame(
    A = c(a, rep(NA, max_len - length(a))),
  )
  
  
  for (i in 1:6) {
    
    data.frame(glob[i]) |> 
    ggplot( aes(x = nota_final)) +  
    geom_boxplot() + 
    coord_flip() 
    
  }
  
  
  
  
  
  
  
  
  
  plot1 <-  
    dat4[dat4$porte_esc == "Mais de 1000 matrículas de escolarização",] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() +
    ylab("Mais de 1000 alunos") + 
    xlab("")
    
  plot2 <-  
    dat4[dat4$porte_esc == "Entre 501 e 1000 matrículas de escolarização",] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() + 
    ylab("entre 501 e 1000 alunos") + 
    xlab("")
  
  plot3 <-  
    dat4[dat4$porte_esc == "Entre 201 e 500 matrículas de escolarização",] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() +
    ylab("entre 201 e 500 alunos") + 
    xlab("")

  plot4 <-  
    dat4[dat4$porte_esc == "Entre 51 e 200 matrículas de escolarização",] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() +
    ylab("entre 51 e 200 alunos") + 
    xlab("")
  
  plot5 <-  
    dat4[dat4$porte_esc == "Até 50 matrículas de escolarização",] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() +
    ylab("até 50 alunos") + 
    xlab("")
    
  plot6 <-  
    dat4[is.na(dat4$porte_esc),] |> 
    ggplot( aes(x = nota_final)) + 
    geom_boxplot() + 
    coord_flip() +
    ylab("Dados faltantes") + 
    xlab("")

  ggarrange(plot6, plot5, plot4, plot3, plot2, plot1,
            ncol = 6, nrow = 1,
            common.legend = TRUE,
            legend = "none")
  
  #Verifica o peso que cada categoria de escola tem no exame
  table(dat4$porte_esc)/length(dat4$porte_esc)
  
  
  
  
  
  
  
  
  ##########################################################
  #     Distribuição das notas geograficamente:
  ##########################################################

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(geobr, sf, ggplot2, dplyr)
  library(sf)
  library(spdep)
  
  dat1 <- dat1_2022 #seleciona o ano da análise
  
  #criar data frame de cidade em função da media.
  med_muni <- aggregate(nota_final ~ CO_MUNICIPIO_ESC , data = dat1, mean)
  colnames(med_muni)[1] <- "code_muni"
  
  
  # Ler todos os municipios em 2022 (Ano de Censo)
  municipios <- read_municipality(code_muni = "all", year = 2022, showProgress = FALSE)
  
  mapa <- municipios %>%
    left_join(med_muni, by = "code_muni")
  # municipios sem nota são substituidos pela mediana nacional
  mapa$nota_final[is.na(mapa$nota_final)] <- median(mapa$nota_final, na.rm = TRUE)
  
  mapa <- st_make_valid(mapa) |> st_cast("MULTIPOLYGON")
  
  # 1) Criar vizinhança (contiguidade)
  nb <- poly2nb(mapa)
  
  # 2) Criar matriz de pesos espaciais
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # 3) Teste de Moran
  moran.test(mapa$nota_final, lw, zero.policy = TRUE)
  
  #2024:
  moran_text <- sprintf("Moran I statistic = %.5f , valor-p < 2.2e-16", 4.531940e-01)
  
  #2023: 
  moran_text <- sprintf("Moran I statistic = %.5f , valor-p < 2.2e-16", 3.153219e-01)
  
  #2022: 
  moran_text <- sprintf("Moran I statistic = %.5f , valor-p < 2.2e-16", 3.346726e-01)
  
  ggplot(mapa) +
    geom_sf(aes(fill = nota_final), color = NA) +
    scale_fill_viridis_c(option = "magma", na.value = "grey90") +
    theme_minimal() +
    labs(
      fill = "Nota média",
      title = "Nota média do enem 2022 por municipio brasileiro",
      subtitle = moran_text
    )
  
  ggsave("Gráficos_gerados\\distribuição_mapa_2022.png", width = 8, height = 6, dpi = 300)
  
  
  
  
  

  
  
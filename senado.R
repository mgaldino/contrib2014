##Investigando Senado

baseSenado <- subset(base1, cargo =="sen")
setkey(baseSenado, "status_eleito")

# Num candidatos e Receia média dos eleitos
baseSenado["ELEITO" , list(receitaMedia = mean(receita), num_candidato=length(CPF.do.candidato)), by=genero]

# num candidatos e receita média de todos candidatos
baseSenado[ , list(receitaMedia = mean(receita), num_candidato=length(CPF.do.candidato)), by=genero]

senadoUF <- baseSenado[ , list(receitaMedia = mean(receita), num_candidato=length(CPF.do.candidato)), by=c("genero", "sigla", "status_eleito")]

View(subset(baseSenado, sigla == "SP"))


dodgewidth <- position_dodge(width=0.9)

p <- ggplot(senadoUF, aes(sigla, y=receitaMedia, fill=genero) ) +
  geom_bar(stat = "identity", position=dodgewidth)  + scale_fill_grey(start = 0.1, end = .9, name="") + 
  scale_y_continuous(labels = dollar, limits=c(0, 1700000)) +theme_tb(legenda="right") + ylab("Receita Média") + xlab("")
p

ggsave(p, file="receita_genero_cargo_barra_V2.pdf", scale=2)






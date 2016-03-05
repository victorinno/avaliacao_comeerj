library(dplyr)
library(ggplot2)
library(stats)
library(RColorBrewer)

data_av <- read.csv2("AVALIACAO.csv", stringsAsFactors = F)

colunas <- c("Resumo de Notas","A COMEERj como um todo", "Organização", "Limpeza", "Conforto", "Filas",
             "Alegria e Participação", "Ambiente Físico", "Sintonia Espiritual",
             "Recepção", "Alojamento", "Coordenador de Alojamento", "Secretaria",
             "Divulgação", "Livraria", "Financeiro", "Assitência Médica",
             "Passes", "Atendimento Fraterno","Alimentação", "Cardápio",
             "Cantina", "Multimeios", "Serviços Gerais", "Grupo de Estudos",
             "Facilitadores", "Atividades e Dinâmicas", "Músicas", "Ambientações", "Oficinas", 
             "Pequenos Companheiros")

data_av_total <-  data_av %>% select(- CLASSE) %>%  group_by(AG2) %>%  summarise(total = n())

dados <- list(data_av_total, data_av %>% select(AG1) %>%  group_by(AG1) %>%  summarise(total = n()),
           data_av %>% select(AG2) %>%  group_by(AG2) %>%  summarise(total = n()),
           data_av %>% select(AG3) %>%  group_by(AG3) %>%  summarise(total = n()),
           data_av %>% select(AG4) %>%  group_by(AG4) %>%  summarise(total = n()),
           data_av %>% select(AG5) %>%  group_by(AG5) %>%  summarise(total = n()),
           data_av %>% select(AG6) %>%  group_by(AG6) %>%  summarise(total = n()),
           data_av %>% select(AG7) %>%  group_by(AG7) %>%  summarise(total = n()),
           data_av %>% select(AG8) %>%  group_by(AG8) %>%  summarise(total = n()),
           data_av %>% select(AA1) %>%  group_by(AA1) %>%  summarise(total = n()),
           data_av %>% select(AA2) %>%  group_by(AA2) %>%  summarise(total = n()),
           data_av %>% select(AA3) %>%  group_by(AA3) %>%  summarise(total = n()),
           data_av %>% select(AA4) %>%  group_by(AA4) %>%  summarise(total = n()),
           data_av %>% select(AA5) %>%  group_by(AA5) %>%  summarise(total = n()),
           data_av %>% select(AA6) %>%  group_by(AA6) %>%  summarise(total = n()),
           data_av %>% select(AA7) %>%  group_by(AA7) %>%  summarise(total = n()),
           data_av %>% select(AF1) %>%  group_by(AF1) %>%  summarise(total = n()),
           data_av %>% select(AF2) %>%  group_by(AF2) %>%  summarise(total = n()),
           data_av %>% select(AF3) %>%  group_by(AF3) %>%  summarise(total = n()),
           data_av %>% select(AL1) %>%  group_by(AL1) %>%  summarise(total = n()),
           data_av %>% select(AL2) %>%  group_by(AL2) %>%  summarise(total = n()),
           data_av %>% select(AL3) %>%  group_by(AL3) %>%  summarise(total = n()),
           data_av %>% select(AL4) %>%  group_by(AL4) %>%  summarise(total = n()),
           data_av %>% select(AL5) %>%  group_by(AL5) %>%  summarise(total = n()),
           data_av %>% select(EA1) %>%  group_by(EA1) %>%  summarise(total = n()),
           data_av %>% select(EA2) %>%  group_by(EA2) %>%  summarise(total = n()),
           data_av %>% select(EA3) %>%  group_by(EA3) %>%  summarise(total = n()),
           data_av %>% select(EA4) %>%  group_by(EA4) %>%  summarise(total = n()),
           data_av %>% select(EA5) %>%  group_by(EA5) %>%  summarise(total = n()),
           data_av %>% select(EA6) %>%  group_by(EA6) %>%  summarise(total = n()),
           data_av %>% select(EA7) %>%  group_by(EA7) %>%  summarise(total = n()))
g <- ggplot(dados[[1]] , aes(x =as.factor(AG2), y = total, group=AG2,fill = factor(dados[[1]]$AG2)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[1] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[1], ".jpg") )
g <- ggplot(dados[[2]] , aes(x =as.factor(AG1), y = total, group=AG1,fill = factor(dados[[2]]$AG1)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[2] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[2], ".jpg") )
g <- ggplot(dados[[3]] , aes(x =as.factor(AG2), y = total, group=AG2,fill = factor(dados[[3]]$AG2)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[3] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[3], ".jpg") )
g <- ggplot(dados[[4]] , aes(x =as.factor(AG3), y = total, group=AG3,fill = factor(dados[[4]]$AG3)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[4] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[4], ".jpg") )
g <- ggplot(dados[[5]] , aes(x =as.factor(AG4), y = total, group=AG4,fill = factor(dados[[5]]$AG4)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[5] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[5], ".jpg") )
g <- ggplot(dados[[6]] , aes(x =as.factor(AG5), y = total, group=AG5,fill = factor(dados[[6]]$AG5)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[6] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[6], ".jpg") )
g <- ggplot(dados[[7]] , aes(x =as.factor(AG6), y = total, group=AG6,fill = factor(dados[[7]]$AG6)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[7] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[7], ".jpg") )
g <- ggplot(dados[[8]] , aes(x =as.factor(AG7), y = total, group=AG7,fill = factor(dados[[8]]$AG7)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[8] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[8], ".jpg") )
g <- ggplot(dados[[9]] , aes(x =as.factor(AG8), y = total, group=AG8,fill = factor(dados[[9]]$AG8)))  + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[9] ) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[9], ".jpg") )
g <- ggplot(dados[[10]], aes(x =as.factor(AA1), y = total, group=AA1,fill = factor(dados[[10]]$AA1))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[10]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[10], ".jpg") )
g <- ggplot(dados[[11]], aes(x =as.factor(AA2), y = total, group=AA2,fill = factor(dados[[11]]$AA2))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[11]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[11], ".jpg") )
g <- ggplot(dados[[12]], aes(x =as.factor(AA3), y = total, group=AA3,fill = factor(dados[[12]]$AA3))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[12]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[12], ".jpg") )
g <- ggplot(dados[[13]], aes(x =as.factor(AA4), y = total, group=AA4,fill = factor(dados[[13]]$AA4))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[13]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[13], ".jpg") )
g <- ggplot(dados[[14]], aes(x =as.factor(AA5), y = total, group=AA5,fill = factor(dados[[14]]$AA5))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[14]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[14], ".jpg") )
g <- ggplot(dados[[15]], aes(x =as.factor(AA6), y = total, group=AA6,fill = factor(dados[[15]]$AA6))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[15]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[15], ".jpg") )
g <- ggplot(dados[[16]], aes(x =as.factor(AA7), y = total, group=AA7,fill = factor(dados[[16]]$AA7))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[16]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[16], ".jpg") )
g <- ggplot(dados[[17]], aes(x =as.factor(AF1), y = total, group=AF1,fill = factor(dados[[17]]$AF1))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[17]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[17], ".jpg") )
g <- ggplot(dados[[18]], aes(x =as.factor(AF2), y = total, group=AF2,fill = factor(dados[[18]]$AF2))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[18]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[18], ".jpg") )
g <- ggplot(dados[[19]], aes(x =as.factor(AF3), y = total, group=AF3,fill = factor(dados[[19]]$AF3))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[19]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[19], ".jpg") )
g <- ggplot(dados[[20]], aes(x =as.factor(AL1), y = total, group=AL1,fill = factor(dados[[20]]$AL1))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[20]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[20], ".jpg") )
g <- ggplot(dados[[21]], aes(x =as.factor(AL2), y = total, group=AL2,fill = factor(dados[[21]]$AL2))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[21]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[21], ".jpg") )
g <- ggplot(dados[[22]], aes(x =as.factor(AL3), y = total, group=AL3,fill = factor(dados[[22]]$AL3))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[22]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[22], ".jpg") )
g <- ggplot(dados[[23]], aes(x =as.factor(AL4), y = total, group=AL4,fill = factor(dados[[23]]$AL4))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[23]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[23], ".jpg") )
g <- ggplot(dados[[24]], aes(x =as.factor(AL5), y = total, group=AL5,fill = factor(dados[[24]]$AL5))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[24]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[24], ".jpg") )
g <- ggplot(dados[[25]], aes(x =as.factor(EA1), y = total, group=EA1,fill = factor(dados[[25]]$EA1))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[25]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[25], ".jpg") )
g <- ggplot(dados[[26]], aes(x =as.factor(EA2), y = total, group=EA2,fill = factor(dados[[26]]$EA2))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[26]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[26], ".jpg") )
g <- ggplot(dados[[27]], aes(x =as.factor(EA3), y = total, group=EA3,fill = factor(dados[[27]]$EA3))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[27]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[27], ".jpg") )
g <- ggplot(dados[[28]], aes(x =as.factor(EA4), y = total, group=EA4,fill = factor(dados[[28]]$EA4))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[28]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[28], ".jpg") )
g <- ggplot(dados[[29]], aes(x =as.factor(EA5), y = total, group=EA5,fill = factor(dados[[29]]$EA5))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[29]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[29], ".jpg") )
g <- ggplot(dados[[30]], aes(x =as.factor(EA6), y = total, group=EA6,fill = factor(dados[[30]]$EA6))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[30]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[30], ".jpg") )
g <- ggplot(dados[[31]], aes(x =as.factor(EA7), y = total, group=EA7,fill = factor(dados[[31]]$EA7))) + geom_bar(stat = "identity") + geom_text(aes(label = sprintf("%.2f%%", total/sum(total) * 100)),  vjust = -.05) + xlab("Notas") + ylab("Total") + ggtitle(colunas[31]) + scale_fill_brewer(palette="Set1") + theme(legend.title = element_text(colour="blue", size=10, face="bold")) + scale_fill_discrete(name="Notas", labels=c("Não opnou", "Ruim", "Regular", "Bom")) 
ggsave(filename = paste0(colunas[31], ".jpg") )

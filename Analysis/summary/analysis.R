library(data.table); library(tibble); library(dplyr); library(Rtsne); library(caret)


tb <- as_tibble(fread("train.csv"))
tb$label <- tb$label %>% as.factor()

colors <- rainbow(length(unique(tb$label)))



##### tsne ####################################################################################
tsne <- Rtsne(X = tb[, -1], dim = 2, perplexity = 30, verbose = T, max.iter = 600)

pdf("figure/fig_tsne_2.pdf", height = 7, width = 9)
plot(tsne$Y, t='n', main="", xlab = 'X', ylab = 'Y', xlim = c(-45, 45), ylim = c(-45, 45))
text(tsne$Y, labels = '.', col=colors[tb$label], cex = .8)
legend("bottomleft", pch = 16, legend = 0:9, col = rainbow(10))
dev.off()

pdf("figure/fig_tsne_2_sep.pdf", height = 6, width = 12)
par(mfrow = c(2, 5))
sapply(0:9, function(d) {
  plot(1, 1, t = "n", asp = 1, xlab = "X", ylab = "Y", 
       xlim = c(-45, 45), ylim = c(-45, 45), 
       main = d)
  text(x = tsne$Y[train$label != d, 1], y = tsne$Y[train$label != d, 2], 
       labels = ".", col = gray(.87))
  text(x = tsne$Y[train$label == d, 1], y = tsne$Y[train$label == d, 2], 
       labels = ".", col = rainbow(10)[d+1] )
})
par(mfrow = c(1, 1))
dev.off()


##### dimension reduction ######################################################
P_90 <- 
  sapply(1:784, function(pix) {
    quantile(x = as.data.frame(tb[, pix+1]), probs = .90, na.rm = T)
  }) %>% as.integer %>% as.data.frame()

reduce_num <- which(P_90 > 0) 
tb_r <- tb[, c(1, reduce_num+1)]

pdf('figure/fig_99tile.pdf')
image(1:28, 1:28, z = matrix(P_90$., nrow = 28, ncol = 28)[, 28:1], col = col_trn(255), 
      xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', asp = 1, frame.plot = F)
dev.off()

##### sampling 10000 test data #################################################
smp <- sample(42000, 10000)
tb_tst <- tb_r[ smp, ]
tb_trn <- tb_r[-smp, ]

##### 1 vs all ################################################################
response_1_vs_all <- 
    sapply(0:9, function(d){
      fit <- glm(as.factor(ifelse(label==d, 1, 0)) ~ ., 
                 family = 'binomial', data = tb_trn)
      predict.glm(fit, newdata = tb_tst, type = 'response')
    })
predict_1_vs_all <- 
  sapply(1:10000, function(k){
    c(0:9)[sample(which(response_1_vs_all[k, ] == max(response_1_vs_all[k, ])), 1)]
  })
mean(predict_1_vs_all == tb_tst$label)
table(predict_1_vs_all, tb_tst$label, deparse.level = 2, dnn = c("prd", "lbl")) %>% t
ifelse(predict.glm(object = fits, newdata = tb_r[1:100, ], type = 'response')>.5, 1, 0) == as.factor(ifelse(tb_r$label[1:100]==0, 1, 0))



###### NN ######################################################################
W1 <- fread("NN/W1.csv") %>% as.tibble() %>% as.matrix()
b1 <- fread("NN/b1.csv") %>% as.tibble() %>% as.matrix()
W2 <- fread("NN/W2.csv") %>% as.tibble() %>% as.matrix()
b2 <- fread("NN/b2.csv") %>% as.tibble() %>% as.matrix()
W3 <- fread("NN/W3.csv") %>% as.tibble() %>% as.matrix() %>% matrix(ncol = 10) %>% t
b3 <- fread("NN/b3.csv") %>% as.tibble() %>% as.matrix()
prd <- 
  sapply(1:10000, function(k){
    V1 <- W1 %*% t(tb[k, -1]) + b1
    V1[V1<=0] <- 0
    V2 <- W2 %*% V1 + b2
    V2[V2<=0] <- 0
    V3 <- W3 %*% V2 + b3
    c(0:9)[which.max(V3)] 
  })

table(prd, as.data.frame(tb[1:10000, 1])[,1], deparse.level = 2, dnn = c("prd", "lbl"))

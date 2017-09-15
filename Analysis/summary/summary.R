library(plotly); library(caTools); library(animation)
train <- read.csv("train.csv")
train$label <- as.factor(train$label)
test  <- read.csv("test.csv")

##### Summary Statistics#######################################################
summary(train[, 1])

# color for the digits
col_trn <- colorRampPalette(c(gray(.87), "Blue"))
col_tst <- colorRampPalette(c(gray(.87), 1))

##### Ploting function for individual training data
plot_digit_trn <- 
  function(i){
    im_tmp <- as.numeric(train[i, ])
    im <- (matrix(im_tmp[-1], nrow = 28, ncol = 28))[, 28:1]
    image(1:28, 1:28, z = im, col = col_trn(255), 
          xlab = "", ylab = "", axes = F, asp = 1, 
#          main = train[i, 1] 
          main = ""
          )
  }
##### Ploting function for individual test data
plot_digit_tst <- 
  function(i){
    im_tmp <- as.numeric(test[i, ])
    im <- (matrix(im_tmp, nrow = 28, ncol = 28))[, 28:1]
    image(1:28, 1:28, z = im, col = col_tst(255), 
          xlab = "", ylab = "", main = "", 
          axes = F, asp = 1)
  }


########Draw a image with some example train ##############################
pdf("figure/fig_ex_trn.pdf", width = 8, height = 4)  
par(mfrow = c(2, 5))
sapply(c(5, 1, 23, 254, 1733, 244, 110, 7, 223, 28), function(i) plot_digit_trn(i))
par(mfrow = c(1, 1))
dev.off()

########Draw a image with some example test ##############################
pdf("figure/fig_ex_tst.pdf", width = 8, height = 4)  
par(mfrow = c(2, 5))
sapply(sample(1:18000, 10), function(i) plot_digit_tst(i))
par(mfrow = c(1, 1))
dev.off()

##### Draw Weird Writing ###########################################
weird <- c(9494, 9490, 33)
pdf("figure/fig_weird.pdf", width = 6, height = 2)
par(mfrow = c(1, 3))
sapply(weird, function(w) plot_digit_trn(w))
par(mfrow = c(1, 1))
dev.off()

########Draw a image with median pixels##############################
#digit_median <- 
#  sapply(0:9, function(d) {
#    digit_tmp <- train[train[, 1] == d, -1]
#    sapply(1:784, function(j) median(digit_tmp[, j]))
#})

#pdf("figure/fig_median.pdf", width = 8, height = 4)  
#par(mfrow = c(2, 5))
#sapply(1:10, function(d) {
#  im <- matrix(digit_median[, d], nrow = 28, ncol = 28)[, 28:1]
#  image(1:28, 1:28, z = im, col = col_trn(255), 
#        xlab = "", ylab = "", main = d - 1, 
#        axes = F, asp = 1)
#})
#par(mfrow = c(1, 1))
#dev.off()

########Draw a image with Q1 pixels##############################
#digit_Q1 <- 
#  sapply(0:9, function(d) {
#    digit_tmp <- train[train[, 1] == d, -1]
#    sapply(1:784, function(j) quantile(digit_tmp[, j], probs = .25))
#  })

#pdf("figure/fig_Q1.pdf", width = 8, height = 4)  
#par(mfrow = c(2, 5))
#sapply(1:10, function(d) {
#  im <- matrix(digit_Q1[, d], nrow = 28, ncol = 28)[, 28:1]
#  image(1:28, 1:28, z = im, col = col_trn(255), 
#        xlab = "", ylab = "", main = d - 1, 
#        axes = F, asp = 1)
#})
#par(mfrow = c(1, 1))
#dev.off()

########Draw a image with Q3 pixels##############################
#digit_Q3 <- 
#  sapply(0:9, function(d) {
#    digit_tmp <- train[train[, 1] == d, -1]
#    sapply(1:784, function(j) quantile(digit_tmp[, j], probs = .75))
#  })

#pdf("figure/fig_Q3.pdf", width = 8, height = 4)  
#par(mfrow = c(2, 5))
#sapply(1:10, function(d) {
#  im <- matrix(digit_Q3[, d], nrow = 28, ncol = 28)[, 28:1]
#  image(1:28, 1:28, z = im, col = col_trn(255), 
#        xlab = "", ylab = "", main = d - 1, 
#        axes = F, asp = 1)
#})
#par(mfrow = c(1, 1))
#dev.off()



########Draw a image with 100 percentiles pixels##############################
#file_name <- paste0(paste0("figure/fig_100tile/fig_", sprintf(fmt = "%03d", 100*seq(0, 1, 0.01))), ".pdf")
#perct <- seq(0, 1, 0.01)
#sapply(1:101, function(p){
#  digit_perc <- 
#    sapply(0:9, function(d) {
#      digit_tmp <- train[train[, 1] == d, -1]
#      sapply(1:784, function(j) quantile(digit_tmp[, j], probs = perct[p]))
#    })
#  pdf(file_name[p], width = 8, height = 4)  
#  par(mfrow = c(2, 5))
#  sapply(1:10, function(d) {
#    im <- matrix(digit_perc[, d], nrow = 28, ncol = 28)[, 28:1]
#    image(1:28, 1:28, z = im, col = col_trn(255), 
#          xlab = "", ylab = "", main = d - 1, 
#          axes = F, asp = 1)
#  })
#  par(mfrow = c(1, 1))
#  dev.off()
#})



########Draw a image with mean pixels##############################
# maen
digit_mean <- sapply(0:9, function(d) colMeans(train[train[, 1] == d, -1]) )
# trimmed mean
digit_mean <- 
  sapply(0:9, function(d) {
    sapply(2:785, function(px) mean(train[train$label == d, px], trim = .25))
  } )


# pdf("figure/fig_mean_digit.pdf", width = 8, height = 4)  
pdf("figure/fig_mean_digit_trim.pdf", width = 8, height = 4)  
par(mfrow = c(2, 5))
sapply(1:10, function(d) {
  im <- matrix(digit_mean[, d], nrow = 28, ncol = 28)[, 28:1]
  image(1:28, 1:28, z = im, col = col_trn(255), 
        xlab = "", ylab = "", main = d - 1, 
        axes = F, asp = 1)
})
par(mfrow = c(1, 1))
dev.off()

##### Mean curve ################################################################

# pdf("figure/fig_mean_curve.pdf", height = 6, width = 12)
pdf("figure/fig_mean_curve_trim.pdf", height = 6, width = 12)
par(mfrow = c(2, 5))
sapply(0:9, function(d) {
  plot(1, 1, t = 'n', 
       xlab = "Pixel ID", ylab = "Gray Scale Value", 
       main = paste("Mean Curve of", d), 
       frame.plot = F, xlim = c(0, 800), ylim = c(0, 255))
  polygon(x = c(-5, 790, 790, -5), y = c(-5, -5, 260, 260), col = gray(.7), border = F)
  points(digit_mean[, d+1], type = "l", 
         col = rainbow(10)[d+1], 
         lwd = 1.1)
})
par(mfrow = c(1, 1))
dev.off()


##### Mean Draw Both ################################################################
digit_mean <- sapply(0:9, function(d) colMeans(train[train[, 1] == d, -1]) )

pdf("figure/fig_mean.pdf", width = 8, height = 6)  
par(mfrow = c(4, 5))
sapply(1:5, function(d) {
  im <- matrix(digit_mean[, d], nrow = 28, ncol = 28)[, 28:1]
  image(1:28, 1:28, z = im, col = col_trn(255), 
        xlab = "", ylab = "", main = "", 
        axes = F, asp = 1)
})
sapply(0:4, function(d) {
  plot(1, 1, t = 'n', 
       xlab = "", ylab = "", 
       main = "", 
       frame.plot = F, xlim = c(0, 800), ylim = c(0, 255))
  polygon(x = c(-5, 790, 790, -5), y = c(-5, -5, 260, 260), col = gray(.7), border = F)
  points(colMeans(train[train$label == d, -1]), type = "l", 
         col = rainbow(10)[d+1], 
         lwd = 1.1)
})
sapply(6:10, function(d) {
  im <- matrix(digit_mean[, d], nrow = 28, ncol = 28)[, 28:1]
  image(1:28, 1:28, z = im, col = col_trn(255), 
        xlab = "", ylab = "", main = "", 
        axes = F, asp = 1)
})
sapply(5:9, function(d) {
  plot(1, 1, t = 'n', 
       xlab = "", ylab = "", 
       main = "", 
       frame.plot = F, xlim = c(0, 800), ylim = c(0, 255))
  polygon(x = c(-5, 790, 790, -5), y = c(-5, -5, 260, 260), col = gray(.7), border = F)
  points(colMeans(train[train$label == d, -1]), type = "l", 
         col = rainbow(10)[d+1], 
         lwd = 1.1)
})
par(mfrow = c(1, 1))
dev.off()



########Draw a image with 100 percentiles pixels "png" ##############################
file_name <- paste0(paste0("figure/fig_100tile_png/fig-", 100*seq(0, 1, 0.01)), ".png")
perct <- seq(0, 1, 0.01)
sapply(1:101, function(p){
  digit_perc <- 
    sapply(0:9, function(d) {
      digit_tmp <- train[train[, 1] == d, -1]
      sapply(1:784, function(j) quantile(digit_tmp[, j], probs = perct[p]))
    })
  png(file_name[p], width = 1200, height = 950)  
  par(mfrow = c(4, 5))
  sapply(1:5, function(d) {
    im <- matrix(digit_perc[, d], nrow = 28, ncol = 28)[, 28:1]
    image(1:28, 1:28, z = im, col = col_trn(255), 
          xlab = "", ylab = "", main = "", 
          axes = F, asp = 1)
  })
  sapply(1:5, function(d) {
    plot(1, 1, t = 'n', 
         xlab = "Pixel ID", ylab = "Gray Scale Value", 
         main = "", 
         frame.plot = F, xlim = c(0, 800), ylim = c(0, 255))
    polygon(x = c(-5, 790, 790, -5), y = c(-5, -5, 260, 260), col = gray(.7), border = F)
    points(digit_perc[, d], type = "l", 
           col = rainbow(10)[d], 
           lwd = 1.1)
  })
  sapply(6:10, function(d) {
    im <- matrix(digit_perc[, d], nrow = 28, ncol = 28)[, 28:1]
    image(1:28, 1:28, z = im, col = col_trn(255), 
          xlab = "", ylab = "", main = "", 
          axes = F, asp = 1)
  })
  sapply(6:10, function(d) {
    plot(1, 1, t = 'n', 
         xlab = "Pixel ID", ylab = "Gray Scale Value", 
         main = "", 
         frame.plot = F, xlim = c(0, 800), ylim = c(0, 255))
    polygon(x = c(-5, 790, 790, -5), y = c(-5, -5, 260, 260), col = gray(.7), border = F)
    points(digit_perc[, d], type = "l", 
           col = rainbow(10)[d], 
           lwd = 1.1)
  })
  par(mfrow = c(1, 1))
  dev.off()
})



##### Make GIF ########################################################################
three_digit <- sprintf(fmt = "%03d", 100*seq(0, 1, 0.01))
saveGIF(expr = sapply(three_digit, function(p) paste0("figure/fig_100tile_png/fig_", p, ".png")), convert = 'convert')

##### PCA #############################################################################
smp <- 1:42000
pca <- prcomp(train[smp, -1])
# pcaa <- prcomp(train[smp, -1]/255)
pca_2 <- pca$x[, 1:2]

pca_summary <- rbind(as.matrix(pca$rotation), (pca$sdev)^2, cumsum((pca$sdev)^2)/sum((pca$sdev)^2))
round(pcaa_summary[c(1, 2, 463, dim(pcaa_summary)[1]-2, dim(pcaa_summary)[1]-1, dim(pcaa_summary)[1]), 
                   c(1, 2, 43, 784)], 3)

cum_perc <- cumsum((pca$sdev)^2)/sum((pca$sdev)^2)

pdf("figure/fig_pca_cum.pdf", height = 7, width = 9)
plot(cum_perc, type = "n", col = "brown2", 
     xlim = c(0, 100), ylim = c(0, 1), 
     xlab = "Number of the Component", ylab = "")
segments(x0 = 43, y0 = 0  , x1 = 43, y1 = cum_perc[43], lty = 2, col = gray(.5))
segments(x0 =  0, y0 = 0.8, x1 = 43, y1 = 0.8         , lty = 2, col = gray(.5))
#points((pca$sdev)^2/sum((pca$sdev)^2), type = "l", col = "brown4")
points((pca$sdev)^2/sum((pca$sdev)^2), pch = 16, cex = 1, col = "brown4")
points(cum_perc, pch = 16, cex = 1, col = "brown2")
abline(h = 1, lty = 2, col = gray(.5))
dev.off()

pdf("figure/fig_pca_2.pdf", height = 7, width = 9)
plot(pca_2, t = "n", asp = 1, xlim = c(-2500, 1500), ylim = c(-1500, 1500), 
     main = "")
text(x = pca_2[, 1], y = pca_2[, 2], labels = ".", col = rainbow(10)[train$label[smp]])
legend("bottomleft", pch = 16, legend = 0:9, col = rainbow(10))
dev.off()


pdf("figure/fig_pca_2_sep.pdf", height = 6, width = 12)
par(mfrow = c(2, 5))
sapply(0:9, function(d) {
  plot(1, 1, t = "n", asp = 1, xlab = "Comp.1", ylab = "Comp.2",
       xlim = c(-2500, 1500), ylim = c(-1500, 1500), 
       main = d)
  text(x = pca_2[train$label != d, 1], y = pca_2[train$label != d, 2], 
       labels = ".", col = gray(.87))
  text(x = pca_2[train$label == d, 1], y = pca_2[train$label == d, 2], 
       labels = ".", col = rainbow(10)[d+1] )
})
par(mfrow = c(1, 1))
dev.off()

image(1:28, 1:28, z = matrix(c(0, 25), nrow = 28, ncol = 28), col = col_trn(255))

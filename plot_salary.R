load('data/jobs.RData')

mycols <- c('#E16757', '#E3935D', '#EECB5F', '#7ECF51', '#61A5E8', '#605FF0', '#9570E5')

plot_salary <- function(x, key, col) {
  ht <- hist(x$salary, breaks = 5)
  prop <- ht$counts / sum(ht$counts) * 100
  salary <- paste0(ht$breaks[1:5], '~', ht$breaks[2:6])
  barplot(prop, ylim=c(0,40), col=col, space = 0.5, ylab='%', xlab='万元', las=1, border=NA, name=salary, main=key)
}

plot_city <- function(x, key, col) {
  city <- list(bj=x$salary[x$city=='北京'],
               sh=x$salary[x$city=='上海'],
               sz=x$salary[x$city=='深圳'])
  boxplot(city, outline = F, col=col, names=c('北京', '上海', '深圳'), ylab='万元', main=key)
}

plot_edu <- function(x, key, cols) {
  prop <- round(table(x$education) / nrow(x) * 100)
  prop <- prop[c('学历不限', '本科或以上', '硕士或以上', '博士或以上')]
  pie(prop,labels = paste0(prop, '%'), col=cols, init.angle = 90, border = 'white', main=key, radius = 0.7)
  legend('topleft', names(prop), fill=cols, bty='n', border = NA)
}

plot_experience <- function(x, key, cols) {
  prop <- round(table(x$experience) / nrow(x) * 100)
  pie(prop,labels = paste0(prop, '%'), col=cols, init.angle = 90, border = 'white', main=key, radius = 0.6)
  legend('topleft', names(prop), fill=cols, bty='n', border = NA)
}

plot_salary(bioinfo, key='生物信息', col=mycols[1])
plot_salary(deeplearning, key='深度学习', col=mycols[1])

plot_city(bioinfo, key='生物信息', col=mycols[5])
plot_city(deeplearning, key='深度学习', col=mycols[5])

plot_edu(bioinfo, key='生物信息', cols=mycols[1:4])
plot_edu(deeplearning, key='深度学习', cols=mycols[1:4])

plot_experience(bioinfo, key='生物信息', cols=mycols[1:5])
plot_experience(deeplearning, key='深度学习', cols=mycols[1:5])


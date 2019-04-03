library(rvest)
library(xlsx)

get_job_on_one_page <- function(url) {
  ## html <- read_html(url)
  ## Error in open.connection(x, "rb") : Timeout was reached 
  ## http://stackoverflow.com/questions/33295686/rvest-error-in-open-connectionx-rb-timeout-was-reached
  
  download.file(url, destfile = 'scrapedpage.html', quiet = TRUE)
  html <- read_html('scrapedpage.html')
  joblist <- html %>% html_nodes('.condition') %>% html_attr('title') %>% 
    strsplit('_') %>% as.data.frame() %>% t()
  rownames(joblist) <- NULL
  colnames(joblist) <- c('salary', 'city', 'education', 'experience')
  joblist <- as.data.frame(joblist)
  joblist$name <- jobname <- html %>% html_nodes('.job-info h3 a') %>% html_text()
  joblist$name <- gsub('\\s+', '', joblist$name)
  
  joblist <- joblist[joblist$salary != "面议" & !grepl("省", joblist$city) & grepl("生物信息", joblist$name),]
  joblist$salary <- sub('万', '', joblist$salary) %>% strsplit('-') %>% 
    lapply(function(x){as.numeric(x) %>% mean()}) %>% unlist()
  joblist$city <- sub('-.*', '', joblist$city)
  return(joblist)
}

get_job_on_all_pages <- function(keyword, max_page) {
  base_url <- paste0('https://www.liepin.com/zhaopin/?isAnalysis=&dqs=&pubTime=&salary=&subIndustry=&industryType=&compscale=&init=-1&searchType=1&headckid=be882355ca77e627&compkind=&fromSearchBtn=2&sortFlag=15&ckid=be882355ca77e627&degradeFlag=0&jobKind=&industries=&clean_condition=&siTag=iag54KNHwL5zeT0B5SwxrQ%7EfA9rXquZc5IkJpXC-Ycixw&d_sfrom=search_unknown&d_ckId=7c3db8ded48fb846fcfcd73288bf43a9&d_curPage=1&d_pageSize=40&d_headId=7c3db8ded48fb846fcfcd73288bf43a9&key=', keyword, '&curPage=')
  jobs <- NULL
  for (i in 0:(max_page-1)) {
    print(i)
    cur_url <- paste0(base_url, i)
    #print(cur_url)
    jobs <- rbind(jobs, get_job_on_one_page(cur_url))
    Sys.sleep(5)
  }
  return(jobs)
}

bioinfo <- get_job_on_all_pages(keyword = '生物信息', max_page = 20)
save(bioinfo, file=paste0('data/jobs_bioinfo_', format(Sys.time(), "%Y_%b_%d"), '.RData'))

bioinfo <- bioinfo[bioinfo$education == '博士' | bioinfo$education == '硕士及以上',]
salary <- rev(quantile(bioinfo$salary, probs = seq(0, 1, 0.05)))
result <- data.frame(quantile=names(salary), salary_10k=round(salary))

wb <- createWorkbook()
sheet <- createSheet(wb)
addDataFrame(result, sheet, row.names = F)
saveWorkbook(wb, paste0('data/bioinfo_salary_distribution_of_', nrow(bioinfo), '_jobs_', format(Sys.time(), "%Y_%b_%d"), '.xlsx'))

system('rm scrapedpage.html')

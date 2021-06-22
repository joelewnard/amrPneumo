#5. RUNNING **PENICILLIN**  MODELS FOR FOREST PLOTS + CREATING DATA FILE FOR FOREST PLOT CODE  ----


load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/gbd_list.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/sregion_list.rda")

# 5.2 Penicillin + Nonsusceptible + Invasive + Pre,boundary singular fit error   ----

matrix_pen_inv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  # View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre) #, family = poisson(link = 'log')
    #model_pre_loop <- glm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre))
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    # id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #model_pre_loop <- geeglm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #id=r_id, corstr = "exchangeable") 
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    #coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    #vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1]) 
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output) #remove exp
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_pre[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_pre[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_inv_ns_pre[i, 7] <- total_cases
    matrix_pen_inv_ns_pre[i, 8] <- total_denom
    matrix_pen_inv_ns_pre[i, 9] <- study_arms
    matrix_pen_inv_ns_pre[i, 10] <- "pre"
    matrix_pen_inv_ns_pre[i, 11] <- output_exp[3]
    
  }
  else {
    mean <- mean(dta_loop_pre$ns)
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_pre[i, 1] <- mean
    matrix_pen_inv_ns_pre[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_pre[i, 7] <- total_cases
    matrix_pen_inv_ns_pre[i, 8] <- total_denom
    matrix_pen_inv_ns_pre[i, 9] <- study_arms
    matrix_pen_inv_ns_pre[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_inv_ns_pre[i, 10] <- "pre"
    
  }
}


#Create cleaned frame 
df_pen_inv_ns_pre<- data.frame(matrix_pen_inv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_pre) <- list_rownames
df_pen_inv_ns_pre[,4] <- gbd_list
#
# 5.3 Penicillin + Nonsusceptible + Invasive + Post, boundary singular fit error  ---- 

matrix_pen_inv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11) #change to 20

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  #if((length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$r_id))) ){ 
  #model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop) #for glmer
    vars <- (diag(vcov(model_post_loop))) #for glmer
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    # id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    
    #coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    #vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_post[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_post[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_post[i, 5] <- "invasive"
    matrix_pen_inv_ns_post[i, 6] <- "ns"
    matrix_pen_inv_ns_post[i, 7] <- total_cases
    matrix_pen_inv_ns_post[i, 8] <- total_denom
    matrix_pen_inv_ns_post[i, 9] <- study_arms
    matrix_pen_inv_ns_post[i, 10] <- "post"
    matrix_pen_inv_ns_post[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_post[i, 1] <- mean
    matrix_pen_inv_ns_post[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_post[i, 7] <- total_cases
    matrix_pen_inv_ns_post[i, 8] <- total_denom
    matrix_pen_inv_ns_post[i, 9] <- study_arms
    matrix_pen_inv_ns_post[i, 5] <- "invasive"
    matrix_pen_inv_ns_post[i, 6] <- "ns"
    matrix_pen_inv_ns_post[i, 10] <- "post"
  }
}


#Create cleaned frame 
df_pen_inv_ns_post<- data.frame(matrix_pen_inv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_post) <- list_rownames
df_pen_inv_ns_post[,4] <- gbd_list


# 5.4 Penicillin + Nonsusceptible + Noninvasive + Pre  ---- 
matrix_pen_ninv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    #model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #    id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_pre[i, 2] <- output_exp[2]
    matrix_pen_ninv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre[i, 10] <- "pre"
    matrix_pen_ninv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_pre[i, 1] <- mean
    matrix_pen_ninv_ns_pre[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_pre[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_ninv_ns_pre<- data.frame(matrix_pen_ninv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_pre) <- list_rownames
df_pen_ninv_ns_pre[,4] <- gbd_list

# 5.5 Penicillin + Nonsusceptible + Noninvasive + Post  ----

matrix_pen_ninv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  #model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id),  data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    # 
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_post[i, 2] <- output_exp[2] 
    matrix_pen_ninv_ns_post[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post[i, 6] <- "ns"
    matrix_pen_ninv_ns_post[i, 7] <- total_cases
    matrix_pen_ninv_ns_post[i, 8] <- total_denom
    matrix_pen_ninv_ns_post[i, 9] <- study_arms
    matrix_pen_ninv_ns_post[i, 10] <- "post"
    matrix_pen_ninv_ns_post[i, 11] <- output_exp[3] 
    
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_post[i, 1] <- mean
    matrix_pen_ninv_ns_post[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_post[i, 7] <- total_cases
    matrix_pen_ninv_ns_post[i, 8] <- total_denom
    matrix_pen_ninv_ns_post[i, 9] <- study_arms
    matrix_pen_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post[i, 6] <- "ns"
    matrix_pen_ninv_ns_post[i, 10] <- "post"
    
  }
}


#Create cleaned frame 
df_pen_ninv_ns_post<- data.frame(matrix_pen_ninv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_post) <- list_rownames
df_pen_ninv_ns_post[,4] <- gbd_list

# 5.6 Penicillin + Nonsusceptible + Invasive + Pre SUPER REGION  ----

matrix_pen_inv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11) #change to super region + 7 rows 

for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_pre) 
  
  if (length(unique(dta_loop_pre$r_id))>1){
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    #model_pre_loop <- glm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre))
    
    coefs <- fixef(model_pre_loop) #for glmer
    vars <- (diag(vcov(model_pre_loop))) #for glmer and glm
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #for gee and glm
    # vars <- model_pre_loop$robust.variance #for gee
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_pre_sregion[i, 10] <- "pre"
    matrix_pen_inv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_pre_sregion[i, 1] <- mean
    matrix_pen_inv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre_sregion[i, 6] <- "ns" #temporarily change this to sregion
    matrix_pen_inv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_inv_ns_pre_sregion<- data.frame(matrix_pen_inv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_pre_sregion) <- list_rownames
df_pen_inv_ns_pre_sregion[,4] <- sregion_list


# 5.7 Penicillin + Nonsusceptible + Invasive + Post- SUPER REGION ---- 

matrix_pen_inv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_post_sregion[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_post_sregion[i, 10] <- "post"
    matrix_pen_inv_ns_post_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_post_sregion[i, 1] <- mean
    matrix_pen_inv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_post_sregion[i, 6] <- "ns" 
    matrix_pen_inv_ns_post_sregion[i, 10] <- "post"
    
  }
}  


#Create cleaned frame 
df_pen_inv_ns_post_sregion<- data.frame(matrix_pen_inv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_post_sregion) <- list_rownames
df_pen_inv_ns_post_sregion[,4] <- sregion_list
#View(df_pen_inv_ns_post_sregion)


# 5.8 Penicillin + Nonsusceptible + Noninvasive + Pre + SUPER REGION ----

# check <- fulldta_ns %>% 
#   filter(prepost == "naive") %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(isolate_type == 1) %>% 
#   filter(super_region == "high_income") 
# nrow(check)
# check <- check %>% 
#   dplyr::select(r_id, total_isolates_drug, no_ns)
# View(check)

matrix_pen_ninv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop  
for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% 
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    
    #  model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #  
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_pen_ninv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre_sregion[i, 10] <- "pre"
    matrix_pen_ninv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_pre_sregion[i, 1] <- mean
    matrix_pen_ninv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_ninv_ns_pre_sregion<- data.frame(matrix_pen_ninv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_pre_sregion) <- list_rownames
df_pen_ninv_ns_pre_sregion[,4] <- sregion_list
#View(df_pen_ninv_ns_pre_sregion)

# 5.9 Penicillin + Nonsusceptible + Noninvasive + Post + SUPER REGION ----
matrix_pen_ninv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                         id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #  
    #  coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    #  vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_post_sregion[i, 2] <- output_exp[2] 
    matrix_pen_ninv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_post_sregion[i, 10] <- "post"
    matrix_pen_ninv_ns_post_sregion[i, 11] <- output_exp[3] 
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_post_sregion[i, 1] <- mean
    matrix_pen_ninv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_pen_ninv_ns_post_sregion<- data.frame(matrix_pen_ninv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_post_sregion) <- list_rownames
df_pen_ninv_ns_post_sregion[,4] <- sregion_list
#View(df_pen_ninv_ns_post_sregion)



# 5.10. Bind together pre-post region & super region eestimates for penicillin, nonsusceptible to create final data frames ----

# 1. Penicillin + Nonsusceptible + Invasive + Pre/ Post- Super Region 

df_pen_inv_ns_prepost_sregion <- rbind(df_pen_inv_ns_pre_sregion, df_pen_inv_ns_post_sregion)
#View(df_pen_inv_ns_prepost_sregion)

df_pen_inv_ns_prepost_sregion <- df_pen_inv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_pen_inv_ns_prepost_sregion <- df_pen_inv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_pen_inv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_pen_inv_ns_prepost_region <- rbind(df_pen_inv_ns_pre, df_pen_inv_ns_post)
df_pen_inv_ns_prepost_region <- df_pen_inv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_inv_ns_prepost_region)

df_pen_inv_ns_prepost_comb <- rbind(df_pen_inv_ns_prepost_sregion, df_pen_inv_ns_prepost_region) 
#df_pen_inv_ns_prepost_comb[nrow(df_pen_inv_ns_prepost_comb)+21,] <- NA
View(df_pen_inv_ns_prepost_comb)

df_pen_inv_ns_prepost_comb <- df_pen_inv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                           53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                           27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]
View(df_pen_inv_ns_prepost_comb) #FINAL DATA FRAME 
save(df_pen_inv_ns_prepost_comb, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_pen_inv_ns_prepost_comb.RData")

#2. Penicillin + Nonsusceptible + Noninvasive + Pre/ Post - Super Region 
df_pen_ninv_ns_prepost_sregion <- rbind(df_pen_ninv_ns_pre_sregion, df_pen_ninv_ns_post_sregion)
#View(df_pen_ninv_ns_prepost_sregion)

df_pen_ninv_ns_prepost_sregion <- df_pen_ninv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_pen_ninv_ns_prepost_sregion <- df_pen_ninv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_pen_ninv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_pen_ninv_ns_prepost_region <- rbind(df_pen_ninv_ns_pre, df_pen_ninv_ns_post)
df_pen_ninv_ns_prepost_region <- df_pen_ninv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_ninv_ns_prepost_region)

df_pen_ninv_ns_prepost_comb <- rbind(df_pen_ninv_ns_prepost_sregion, df_pen_ninv_ns_prepost_region) 
#df_pen_ninv_ns_prepost_comb[nrow(df_pen_ninv_ns_prepost_comb)+21,] <- NA
#View(df_pen_ninv_ns_prepost_comb)

# df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(41:42, 47:48, 33:34, 17:18, 31:32, 51,  3:4, 52:53, 19:20, 23:24, 45:46, 15:16, 54, 5:6, 
#                                                              55:56, 43:44, 49:50, 29:30, 57, 13:14, 58:59, 35:36, 60, 7:8, 61:62, 37:38, 63, 11:12, 
#                                                              64:65, 25:26, 39:40, 67, 9:10, 68:69, 27:28, 21:22, 70, 1:2, 71),]
# View(df_pen_ninv_ns_prepost_comb) #FINAL DATA FRAME 


# df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 55, 3:4, 56:57, 19:20, 23:24, 49:50, 58, 5:6, 59:60, 47:48, 
#                                                              53:54, 25:26, 31:32, 61, 13:14, 62:63, 37:38, 64, 7:8, 65:66, 41:42, 67, 9:10, 68:69, 
#                                                              27:28, 43:44, 39:40, 70, 9:10, 71:72, 29:30, 21:22, 73, 1:2),]

#NEW 
df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                             53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                             27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

#View(df_pen_ninv_ns_prepost_comb) #FINAL DATA FRAME 

save(df_pen_ninv_ns_prepost_comb, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_pen_ninv_ns_prepost_comb.RData")

# 5.11 Clean data file (dat and dat1) before inputting to plots ----

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_pen_inv_ns_prepost_comb.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_pen_ninv_ns_prepost_comb.Rdata")

dat = df_pen_inv_ns_prepost_comb #1. ORIGINAL - Penicillin Invasive Nonsusceptible 
dat2 = df_pen_ninv_ns_prepost_comb #2. New - Penicillin Noninvasive Nonsusceptible 

# Clean data for 0/n isolates

row.names(dat) <- 1:nrow(dat)

dat <- dat %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat)) {
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ee[i] <- NaN, dat$ee[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$total_denom[i] <- 0, dat$total_denom[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$total_cases[i] <- 0, dat$total_cases[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ci_ub_new[i] <- 0, dat$ci_ub_new[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ci_lb_new[i] <- 0, dat$ci_lb_new[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$study_arms[i] <- 0, dat$study_arms[i]) 
}
View(dat)

#Check confidence intervals here:
for (i in 1:nrow(dat)){
  ifelse(dat$total_cases[i] == 0 & dat$total_denom[i] != 0,  
         dat$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat$total_denom[i])+0.5)[3], 
         dat$ci_ub_new[i])
}

#dat[8, "ci_ub_new"] <- qbeta(c(0.5, 0.025, 0.975), 0.5, 14.5)[3] #Old manual CI code 

#Repeat process for dat2
dat2 = df_pen_ninv_ns_prepost_comb #2. New - Penicillin Noninvasive Nonsusceptible 
row.names(dat2) <- 1:nrow(dat2)

dat2 <- dat2 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Reset confidence intervals using qbeta 
#dat2[16, "ci_ub_new"] <- qbeta(c(0.5, 0.025, 0.975), 0.5, 194.5)[3]

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat2)) {
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ee[i] <- NaN, dat2$ee[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$total_denom[i] <- 0, dat2$total_denom[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$total_cases[i] <- 0, dat2$total_cases[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ci_ub_new[i] <- 0, dat2$ci_ub_new[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ci_lb_new[i] <- 0, dat2$ci_lb_new[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$study_arms[i] <- 0, dat2$study_arms[i]) 
}
View(dat2)

#Check confidence intervals here:
for (i in 1:nrow(dat2)){
  ifelse(dat2$total_cases[i] == 0 & dat2$total_denom[i] != 0,  
         dat2$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat2$total_denom[i])+0.5)[3], 
         dat2$ci_ub_new[i])
}

# 5.12 SAVE Dat and Dat2 final data frames ####
save(dat, file ="/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_inv.Rdata")
save(dat2, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_ninv.Rdata")



# 6. RUNNING **MACROLIDE** MODELS FOR FOREST PLOTS + CREATING DATA FILE FOR FOREST PLOT CODE  ----

# 6.1 NEW: Macrolide + Nonsusceptible + Invasive + Pre, boundary singular fit   ----

# Note that this MODEL did not fit DUE TO CARRIBEAN 0/49 isolates from 2 studies AND Western Sub Saharan Africa (0/31)
# Work around was removing carribean from loop and hard coding 0/49.5 w/ qbeta estimate for confidence intervals. 

#Look at what's happening for western sub-Saharan Africa 
# dta_loop_pre <- novax_ns %>%
#   filter(drug == 8) %>% filter(isolate_type ==1) %>% filter(region == gbd_list[20])
# View(dta_loop_pre)

matrix_mac_inv_ns_pre <- matrix(rep(NA), nrow= 18, ncol= 11) #change nrow to 18 to account for custom GBD list

#make custom GBD_list to exclude estimate for the carribean 

gbd_list_custom <- gbd_list[-3]
gbd_list_custom <- gbd_list_custom[-19]

for (i in 1:length(gbd_list_custom)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list_custom[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive
  #View(dta_loop_pre)
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_pre[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre[i, 4] <- gbd_list_custom[i]
    matrix_mac_inv_ns_pre[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre[i, 6] <- "ns"
    matrix_mac_inv_ns_pre[i, 7] <- total_cases
    matrix_mac_inv_ns_pre[i, 8] <- total_denom
    matrix_mac_inv_ns_pre[i, 9] <- study_arms
    matrix_mac_inv_ns_pre[i, 10] <- "pre"
    matrix_mac_inv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_pre[i, 1] <- mean
    matrix_mac_inv_ns_pre[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre[i, 4] <- gbd_list_custom[i]
    matrix_mac_inv_ns_pre[i, 7] <- total_cases
    matrix_mac_inv_ns_pre[i, 8] <- total_denom
    matrix_mac_inv_ns_pre[i, 9] <- study_arms
    matrix_mac_inv_ns_pre[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre[i, 6] <- "ns"
    matrix_mac_inv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_inv_ns_pre<- data.frame(matrix_mac_inv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_pre) <- list_rownames
df_mac_inv_ns_pre[,4] <- gbd_list_custom

#Add in estimate for carribean 
df_mac_inv_ns_pre[nrow(df_mac_inv_ns_pre) +1,] <- NA 
#Fill in values for the row manually 
df_mac_inv_ns_pre[20,1] <- 0
df_mac_inv_ns_pre[20,2] <- 0
df_mac_inv_ns_pre[20,3] <- "macrolide"
df_mac_inv_ns_pre[20,4] <- gbd_list[3]
df_mac_inv_ns_pre[20,5] <- "invasive"
df_mac_inv_ns_pre[20,6] <- "ns"
df_mac_inv_ns_pre[20,7] <- 0 
df_mac_inv_ns_pre[20,8] <- 49
df_mac_inv_ns_pre[20,9] <- 2
df_mac_inv_ns_pre[20,10] <- "pre"
df_mac_inv_ns_pre[20,11] <-  qbeta(c(0.975), 0.5, 49.5)

#Add in estimate for western sub-Saharan Africa 
#Fill in values for the row manually 
df_mac_inv_ns_pre[19,1] <- 0
df_mac_inv_ns_pre[19,2] <- 0
df_mac_inv_ns_pre[19,3] <- "macrolide"
df_mac_inv_ns_pre[19,4] <- gbd_list[20]
df_mac_inv_ns_pre[19,5] <- "invasive"
df_mac_inv_ns_pre[19,6] <- "ns"
df_mac_inv_ns_pre[19,7] <- 0 
df_mac_inv_ns_pre[19,8] <- 31
df_mac_inv_ns_pre[19,9] <- 2
df_mac_inv_ns_pre[19,10] <- "pre"
df_mac_inv_ns_pre[19,11] <-  qbeta(c(0.975), 0.5, 31.5) #(0/31)
#View(df_mac_inv_ns_pre)

#Re-order to push carribean back up 

df_mac_inv_ns_pre <- df_mac_inv_ns_pre[c(1,2,20, 3:19),]
View(df_mac_inv_ns_pre)


# 6.2 Macrolide + Nonsusceptible + Invasive + Post boundary singular fit   ---- 

#Checking that # of isolates reported in table is correct w/ fulldta set 
# check <- fulldta %>%
#   filter(prepost == "not_naive") %>%
#   filter(drug_class == "macrolide") %>%
#   filter(isolate_type == 1) %>%
#   filter(region == "hi_asia_pacific")
# nrow(check)
# View(check)
# check <- check %>%
#   dplyr::select(r_id, total_isolates_drug, no_ns)
# View(check)

matrix_mac_inv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region, gbd_list[10] for hi_asia_pacific 
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  
  # dta_loop_post2 <- dta_loop_post %>% 
  #   filter(r_id == 418) %>% 
  #   summarise(count = sum(ns))
  # dta_loop_post2
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    #if((length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post) #see if this runs with r_id instead of id 
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_post[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_post[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_inv_ns_post[i, 5] <- "invasive"
    matrix_mac_inv_ns_post[i, 6] <- "ns"
    matrix_mac_inv_ns_post[i, 7] <- total_cases
    matrix_mac_inv_ns_post[i, 8] <- total_denom
    matrix_mac_inv_ns_post[i, 9] <- study_arms
    matrix_mac_inv_ns_post[i, 10] <- "post"
    matrix_mac_inv_ns_post[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_post[i, 1] <- mean
    matrix_mac_inv_ns_post[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_inv_ns_post[i, 7] <- total_cases
    matrix_mac_inv_ns_post[i, 8] <- total_denom
    matrix_mac_inv_ns_post[i, 9] <- study_arms
    matrix_mac_inv_ns_post[i, 5] <- "invasive"
    matrix_mac_inv_ns_post[i, 6] <- "ns"
    matrix_mac_inv_ns_post[i, 10] <- "post"
    
  }
}  


#Create cleaned frame 
df_mac_inv_ns_post<- data.frame(matrix_mac_inv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_post) <- list_rownames
df_mac_inv_ns_post[,4] <- gbd_list


# 6.3 Macrolide + Nonsusceptible + Noninvasive + Pre, boundary singular fit  ---- 
matrix_mac_ninv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

#loop 
for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_pre[i, 2] <- output_exp[2]
    matrix_mac_ninv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre[i, 10] <- "pre"
    matrix_mac_ninv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_pre[i, 1] <- mean
    matrix_mac_ninv_ns_pre[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_pre[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_ninv_ns_pre<- data.frame(matrix_mac_ninv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_pre) <- list_rownames
df_mac_ninv_ns_pre[,4] <- gbd_list

# 6.4 Macrolide + Nonsusceptible + Noninvasive + Post, boundary singular fit   ----

matrix_mac_ninv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

#loop
for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_post[i, 2] <- output_exp[2] 
    matrix_mac_ninv_ns_post[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post[i, 6] <- "ns"
    matrix_mac_ninv_ns_post[i, 7] <- total_cases
    matrix_mac_ninv_ns_post[i, 8] <- total_denom
    matrix_mac_ninv_ns_post[i, 9] <- study_arms
    matrix_mac_ninv_ns_post[i, 10] <- "post"
    matrix_mac_ninv_ns_post[i, 11] <- output_exp[3] 
    
  }
  
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_post[i, 1] <- mean
    matrix_mac_ninv_ns_post[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_post[i, 7] <- total_cases
    matrix_mac_ninv_ns_post[i, 8] <- total_denom
    matrix_mac_ninv_ns_post[i, 9] <- study_arms
    matrix_mac_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post[i, 6] <- "ns"
    matrix_mac_ninv_ns_post[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_ninv_ns_post<- data.frame(matrix_mac_ninv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_post) <- list_rownames
df_mac_ninv_ns_post[,4] <- gbd_list

# 6.6 Macrolide + Nonsusceptible + Invasive + Pre SUPER REGION ----

matrix_mac_inv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11) #change to super region + 7 rows 

for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_pre) #has out when not using 
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_pre_sregion[i, 10] <- "pre"
    matrix_mac_inv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_pre_sregion[i, 1] <- mean
    matrix_mac_inv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre_sregion[i, 6] <- "ns" #temporarily change this to sregion
    matrix_mac_inv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_inv_ns_pre_sregion<- data.frame(matrix_mac_inv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_pre_sregion) <- list_rownames
df_mac_inv_ns_pre_sregion[,4] <- sregion_list
#View(df_mac_inv_ns_pre_sregion)

# 6.7 Macrolide + Nonsusceptible + Invasive + Post- SUPER REGION ---- 

matrix_mac_inv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    #if((length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$r_id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_post_sregion[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_post_sregion[i, 10] <- "post"
    matrix_mac_inv_ns_post_sregion[i, 11] <- output_exp[3]
    
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_post_sregion[i, 1] <- mean
    matrix_mac_inv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_post_sregion[i, 6] <- "ns" 
    matrix_mac_inv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_inv_ns_post_sregion<- data.frame(matrix_mac_inv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_post_sregion) <- list_rownames
df_mac_inv_ns_post_sregion[,4] <- sregion_list
#View(df_mac_inv_ns_post_sregion)


# 6.8 Macrolide + Nonsusceptible + Noninvasive + Pre + SUPER REGION ----

matrix_mac_ninv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop  
for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% 
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_mac_ninv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre_sregion[i, 10] <- "pre"
    matrix_mac_ninv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_pre_sregion[i, 1] <- mean
    matrix_mac_ninv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_ninv_ns_pre_sregion<- data.frame(matrix_mac_ninv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_pre_sregion) <- list_rownames
df_mac_ninv_ns_pre_sregion[,4] <- sregion_list
#View(df_mac_ninv_ns_pre_sregion)

# 6.9 Macrolide + Nonsusceptible + Noninvasive + Post + SUPER REGION ----
matrix_mac_ninv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_post_sregion[i, 2] <- output_exp[2] 
    matrix_mac_ninv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_post_sregion[i, 10] <- "post"
    matrix_mac_ninv_ns_post_sregion[i, 11] <- output_exp[3] 
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_post_sregion[i, 1] <- mean
    matrix_mac_ninv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_ninv_ns_post_sregion<- data.frame(matrix_mac_ninv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_post_sregion) <- list_rownames
df_mac_ninv_ns_post_sregion[,4] <- sregion_list
#View(df_mac_ninv_ns_post_sregion)


# 6.10 Bind together pre-post region & super region eestimates for Macrolide, nonsusceptible to create final data frames ----

#1. Macrolide + Nonsusceptible + Invasive + Pre/ Post - Super Region 

df_mac_inv_ns_prepost_sregion <- rbind(df_mac_inv_ns_pre_sregion, df_mac_inv_ns_post_sregion)
#View(df_mac_inv_ns_prepost_sregion)

df_mac_inv_ns_prepost_sregion <- df_mac_inv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_mac_inv_ns_prepost_sregion <- df_mac_inv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_mac_inv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_mac_inv_ns_prepost_region <- rbind(df_mac_inv_ns_pre, df_mac_inv_ns_post)
df_mac_inv_ns_prepost_region <- df_mac_inv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_inv_ns_prepost_region)

df_mac_inv_ns_prepost_comb <- rbind(df_mac_inv_ns_prepost_sregion, df_mac_inv_ns_prepost_region) 
#df_mac_inv_ns_prepost_comb[nrow(df_mac_inv_ns_prepost_comb)+21,] <- NA
#View(df_mac_inv_ns_prepost_comb)

#reorder 
df_mac_inv_ns_prepost_comb <- df_mac_inv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                           53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                           27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

View(df_mac_inv_ns_prepost_comb)
save(df_mac_inv_ns_prepost_comb, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_mac_inv_ns_prepost_comb.RData")

#2. Macrolide + Nonsusceptible + Noninvasive + Pre/ Post - Super Region 
df_mac_ninv_ns_prepost_sregion <- rbind(df_mac_ninv_ns_pre_sregion, df_mac_ninv_ns_post_sregion)
#View(df_pen_ninv_ns_prepost_sregion)

df_mac_ninv_ns_prepost_sregion <- df_mac_ninv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_mac_ninv_ns_prepost_sregion <- df_mac_ninv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_mac_ninv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_mac_ninv_ns_prepost_region <- rbind(df_mac_ninv_ns_pre, df_mac_ninv_ns_post)
df_mac_ninv_ns_prepost_region <- df_mac_ninv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_ninv_ns_prepost_region)

df_mac_ninv_ns_prepost_comb <- rbind(df_mac_ninv_ns_prepost_sregion, df_mac_ninv_ns_prepost_region) 
#df_mac_ninv_ns_prepost_comb[nrow(df_mac_ninv_ns_prepost_comb)+21,] <- NA
#View(df_mac_ninv_ns_prepost_comb)

df_mac_ninv_ns_prepost_comb <- df_mac_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                             53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                             27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

#View(df_mac_ninv_ns_prepost_comb)

save(df_mac_ninv_ns_prepost_comb, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_mac_ninv_ns_prepost_comb.RData")


# 6.11 Clean Macrolide Data file before uploading to plots ----

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_mac_inv_ns_prepost_comb.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/df_mac_ninv_ns_prepost_comb.Rdata")

dat3 = df_mac_inv_ns_prepost_comb #3. New - Macrolide Invasive Nonsusceptible- have not created
row.names(dat3) <- 1:nrow(dat3)

dat3 <- dat3 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 


#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat3)) {
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ee[i] <- NaN, dat3$ee[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$total_denom[i] <- 0, dat3$total_denom[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$total_cases[i] <- 0, dat3$total_cases[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ci_ub_new[i] <- 0, dat3$ci_ub_new[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ci_lb_new[i] <- 0, dat3$ci_lb_new[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$study_arms[i] <- 0, dat3$study_arms[i]) 
}
#View(dat3)

#Check confidence intervals here:
for (i in 1:nrow(dat3)){
  ifelse(dat3$total_cases[i] == 0 & dat3$total_denom[i] != 0,  
         dat3$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat3$total_denom[i])+0.5)[3], 
         dat3$ci_ub_new[i])
}

#Repeat for dat5
dat4 = df_mac_ninv_ns_prepost_comb #4. New - Macrolide Noninvasive Nonsusceptible - have note created
row.names(dat4) <- 1:nrow(dat4)

dat4 <- dat4 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat4)) {
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ee[i] <- NaN, dat4$ee[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$total_denom[i] <- 0, dat4$total_denom[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$total_cases[i] <- 0, dat4$total_cases[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ci_ub_new[i] <- 0, dat4$ci_ub_new[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ci_lb_new[i] <- 0, dat4$ci_lb_new[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$study_arms[i] <- 0, dat4$study_arms[i]) 
}
#View(dat4)

#Check confidence intervals here:
for (i in 1:nrow(dat4)){
  ifelse(dat4$total_cases[i] == 0 & dat4$total_denom[i] != 0,  
         dat4$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat4$total_denom[i])+0.5)[3], 
         dat4$ci_ub_new[i])
}

save(dat3, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_inv.Rdata") #use these files in the forest plot code 
save(dat4, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_ninv.Rdata") #use these files in the forest plot code 


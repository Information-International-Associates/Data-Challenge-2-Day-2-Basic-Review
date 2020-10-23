library(lpSolveAPI)

fuel_quarter <- matrix(c(c(1,0,0,0,0,1), c(1,1,0,0,0,0), c(0,0,1,1,0,0), c(0,0,0,1,1,0)), nrow=6, ncol=4)
corn_quarter <- matrix(c(c(1,0,0,0,1,1), c(1,1,0,0,0,1), c(0,1,1,1,0,0), c(0,0,1,1,1,0)), nrow=6, ncol=4)
buy_ind <- cbind(fuel_quarter, corn_quarter)
demand_ind <- rbind(fuel_quarter, corn_quarter)
qtr_demand <- c(1200, 1100, 1300, 1000)

fuel_price <- c(2,2.5,2,1,1.5,3)
corn_price <- c(1.5,1,2,1,2,2.5)
cost <- c(fuel_price, corn_price)
obj_var <- list()
loc <-0
ctype <- 0
for (j in 1:12) {
  for (i in 1:4) {
    if (demand_ind[j,i]== 1){
      if (j <= 6) {
        ctype <- 1
        p <- j
      } else {
        ctype <- 2
        p <- j-6
      }
      loc = loc+1
      obj_var<- c(obj_var, c(loc, ctype,i,p, unlist(cost[j])))
    }
  }
}

obj_var <- unlist(obj_var)
in_mat <- matrix(obj_var, ncol=5, byrow=TRUE)

deflp <- function(numcons, decvar){
  lpm <- make.lp(numcons, decvar)
  lp.control(lpm, sense="min")
  return(lpm)
}


add_demand_cnstrnt <- function(lpm, in_mat, fuel_pct, q_demand){
  for (i in 1:4){
    ft_idx <- list()
    ft_coef <- list()
    ct_idx <- list()
    ct_coef <- list()
    for (j in 1:NROW(in_mat)) {
      rec = in_mat[j,]
      if (rec[3]==i){
        if (rec[2]==1){
          ft_idx <- c(ft_idx, rec[1])
          ft_coef <-c(ft_coef, 1)
        }
        if (rec[2]==2) {
          ct_idx <- c(ct_idx, rec[1])
          ct_coef <- c(ct_coef, 1)
        }
      }
    }
    ft_idx <- unlist(ft_idx)
    ft_coef <- unlist(ft_coef)
    ct_idx <- unlist(ct_idx)
    ct_coef <- unlist(ct_coef)
    #---
    rhs1 <- (fuel_pct/100)*q_demand[i]
    rhs2 <- (1-(fuel_pct/100))*q_demand[i]
    #---
    add.constraint(lpm, ft_coef, ">=", rhs1, ft_idx)
    #print(c(ft_coef,'||', ft_idx, '||',rhs1))
    add.constraint(lpm, ct_coef, ">=", rhs2, ct_idx)
  }
  return(lpm)
}


## Purchse Constraints
add_purchase_constraints <- function(lpm, p_Mix, in_mat){
for (i in 1:6){
  p_idx <- list()
  p_coef <- list()
  for (j in 1:NROW(in_mat)){
    rec = in_mat[j,]
    if (rec[4]==i){
      if  (rec[2]==1){
        p_coef <- c(p_coef,1)
        p_idx <- c(p_idx,rec[1])
      } else {
        p_coef <- c(p_coef,-1*p_Mix)
        p_idx <- c(p_idx,rec[1])
      }
    }
  }
  rhs=0
  p_coef<- unlist(p_coef)
  p_idx <- unlist(p_idx)
  #print(c(p_idx,p_coef,rhs))
  add.constraint(lpm, p_coef, "<=", rhs, p_idx)
}
  return(lpm)
}

solv_lp<- function(cnstr_n,decvar, in_mat, fuel_pct, q_demand, p_Mix){
  lpm <- deflp(cnstr_n, decvar)
  lpm <- add_demand_cnstrnt(lpm, in_mat, fuel_pct, q_demand)
  lpm <- add_purchase_constraints(lpm, p_Mix, in_mat)
  #set.bounds(lpm, lower = rep(0, NROW(in_mat)), columns = 1:NROW(in_mat))
  set.objfn(lpm, in_mat[,5])
  solve(lpm)
  optm_obj<- get.objective(lpm)
  out_values <-get.variables(lpm)
  out_mat <- cbind(in_mat, out_values)
  colnames(out_mat)<- c('var_num', 'Feed_Stock', 'Demand_Period', 'Purchase_Period', 'Cost $', 'Tonnes')
  out_mat.df <- as.data.frame(out_mat)
  out_mat.df$Feed_Stock <- ifelse(out_mat.df$Feed_Stock==1, 'Fuel', 'Corn')
  out_mat.df$Demand_Period <- paste('Q', as.character(out_mat.df$Demand_Period))
  out_mat.df$Purchase_Period <- paste('P', as.character(out_mat.df$Purchase_Period))
  df <- out_mat.df %>% select(2,3,4,6)
  #by_dmd_period <- out_mat.df %>% group_by(Demand_Period, Feed_Stock) %>% summarise(sum(Tonnes))
  #colnames(by_dmd_period)[3] <- 'Tonnes'
  #by_prchs_period <- out_mat.df %>% group_by(Purchase_Period, Feed_Stock) %>% summarise(sum(Tonnes))
  #colnames(by_prchs_period)[3] <- 'Tonnes'
  return(list(optm_obj, df))
}

results<- solv_lp(14,20, in_mat, .35, qtr_demand, 3)

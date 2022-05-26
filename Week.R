rm(list=ls())
library(deSolve); library(dplyr); library(ggplot2); library(openxlsx); library(tidyr); library(writexl); library(TTR)


### Deterministic SIR Model
sir = function(time, y, params) {
  S=y[1]
  I=y[2]
  R=y[3] 
  
  beta = params[["beta"]]
  gamma = params[["gamma"]]
  dS = -beta * S * I
  dI = beta * S * I - gamma * I
  dR = gamma * I
  
  
  solution = c(dS, dI, dR)
  return(list(solution))
}


### Initial condition and parameters
 # beta 전파계수
beta1 = 0.0000569

  # gamma_list 바이러스 배출기간 관찰값에서 무작위로 1000번 골라내기 
ve <- c(23, 19, 20, 17, 22, 20, 11)
gamma_list_dist <- sample(ve, 1000, replace = TRUE)
gamma_list_dist 
gamma_list <- 1/gamma_list_dist  # 바이러스 배출기간의 역수로 gamma 계산
gamma_list

out_list <- NULL
df_out_list <- NULL




## 확률분포에서 random으로 값 추출하라고 gamma_list에서 지정한 횟수(1000번)만큼 반복하여 미분방정식 실행
for(i in 1:length(gamma_list)){
  ## (S(0), I(0), R(0)) =(N-1, 1, 0)  N은 닭 농장수 + 오리 농장수
  # week마다 S0, I0, R0 수정 필요
  S0 = 3220; I0 = 1; R0 = 0 
  N = S0 + I0 + R0
  init = c(S = S0, I = I0, R = R0)
  times = seq(1, 16, by = 1) # 16일 동안 1일 간격으로 SIR 실행
  para = c(beta = beta1, gamma = gamma_list[i]) 
  
  ### Solve the differential equation
  out = ode(y = init, times = times, func = sir, parms = para)
  out = as.data.frame(out)
  out$N = out$S + out$I + out$R # N(t) = S(t) + I(t) + R(t)
  out$New = beta1 * out$S * out$I  
  out_list[[i]] <- out
  df_out1 <- out %>%mutate(cum_new = cumsum(out$New))  # 누적 발생(감염)농장수 산출
  df_out2 <- subset(df_out1, select=-New)
  df_out_list[[i]] <- df_out2
}


## 반복 실행한 결과에서 '누적 발생농장수' 통계 뽑아내기
df_out <- do.call("rbind", df_out_list)
save_result <- NULL
for(i in 1:nrow(df_out_list[[1]])){
  temp <- subset(df_out, time == i)
  p50 <- quantile(temp$cum_new, 0.5)
  
  data <- data.frame(time = i, p50)
  save_result <- rbind(save_result, data)
}
row.names(save_result) <- 1:nrow(save_result)
 
 
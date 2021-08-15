# movie <- read.xlsx("C:/hyunnee/pythonwork/movie/중간저장8.xlsx", sheetName = "중간저장8", header = T)
str(movie)

movie1 <- movie[c(4, 6, 7, 8, 14, 15, 16, 17)]
str(movie1)

# 다중회귀분석 : 모든 변수 유의하고 모형 또한 유의
fit <- lm(누적관객수 ~ ., data = movie1)
summary(fit)

# 정규성 검정 : 샘플의 크기가 커서 정규성 만족한다고 할 수 있음
shapiro.test(resid(fit))

# 등분산성 검정 : 등분산성 만족
ncvTest(fit)

# 설명변수들 간의 상관관계 분석 : 스크린수-상영횟수, 배우합-배우평균 높음
movie2 <- movie1[-2]
cor(movie2)

# 상관관계 분석 : 상영횟수와 누적매출액의 상관관계가 가장 큼
cor(movie1)

# 다중공선성 : 배우합과 배우평균의 vif값이 20 이상으로 매우 높음
vif(fit)

# 배우합의 p-value값이 배우평균보다 높았기때문에 배우평균 변수 삭제
movie3 <- movie1[-8]

# 다중회귀분석
fit1 <- lm(누적관객수 ~ ., data = movie3)
summary(fit1)

# 다중공선성 : 모든 변수들의 vif값이 10 이하이므로 다중공선성 없음
vif(fit1)

# 변수의 설명력 : 상영횟수 변수가 가장 설명력이 높음
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square =", round(rsquare, digits=3)),
           ...)
  return(import)
}  
relweights(fit1,col="black")

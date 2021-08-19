library(xlsx);
df <- read.xlsx(file = '중간저장20.xlsx', sheetName = "중간저장20")

View(df)
df <- read.csv(file = '중간저장20.csv')

df1 <- df[c("누적관객수", "스크린수", "상영횟수", "감독값", "배급사값", "배우합", "별점", "별점평가자수", "리뷰평가자수")]
str(df1)

# 전진선택법
min.model <- lm(누적관객수 ~ 1, data=df1)
fwd.model <- step(min.model, direction = "forward", 
                  scope = (누적관객수 ~ 스크린수 + 상영횟수 + 감독값 + 배급사값 + 배우합 + 별점 + 별점평가자수 + 리뷰평가자수))
forward <- lm(formula = 누적관객수 ~ ., data = df1)
summary(forward)

# 후진제거법
full.model <- lm(누적관객수 ~ ., data = df1)
reduced.model <- step(full.model, direction = "backward")

# 단계적 선택법
stepwise <- step(forward, direction = "both")
# ==========================================================================================

df2 <- df[c("누적관객수", "스크린수", "상영횟수", "감독값", "배급사값", "배우합", "별점", "별점평가자수", "리뷰평가자수", "대표국적", "등급", "장르")]

# 전진선택법
min.model <- lm(누적관객수 ~ 1, data=df2)
fwd.model <- step(min.model, direction = "forward", 
                  scope = (누적관객수 ~ 스크린수 + 상영횟수 + 감독값 + 배급사값 + 배우합 + 별점 + 별점평가자수 + 리뷰평가자수 + 대표국적 + 등급 + 장르))
forward <- lm(formula = 누적관객수 ~ ., data = df2)
summary(forward)

# 후진제거법
full.model <- lm(누적관객수 ~ ., data = df2)
reduced.model <- step(full.model, direction = "backward")

# 단계적 선택법
stepwise <- step(forward, direction = "both")

# 회귀분석
fit <- lm(formula = 누적관객수 ~ 스크린수 + 상영횟수 + 감독값 + 배급사값 + 배우합 + 별점 + 별점평가자수 + 리뷰평가자수 + 등급 + 장르, data = df2)
summary(fit)

# 그룹별 통계량
lapply(df1, quantile)
lapply(df1, mean)

# 상관계수
cor(df1)

# 다중공선성
library(car)
vif(forward)

# QQplot -> 잔차의 정규성
qqnorm(resid(forward))
qqline(resid(forward), col = "blue")

# 이상점 확인 : 2842, 11098, 17710 삭제
plot(forward, 4)

df1 <- df1[-2842, ]
df1 <- df1[-11098, ]
df1 <- df1[-17710, ]

# 회귀분석
reg <- lm(formula = 누적관객수 ~ ., data = df1)
summary(reg)

# 변수 설명력
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
relweights(reg,col="black")

# 정규화
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

df1$스크린수 <- normalize(df1$스크린수)
df1$상영횟수 <- normalize(df1$상영횟수)
df1$감독값 <- normalize(df1$감독값)
df1$배급사값 <- normalize(df1$배급사값)
df1$배우합 <- normalize(df1$배우합)
df1$리뷰평가자수 <- normalize(df1$리뷰평가자수)
df1$별점평가자수 <- normalize(df1$별점평가자수)
df1$별점 <- normalize(df1$별점)

# 회귀분석
reg2 <- lm(formula = 누적관객수 ~ ., data = df1)
summary(reg2)
relweights(reg2,col="black")

# 표준화
df1 <- transform(df1, 스크린수 = scale(스크린수))
df1 <- transform(df1, 상영횟수 = scale(상영횟수))
df1 <- transform(df1, 감독값 = scale(감독값))
df1 <- transform(df1, 배급사값 = scale(배급사값))
df1 <- transform(df1, 배우합 = scale(배우합))
df1 <- transform(df1, 리뷰평가자수 = scale(리뷰평가자수))
df1 <- transform(df1, 별점평가자수 = scale(별점평가자수))
df1 <- transform(df1, 별점 = scale(별점))

df1$스크린수 <- df1$스크린수[1:21325]
df1$상영횟수 <- df1$상영횟수[1:21325]
df1$감독값 <- df1$감독값[1:21325]
df1$배급사값 <- df1$배급사값[1:21325]
df1$배우합 <- df1$배우합[1:21325]
df1$리뷰평가자수 <- df1$리뷰평가자수[1:21325]
df1$별점평가자수 <- df1$별점평가자수[1:21325]
df1$별점 <- df1$별점[1:21325]

# 회귀분석
reg3 <- lm(formula = 누적관객수 ~ ., data = df1)
summary(reg3)
relweights(reg3,col="black")

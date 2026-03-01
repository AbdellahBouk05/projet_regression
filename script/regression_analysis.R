

# Données des ancienne arbre
X <- c(-1.61, -1.20, -0.97, -0.51, -0.42)
Y <- c(2.22, 2.27, 2.38, 2.60, 2.65)

data <- data.frame(X, Y)


# Modèle
model <- lm(Y ~ X, data=data)

# resultat de model
summary(model)

# verification de l'existance d'une relation entre X,Y
png("report/regression_plot.png")
plot(X, Y,
     main="Log-Log Regression: Tree Height vs Diameter by ABDEELAH BOUK",
     xlab="ln(D)",
     ylab="ln(H)",
     pch=19)

abline(model, col="red", lwd=2)
dev.off()

# R² qualiter d'ajustement
r_squared <- summary(model)$r.squared
print(paste("R² =", r_squared))

# Prédiction de hauteur pour D = 0.7
D <- 0.7
X_new <- log(D)
Y_pred <- predict(model, newdata=data.frame(X=X_new))
H_pred <- exp(Y_pred)

print(paste("Predicted Height for D=0.7:", H_pred))


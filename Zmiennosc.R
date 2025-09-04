library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest) 

# dane dzienne walutowe
europln_d <- read.csv("C:/Users/mikol/OneDrive/Desktop/Pomiary_Ryzyka/waluty/europln/eurpln_dziennie.csv")

str(europln_d)

europln_d$Data <- as.Date(europln_d$Data, format = "%Y-%m-%d")

# dane od 2012 do 2023
europln_d_filtered <- subset(europln_d, Data >= as.Date("2012-01-01") & Data <= as.Date("2023-12-31"))

View(europln_d_filtered)
# Zad1 --------------------------------------------------------------------

##POLECENIE
#Dla wybranego przez Ciebie zestawu danych, na podstawie pierwszego, podanego na wykładzie wzoru, 
#wyznaczyć zmienność stóp zwrotu (przyjąć m = 10, 25, 50 i 100). Wyniki przedstawić na wykresie. 
#Jak wykres zależy od wyboru wartości m?

# stopy logarytmiczna i stopa prosta
europln_d_filtered <- europln_d_filtered |> 
  mutate(
    Stopa_Prosta = (Zamkniecie - dplyr::lag(Zamkniecie)) / dplyr::lag(Zamkniecie),
    Stopa_Logarytmiczna = log(Zamkniecie / dplyr::lag(Zamkniecie))
  )

# wzór1 
oblicz_zmiennosc <- function(stopy, m) {
  n <- length(stopy)
  zmiennosc <- numeric(n)
  for (i in seq(m, n)) {
    zmiennosc[i] <- sqrt(sum(stopy[(i - m + 1):i]^2) / m)
  }
  return(zmiennosc)
}


m_values <- c(10, 25, 50, 100)

# do 1
for (m in m_values) {
  europln_d_filtered <- europln_d_filtered |> 
    mutate(!!paste0("Zmiennosc_Logarytmiczna_m", m) := oblicz_zmiennosc(Stopa_Logarytmiczna, m))
}


zmiennosc_dlugie <- europln_d_filtered |> 
  select(Data, starts_with("Zmiennosc_Logarytmiczna_")) |> 
  pivot_longer(
    cols = starts_with("Zmiennosc_Logarytmiczna_"),
    names_to = "Okres",
    names_prefix = "Zmiennosc_Logarytmiczna_m",
    values_to = "Zmiennosc"
  )

ggplot(zmiennosc_dlugie, aes(x = Data, y = Zmiennosc, color = Okres)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  
  labs(
    title = "Zmienność logarytmicznej stopy zwrotu dla różnych wartości m",
    y = "Zmienność", x = "Rok", color = "m"
  ) +
  theme_minimal()

# jak widać im mniejsz m, krótkoterminowa zmiennośc jest bardziej szpiczasta
# widać więcej wznosów i górowań, co oznacza że wsk. jest bardziej podatny na w rynkowe
# wykres bardziej reaguje na pojedyncze duże zmiany cen
# Natomiast czym większe m, zmienność staje się bardziej wygładzona, mniej gwałtownych skoków


# Zad2 --------------------------------------------------------------------

#wzór 2
oblicz_zmiennosc2 <- function(stopy, m) {
  n <- length(stopy)
  zmiennosc <- numeric(n)
  
  for (i in seq(m, n)) {
    ostatnie_m <- stopy[(i - m + 1):i]
    srednia_m <- mean(ostatnie_m)
    zmiennosc[i] <- sqrt(sum((ostatnie_m - srednia_m)^2) / (m - 1))
  }
  
  return(zmiennosc)
}

m2 <- 25

# Obliczenie zmienności dla obu wzorów
europln_d_filtered <- europln_d_filtered |> 
  mutate(
    Zmiennosc_Wzor1_m25 = oblicz_zmiennosc(Stopa_Logarytmiczna, m2),
    Zmiennosc_Wzor2_m25 = oblicz_zmiennosc2(Stopa_Logarytmiczna, m2)
  )


zmiennosc_porownanie <- europln_d_filtered %>%
  select(Data, Zmiennosc_Wzor1_m25, Zmiennosc_Wzor2_m25) %>%
  pivot_longer(
    cols = starts_with("Zmiennosc"),
    names_to = "Wzor",
    names_prefix = "Zmiennosc_",
    values_to = "Zmiennosc"
  )

ggplot(zmiennosc_porownanie, aes(x = Data, y = Zmiennosc, color = Wzor)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Porównanie zmienności stóp zwrotu (m = 25) dla dwóch wzorów",
    x = "Rok",
    y = "Zmienność",
    color = "Wzór"
  ) +
  theme_minimal()

# Podobieństwo między liniami jest bardzo duże kiedy nie występują duże skoki danych
# Różnice występują w momentach dużych skoków zmienności , wzór 1 jest bardziej podatny na skrajne wartości
# uproszczenie procedury liczenia zmienności, nie powodują dużych zakłóceń
# w wyznaczanej wartości zmienności. 

# Zad3 --------------------------------------------------------------------
europln_d_filtered <- europln_d_filtered %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
sum(is.na(europln_d_filtered))
####EWMA

oblicz_ewma <- function(stopy, lambda) {
  n <- length(stopy)
  ewma <- numeric(n)
  ewma[1] <- stopy[1]^2 # Pierwsza wartość inicjalizacyjna
  
  for (t in 2:n) {
    ewma[t] <- lambda * ewma[t-1] + (1 - lambda) * stopy[t-1]^2
  }
  return(sqrt(ewma)) # odchylenie standardowe
}

lambda <- 0.94
ewma_zmiennosc <- oblicz_ewma(europln_d_filtered$Stopa_Logarytmiczna, lambda)

europln_d_filtered$EWMA <- ewma_zmiennosc #dodanie do tabeli dannych

ggplot(europln_d_filtered, aes(x = Data)) +
  geom_line(aes(y = EWMA, color = "EWMA")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monitorowanie zmienności - model EWMA", y = "Zmienność", x = "Data") +
  theme_minimal()

# Porownanie EWMA i wzóru z zad 2
ggplot(europln_d_filtered, aes(x = Data)) +
  geom_line(aes(y = EWMA, color = "EWMA")) +
  geom_line(aes(y = Zmiennosc_Wzor2_m25, color = "Wzór2 m=25")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Porównanie zmienności EWMA i Wzoru z Zadania 2", x = "Rok", y = "Zmienność", color = "Metoda") +
  theme_minimal()

# jak widać model EWMA jest niemal identyczny 
#do tego z zad2, co prawda nie pokrywa się 1:1 z wykrezem z zad2
# 

# ugarchspec

library(rugarch)

# Sprawdzenie długości danych (rugarch wymaga wektora danych)
stopy_log <- europln_d_filtered$Stopa_Logarytmiczna

# Specyfikacja modelu GARCH(1,1)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

# Dopasowanie modelu do danych
fit <- ugarchfit(spec = spec, data = stopy_log)

# Wyciągnięcie wyników
print(fit)  # Podsumowanie wyników

# Warunkowa wariancja i zmienność
sigma_t <- sigma(fit)  # Zmienność (odchylenie standardowe)
sigma_t <- as.vector(sigma_t)  # Konwersja na wektor

# Dodanie wyników do tabeli
europln_d_filtered <- europln_d_filtered[seq_along(sigma_t), ]  # Dopasowanie długości
europln_d_filtered$GARCH_rugarch <- sigma_t

ggplot(europln_d_filtered, aes(x = Data)) +
  geom_line(aes(y = GARCH_rugarch, color = "GARCH")) +
  labs(title = "Zmienność wg modelu GARCH(1,1)", y = "Zmienność", x = "Data") +
  scale_color_manual(name = "Metoda", values = c("GARCH" = "red")) +
  theme_minimal()




# model Garch jest bardziej wrażliwy na wartosci ekstremalne, i jednoczesnie abrdziej prezycyjny
# lepiej wychwytuje skoki zmiennosci
# silniej reguje na duże zmiennosci, przezco charakteryzuje sie licznymi skokami
# znacznie odbiega od tego z zad2

# Zad4 --------------------------------------------------------------------

europln_d1 <- read.csv("C:/Users/mikol/OneDrive/Desktop/Pomiary_Ryzyka/waluty/europln/eurpln_dziennie.csv")
eurogbp_d1 <- read.csv("C:/Users/mikol/OneDrive/Desktop/Pomiary_Ryzyka/waluty/eurogbp/eurgbp_d.csv")
eurousd_d1 <- read.csv("C:/Users/mikol/OneDrive/Desktop/Pomiary_Ryzyka/waluty/eurousd/eurusd_d.csv")

europln_d1$Data <- as.Date(europln_d1$Data, format = "%Y-%m-%d")
eurogbp_d1$Data <- as.Date(eurogbp_d1$Data, format = "%Y-%m-%d")
eurousd_d1$Data <- as.Date(eurousd_d1$Data, format = "%Y-%m-%d")

europln_d_filtered1 <- subset(europln_d1, Data >= as.Date("2021-01-01") & Data <= as.Date("2021-10-31"))
eurogbp_d_filtered1 <- subset(eurogbp_d1, Data >= as.Date("2021-01-01") & Data <= as.Date("2021-10-31"))
eurousd_d_filtered1 <- subset(eurousd_d1, Data >= as.Date("2021-01-01") & Data <= as.Date("2021-10-31"))

# logarytmicznych stóp zwrotu dla EUR/PLN
europln_d_filtered1 <- europln_d_filtered1 |> 
  mutate(Stopa_Logarytmiczna = log(Zamkniecie / dplyr::lag(Zamkniecie)))

# logarytmicznych stóp zwrotu dla EUR/GBP
eurogbp_d_filtered1 <- eurogbp_d_filtered1 |> 
  mutate(Stopa_Logarytmiczna = log(Zamkniecie / dplyr::lag(Zamkniecie)))

# logarytmicznych stóp zwrotu dla EUR/USD
eurousd_d_filtered1 <- eurousd_d_filtered1 |> 
  mutate(Stopa_Logarytmiczna = log(Zamkniecie / dplyr::lag(Zamkniecie)))

df <- data.frame(
  Data = europln_d_filtered1$Data,
  EUR_PLN = europln_d_filtered1$Stopa_Logarytmiczna,
  EUR_GBP = eurogbp_d_filtered1$Stopa_Logarytmiczna,
  EUR_USD = eurousd_d_filtered1$Stopa_Logarytmiczna
)

korelacja <- cor(df[,-1], use = "complete.obs")
korelacja

# korelacja między kursami jest stosunkowo słaba.
# stopy zwrotu dla tych kursów nie wykazują silnych związków i mogą być bardziej niezależne od siebie.


# Zad5 --------------------------------------------------------------------

# Test Durbina-Watsona (pierwszy rząd)
dwtest(Stopa_Logarytmiczna ~ 1, data = europln_d_filtered)

# nie odrzucamy H0
# Brak autokorelacji rzędu pierwszego

#sprawdzić dla danych z pandemii

# Test Ljunga-Boxa dla wyższych rzędóww
Box.test(europln_d_filtered$Stopa_Logarytmiczna, lag =2 , type = "Ljung-Box")
Box.test(europln_d_filtered$Stopa_Logarytmiczna, lag =3 , type = "Ljung-Box")
Box.test(europln_d_filtered$Stopa_Logarytmiczna, lag =4 , type = "Ljung-Box")
Box.test(europln_d_filtered$Stopa_Logarytmiczna, lag =5 , type = "Ljung-Box")

# istotna autokorelacja


# Niska/negatywna korelacja zmniejsza ryzyko poprzez dywersyfikację. 
# Wysoka pozytywna korelacja zwiększa ryzyko.
# Silna korelacja może zwiększać ryzyko skrajnych zmian wartości



# Zad6 --------------------------------------------------------------------

lambda_values <- c(0.85, 0.90, 0.95, 0.99)
for (lambda in lambda_values) {
  ewma_column <- oblicz_ewma(europln_d_filtered$Stopa_Logarytmiczna, lambda)
  column_name <- paste0("EWMA_lambda_", lambda)
  europln_d_filtered[[column_name]] <- ewma_column
}

ewma_porownanie <- europln_d_filtered %>%
  select(Data, starts_with("EWMA_lambda_")) %>%
  pivot_longer(
    cols = starts_with("EWMA_lambda_"),
    names_to = "Lambda",
    values_to = "Zmiennosc"
  )

ggplot(ewma_porownanie, aes(x = Data, y = Zmiennosc, color = Lambda)) +
  geom_line() +
  labs(
    title = "Krocząca zmienność - Model EWMA dla różnych wartości lambda",
    x = "Data",
    y = "Zmienność"
  ) +
  theme_minimal()

# Mniejsze wartości lambda bardziej reagują na zmiany,są mniej stabilne
# czuły na krótko terminowe ruchy cenowe
# Większe wartości lambda mają bardziej wygładzoną zmienność, mniej 
#dynamicznie reagują na skrajne ruchy cenowe. Redukuje wpływ nagłych skoków, bardziej stabilne


# Zad 7 -------------------------------------------------------------------

m <- 25
europln_d_filtered$Zmiennosc_Wzor1_m25 <- oblicz_zmiennosc(europln_d_filtered$Stopa_Logarytmiczna, m)

zmiennosc_porownanie <- europln_d_filtered %>%
  select(Data, Zmiennosc_Wzor1_m25, starts_with("EWMA_lambda")) %>%
  pivot_longer(
    cols = -Data,
    names_to = "Typ_Zmiennosci",
    values_to = "Wartosc"
  )

ggplot(zmiennosc_porownanie, aes(x = Data, y = Wartosc, color = Typ_Zmiennosci)) +
  geom_line() +
  labs(title = "Porównanie zmienności EWMA dla różnych lambda z kroczącą zmiennością (m=25)",
       x = "Data", y = "Zmienność") +
  theme_minimal()


rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

rmse_values <- sapply(lambda_values, function(lambda) {
  colname <- paste0("EWMA_lambda_", lambda)
  rmse(europln_d_filtered$Zmiennosc_Wzor1_m25, europln_d_filtered[[colname]])
})

names(rmse_values) <- lambda_values
rmse_values

#najlepiej sprawdzi sie lambda 0.95
#oznacza to, że model EWMA z tym parametrem dobrze oddaje rzeczywistą zmienność kroczącą.




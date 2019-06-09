# Funkcje do rzutu kostkami
rzut_kostkami = function(liczba_kostek = 1)
       sample(1:6, size = liczba_kostek, replace = TRUE)

rzut_kostkami(4)

# Czołgi
czołg_1 = list(atak = 5, 
               pancerz = 2, 
               nazwa = "Pantera")
czołg_2 = list(atak = 6, 
               pancerz = 10, 
               nazwa = "IS-122")

# rozgrywka

wynik_ataku = rzut_kostkami(czołg_1$atak) 
wynik_ataku
liczba_trafien = sum(wynik_ataku > 3)
liczba_trafien

wynik_obrony = rzut_kostkami(czołg_2$pancerz)
wynik_obrony
liczba_obron = sum(wynik_obrony > 3)
liczba_obron


# gra
liczba_gier = 10000

rozgrywki_1 = replicate(liczba_gier, {
  wynik_ataku = rzut_kostkami(czołg_1$atak) 
  liczba_trafien = sum(wynik_ataku > 3)
  
  wynik_obrony = rzut_kostkami(czołg_2$pancerz)
  liczba_obron = sum(wynik_obrony > 3)
  
  if (liczba_trafien > liczba_obron) {
    trafień = liczba_trafien - liczba_obron
  } else {
    trafień = 0
  }
  
  trafień
})


table(rozgrywki_1)

barplot(table(rozgrywki_1), main="Czołg Pantera trafia IS")

mean(rozgrywki_1)


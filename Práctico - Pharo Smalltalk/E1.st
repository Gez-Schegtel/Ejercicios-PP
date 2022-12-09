
"Convertir una cadena a mayúsculas, a nivel de caracter. Opción 1."

| toyStory wendyMalibú stacyMalibú |
toyStory:= 'Hay una serpiente en mi bota.'.
stacyMalibú:= ''.
1 to: toyStory size do: [:i| wendyMalibú:= toyStory at:i. stacyMalibú:= stacyMalibú, wendyMalibú asString asUppercase ].
^stacyMalibú.

"Convertir una cadena a mayúsculas, a nivel de caracter. Opción 2."

| toyStory stacyMalibú |
toyStory:= 'Hay una serpiente en mi bota.'.
stacyMalibú:= ''.
1 to: toyStory size do: [:i| stacyMalibú:= stacyMalibú, ((toyStory at:i) asString asUppercase) ].
^stacyMalibú.
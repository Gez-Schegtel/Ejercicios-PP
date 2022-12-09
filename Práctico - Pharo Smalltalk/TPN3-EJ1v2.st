"1. Convertir una cadena a mayúsculas y minúsculas."

"A nivel de caracter. "

| cadena1 cadena2 caracter |
cadena1:= 'a esta cadena la vas a estar viendo en mayúsculas, aunque se escribió en minúsculas.'.
cadena2:= 'A ESTA CADENA LA VAS A ESTAR VIENDO EN MINÚSCULAS, AUNQUE SE ESCRIBIÓ EN MAYÚSCULAS.'.
1 to: (cadena1 size) do: [:i| caracter:= cadena1 at:i. cadena1 at:i put: (caracter asUppercase) ].
1 to: (cadena2 size) do: [:i| caracter:= cadena2 at:i. cadena2 at:i put: (caracter asLowercase) ].
^cadena1, String cr, cadena2.
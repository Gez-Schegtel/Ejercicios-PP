"1. Convertir una cadena a mayúsculas y minúsculas."

"A nivel de caracter, forma 3. La mejor de todas. "

| cadena1 cadena2 |
cadena1:= 'a esta cadena la vas a estar viendo en mayúsculas, aunque se escribió en minúsculas.'.
cadena2:= 'A ESTA CADENA LA VAS A ESTAR VIENDO EN MINÚSCULAS, AUNQUE SE ESCRIBIÓ EN MAYÚSCULAS.'.
1 to: (cadena1 size) do: [:i| cadena1 at:i put: ((cadena1 at:i) asUppercase) ].
1 to: (cadena2 size) do: [:i| cadena2 at:i put: ((cadena2 at:i) asLowercase) ].
^cadena1, String cr, cadena2.
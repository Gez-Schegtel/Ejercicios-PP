"2. Dada una cadena de entrada, devolver otra en la que los caracteres en mayúsculas hayan sido cambiados por caracteres en minúsculas y viceversa."

"Recordar: $d = ($d asLowercase). --> 'true'"

| cadena caracter|
cadena:= 'hoLA, ¿cÓmo ESTÁs?'.
1 to: (cadena size) do: [:i| caracter:= cadena at:i. ( (caracter) = (caracter asUppercase) ) ifTrue: [ cadena at:i put: caracter asLowercase ] ifFalse: [ cadena at:i put: caracter asUppercase ] ].
^cadena.
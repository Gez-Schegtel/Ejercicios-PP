
"Ejemplo pedorro con el mensaje ‘include:’ :"

| b m w |
b:= 'h,o.rse'.
m:= #($, $.).
w:= ((b at:2) = ($,)).
w:= m includes: (b at:2).
^(w not).

"Ejemplo del uso de bloques y Transcrip para mostrar mensajes de forma muy cool:"

| varAfueraDelBlock1 varAfueraDelBlock2 coflerBlockSuma coflerBlockProd resultSuma resultProd |
Transcript clear open.
coflerBlockSuma:= [:x :y| x + y ].
coflerBlockProd:= [:a :b| a * b ].
varAfueraDelBlock1:= 4. "Es una valor definido fuera del bloque que utilizaremos."
varAfueraDelBlock2:= 8. "Ídem a la línea anterior."
resultSuma:= coflerBlockSuma value: varAfueraDelBlock1 value: varAfueraDelBlock2.
resultProd:= coflerBlockProd value: varAfueraDelBlock1 value: varAfueraDelBlock2.
^Transcript show: resultSuma asString, String cr, resultProd asString.
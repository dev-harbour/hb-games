/*
 *
 * Copyright 2017 - 2021 Rafał Jopek ( rafaljopek at hotmail com )
 *
 * hb_Shuffle() is a function in the Harbour programming language that randomly shuffles the elements of an array.
 * It takes an array as its input and returns the shuffled array. The elements of the array are rearranged in a
 * random order, ensuring that each possible permutation is equally likely. This function can be useful in a variety
 * of situations, such as when generating random levels or randomizing the order of elements in a list.
 * The function can be used on any array of any data type, including numbers, strings, and other arrays.
 * It is important to note that the original array is modified by this function and no new array is created.
 *
 */

// hb_Shuffle( aArray ) --> aArray
FUNCTION hb_Shuffle( aArray )

   LOCAL i, temp, nRandom

   FOR i := 1 TO Len( aArray )
      nRandom := hb_randInt( Len( aArray ) )
      temp := aArray[ i ]
      aArray[ i ] := aArray[ nRandom ]
      aArray[ nRandom ] := temp
   NEXT

RETURN aArray

/* Example
PROCEDURE Main()

   LOCAL aArray, nIndex

   aArray := { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }

   aArray := hb_Shuffle( aArray )
   FOR nIndex := 1 TO Len( aArray )
      OutStd( Str( aArray[ nIndex ] ) + " " )
   NEXT

RETURN
*/

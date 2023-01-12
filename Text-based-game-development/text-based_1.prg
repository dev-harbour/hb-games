#include "inkey.ch"

PROCEDURE Main()

   LOCAL nKey, aMap, nPlayerX, nPlayerY

   aMap := { ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", ".", "#", "#", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", "#", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" } }

   nPlayerX := 2
   nPlayerY := 2

   WHILE( .T. )

      DrawMap( aMap, nPlayerX, nPlayerY )

      nKey := Inkey( 1 )

      IF nKey == K_UP
         nPlayerY := nPlayerY - 1
      ELSEIF nKey == K_DOWN
         nPlayerY := nPlayerY + 1
      ELSEIF nKey == K_LEFT
         nPlayerX := nPlayerX - 1
      ELSEIF nKey == K_RIGHT
         nPlayerX := nPlayerX + 1
      ENDIF
   ENDDO

   RETURN

FUNCTION DrawMap( aMap, nPlayerX, nPlayerY )

   LOCAL nX, nY

   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF nX == nPlayerX .AND. nY == nPlayerY
            hb_DispOutAt( nY - 1, nX - 1, "P", 0x22 )
         ELSE
            hb_DispOutAt( nY - 1, nX - 1, aMap[ nY ][ nX ], 0x7 )
         ENDIF
      NEXT
   NEXT

   RETURN NIL
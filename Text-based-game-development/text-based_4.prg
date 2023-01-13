#include "inkey.ch"

PROCEDURE Main()

   LOCAL nKey, aMap, nPlayerX, nPlayerY, nLives, nStartTime, nTimeLimit

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
   nLives := 3
   nTimeLimit := 30000 // 30 seconds in milliseconds

   nStartTime := hb_MilliSeconds()

   WHILE( nLives > 0 )

      DrawMap( aMap, nPlayerX, nPlayerY )

      hb_DispOutAt( 20, 1, "Lives: " + Str( nLives ) )

      nKey := Inkey( 1 )

      IF nKey == K_UP
         IF aMap[ nPlayerY - 1 ][ nPlayerX ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerY := nPlayerY - 1
         ENDIF
      ELSEIF nKey == K_DOWN
         IF aMap[ nPlayerY + 1 ][ nPlayerX ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerY := nPlayerY + 1
         ENDIF
      ELSEIF nKey == K_LEFT
         IF aMap[ nPlayerY ][ nPlayerX - 1 ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerX := nPlayerX - 1
         ENDIF
      ELSEIF nKey == K_RIGHT
         IF aMap[ nPlayerY ][ nPlayerX + 1 ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerX := nPlayerX + 1
         ENDIF
      ENDIF

      IF aMap[ nPlayerY ][ nPlayerX ] == "."
         aMap[ nPlayerY ][ nPlayerX ] := " "
      ENDIF

      /* check time limit */
      IF hb_MilliSeconds() - nStartTime > nTimeLimit
         Alert("Time's up!")
         nLives := 0
      ENDIF

   ENDDO

   IF nLives == 0
      Alert( "Game Over!" )
   ENDIF

   RETURN

FUNCTION DrawMap( aMap, nPlayerX, nPlayerY )

   LOCAL nX, nY

   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF nX == nPlayerX .AND. nY == nPlayerY
            hb_DispOutAt( nY - 1, nX - 1, "P", 0x22 )
         ELSE
            IF aMap[ nY ][ nX ] == "." .AND. nX == nPlayerX .AND. nY == nPlayerY
               hb_DispOutAt( nY - 1, nX - 1, " ", 0x7 )
            ELSE
               hb_DispOutAt( nY - 1, nX - 1, aMap[ nY ][ nX ], 0x7 )
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN NIL

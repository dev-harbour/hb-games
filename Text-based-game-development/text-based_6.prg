#include "inkey.ch"

PROCEDURE Main()

   LOCAL nKey, aMap, nPlayerX, nPlayerY, nLives, lQuit := .F., nStartTime, nDotsRemaining, nElapsedTime

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

   nStartTime := hb_MilliSeconds()
   nDotsRemaining := CountDots( aMap )

   WHILE( nLives > 0 .AND. ! lQuit )

      DrawMap( aMap, nPlayerX, nPlayerY )

      nElapsedTime := hb_MilliSeconds() - nStartTime
      hb_DispOutAt( 11, 1, "Time: " +  Transform( nElapsedTime, "9,999 999" ) + " s" )

      hb_DispOutAt( 20, 1, "Lives: " + Str( nLives ) )

      nKey := Inkey()

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
         nDotsRemaining -= 1
      ENDIF

      IF nDotsRemaining == 0
         DrawMap( aMap, nPlayerX, nPlayerY )
         Alert( "You won!" )
         lQuit := .T.
      ENDIF

      IF hb_MilliSeconds() - nStartTime > 30000
         Alert( "Time's up!" )
         lQuit := .T.
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

FUNCTION CountDots( aMap )

   LOCAL nDots, nX, nY

   nDots := 0
   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF aMap[ nY ][ nX ] == "."
            nDots += 1
         ENDIF
      NEXT
   NEXT

   RETURN nDots

/*
 * Copyright 2019 Rafał Jopek ( rafaljopek at hotmail com )
 * Copyright 2022 dev-harbour
 */

#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define BOX_UNI   hb_UTF8ToStrBox( "│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌" )
#define CHR_249   hb_UTF8ToStrBox( "·" )
#define CHR_254   hb_UTF8ToStrBox( "■" )

STATIC nRow       := 0
STATIC nCol       := 0
STATIC cDirection := ""
STATIC nLife      := 3

STATIC nRowRed       := 0
STATIC nColRed       := 0
STATIC cDirectionRed := ""

STATIC aMaze
STATIC aPath
STATIC aPoints
STATIC nStartGame := 0
STATIC nLevel     := 1
STATIC nScore     := 0
STATIC nColorMaze := 2

STATIC nTop    := 0
STATIC nLeft   := 0
STATIC nBottom := 0
STATIC nRight  := 0

PROCEDURE Main()

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT, INKEY_ALL ) )

   SetCursor( SC_NONE )

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )
   Set( _SET_DBCODEPAGE, "EN" )

   hb_gtInfo( HB_GTI_MAXIMIZED, .T. )
   hb_gtInfo( HB_GTI_WINTITLE, "Hb pac man game" )
   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   Scroll()

   WelcomeScreen()
   PacMan()

   SetCursor( SC_NORMAL )

   RETURN

STATIC PROCEDURE WelcomeScreen()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL cLine
   LOCAL nRow

   LOCAL aLogo := { ;
      "    __  ____                                               ", ;
      "   / / / / /_     ____  ____  _____   ____ ___  ____  ____ ", ;
      "  / /_/ / __ \   / __ \/ __ `/ ___/  / __ `__ \/ __ `/ __ \", ;
      " / __  / /_/ /  / /_/ / /_/ / /__   / / / / / / /_/ / / / /", ;
      "/_/ /_/_.___/  / .___/\__,_/\___/  /_/ /_/ /_/\__,_/_/ /_/ ", ;
      "              /_/                                          " }

   DO WHILE lContinue

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()

         Scroll()
         DispBegin()
         FOR EACH cLine IN aLogo
            nRow := Int( ( nMaxRow - Len( aLogo ) ) / 2 ) - 5
            hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, PadC( cLine, nMaxCol ), hb_RandomInt( 0x1, 0xf ) )
         NEXT

         hb_DispOutAt( nRow + 16, 0,  PadC( "Copyright 2017-2019 Rafał Jopek", nMaxCol ), 0x8 )
         DispEnd()

      ENDIF

      hb_DispOutAt( nRow +  8, 0,  PadC( "Play games", nMaxCol ), 0x6 )
      hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0x6 )

      IF MRow() == nRow + 8 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
         hb_DispOutAt( nRow + 8, 0,  PadC( "Play games", nMaxCol ), 0xa )
      ENDIF

      IF MRow() == nRow + 18 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
         hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0xa )
      ENDIF

      SWITCH Inkey( 0 )

      CASE K_ENTER
         lContinue := .F.
         EXIT

      CASE K_LBUTTONDOWN

         IF MRow() == nRow + 8 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            lContinue := .F.
         ENDIF

         IF MRow() == nRow + 18 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_run( "start " + "https://harbour.pl/" )
         ENDIF
         EXIT

      CASE K_MOUSEMOVE

         IF MRow() == nRow + 8 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_DispOutAt( nRow + 8, 0,  PadC( "Play games", nMaxCol ), 0xa )
         ENDIF

         IF MRow() == nRow + 18 .AND. MCol() > Int( ( nMaxCol - 10 ) / 2 ) - 1 .AND. MCol() <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0xa )
         ENDIF
         EXIT

      CASE HB_K_RESIZE
         EXIT

      ENDSWITCH

   ENDDO

   RETURN

STATIC PROCEDURE PacMan()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nKey

   GetAll()

   DO WHILE lContinue

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
         Scroll()

         nTop    := Int( ( MaxRow() - 31 ) / 2 )
         nLeft   := Int( ( MaxCol() - 55 ) / 2 )
         nBottom := Int( ( MaxRow() + 31 ) / 2 )
         nRight  := Int( ( MaxCol() + 55 ) / 2 )

         DrawMaze()

         IF nMaxRow == 0 .OR. nMaxCol == 0

            nRow := nTop + 29
            nCol := nLeft + 28

            nRowRed := nTop + 11
            nColRed := nLeft + 28

         ELSE /* TO DO */

            nRow -= Int( ( nMaxRow - MaxRow() ) / 2 )
            nCol -= Int( ( nMaxCol - MaxCol() ) / 2 )

            nRowRed -= Int( ( nMaxRow - MaxRow() ) / 2 )
            nColRed -= Int( ( nMaxCol - MaxCol() ) / 2 )

         ENDIF

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()

      ENDIF

      DO WHILE .T.

         Move()
         MoveRed()

         Refresh()

         IF AScan( aPoints, {| x | x[ 4 ] != 0x0 } ) == 0
            Scroll()
            hb_Alert( "End of level " + hb_ntos( nLevel ),, 0xf0, 3 ) // TO DO - zatrzymać zegar na 3 sekundy

            IF ++nLevel > ADir( "level" + hb_ps() + "*.txt" )
               nLevel := 1
            ENDIF

            GetAll()
            DrawMaze()

            cDirection    := ""
            cDirectionRed := SubStr( "LR", hb_randInt( 1, 2 ), 1 )

            nRow := nTop + 29
            nCol := nLeft + 28

            nRowRed := nTop + 11
            nColRed := nLeft + 28

         ENDIF

         hb_idleSleep( 0.2 )

         nKey := Inkey()

         IF nKey == K_UP .OR. nKey == K_DOWN .OR. nKey == K_LEFT .OR. nKey == K_RIGHT .OR. nKey == K_ESC .OR. nKey == K_F5 .OR. nKey == HB_K_RESIZE
            EXIT
         ENDIF

         IF nRow == nRowRed .AND. nCol == nColRed

            IF nLife-- < 1
               __Quit()
            ENDIF

            nRow := nTop + 29
            nCol := nLeft + 28

            nRowRed := nTop + 11
            nColRed := nLeft + 26

            cDirectionRed := SubStr( "LR", hb_randInt( 1, 2 ), 1 )

         ENDIF

      ENDDO

      IF nStartGame == 0
         IF ! nKey == HB_K_RESIZE .OR. ! nKey == K_F5
            nStartGame := hb_MilliSeconds()
            cDirectionRed := SubStr( "LR", hb_randInt( 1, 2 ), 1 )
            nLife--
         ENDIF
      ENDIF

      SWITCH nKey

      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_UP
         MoveUp()
         EXIT

      CASE K_DOWN
         MoveDown()
         EXIT

      CASE K_LEFT
         MoveLeft()
         EXIT

      CASE K_RIGHT
         MoveRight()
         EXIT

      CASE K_F5
         IF ++nColorMaze > 0xf
            nColorMaze := 0x1
         ENDIF
         DrawMaze()
         EXIT

      ENDSWITCH

   ENDDO

   RETURN

STATIC PROCEDURE Refresh()

   DispBegin()

   DrawPoints()

   hb_DispOutAtBox( nRow, nCol, iif( nStartGame == 0, "", Chr( 2 ) ) /* LOW-ASCII "☺" */, 0xe )
   hb_DispOutAtBox( nRowRed, nColRed, iif( nStartGame == 0, "", Chr( 234 ) ) /* LOW-ASCII "Ω" */, 0x4 )

   hb_DispOutAt( nBottom, nLeft, PadC( Replicate( Chr( 2 ) + " ", nLife ), 55 ), 0xe )
   hb_DispOutAt( nBottom, nLeft, "Score : " + hb_ntoc( nScore ), NIL  )
   hb_DispOutAt( nBottom + 1, nLeft, PadR( "Time  : " + hb_ntos( iif( nStartGame > 0, Int( ( ( hb_MilliSeconds() - nStartGame ) / 1000 ) ), 0 ) ), 55 ), NIL )

   DispEnd()

   RETURN

STATIC PROCEDURE GetAll()

   LOCAL cString
   LOCAL cLine
   LOCAL tmp

   /* Czyszczenie tablicy z poprzedniego poziomu */
   aMaze   := {}
   aPath   := {}
   aPoints := {}

   IF ( cString := hb_MemoRead( "level" + hb_ps() + "level-" + hb_ntos( nLevel ) + ".txt" ) ) == ""
      hb_Alert( "Error reading: " + "level" + hb_ps() + "level-" + hb_ntos( nLevel ) + ".txt",, 0xf0, 3 )
      __Quit()
   ENDIF

   FOR EACH cLine IN hb_ATokens( cString, .T. )
      FOR EACH tmp IN hb_UTF8ToStrBox( cLine )
         IF tmp $ BOX_UNI
            AAdd( aMaze, { cLine:__enumIndex() - 1, tmp:__enumIndex() - 1, tmp } )
         ENDIF
         IF tmp == CHR_249 .OR. tmp == CHR_254
            AAdd( aPath, { cLine:__enumIndex() - 1, tmp:__enumIndex() - 1 } )
         ENDIF
         IF tmp == CHR_249 .OR. tmp == CHR_254
            AAdd( aPoints, { cLine:__enumIndex() - 1, tmp:__enumIndex() - 1, tmp, iif( tmp == CHR_249, 0x8, 0x6 ) } )
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE DrawMaze()

   LOCAL cLine

   FOR EACH cLine IN aMaze
      hb_DispOutAtBox( nTop + cLine[ 1 ], nLeft + cLine[ 2 ], cLine[ 3 ], nColorMaze )
   NEXT

   RETURN

STATIC PROCEDURE DrawPoints()

   LOCAL cLine

   FOR EACH cLine IN aPoints
      hb_DispOutAtBox( nTop + cLine[ 1 ], nLeft + cLine[ 2 ], cLine[ 3 ], cLine[ 4 ] )
   NEXT

   RETURN


/* Hb pac man */
STATIC PROCEDURE Move()

   SWITCH cDirection
   CASE "U" ; MoveUp()    ; EXIT
   CASE "D" ; MoveDown()  ; EXIT
   CASE "L" ; MoveLeft()  ; EXIT
   CASE "R" ; MoveRight() ; EXIT
   ENDSWITCH

   RETURN

STATIC PROCEDURE MoveUp()

   LOCAL nPos

   IF nRow == nTop
      nRow := nBottom
   ENDIF

   IF ( nPos := AScan( aPath, {| x | nTop + x[ 1 ] == nRow - 1 .AND. nLeft + x[ 2 ] == nCol } ) ) > 0
      nRow--
      Scroll( nRow + 1, nCol, nRow + 1, nCol )
      IF aPoints[ nPos ][ 4 ] != 0x0
         IF aPoints[ nPos ][ 3 ] == CHR_249
            nScore += 10
         ELSE
            nScore += 50
         ENDIF
         aPoints[ nPos ][ 4 ] := 0x0
      ENDIF
      cDirection := "U"
   ENDIF

   RETURN

STATIC PROCEDURE MoveDown()

   LOCAL nPos

   IF nRow == nBottom - 1
      nRow := nTop - 1
   ENDIF

   IF ( nPos := AScan( aPath, {| x | nTop + x[ 1 ] == nRow + 1 .AND. nLeft + x[ 2 ] == nCol } ) ) > 0
      nRow++
      Scroll( nRow - 1, nCol, nRow - 1, nCol )
      IF aPoints[ nPos ][ 4 ] != 0x0
         IF aPoints[ nPos ][ 3 ] == CHR_249
            nScore += 10
         ELSE
            nScore += 50
         ENDIF
         aPoints[ nPos ][ 4 ] := 0x0
      ENDIF
      cDirection := "D"
   ENDIF

   RETURN

STATIC PROCEDURE MoveLeft()

   LOCAL nPos

   IF nCol == nLeft
      nCol := nRight + 1
   ENDIF

   IF ( nPos := AScan( aPath, {| x | nTop + x[ 1 ] == nRow .AND. nLeft + x[ 2 ] == nCol - 2 } ) ) > 0
      nCol -= 2
      Scroll( nRow, nCol + 2, nRow, nCol + 2 )
      IF aPoints[ nPos ][ 4 ] != 0x0
         nScore += 10
         aPoints[ nPos ][ 4 ] := 0x0
      ENDIF
      cDirection := "L"
   ENDIF

   RETURN

STATIC PROCEDURE MoveRight()

   LOCAL nPos

   IF nCol == nRight - 1
      nCol := nLeft - 2
   ENDIF

   IF ( nPos := AScan( aPath, {| x | nTop + x[ 1 ] == nRow .AND. nLeft + x[ 2 ] == nCol + 2 } ) ) > 0
      nCol += 2
      Scroll( nRow, nCol - 2, nRow, nCol - 2 )
      IF aPoints[ nPos ][ 4 ] != 0x0
         nScore += 10
         aPoints[ nPos ][ 4 ] := 0x0
      ENDIF
      cDirection := "R"
   ENDIF

   RETURN

/* Red ghost */
STATIC PROCEDURE MoveRed()

   SWITCH cDirectionRed
   CASE "U" ; MoveRedUp()    ; EXIT
   CASE "D" ; MoveRedDown()  ; EXIT
   CASE "L" ; MoveRedLeft()  ; EXIT
   CASE "R" ; MoveRedRight() ; EXIT
   ENDSWITCH

   RETURN

STATIC PROCEDURE MoveRedUp()

   IF nRowRed == nTop
      nRowRed := nBottom
   ENDIF

   DO CASE
      /* Jeżeli idzie w górę i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ;  /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0         /* → */
      /* cDirectionRed :=  SubStr( "ULR", hb_randInt( 1, 3 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      ENDCASE

      /* Jeżeli idzie w górę i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0        /* → */
      /* cDirectionRed :=  SubStr( "LR", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE

      ENDCASE

      /* Jeżeli idzie w górę i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0        /* ← */
      /* cDirectionRed :=  SubStr( "UL", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      OTHERWISE
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      ENDCASE

      /* Jeżeli idzie w górę i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0        /* → */
      /* cDirectionRed :=  SubStr( "UR", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      ENDCASE

      /* Jeżeli idzie w górę i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 /* ← */
      nColRed -= 2
      Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
      cDirectionRed := "L"

      /* Jeżeli idzie w górę i ma do wyboru - tylko w prawo */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0 /* → */
      nColRed += 2
      Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
      cDirectionRed := "R"

   OTHERWISE

      /* Gdy żaden z warunków określanych w instrukcjach CASE nie jest spełniony: ↑ */
      nRowRed--
      Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
      cDirectionRed := "U"

   ENDCASE

   RETURN

STATIC PROCEDURE MoveRedDown()

   IF nRowRed == nBottom - 1
      nRowRed := nTop - 1
   ENDIF

   DO CASE
      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ;  /* ↓ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0         /* → */
      /* cDirectionRed :=  SubStr( "DLR", hb_randInt( 1, 3 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      ENDCASE

      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0        /* → */
      /* cDirectionRed :=  SubStr( "LR", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE

      ENDCASE

      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↓ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0        /* ← */
      /* cDirectionRed :=  SubStr( "DL", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed > nCol
         nColRed -= 2
         Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
         cDirectionRed := "L"

      OTHERWISE
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      ENDCASE

      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↓ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0        /* → */
      /* cDirectionRed :=  SubStr( "DR", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nColRed < nCol
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      OTHERWISE
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      ENDCASE

      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 /* ← */
      nColRed -= 2
      Scroll( nRowRed, nColRed + 2, nRowRed, nColRed + 2 )
      cDirectionRed := "L"

      /* Jeżeli idzie w dół i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0 /* → */
      nColRed += 2
      Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
      cDirectionRed := "R"

   OTHERWISE

      /* Gdy żaden z warunków określanych w instrukcjach CASE nie jest spełniony: ↓ */
      nRowRed++
      Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
      cDirectionRed := "D"

   ENDCASE

   RETURN

STATIC PROCEDURE MoveRedLeft()

   IF nColRed == nLeft
      nColRed := nRight + 1
   ENDIF

   DO CASE
      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ;  /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0         /* ↓ */
      /* cDirectionRed :=  SubStr( "LUD", hb_randInt( 1, 3 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE
         nColRed -= 2
         Scroll( nColRed, nColRed + 2, nColRed, nColRed + 2 )
         cDirectionRed := "L"

      ENDCASE

      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0        /* ↓ */
      /* cDirectionRed :=  SubStr( "UD", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE

      ENDCASE

      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0        /* ↑ */
      /* cDirectionRed :=  SubStr( "LU", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      OTHERWISE
         nColRed -= 2
         Scroll( nColRed, nColRed + 2, nColRed, nColRed + 2 )
         cDirectionRed := "L"

      ENDCASE

      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed - 2 } ) > 0 .AND. ; /* ← */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0        /* ↓ */
      /* cDirectionRed :=  SubStr( "LD", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE
         nColRed -= 2
         Scroll( nColRed, nColRed + 2, nColRed, nColRed + 2 )
         cDirectionRed := "L"

      ENDCASE

      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 /* ↑ */
      nRowRed--
      Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
      cDirectionRed := "U"

      /* Jeżeli idzie w lewo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 /* ↓ */
      nRowRed++
      Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
      cDirectionRed := "D"

   OTHERWISE

      /* Gdy żaden z warunków określanych w instrukcjach CASE nie jest spełniony: ← */
      nColRed -= 2
      Scroll( nColRed, nColRed + 2, nColRed, nColRed + 2 )
      cDirectionRed := "L"

   ENDCASE

   RETURN

STATIC PROCEDURE MoveRedRight()

   IF nColRed == nRight - 1
      nColRed := nLeft - 2
   ENDIF

   DO CASE
      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0 .AND. ;  /* → */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0         /* ↓ */
      /* cDirectionRed :=  SubStr( "RUD", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      ENDCASE

      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 .AND. ; /* ↑ */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0        /* ↓ */
      /* cDirectionRed :=  SubStr( "UD", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE

      ENDCASE

      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0 .AND. ; /* → */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0        /* ↑ */
      /* cDirectionRed :=  SubStr( "RU", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed > nRow
         nRowRed--
         Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
         cDirectionRed := "U"

      OTHERWISE
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      ENDCASE

      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE  AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed .AND. nLeft + x[ 2 ] == nColRed + 2 } ) > 0 .AND. ; /* → */
         AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0         /* ↓ */
      /* cDirectionRed :=  SubStr( "RD", hb_randInt( 1, 2 ), 1 ) */

      DO CASE
      CASE nRowRed < nRow
         nRowRed++
         Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
         cDirectionRed := "D"

      OTHERWISE
         nColRed += 2
         Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
         cDirectionRed := "R"

      ENDCASE

      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed - 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 /* ↑ */
      nRowRed--
      Scroll( nRowRed + 1, nColRed, nRowRed + 1, nColRed )
      cDirectionRed := "U"

      /* Jeżeli idzie w prawo i ma do wyboru: */
   CASE AScan( aPath, {| x | nTop + x[ 1 ] == nRowRed + 1 .AND. nLeft + x[ 2 ] == nColRed } ) > 0 /* ↓ */
      nRowRed++
      Scroll( nRowRed - 1, nColRed, nRowRed - 1, nColRed )
      cDirectionRed := "D"

   OTHERWISE

      /* Gdy żaden z warunków określanych w instrukcjach CASE nie jest spełniony: → */
      nColRed += 2
      Scroll( nRowRed, nColRed - 2, nRowRed, nColRed - 2 )
      cDirectionRed := "R"

   ENDCASE

   RETURN

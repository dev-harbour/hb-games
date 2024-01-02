/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define BOX_UNI   hb_UTF8ToStrBox( "│┤┐└┴┬├─┼┘┌" )

REQUEST HB_CODEPAGE_UTF8EX

STATIC aArray

PROCEDURE Main()

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT, INKEY_ALL ) )

   SetCursor( SC_NONE )

   /* Setup input CP of the translation */
/*
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )
*/
   /* Configure terminal and OS codepage */
/*
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )
   Set( _SET_DBCODEPAGE, "EN" )
*/

   //hb_gtInfo( HB_GTI_ICONFILE, "../docs/img/harbour.ico" )
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour Maze 2024" )

   // hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   Scroll()

   aArray := MazeInit()
   WelcomeScreen()

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )

   RandomMaze()
   Maze()

   SetCursor( SC_NORMAL )

   RETURN

STATIC PROCEDURE WelcomeScreen()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL cLine
   LOCAL nRow

   LOCAL aLogo := { ;
      "    __  ____                                ", ;
      "   / / / / /_     ____ ___  ____  ____  ___ ", ;
      "  / /_/ / __ \   / __ `__ \/ __ `/_  / / _ \", ;
      " / __  / /_/ /  / / / / / / /_/ / / /_/  __/", ;
      "/_/ /_/_.___/  /_/ /_/ /_/\__,_/ /___/\___/ " }

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

         hb_DispOutAt( nRow + 16, 0,  PadC( "Copyright 2017-2019 Rafal Jopek", nMaxCol ), 0x8 )
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
            #if defined( __PLATFORM__DARWIN )
               hb_run( "open " + "https://harbour.pl/" )
            #elif defined( __PLATFORM__WINDOWS )
               hb_run( "start " + "https://harbour.pl/" )
            #elif defined( __PLATFORM__UNIX )
               hb_run( "xdg-open " + "https://harbour.pl/" )
            #else
               OutStd( e"Unsupported platform\n" )
            #endif
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

STATIC FUNCTION MazeInit()
RETURN { ;
      'aMaze'        => {}, ;
      'aPath'        => {}, ;
      'aMouse'       => {}, ;
      'nStartGame'   =>  0, ;
      'nNumberMoves' =>  0, ;
      'nLevel'       =>  1 }

STATIC PROCEDURE Maze()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL nRow := 38, nCol := 114
   LOCAL tmp
   LOCAL nNewRow, nNewCol

   SetMode( 44, 117 )

   DO WHILE lContinue

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()

         RefreshAll()
         aArray[ 'nStartGame' ] := hb_MilliSeconds()

         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      DispBegin()
      hb_DispOutAt( nRow, nCol, Chr( 2 )  /* LOW-ASCII "☺" */, 0xe )

      hb_DispOutAt( 40, 49, "Maze level      : " + hb_ntos( aArray[ 'nLevel' ] ) )
      hb_DispOutAt( 41, 49, "Game duration   : " + hb_ntos( Int( ( ( hb_MilliSeconds() - aArray[ 'nStartGame' ] ) / 1000 ) ) ) + " second(s)" )
      hb_DispOutAt( 42, 49, "Number of moves : " + hb_ntos( aArray[ 'nNumberMoves' ] ) )
      DispEnd()

      SWITCH Inkey()

      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_UP
         nNewRow := nRow - 1
         FOR EACH tmp IN aArray[ 'aPath' ]
            IF tmp[ 1 ] == nNewRow .AND. tmp[ 2 ] == nCol
               nRow--
               Scroll( nRow + 1, nCol, nRow + 1, nCol )
               nNewRow := 0
               ++aArray[ 'nNumberMoves' ]
            ENDIF
         NEXT

         EXIT

      CASE K_DOWN
         nNewRow := nRow + 1
         FOR EACH tmp IN aArray[ 'aPath' ]
            IF tmp[ 1 ] == nNewRow .AND. tmp[ 2 ] == nCol
               nRow++
               Scroll( nRow - 1, nCol, nRow - 1, nCol )
               nNewRow := 0
               ++aArray[ 'nNumberMoves' ]
            ENDIF
         NEXT

         EXIT

      CASE K_LEFT
         nNewCol := nCol - 1
         FOR EACH tmp IN aArray[ 'aPath' ]
            IF tmp[ 1 ] == nRow .AND. tmp[ 2 ] == nNewCol
               nCol--
               Scroll( nRow, nCol + 1, nRow, nCol + 1 )
               nNewCol := 0
               ++aArray[ 'nNumberMoves' ]
            ENDIF
         NEXT

         EXIT

      CASE K_RIGHT
         nNewCol := nCol + 1
         FOR EACH tmp IN aArray[ 'aPath' ]
            IF tmp[ 1 ] == nRow .AND. tmp[ 2 ] == nNewCol
               nCol++
               Scroll( nRow, nCol - 1, nRow, nCol - 1 )
               nNewCol := 0
               ++aArray[ 'nNumberMoves' ]
            ENDIF
         NEXT

         EXIT

      ENDSWITCH

      IF nRow == -1 .AND. nCol == 2
         lContinue := .F.
      ENDIF

   ENDDO

   PrintResults()

RETURN

STATIC PROCEDURE RefreshAll()

   DispBegin()
   Scroll()

   RandomMaze()
   DrawMaze()

   DispEnd()

RETURN

STATIC PROCEDURE RandomMaze()

   LOCAL cLine
   LOCAL tmp

   FOR EACH cLine IN { ;
         "┌ · ────────────────────────────┬───────────────────────────────────────────┬───────────┬───────────────────────────┐", ;
         "│ ····························· │ ········································· │ ········· │ ························· │", ;
         "│ · ────────────┬───────────┐ · │ · ┌──────────────────────── · ┌───────┐ · ├──── · │ · │ · ┌───────────┬───┬──── · │", ;
         "│ ············· │ ········· │ · │ · │ ························· │ ····· │ · │ ····· │ · │ · │ ········· │ · │ ····· │", ;
         "│ · ┌───────┐ · │ · ┌───┐ · ├───┘ · │ · ┌───────────┬───────────┤ · ────┤ · │ · ────┴───┤ · │ · ┌───┐ · │ · │ · ────┤", ;
         "│ · │ ····· │ ····· │ · │ · │ ····· │ · │ ········· │ ········· │ ····· │ · │ ········· │ · │ · │ · │ · │ · │ ····· │", ;
         "│ · └──── · └───────┤ · │ · │ · ┌───┘ · │ · │ · ────┤ · ┌───┐ · │ · │ · │ · ├──────── · │ · │ · │ · │ · │ · └───┐ · │", ;
         "│ ················· │ · │ ····· │ ····· │ · │ ····· │ · │ · │ · │ · │ · │ · │ ········· │ ····· │ · │ · │ ····· │ · │", ;
         "├───────┬───────┐ · │ · ├───────┤ · ────┤ · └───┐ · │ · │ · │ · │ · │ · │ · │ · ┌───┐ · └───┬───┘ · │ · │ · │ · │ · │", ;
         "│ ····· │ ····· │ · │ · │ ····· │ ····· │ ····· │ ····· │ · │ ····· │ ····· │ · │ · │ ····· │ ····· │ · │ · │ · │ · │", ;
         "│ · ────┤ · │ · │ · │ · │ · │ · └───┐ · ├───┐ · ├───────┤ · └───────┴───────┘ · │ · └───┐ · └──── · │ · │ · ├───┘ · │", ;
         "│ ····· │ · │ ····· │ ····· │ ····· │ · │ · │ · │ ····· │ ····················· │ ····· │ ········· │ ····· │ ····· │", ;
         "├──── · │ · ├───────┴───┐ · ├──── · │ · │ · │ · │ · │ · └───┐ · ────┬───────┬───┘ · ┌───┴──────── · │ · ┌───┘ · │ · │", ;
         "│ ····· │ · │ ········· │ · │ ····· │ · │ · │ ····· │ ····· │ ····· │ ····· │ ····· │ ············· │ · │ ····· │ · │", ;
         "│ · │ · │ · │ · ────┐ · └───┤ · │ · │ · │ · └───────┴───┐ · └──── · │ · │ · │ · │ · │ · ────────┬───┤ · │ · ┌───┴───┤", ;
         "│ · │ · │ · │ ····· │ ····· │ · │ · │ · │ ············· │ ········· │ · │ ····· │ ············· │ · │ · │ · │ ····· │", ;
         "│ · │ · │ · │ · │ · ├──── · │ · │ · │ · │ · ┌───────┐ · └───────────┤ · └───┬───┴───────────┐ · │ · │ · │ · │ · │ · │", ;
         "│ · │ · │ · │ · │ · │ ········· │ · │ · │ · │ ····· │ ············· │ ····· │ ············· │ ····· │ · │ ····· │ · │", ;
         "│ · │ · │ · ├───┘ · │ · ┌───────┴───┘ · │ · │ · ┌───┴──────── · │ · ├───┐ · │ · ────────┐ · └───┐ · │ · └───┬───┘ · │", ;
         "│ · │ · │ · │ ····· │ · │ ············· │ ····· │ ············· │ · │ · │ · │ ········· │ ····· │ · │ ····· │ ····· │", ;
         "│ · └───┘ · │ · ┌───┘ · │ · ┌───┬───────┼──── · │ · ────┬───────┤ · │ · │ · └───┬──── · ├───┐ · │ · └───┐ · │ · │ · │", ;
         "│ ········· │ · │ ····· │ · │ · │ ····· │ ····· │ ····· │ ····· │ ····· │ ····· │ ····· │ · │ · │ ····· │ · │ · │ · │", ;
         "│ · ┌───────┘ · ├───────┤ · │ · │ · │ · │ · ┌───┴───┐ · │ · ────┘ · ┌───┴──── · │ · ┌───┘ · │ · │ · ┌───┘ · │ · │ · │", ;
         "│ · │ ········· │ ····· │ · │ ····· │ · │ · │ ····· │ · │ ········· │ ········· │ · │ ····· │ · │ · │ ····· │ · │ · │", ;
         "│ · └──── · │ · │ · │ · │ · └───┐ · │ · │ · │ · │ · │ · ├───────┐ · │ · ────────┤ · └───┐ · │ · └───┤ · ┌───┘ · │ · │", ;
         "│ · ······· │ · │ · │ · │ ····· │ · │ ····· │ · │ ····· │ ····· │ · │ ········· │ ····· │ · │ ····· │ · │ ····· │ · │", ;
         "├───────────┤ · │ · │ · └──── · │ · └───────┤ · └───────┘ · │ · └───┴───┬──── · ├──── · │ · └───┐ · │ · │ · ────┴───┤", ;
         "│ ········· │ · │ · │ ········· │ ········· │ ············· │ ········· │ ····· │ ····· │ ····· │ · │ · │ ········· │", ;
         "│ · ┌──── · │ · │ · ├──── · ┌───┴───────┬───┴───┐ · ┌───────┴───────┐ · │ · ┌───┤ · ────┤ · │ · │ · │ · └───┬──── · │", ;
         "│ · │ ····· │ · │ · │ ····· │ ········· │ ····· │ · │ ············· │ ····· │ · │ ····· │ · │ · │ ········· │ ····· │", ;
         "│ · │ · ────┘ · │ · │ · ────┤ · ────┐ · │ · │ · │ · └──── · ┌──── · └───────┘ · ├──── · │ · ├───┴───────┬───┘ · │ · │", ;
         "│ · │ ········· │ · │ ····· │ ····· │ ····· │ · │ ········· │ ················· │ ····· │ · │ ········· │ ····· │ · │", ;
         "│ · ├───────┬───┘ · ├───────┴───┐ · ├───────┤ · └───────────┤ · ┌───────────┐ · │ · ┌───┘ · │ · ────┐ · │ · ┌───┘ · │", ;
         "│ · │ ····· │ ····· │ ········· │ · │ ····· │ ············· │ · │ ········· │ · │ · │ ····· │ ····· │ ····· │ ····· │", ;
         "│ · └──── · │ · │ · │ · ────┐ · │ · │ · │ · └───────────┐ · │ · │ · ┌───┐ · └───┘ · │ · ────┴───┐ · └───────┴───────┤", ;
         "│ ········· │ · │ · │ ····· │ ····· │ · │ ············· │ · │ · │ · │ · │ ········· │ ········· │ ················· │", ;
         "├──────── · │ · └───┴──── · └───────┴───┴──────────── · │ · └───┘ · │ · └───────────┴──── · │ · └───────────────┐ · │", ;
         "│ ········· │ ········································· │ ································· │ ··················│ · │", ;
         "└───────────┴───────────────────────────────────────────┴───────────────────────────────────┴───────────────────┴ · ┘" }

      FOR EACH tmp IN hb_UTF8ToStrBox( cLine  )
         IF tmp $ BOX_UNI
            AAdd( aArray[ 'aMaze' ], { cLine:__enumIndex() - 1, tmp:__enumIndex() - 1, tmp } )
         ENDIF
         IF tmp == hb_UTF8ToStrBox( "·" )
            AAdd( aArray[ 'aPath' ], { cLine:__enumIndex() - 1, tmp:__enumIndex() - 1 } )
         ENDIF
      NEXT
   NEXT

RETURN

STATIC PROCEDURE DrawMaze()

   LOCAL tmp

   FOR EACH tmp IN aArray[ 'aMaze' ]
      hb_DispOutAt( tmp[ 1 ], tmp[ 2 ], tmp[ 3 ], 0x2 )
   NEXT

RETURN

STATIC PROCEDURE PrintResults()

   Scroll()

   ? "Results:"
   ?
   ? "Game duration   -", hb_ntos( Int( ( ( hb_MilliSeconds() - aArray[ 'nStartGame' ] ) / 1000 ) ) ), "second(s)"
   ?
   ? "Number of moves -", hb_ntos( aArray[ 'nNumberMoves' ] )

   Inkey( 0 )

RETURN

/*
 * Game of Life
 */

#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT, INKEY_ALL ) )
   SetCursor( SC_NONE )

   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )
   Set( _SET_DBCODEPAGE, "EN" )

   hb_gtInfo( HB_GTI_MAXIMIZED, .T. )
   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   Scroll()

   game_of_life_welcome_screen()
   game_of_life()

   SetCursor( SC_NORMAL )

RETURN

STATIC PROCEDURE game_of_life()

   LOCAL quit := .F.
   LOCAL aGrid
   LOCAL aTemp
   LOCAL nCol, nRow
   LOCAL nLenRow
   LOCAL nLenCol
   LOCAL aNeighbors[ 8 ]
   LOCAL nAliveNeighbors
   LOCAL nIndex

   aGrid := game_of_life_init()
   nLenRow := Len( aGrid )
   nLenCol := Len( aGrid[ 1 ] )

   aTemp := Array( nLenRow, nLenCol )

   hb_gtInfo( HB_GTI_WINTITLE, "Game of Life" )

   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT ) )

   WHILE( ! quit )

      DispBegin()
      Scroll()
      FOR nRow := 1 TO nLenRow
         FOR nCol := 1 TO nLenCol
            IF aGrid[ nRow, nCol ] == 1
               hb_DispOutAt( nRow -1, nCol -1, " ", 0xbb )
            ENDIF
         NEXT
      NEXT
      DispEnd()

      SWITCH Inkey( 1 )

      CASE K_ESC
         quit := .T.
         EXIT

      ENDSWITCH

      FOR nRow := 2 TO nLenRow -2
         FOR nCol := 2 TO nLenCol -2
            aNeighbors[ 1 ] := aGrid[ nRow -1, nCol -1 ]
            aNeighbors[ 2 ] := aGrid[ nRow -1, nCol    ]
            aNeighbors[ 3 ] := aGrid[ nRow -1, nCol +1 ]
            aNeighbors[ 4 ] := aGrid[ nRow,    nCol -1 ]
            aNeighbors[ 5 ] := aGrid[ nRow,    nCol +1 ]
            aNeighbors[ 6 ] := aGrid[ nRow +1, nCol -1 ]
            aNeighbors[ 7 ] := aGrid[ nRow +1, nCol    ]
            aNeighbors[ 8 ] := aGrid[ nRow +1, nCol +1 ]

            nAliveNeighbors := 0
            FOR nIndex := 1 TO 8
               IF aNeighbors[ nIndex ] == 1
                  nAliveNeighbors += aNeighbors[ nIndex ]
               ENDIF
            NEXT

            IF aGrid[ nRow, nCol ] == NIL .AND. nAliveNeighbors == 3
               aTemp[ nRow, nCol ] := 1
            ELSEIF aGrid[ nRow, nCol ] == 1 .AND. ( nAliveNeighbors < 2 .OR. nAliveNeighbors > 3 )
               aTemp[nRow, nCol] := NIL
            ELSE
               aTemp[ nRow, nCol ] := aGrid[ nRow, nCol ]
            ENDIF

         NEXT
      NEXT

      FOR nRow := 1 TO nLenRow
         FOR nCol := 1 TO nLenCol
            aGrid[ nRow, nCol ] := aTemp[ nRow, nCol ]
         NEXT
      NEXT

   ENDDO

   RETURN

 STATIC FUNCTION game_of_life_init()

   LOCAL quit := .F.
   LOCAL nMaxRow := MaxRow()
   LOCAL nMaxCol := MaxCol()
   LOCAL aGrid[ nMaxRow +1, nMaxCol +1 ]
   LOCAL nLenRow := Len( aGrid )
   LOCAL nLenCol := Len( aGrid[ 1 ] )
   LOCAL nRow, nCol
   LOCAL nMRow, nMCol

   hb_gtInfo( HB_GTI_WINTITLE, "Game of Life Init - P r e s s    s p a c e    t o    s t a r t" )

   WHILE( ! quit )

      hb_MousePos( @nMRow, @nMCol )

      DispBegin()
      Scroll()
      hb_DispOutAt( nMRow -1, nMCol -1, " ", 0x22 )

      FOR nRow := 1 TO nLenRow
         FOR nCol := 1 TO nLenCol
            IF aGrid[ nRow, nCol ] == 1
               hb_DispOutAt( nRow -1, nCol -1, " ", 0x22 )
            ENDIF
         NEXT
      NEXT
      DispEnd()

      SWITCH Inkey( 0 )

         CASE K_SPACE
            quit := .T.
            EXIT

         CASE K_LBUTTONDOWN

            aGrid[ nMRow, nMCol ] := 1
            EXIT

         CASE K_RBUTTONDOWN

            WHILE( K_RBUTTONUP == Inkey( 0 ) )
               hb_DispOutAt( nMRow -1, nMCol -1, " ", 0x00 )
               aGrid[ nMRow, nMCol ] := NIL
            ENDDO
            EXIT

      ENDSWITCH

   ENDDO

   RETURN aGrid

STATIC PROCEDURE game_of_life_welcome_screen()

   LOCAL lContinue := .T.
   LOCAL nMaxRow := 0, nMaxCol := 0
   LOCAL cLine
   LOCAL nRow
   LOCAL nMRow, nMCol

   LOCAL aLogo := { ;
   "    __  ____       ______                              ____   __    _ ____   ", ;
   "   / / / / /_     / ____/___ _____ ___  ___     ____  / __/  / /   (_) __/__ ", ;
   "  / /_/ / __ \   / / __/ __ `/ __ `__ \/ _ \   / __ \/ /_   / /   / / /_/ _ \", ;
   " / __  / /_/ /  / /_/ / /_/ / / / / / /  __/  / /_/ / __/  / /___/ / __/  __/", ;
   "/_/ /_/_.___/   \____/\__,_/_/ /_/ /_/\___/   \____/_/    /_____/_/_/  \___/ " }

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

         hb_DispOutAt( nRow + 16, 0,  PadC( "Copyright 2017-2023 Rafał Jopek", nMaxCol ), 0x8 )
         DispEnd()

      ENDIF

      nMRow := MRow()
      nMCol := MCol()

      hb_MousePos( nMRow, nMCol )

      hb_DispOutAt( nRow +  8, 0,  PadC( "Play games", nMaxCol ), 0x6 )
      hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0x6 )

      IF nMRow == nRow + 8 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
         hb_DispOutAt( nRow + 8, 0,  PadC( "Play games", nMaxCol ), 0xa )
      ENDIF

      IF nMRow == nRow + 18 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
         hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0xa )
      ENDIF

      SWITCH Inkey( 0 )

      CASE K_ENTER
         lContinue := .F.
         EXIT

      CASE K_LBUTTONDOWN

         IF nMRow == nRow + 8 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            lContinue := .F.
         ENDIF

         IF nMRow == nRow + 18 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_run( "start " + "https://harbour.pl/" )
         ENDIF
         EXIT

      CASE K_MOUSEMOVE

         IF nMRow == nRow + 8 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_DispOutAt( nRow + 8, 0,  PadC( "Play games", nMaxCol ), 0xa )
         ENDIF

         IF nMRow == nRow + 18 .AND. nMCol >= Int( ( nMaxCol - 10 ) / 2 ) .AND. nMCol <= Int( ( nMaxCol + 10 ) / 2 ) - 1
            hb_DispOutAt( nRow + 18, 0,  PadC( "harbour.pl", nMaxCol ), 0xa )
         ENDIF
         EXIT

      CASE HB_K_RESIZE
         EXIT

      ENDSWITCH

   ENDDO

   RETURN

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbgtcore.h" // hb_gt_Base e.t.c

typedef struct HB_ITEM HB_ITEM;

// hb_MousePos( @nRow, @nCol )
HB_FUNC( HB_MOUSEPOS )
{
   HB_GT_BASE *pGT;

   int iRow, iCol;
   pGT = hb_gt_Base();
   if( pGT )
   {
      HB_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
      hb_gt_BaseFree( pGT );
      hb_storni( iRow +1, 1 );
      hb_storni( iCol +1, 2 );
   }
}

#pragma ENDDUMP

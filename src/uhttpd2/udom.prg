#include 'hbclass.ch'

STATIC hProc := { => }
STATIC hMutexProc

CLASS UDom

   DATA cDlg
   DATA hPairs
   DATA aFiles
   DATA hDel
   DATA hResponse
   DATA hCfgSocket        INIT { 'init' => .F. }

   METHOD New( hPairs )       CONSTRUCTOR


   METHOD SetDlg( cDlg )
   METHOD SetScope( cDlg )    INLINE ::SetDlg( cDlg ) // Compatibility

   METHOD GetDlg()       INLINE if( hb_HHasKey( ::hPairs, 'dlg' ), ::hPairs[ 'dlg' ], '' )
   METHOD GetProc()       INLINE if( hb_HHasKey( ::hPairs, 'proc' ), ::hPairs[ 'proc' ], '' )
   METHOD GetTrigger()
   METHOD GetSocket()      INLINE if( hb_HHasKey( ::hPairs, '_socket' ), ::hPairs[ '_socket' ], '' )
   METHOD Get( cId )
   METHOD Set( cId, uValue )
   METHOD SetData( uData )
   METHOD SetTable( cId, cAction, uValue )
   METHOD GetAll()
   METHOD GetList( lWeb )
   METHOD Files()       INLINE ::aFiles

   METHOD Focus( cId )

   METHOD Visibility( cId, uValue )
   METHOD Show( cId )      INLINE ::Visibility( cId, .T. )
   METHOD Hide( cId )      INLINE ::Visibility( cId, .F. )
   METHOD Active( cId, lValue )
   METHOD Disable( cId )      INLINE ::Active( cId, .F. )
   METHOD Enable( cId )      INLINE ::Active( cId, .T. )
   METHOD Redirect( cProc, cType )

   METHOD SetClass( cId, cClass, uAction )

   METHOD SetUrl( cUrl, cTarget, cSpecs  )

   METHOD SetAlert( cMsg, cTitle )
   METHOD SetError( cError )
   METHOD SetConfirm( cMsg, cTitle )

   METHOD SetJS( cFunc, uData )
   METHOD SetScreen( cProc, cTarget, cId, cCargo )

   METHOD SetMsg( cMsg, cTitle )

   METHOD SetPanel( cId, cHtml )
   METHOD SetWindow( cUrl, cTarget, cSpecs )
   METHOD SetDialog( cId, cHtml, cTitle, cargo )

   METHOD DialogClose( cId )
   METHOD ResetFiles( cId )



   METHOD Send()

   METHOD Test( cId, cMethod, uValue )

// Tabulator messages parser

   METHOD TableInit( cId, aColumns, aData )
   METHOD TableSetData( cId, aData )
   METHOD TableAddData( cId, aData, lTop, nIndex )
   METHOD TableClean( cId )
   METHOD TablePrint( cId, par1, par2 )
   METHOD TableDelete( uId )
   METHOD TableUpdate( cId, aData )
   METHOD TableUpdateRow( cId, nIndex, hRow )
   METHOD TableClearEdited( cId )
   METHOD TableMessage( cId, cMsg )
   METHOD TableNavigateRight( cId )
   METHOD TableTitle( cId, cTitle )
   METHOD TableDestroy( cId )
   METHOD TableDownload( cId, cFormat, cFile )

// --------------------------------------------

   METHOD MsgApi( cApi, cProc, oNewPar, cIdForm )


// --------------------------------------------

   METHOD Console( uData )
   METHOD Trace()

   METHOD SendSocket( cType, cMsg )
   METHOD SendSocketJS( cFunction, uValue )
   METHOD UGetPSocket( pSocket )

   METHOD UGetInfoSocket()

   METHOD WSSetCargo( uCargo )
   METHOD WSGetCargo()

ENDCLASS

METHOD New( hPairs, aFiles ) CLASS UDom

   hb_default( @hPairs, { => } )

   ::hPairs   := hPairs
   ::aFiles   := aFiles
   ::hDel    := { => }
   ::hResponse   := { => }
   ::cDlg     := hb_HGetDef( hPairs, 'dlg', '' )

   RETU Self

METHOD SetDlg( cIdDlg ) CLASS UDom

   LOCAL cOldId := ::cDlg

   hb_default( @cIdDlg, '' )

   ::cDlg := cIdDlg

   RETU cOldId

METHOD GetTrigger() CLASS UDom

   LOCAL cId  := if( hb_HHasKey( ::hPairs, 'trigger' ), ::hPairs[ 'trigger' ], '' )
   LOCAL cTag  := ::cDlg + '-'
   LOCAL nPos

   IF !Empty( cId )

      IF ( nPos :=  At( cTag, cId ) ) > 0

         cId := SubStr( cId, nPOs + Len( cTag ) )

      ENDIF

   ENDIF

   RETU cId

METHOD Get( cId, uDefault ) CLASS UDom

   LOCAL uValue := ''

   hb_default( @uDefault, '' )

   cId := ::cDlg + '-' + cId

   hb_HCaseMatch( ::hPairs[ 'controls' ], .F. )

   IF  hb_HHasKey( ::hPairs[ 'controls' ], cId )
      uValue := ::hPairs[ 'controls' ][ cId ][ 'value' ]
   ELSE
      uValue := uDefault
   ENDIF

   RETU uValue

METHOD Set( cId, uValue, cargo ) CLASS UDom

   IF  ! hb_HHasKey( ::hResponse, 'setter' )
      ::hResponse[ 'setter' ] := {}
   ENDIF

   cId := ::cDlg + '-' + cId

   AAdd( ::hResponse[ 'setter' ], { 'id' => cId, 'value' => uValue, 'cargo' => cargo } )

   RETU NIL

// --------------------------------------------------

METHOD ResetFiles( cId ) CLASS UDom

   cId := '_' + ::cDlg + '-' + cId

   ::hResponse[ 'resetfiles' ] := cId

   RETU NIL
// --------------------------------------------------

METHOD Test( cId, cMethod, uValue )

   IF  ! hb_HHasKey( ::hResponse, 'test' )
      ::hResponse[ 'test' ] := {}
   ENDIF

   cId := ::cDlg + '-' + cId

   AAdd( ::hResponse[ 'test' ], { 'id' => cId, 'method' => cMethod, 'value' => uValue } )

   RETU NIL

// TABLE SYSTEM ------------------------------------------

METHOD SetTable( cId, cAction, uValue, cMsgConfirm )  CLASS UDom

   IF  ! hb_HHasKey( ::hResponse, 'table' )
      ::hResponse[ 'table' ] := {}
   ENDIF

   cId := ::cDlg + '-' + cId

   AAdd( ::hResponse[ 'table' ], { 'id' => cId, 'action' => cAction, 'value' => uValue, 'msgconfirm' => cMsgConfirm } )

   RETU NIL

// NEW ---------------------------------------------------

METHOD TableInit( cId, oOptions, aEvents, oFilter, cargo ) CLASS UDom

   LOCAL hParam := { => }

 /*
 if valtype( hPerformance ) == 'H'
  hPerformance[ 'id' ]  := ::cDlg + '-' + hPerformance[ 'id' ]
 endif
 */

   hParam[ 'options' ] := oOptions
   hParam[ 'events'  ] := aEvents
   hParam[ 'filter'  ] := oFilter
   hParam[ 'cargo'   ] := cargo

   ::SetTable( cId, 'init', hParam )

   RETU NIL

METHOD TableSetData( cId, aData ) CLASS UDom

   ::SetTable( cId, 'setData', { 'data' => aData } )


   RETU NIL

METHOD TableAddData( cId, aData, lTop, nIndex ) CLASS UDom

   ::SetTable( cId, 'addData', { 'data' => aData, 'top' => lTop, 'index' => nIndex } )

   RETU NIL

METHOD TableUpdate( cId, aData ) CLASS UDom

   IF ValType( aData ) != 'A'
      aData := Array( aData )
   ENDIF

   ::SetTable( cId, 'updateData', { 'data' => aData } )

   RETU NIL

METHOD TableUpdateRow( cId, nIndex, hRow ) CLASS UDom

   ::SetTable( cId, 'updateRow', { 'index' => nIndex, 'row' => hRow } )

   RETU NIL

METHOD TableClearEdited( cId ) CLASS UDom

   ::SetTable( cId, 'clearEdited' )

   RETU NIL

METHOD TableClean( cId ) CLASS UDom

   ::SetTable( cId, 'clean' )

   RETU NIL

METHOD TableDelete( cId, uId ) CLASS UDom

   IF  ! hb_HHasKey( ::hDel, cId )
      ::hDel[ cId ] := {}
   ENDIF

   IF AScan( ::hDel[ cId ], uId ) == 0
      AAdd( ::hDel[ cId ], uId )
   ENDIF

   RETU NIL

METHOD TablePrint( cId, par1, par2 ) CLASS UDom

   hb_default( @par1, .F. )
   hb_default( @par2, .T. )

   ::SetTable( cId, 'print', { 'par1' => par1, 'par2' => par2 } )

   RETU NIL

METHOD TableDownload( cId, cFormat, cFile ) CLASS UDom

   hb_default( @cFormat, '' )
   hb_default( @cFile, '' )

   ::SetTable( cId, 'download', { 'format' => cFormat, 'file' => cFile } )

   RETU NIL

METHOD TableNavigateRight( cId ) CLASS UDom

   ::SetTable( cId, 'TableNavigateRight' )

   RETU NIL

METHOD TableMessage( cId, cMsg ) CLASS UDom

   hb_default( @cMsg, '' )

   IF Empty( cMsg )
      ::SetTable( cId, 'clearAlert' )
   ELSE
      ::SetTable( cId, 'alert', { 'msg' => cMsg } )
   ENDIF

   RETU NIL

METHOD TableTitle( cId, cTitle ) CLASS UDom

   hb_default( @cTitle, '' )

   ::SetTable( cId, 'title', cTitle  )

   RETU NIL

METHOD TableDestroy( cId ) CLASS UDom

   ::SetTable( cId, 'destroy' )

   RETU NIL

// --------------------------------------------------

METHOD GetAll() CLASS UDom

   LOCAL hControls  := { => }
   LOCAL nLen    := Len( ::hPairs[ 'controls' ] )
   LOCAL n, cId, h, nPos

   FOR n = 1 TO nLen

      h := hb_HPairAt( ::hPairs[ 'controls' ], n )

      nPos := At( '-', h[ 1 ] )

      IF nPos > 0

         cId := SubStr( h[ 1 ], nPos + 1 )

         hControls[ cId ] := h[ 2 ]

      ENDIF

   NEXT

   RETU hControls

// --------------------------------------------------

METHOD GetList( lWeb ) CLASS UDom

   LOCAL nLen    := Len( ::hPairs[ 'controls' ] )
   LOCAL cTxt    := ''
   LOCAL n, cId, h, nPos, cSep

   hb_default( @lWeb, .T. )

   cSep := If( lWeb, '<br>', Chr( 13 ) + Chr( 10 ) )

   FOR n = 1 TO nLen

      h := hb_HPairAt( ::hPairs[ 'controls' ], n )

      nPos := At( '-', h[ 1 ] )

      IF nPos > 0

         cId := SubStr( h[ 1 ], nPos + 1 )

         cTxt += cId + ' => ' + hb_ValToStr( h[ 2 ][ 'value' ] ) + cSep

      ENDIF

   NEXT

   RETU cTxt

METHOD SetData( uData ) CLASS UDom

   ::hResponse[ 'data' ] := uData

   RETU NIL

METHOD Console( uData, cTitle ) CLASS UDom

   IF  ! hb_HHasKey( ::hResponse, 'console' )
      ::hResponse[ 'console' ] := {}
   ENDIF

   AAdd( ::hResponse[ 'console' ], { uData, cTitle } )

   RETU NIL

METHOD SetJS( cFunc, uData ) CLASS UDom

// ::hResponse[ 'js' ] := { 'func' => 'MyTestJS', 'data' => hData }
   ::hResponse[ 'js' ] := { 'func' => cFunc, 'data' => uData }

   RETU NIL

METHOD MsgApi( cApi, cProc, oNewPar, cIdForm ) CLASS UDom

   hb_default( @cApi, '' )
   hb_default( @cProc, '' )
   hb_default( @cIdForm, '' )

   IF Empty( cApi ) .OR. Empty( cProc )
      RETU NIL
   ENDIF

   ::hResponse[ 'msgapi' ] := { 'api' => cApi, 'proc' => cProc, 'param' => oNewPar, 'idform' => cIdForm }

   RETU NIL

METHOD Active( cId, lValue ) CLASS UDom

   lValue := if( ValType( lValue ) == 'L', lValue, .T. )

   IF  ! hb_HHasKey( ::hResponse, 'active' )
      ::hResponse[ 'active' ] := {}
   ENDIF

   cId := ::cDlg + '-' + cId

   AAdd( ::hResponse[ 'active' ], { 'id' => cId, 'value' => lValue } )

   RETU NIL

METHOD Visibility( cId, uValue ) CLASS UDom

   uValue := if( ValType( uValue ) == 'L', if( uValue, 'on', 'off' ), 'toggle' )

   IF  ! hb_HHasKey( ::hResponse, 'visibility' )
      ::hResponse[ 'visibility' ] := {}
   ENDIF

   cId := ::cDlg + '-' + cId

   AAdd( ::hResponse[ 'visibility' ], { 'id' => cId, 'action' => uValue } )

   RETU NIL

METHOD SetClass( cId, cClass, uAction ) CLASS UDom

   IF  ! hb_HHasKey( ::hResponse, 'class' )
      ::hResponse[ 'class' ] := {}
   ENDIF

   IF ValType( uAction ) == 'L'
      uAction := if( uAction, 'on', 'off' )
   ELSE
      uAction := if( ( ValType( uAction ) == 'C' .AND. Lower( uAction ) == 'toggle' ), uAction, '' )
   ENDIF

   cId := ::cDlg + '-' + cId


   AAdd( ::hResponse[ 'class' ], { 'id' => cId, 'class' => cClass, 'action' => uAction } )

   RETU NIL

METHOD Focus( cId ) CLASS UDom

   cId := ::cDlg + '-' + cId

   ::hResponse[ 'focus' ] := cId

   RETU NIL

METHOD DialogClose( cIdDlg ) CLASS UDom

   hb_default( @cIdDlg, ::cDlg )

   ::hResponse[ 'dialogclose' ] :=  { 'id' => cIdDlg }

   RETU NIL

METHOD SetScreen( cProc, cTarget, cId, cCargo ) CLASS UDom

   hb_default( @cTarget, '_blank' )
   hb_default( @cId, '' )
// hb_default( @cCargo, '' )

   ::hResponse[ 'screen' ] := { 'proc' => cProc, 'target' => cTarget, 'id' => cId, 'cargo' => cCargo }

   RETU NIL

METHOD SetDialog( cId, cHtml, cTitle, cargo ) CLASS UDom

   ::hResponse[ 'dialog' ] := { 'html' => cHtml, 'id' => cId, 'title' => cTitle, 'cargo' => cargo  }

   RETU NIL

METHOD SetPanel( cId, cHtml ) CLASS UDom

   hb_default( @cId, '' )
   hb_default( @cHtml, '' )

   IF Empty( cId )
      RETU NIL
   ENDIF

   ::hResponse[ 'panel' ] := { 'id' => cId, 'html' => cHtml }

   RETU NIL

METHOD SetWindow( cUrl, cTarget, cSpecs ) CLASS UDom

   hb_default( @cTarget, '_self' ) // '_self', _blank
   hb_default( @cSpecs, '' )

   ::hResponse[ 'window' ] := { 'url' => cUrl, 'target' => cTarget, 'specs' => cSpecs }

   RETU NIL

METHOD SetMsg( cHtml, cTitle, hOptions ) CLASS UDom

   hb_default( @cTitle, 'Information' )

   ::hResponse[ 'dialog' ] := { 'html' => cHtml, 'id' => NIL, 'title' => cTitle, 'cargo' => hOptions  }

   RETU NIL

METHOD SetUrl( cUrl, cTarget, cSpecs  ) CLASS UDom

   hb_default( @cTarget, '_self' ) // '_self', _blank
   hb_default( @cSpecs, '' )

   ::hResponse[ 'url' ] := { 'url' => cUrl, 'target' => cTarget, 'specs' => cSpecs }

   RETU NIL

METHOD SetError( cError, cTitle ) CLASS UDom

   hb_default( @cTitle, 'Error' )

   ::hResponse[ 'error' ]  := cError
   ::hResponse[ 'title' ]  := cTitle

   RETU NIL

METHOD SetAlert( cMsg, cTitle ) CLASS UDom

   hb_default( @cTitle, 'System' )

   ::hResponse[ 'alert' ]    := cMsg
   ::hResponse[ 'alert_title' ]  := cTitle

   RETU NIL

METHOD SetConfirm( cMsg, cTitle ) CLASS UDom

   hb_default( @cTitle, 'System' )

   ::hResponse[ 'confirm' ]  := cMsg
   ::hResponse[ 'title' ]   := cTitle

   RETU NIL


// Ha de ser redirect via Ajax !!!

METHOD Redirect( cProc, cType ) CLASS UDom

// ::hResponse[ 'redirect' ] := { 'proc' => cProc, 'type' => cType }
// ::hResponse[ 'redirect' ] := { 'url' => 'http://localhost:8001/splash' }
// ::hResponse[ 'redirect' ] := { 'url' => 'splash' }

// ::hResponse[ 'redirect' ] := { 'proc' => 'splash', 'type' => 'url' }


// Aixo ha de apuntar directament a fw_callscreen

   ::hResponse[ 'redirect' ] := { 'proc' => cProc, 'type' => cType }


   RETU NIL

METHOD Trace() CLASS UDom

   ::hResponse[ 'trace' ] := 'trace...'

   RETU NIL

METHOD Send() CLASS UDom

   LOCAL aHeader := UAddHeader()
   LOCAL nI, cId

   IF Len( ::hDel ) > 0

      FOR nI := 1 TO Len( ::hDel )
         cId := hb_HKeyAt( ::hDel, nI )
         ::SetTable( cId, 'deleteRow', { 'ids' => ::hDel[ cId ] } )
      NEXT

   ENDIF

   IF ( nI := AScan( aHeader, {| x | Upper( x[ 1 ] ) == Upper( 'Content-Type' ) } ) ) == 0
// UAddHeader("Content-Type", "application/x-www-form-urlencoded; charset=ISO-8859-1")
      UAddHeader( "Content-Type", "application/x-www-form-urlencoded" )
   ENDIF

// Por defecto usaremos ISO-8859-1, porque nuestras dbfs NO estan en UTF-8

   IF ( nI := AScan( aHeader, {| x | Upper( x[ 1 ] ) == Upper( 'charset' ) } ) ) == 0
// UAddHeader("charset", "ISO-8859-1")
      UAddHeader( "charset", "UTF-8" )
   ENDIF



   RETU if( !Empty( ::hResponse ), hb_jsonEncode( ::hResponse ), '' )

METHOD SendSocket( cMsg, cType, cTo ) CLASS UDom

   LOCAL aData   := { => }
   LOCAL c, hInfo, hSockets, nLen, n, aPair, cSocket

// hb_default( @cMsg,  '' )   // Casca si arriba un hash
   hb_default( @cType, 'msg' )
   hb_default( @cTo, '' )


   IF Empty( cMsg )
      RETU .F.
   ENDIF

   IF ! ::hCfgSocket[ 'init' ]

      hInfo := ::UGetInfoSocket()

      IF !Empty( hInfo )

         ::hCfgSocket[ 'init' ] := .T.
         ::hCfgSocket[ 'socket' ]  := hInfo[ 'pSocket' ]
         ::hCfgSocket[ 'ssl' ]  := hInfo[ 'pSSL' ]

      ENDIF

   ENDIF


   IF Empty( cTo )

      IF ::hCfgSocket[ 'init' ]

         aData[ 'type' ] := cType
         aData[ 'function' ] := ''
         aData[ 'value' ] := cMsg

         c := hb_jsonEncode( aData )


#ifndef NO_SSL
         MY_SSL_WRITE( ::hCfgSocket[ 'ssl' ], ::hCfgSocket[ 'socket' ], UMask( c ) )   // TIMEOUT ??
#else
         hb_socketSend( ::hCfgSocket[ 'socket' ], UMask( c ) )
#endif
      ENDIF

   ELSE

      cTo   := Lower( cTo )
      hSockets  := UWS_GetSockets()

      nLen := Len( hSockets )

      FOR  n := 1 TO nLen

         aPair := hb_HPairAt( hSockets, n )

         cSocket := aPair[ 1 ]
         hInfo   := aPair[ 2 ]

         IF cTo == '*' .OR. Lower( hInfo[ 'scope' ] ) == cTo

            aData[ 'type' ] := cType
            aData[ 'function' ] := ''
            aData[ 'value' ] := cMsg
            c := hb_jsonEncode( aData )

#ifndef NO_SSL
            MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )   // TIMEOUT ??
#else
            hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) )
#endif

         ENDIF

      NEXT

   ENDIF


   RETU NIL

METHOD SendSocketJS( cFunction, uValue, cTo ) CLASS UDom

   LOCAL aData   := { => }
   LOCAL hInfo, c, n, hSockets, nLen, aPair, cSocket

   hb_default( @cTo, '' )


   IF Empty( cTo )

      IF ! ::hCfgSocket[ 'init' ]

         hInfo := ::UGetInfoSocket()

         IF !Empty( hInfo )

            ::hCfgSocket[ 'init' ] := .T.
            ::hCfgSocket[ 'socket' ]  := hInfo[ 'pSocket' ]
            ::hCfgSocket[ 'ssl' ]  := hInfo[ 'pSSL' ]

         ENDIF

      ENDIF

      IF ::hCfgSocket[ 'init' ]

         aData[ 'type' ] := 'js'
         aData[ 'function' ] := cFunction
         aData[ 'value' ] := uValue
         c := hb_jsonEncode( aData )

#ifndef NO_SSL
         MY_SSL_WRITE( ::hCfgSocket[ 'ssl' ], ::hCfgSocket[ 'socket' ], UMask( c ) )   // TIMEOUT ??
#else
         hb_socketSend( ::hCfgSocket[ 'socket' ], UMask( c ) )
#endif
      ENDIF

   ELSE

      cTo   := Lower( cTo )
      hSockets  := UWS_GetSockets()


      nLen := Len( hSockets )

      FOR  n := 1 TO nLen

         aPair := hb_HPairAt( hSockets, n )

         cSocket := aPair[ 1 ]
         hInfo   := aPair[ 2 ]

         IF cTo == '*' .OR. Lower( hInfo[ 'scope' ] ) == cTo

            aData[ 'type' ] := 'js'
            aData[ 'function' ] := cFunction
            aData[ 'value' ] := uValue
            c := hb_jsonEncode( aData )

#ifndef NO_SSL
            MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )   // TIMEOUT ??
#else
            hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) )
#endif

         ENDIF

      NEXT

   ENDIF


   RETU NIL

METHOD UGetPSocket( pSocket ) CLASS UDom

   LOCAL lSocket  := .F.
   LOCAL aSockets  := UWS_GetSockets()
   LOCAL cSocket  := ::GetSocket()

   IF !Empty( cSocket )

      cSocket := UGetToken( cSocket )

      IF !Empty( cSocket )

         IF hb_HHasKey( aSockets, cSocket )

            pSocket := aSockets[ cSocket ][ 'pSocket' ]

            lSocket := ValType( pSocket ) == 'P'

         ENDIF

      ENDIF

   ENDIF

   RETU lSocket

METHOD UGetInfoSocket() CLASS UDom

   LOCAL lSocket  := .F.
   LOCAL aSockets  := UWS_GetSockets()
   LOCAL cSocket  := ::GetSocket()
   LOCAL hInfo   := { => }

   IF !Empty( cSocket )

      cSocket := UGetToken( cSocket )

      IF !Empty( cSocket )

         IF hb_HHasKey( aSockets, cSocket )

            hInfo := aSockets[ cSocket ]

         ENDIF

      ENDIF

   ENDIF

   RETU hInfo

// ----------------------------------------------- //

METHOD WSSetCargo( uValue ) CLASS UDom

   LOCAL cId := ::GetSocket()

   IF hMutexProc == NIL
      hMutexProc := hb_mutexCreate()
   ENDIF

   hb_mutexLock( hMutexProc )
   hProc[ cId ] := uValue
   hb_mutexUnlock( hMutexProc )

   RETU NIL

// ----------------------------------------------- //

METHOD WSGetCargo() CLASS UDom

   LOCAL cId := ::GetSocket()

   RETU hb_HGetDef( hProc, cId, '' )

// ----------------------------------------------- //

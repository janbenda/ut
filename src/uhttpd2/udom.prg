#include 'hbclass.ch'

static hProc := {=>}
static hMutexProc 

CLASS UDom	
	
	
	DATA cDlg 		
	DATA hPairs 		
	DATA aFiles
	DATA hDel 
	DATA hResponse 		
	DATA hCfgSocket 							INIT { 'init' => .f. }

	METHOD New( hPairs ) 						CONSTRUCTOR

	
	METHOD SetDlg( cDlg )
	METHOD SetScope( cDlg )				INLINE ::SetDlg( cDlg )	//	Compatibility
	
	METHOD GetDlg() 						INLINE if( hb_HHasKey( ::hPairs, 'dlg' ), ::hPairs[ 'dlg' ], '')
	METHOD GetProc() 						INLINE if( hb_HHasKey( ::hPairs, 'proc' ), ::hPairs[ 'proc' ], '')
	METHOD GetTrigger() 					
	METHOD GetSocket() 					INLINE if( hb_HHasKey( ::hPairs, '_socket' ), ::hPairs[ '_socket' ], '')
	METHOD Get( cId ) 
	METHOD Set( cId, uValue ) 
	METHOD SetData( uData ) 
	METHOD SetTable( cId, cAction, uValue ) 
	METHOD GetAll() 
	METHOD GetList( lWeb )	
	METHOD Files() 						INLINE ::aFiles					
	
	METHOD Focus( cId ) 
	
	METHOD Visibility( cId, uValue )
	METHOD Show( cId ) 					INLINE ::Visibility( cId, .T. )
	METHOD Hide( cId ) 					INLINE ::Visibility( cId, .F. )
	METHOD Active( cId, lValue ) 
	METHOD Disable( cId ) 					INLINE ::Active( cId, .F. )
	METHOD Enable( cId ) 					INLINE ::Active( cId, .T. )
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
	
	//	Tabulator messages parser

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
	
	//	--------------------------------------------	
	
	METHOD MsgApi( cApi, cProc, oNewPar, cIdForm )

	
	//	--------------------------------------------	
	
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

	hb_default( @hPairs, {=>} )	

	::hPairs			:= hPairs
	::aFiles			:= aFiles
	::hDel				:= {=>}
	::hResponse 		:= {=>}
	::cDlg 				:= HB_HGetDef( hPairs, 'dlg', '' )

RETU Self 


METHOD SetDlg( cIdDlg ) CLASS UDom

	LOCAL cOldId := ::cDlg
	
	hb_Default( @cIdDlg, '' )
	
	::cDlg := cIdDlg 

RETU cOldId

METHOD GetTrigger() CLASS UDom

	local cId 	:= if( hb_HHasKey( ::hPairs, 'trigger' ), ::hPairs[ 'trigger' ], '')
	local cTag 	:= ::cDlg + '-'
	local nPos
	
	if !empty( cId )
	
		if ( nPos :=  At( cTag, cId ) ) > 0
		
			cId := Substr( cId, nPOs + len(cTag) )
		
		endif
			
	endif

RETU cId

METHOD Get( cId, uDefault ) CLASS UDom

	local uValue := ''
	
	hb_default( @uDefault, '' )
	
	cId := ::cDlg + '-' + cId		

	HB_HCaseMatch( ::hPairs[ 'controls' ], .f. )
	
	if  HB_HHasKey( ::hPairs[ 'controls' ], cId )
		uValue := ::hPairs[ 'controls' ][ cId ][ 'value' ]
	else 
		uValue := uDefault
	endif		
	
RETU uValue



METHOD Set( cId, uValue, cargo ) CLASS UDom
	
	if  ! HB_HHasKey( ::hResponse, 'setter' )		
		::hResponse[ 'setter' ] := {}
	endif
	
	cId := ::cDlg + '-' + cId
	
	Aadd( ::hResponse[ 'setter' ], { 'id' => cId, 'value' => uValue, 'cargo' => cargo } )
	
RETU NIL

//	--------------------------------------------------

METHOD ResetFiles( cId ) CLASS UDom
	
	cId := '_' + ::cDlg + '-' + cId
	
	::hResponse[ 'resetfiles' ] := cId
	
RETU NIL
//	--------------------------------------------------


METHOD Test( cId, cMethod, uValue ) 

	if  ! HB_HHasKey( ::hResponse, 'test' )		
		::hResponse[ 'test' ] := {}
	endif
	
	cId := ::cDlg + '-' + cId
	
	Aadd( ::hResponse[ 'test' ], { 'id' => cId, 'method' => cMethod, 'value' => uValue } )
	
RETU NIL 

//	TABLE SYSTEM ------------------------------------------

METHOD SetTable( cId, cAction, uValue, cMsgConfirm )  CLASS UDom

	if  ! HB_HHasKey( ::hResponse, 'table' )		
		::hResponse[ 'table' ] := {}
	endif
	
	cId := ::cDlg + '-' + cId

	Aadd( ::hResponse[ 'table' ], { 'id' => cId, 'action' => cAction, 'value' => uValue, 'msgconfirm' => cMsgConfirm } )
		
RETU NIL   

//	NEW ---------------------------------------------------

METHOD TableInit( cId, oOptions, aEvents, oFilter, cargo ) CLASS UDom

	local hParam := {=>}

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

	if valtype( aData ) != 'A'
		aData := array( aData )
	endif

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

	if 	! HB_HHasKey( ::hDel, cId )
		::hDel[ cId ] := {}
	endif

	if Ascan( ::hDel[ cId ], uId ) == 0	
		Aadd( ::hDel[ cId ], uId )	
	endif

RETU NIL 

METHOD TablePrint( cId, par1, par2 ) CLASS UDom

	hb_default( @par1, .f. )
	hb_default( @par2, .t. )

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

	if empty( cMsg )
		::SetTable( cId, 'clearAlert' )
	else
		::SetTable( cId, 'alert', { 'msg' => cMsg } )
	endif

RETU NIL 

METHOD TableTitle( cId, cTitle ) CLASS UDom

	hb_default( @cTitle, '' )	

	::SetTable( cId, 'title', cTitle  )

RETU NIL 

METHOD TableDestroy( cId ) CLASS UDom

	::SetTable( cId, 'destroy' )

RETU NIL 

//	--------------------------------------------------

METHOD GetAll() CLASS UDom

	local hControls 	:= {=>}
	local nLen 			:= len( ::hPairs[ 'controls' ] )
	local n, cId, h, nPos	
	
	for n = 1 to nLen
	
		h := HB_HPairAt( ::hPairs[ 'controls' ], n )	

		nPos := AT( '-', h[1] )
		
		if nPos > 0
		
			cId := Substr( h[1], nPos+1 )
			
			hControls[ cId ] := h[2]
			
		endif
		
	next				

RETU hControls

//	--------------------------------------------------

METHOD GetList( lWeb ) CLASS UDom
	
	local nLen 			:= len( ::hPairs[ 'controls' ] )
	local cTxt 			:= ''
	local n, cId, h, nPos, cSep 	
	
	hb_default( @lWeb, .t. )
	
	cSep := If( lWeb, '<br>', chr(13)+chr(10) )
	
	for n = 1 to nLen
	
		h := HB_HPairAt( ::hPairs[ 'controls' ], n )	

		nPos := AT( '-', h[1] )
		
		if nPos > 0
		
			cId := Substr( h[1], nPos+1 )
			
			cTxt += cId + ' => ' + HB_VALTOSTR( h[2][ 'value' ] ) + cSep						
			
		endif
		
	next				

RETU cTxt


METHOD SetData( uData ) CLASS UDom
	
	::hResponse[ 'data' ] := uData
	
RETU NIL  

METHOD Console( uData, cTitle ) CLASS UDom

		
	if  ! HB_HHasKey( ::hResponse, 'console' )		
		::hResponse[ 'console' ] := {}
	endif	
	
	Aadd( ::hResponse[ 'console' ], { uData, cTitle } )
	
RETU NIL  

METHOD SetJS( cFunc, uData ) CLASS UDom
		
	
	//::hResponse[ 'js' ] := { 'func' => 'MyTestJS', 'data' => hData }		
	::hResponse[ 'js' ] := { 'func' => cFunc, 'data' => uData }		
	
RETU NIL  

METHOD MsgApi( cApi, cProc, oNewPar, cIdForm ) CLASS UDom			

	hb_default( @cApi, '' )
	hb_default( @cProc, '' )
	hb_default( @cIdForm, '' )
	
	if empty( cApi ) .or. empty( cProc )
		retu nil
	endif
	
	::hResponse[ 'msgapi' ] := { 'api' => cApi, 'proc' => cProc, 'param' => oNewPar, 'idform' => cIdForm }		
	
RETU NIL  

METHOD Active( cId, lValue ) CLASS UDom

	lValue := if( valtype( lValue ) == 'L', lValue, .t. )
	
	if  ! HB_HHasKey( ::hResponse, 'active' )		
		::hResponse[ 'active' ] := {}
	endif
	
	cId := ::cDlg + '-' + cId
	
	Aadd( ::hResponse[ 'active' ], { 'id' => cId, 'value' => lValue } )
	
RETU NIL 

METHOD Visibility( cId, uValue ) CLASS UDom


	uValue := if( valtype( uValue ) == 'L', if( uValue, 'on', 'off'), 'toggle' )
	
	if  ! HB_HHasKey( ::hResponse, 'visibility' )		
		::hResponse[ 'visibility' ] := {}
	endif
	
	cId := ::cDlg + '-' + cId				
	
	Aadd( ::hResponse[ 'visibility' ], { 'id' => cId, 'action' => uValue } )
	
RETU NIL 

METHOD SetClass( cId, cClass, uAction ) CLASS UDom


	if  ! HB_HHasKey( ::hResponse, 'class' )		
		::hResponse[ 'class' ] := {}
	endif
	
	if valtype(uAction) == 'L'
		uAction := if( uAction, 'on', 'off' )
	else	
		uAction := if( ( valtype(uAction) == 'C' .and. lower( uAction ) == 'toggle' ), uAction, '' )	
	endif 
	
	cId := ::cDlg + '-' + cId
	
	
	Aadd( ::hResponse[ 'class' ], { 'id' => cId, 'class' => cClass, 'action' => uAction } )
	
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
	//hb_default( @cCargo, '' )

	::hResponse[ 'screen' ] := { 'proc' => cProc, 'target' => cTarget, 'id' => cId, 'cargo' => cCargo }

retu nil 

METHOD SetDialog( cId, cHtml, cTitle, cargo ) CLASS UDom			

	::hResponse[ 'dialog' ] := { 'html' => cHtml, 'id' => cId, 'title' => cTitle, 'cargo' => cargo  }

retu nil

METHOD SetPanel( cId, cHtml ) CLASS UDom

	hb_default( @cId, '' )		
	hb_default( @cHtml, '' )
	
	if empty( cId )
		retu nil 
	endif
	
	::hResponse[ 'panel' ] := { 'id' => cId, 'html' => cHtml }

RETU NIL  

METHOD SetWindow( cUrl, cTarget, cSpecs ) CLASS UDom

	hb_default( @cTarget, '_self' )	//	'_self', _blank 
	hb_default( @cSpecs, '' )
	
	::hResponse[ 'window' ] := { 'url' => cUrl, 'target' => cTarget, 'specs' => cSpecs }

RETU NIL 

METHOD SetMsg( cHtml, cTitle, hOptions ) CLASS UDom	

	hb_default( @cTitle, 'Information' )

	::hResponse[ 'dialog' ] := { 'html' => cHtml, 'id' => nil , 'title' => cTitle, 'cargo' => hOptions  }

retu nil 



METHOD SetUrl( cUrl, cTarget, cSpecs  ) CLASS UDom

	hb_default( @cTarget, '_self' )	//	'_self', _blank 
	hb_default( @cSpecs, '' )
	
	::hResponse[ 'url' ] := { 'url' => cUrl, 'target' => cTarget, 'specs' => cSpecs }

RETU NIL 

METHOD SetError( cError, cTitle ) CLASS UDom
	
	hb_default( @cTitle, 'Error' )
	
	::hResponse[ 'error' ] 	:= cError
	::hResponse[ 'title' ] 	:= cTitle

RETU NIL 

METHOD SetAlert( cMsg, cTitle ) CLASS UDom

	hb_default( @cTitle, 'System' )
	
	::hResponse[ 'alert' ] 			:= cMsg
	::hResponse[ 'alert_title' ] 	:= cTitle

RETU NIL 

METHOD SetConfirm( cMsg, cTitle ) CLASS UDom

	hb_default( @cTitle, 'System' )
	
	::hResponse[ 'confirm' ] 	:= cMsg
	::hResponse[ 'title' ] 		:= cTitle

RETU NIL 


//	Ha de ser redirect via Ajax !!!

METHOD Redirect( cProc, cType ) CLASS UDom
	
	//::hResponse[ 'redirect' ] := { 'proc' => cProc, 'type' => cType }	
	//::hResponse[ 'redirect' ] := { 'url' => 'http://localhost:8001/splash' }	
	//::hResponse[ 'redirect' ] := { 'url' => 'splash' }	
	
	//::hResponse[ 'redirect' ] := { 'proc' => 'splash', 'type' => 'url' }	
	
	
	// Aixo ha de apuntar directament a fw_callscreen
	
	::hResponse[ 'redirect' ] := { 'proc' => cProc, 'type' => cType }	


RETU NIL 

METHOD Trace() CLASS UDom
	
	::hResponse[ 'trace' ] := 'trace...'
	
RETU NIL 

METHOD Send() CLASS UDom

	LOCAl aHeader := UAddHeader()
	LOCAL nI, cId

	if len( ::hDel ) > 0 
	
		for nI := 1 to len(::hDel) 
			cId := HB_HKeyAt( ::hDel, nI ) 
			::SetTable( cId, 'deleteRow', { 'ids' => ::hDel[ cId ] } )
		next 

	endif	

	IF (nI := ASCAN(aHeader, {|x| UPPER(x[1]) == UPPER( 'Content-Type' ) })) == 0
		//UAddHeader("Content-Type", "application/x-www-form-urlencoded; charset=ISO-8859-1")
		UAddHeader("Content-Type", "application/x-www-form-urlencoded")
	ENDIF

	//	Por defecto usaremos ISO-8859-1, porque nuestras dbfs NO estan en UTF-8
	
	IF (nI := ASCAN(aHeader, {|x| UPPER(x[1]) == UPPER( 'charset' ) })) == 0
//		UAddHeader("charset", "ISO-8859-1")
		UAddHeader("charset", "UTF-8")
	ENDIF	
	


RETU if( !empty( ::hResponse ), hb_jsonencode( ::hResponse ), '' ) 


METHOD SendSocket( cMsg, cType, cTo ) CLASS UDom


	local aData  	:= {=>}
	local c, hInfo, hSockets, nLen, n, aPair, cSocket

	
	//hb_default( @cMsg,  '' )			// Casca si arriba un hash
	hb_default( @cType, 'msg' )
	hb_default( @cTo, '' )


	if empty( cMsg )
		retu .f.
	endif
	
	if ! ::hCfgSocket[ 'init' ] 
	
		hInfo := ::UGetInfoSocket()
		
		if !empty( hInfo )
		
			::hCfgSocket[ 'init' ] := .T.
			::hCfgSocket[ 'socket' ] 	:= hInfo[ 'pSocket' ]		
			::hCfgSocket[ 'ssl' ] 	:= hInfo[ 'pSSL' ]				
		
		endif 
	
	endif 
	

	if empty( cTo )	
	
		if ::hCfgSocket[ 'init' ]		
			
			aData[ 'type' ] := cType
			aData[ 'function' ] := ''
			aData[ 'value' ] := cMsg 
		
			c := hb_jsonencode( aData )	
		

			#ifndef NO_SSL					
				MY_SSL_WRITE( ::hCfgSocket[ 'ssl' ], ::hCfgSocket[ 'socket' ], UMask( c ) )			//TIMEOUT ??
			#else
				hb_socketSend( ::hCfgSocket[ 'socket' ], UMask( c ) ) 		
			#endif
		endif 
		
	else
	
		cTo 		:= lower( cTo )
		hSockets 	:= UWS_GetSockets()			
	
		nLen := len( hSockets )
		
		for  n := 1 to nLen 
	
			aPair := HB_HPairAt( hSockets, n )
			
			cSocket := aPair[1]
			hInfo   := aPair[2]
			
			if cTo == '*' .or. lower( hInfo[ 'scope' ] ) == cTo 							
			
				aData[ 'type' ] := cType
				aData[ 'function' ] := ''
				aData[ 'value' ] := cMsg
				c := hb_jsonencode( aData )					
				
				#ifndef NO_SSL					
					MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )			//TIMEOUT ??
				#else		
					hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) ) 		
				#endif

			endif
			
		next

	endif


RETU NIL 

METHOD SendSocketJS( cFunction, uValue, cTo ) CLASS UDom

	local aData  	:= {=>}
	local hInfo, c, n, hSockets, nLen, aPair, cSocket

	hb_default( @cTo, '' )


	if empty( cTo )

		if ! ::hCfgSocket[ 'init' ] 
		
			hInfo := ::UGetInfoSocket()
			
			if !empty( hInfo )
			
				::hCfgSocket[ 'init' ] := .T.
				::hCfgSocket[ 'socket' ] 	:= hInfo[ 'pSocket' ]		
				::hCfgSocket[ 'ssl' ] 	:= hInfo[ 'pSSL' ]				
			
			endif 
		
		endif 					
		
		if ::hCfgSocket[ 'init' ]

			aData[ 'type' ] := 'js'
			aData[ 'function' ] := cFunction 
			aData[ 'value' ] := uValue
			c := hb_jsonencode( aData )			
			
			#ifndef NO_SSL					
				MY_SSL_WRITE( ::hCfgSocket[ 'ssl' ], ::hCfgSocket[ 'socket' ], UMask( c ) )			//TIMEOUT ??
			#else		
				hb_socketSend( ::hCfgSocket[ 'socket' ], UMask( c ) ) 		
			#endif
		endif 
	
	else 
	
		cTo 		:= lower( cTo )
		hSockets 	:= UWS_GetSockets()		
	
		
		nLen := len( hSockets )
		
		for  n := 1 to nLen 
	
			aPair := HB_HPairAt( hSockets, n )
			
			cSocket := aPair[1]
			hInfo   := aPair[2]
			
			if cTo == '*' .or. lower( hInfo[ 'scope' ] ) == cTo 							
			
				aData[ 'type' ] := 'js'
				aData[ 'function' ] := cFunction 
				aData[ 'value' ] := uValue
				c := hb_jsonencode( aData )					
				
				#ifndef NO_SSL					
					MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )			//TIMEOUT ??
				#else		
					hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) ) 		
				#endif

			endif
			
		next				
	
	endif


RETU NIL 


METHOD UGetPSocket( pSocket ) CLASS UDom

	local lSocket 	:= .f. 
	local aSockets 	:= UWS_GetSockets()
	local cSocket 	:= ::GetSocket() 	

	
	if !empty( cSocket )
		
		cSocket := UGetToken( cSocket )
		
		if !empty( cSocket )
		
			if HB_HHasKey( aSockets, cSocket )
			
				pSocket := aSockets[ cSocket ][ 'pSocket' ]
				
				lSocket := valtype(pSocket) == 'P'
			
			endif
		
		endif 		
		
	endif

retu lSocket

METHOD UGetInfoSocket() CLASS UDom

	local lSocket 	:= .f. 
	local aSockets 	:= UWS_GetSockets()
	local cSocket 	:= ::GetSocket() 	
	local hInfo 		:= {=>}
	
	if !empty( cSocket )
		
		cSocket := UGetToken( cSocket )
		
		if !empty( cSocket )
		
			if HB_HHasKey( aSockets, cSocket )
			
				hInfo := aSockets[ cSocket ]								
			
			endif
		
		endif 		
		
	endif

retu hInfo

// ----------------------------------------------- //

METHOD WSSetCargo( uValue ) CLASS UDom

	local cId := ::GetSocket()
	
	if hMutexProc == NIL
		hMutexProc	:= HB_MutexCreate()
	endif

	hb_mutexLock( hMutexProc )
		hProc[ cId ] := uValue
	hb_mutexUnlock( hMutexProc )

retu nil 

// ----------------------------------------------- //

METHOD WSGetCargo() CLASS UDom

	local cId := ::GetSocket()

retu HB_HGetDef( hProc, cId, '' )

// ----------------------------------------------- //
#include 'hbclass.ch'
#include 'common.ch'

STATIC nSessions_Executed := 1
STATIC hmtxSession 

THREAD STATIC oSession

//	---------------------------------------------------------------------- //

function USessionValid()
retu ValType( oSession ) == 'O' .and. oSession:ClassName() == 'USESSIONS'

//	---------------------------------------------------------------------- //

function USessionStart()

	if USessionValid()
		retu .t.	
	endif	

	oSession 		:= USessions():New()

	oSession:Init()	

retu nil

//	---------------------------------------------------------------------- //

function USessionReady()

	local lValidate := .f. 
	
	if USessionValid()
		retu .t.	
	endif	
	
	oSession	:= USessions():New()
	
	lValidate	:= oSession:ReadCookie()

	if ! lValidate 
		oSession:End()
		oSession := NIL 
	endif

retu lValidate 

//	---------------------------------------------------------------------- //

function USessionEnd()	

	local oServer

	if USessionReady()
		oSession:End()	
	else
	
	//	If exist a cookie, we'll delete it
	
		oServer 	:= UGetServer()
	
		if ! empty( oServer:cSessionName )
			USetCookie( oServer:cSessionName, '', -1 )
		endif
		
	endif	
	
	oSession := NIL
	
retu nil

//	---------------------------------------------------------------------- //

function USessionWrite()

	if USessionValid()
		oSession:Write()
	endif
	
retu nil

//	---------------------------------------------------------------------- //

function USession( cKey, uValue )		

	if USessionValid()
		retu oSession:Data( cKey, uValue )		
	endif	
	
retu ''

//	---------------------------------------------------------------------- //

function USessionAll()		

	if USessionValid()
		retu oSession:AllData()		
	endif	

retu nil

//	---------------------------------------------------------------------- //

function USessionEmpty()		

	if USessionValid()
		retu empty( oSession:AllData() )		
	endif
	
	UDo_Error( 'System Sessions not created!', nil, 100 )	

retu nil

//	---------------------------------------------------------------------- //

function UGetSession() 

	local hInfo := {=>}

	if USessionValid()
	
		hInfo[ 'path' ] 		:= oSession:cPath 	
		hInfo[ 'name' ] 		:= oSession:cName		
		hInfo[ 'prefix' ] 		:= oSession:cPrefix 	
		
		//hInfo[ 'seed' ] 		:= oSession:cSeed 	
		
		hInfo[ 'duration' ]		:= oSession:nDuration
		hInfo[ 'expired' ]		:= oSession:hSession[ 'expired' ]
		hInfo[ 'ip' ]			:= oSession:hSession[ 'ip' ]
		hInfo[ 'sid' ]			:= oSession:hSession[ 'sid' ]
		//hInfo[ 'sid' ] 			:= oSession:cSID				
		hInfo[ 'garbage' ] 		:= oSession:nGarbage 	
		hInfo[ 'lifedays' ] 	:= oSession:nLifeDays	
		hInfo[ 'crypt' ] 		:= oSession:lCrypt	
		
	endif
	
retu hInfo

//	---------------------------------------------------------------------- //

function USessionDelete()

	oSession := NIL 

retu nil

//	---------------------------------------------------------------------- //


//	---------------------------------------------------------------------- //
//	Sessions Class for UHttpd2
//	---------------------------------------------------------------------- //


CLASS USessions 

	DATA cPath						
	DATA cName							
	DATA cPrefix							
	DATA nDuration				
	DATA nGarbage					
	DATA nLifeDays					
	DATA hSession 					INIT {=>}
	DATA cSID						INIT ''
	
	DATA cSeed						
	DATA lCrypt						INIT .F.		
	
	METHOD New()
	
	METHOD Init() 
	METHOD InitData( cSID ) 
	
	METHOD ReadCookie()
	METHOD Validate()	
	METHOD Write()
	METHOD Data( cKey, uValue ) 	
	METHOD AllData()				INLINE if( hb_HHasKey( ::hSession, 'data' ), ::hSession[ 'data'   ], {=>} )
		
	METHOD NewId() 				INLINE hb_MD5( DToS( Date() ) + Time() + Str( hb_Random(), 15, 12 ) )
	METHOD SessionFile()			INLINE if( empty( ::cSID ), '', ::cPath + '\' + ::cPrefix + ::cSID )

	METHOD Garbage()
	
	METHOD End() 
	
ENDCLASS 

//	---------------------------------------------------------------------- //

METHOD New() CLASS USessions

	local oServer 	:= UGetServer()

	::cPath 		:= oServer:cSessionPath			//	Default path session ./sessions
	::cName			:= oServer:cSessionName			//	Default session name USESSID
	::cPrefix 		:= oServer:cSessionPrefix			//	Default prefix sess_
	::cSeed 		:= oServer:cSessionSeed			//	Password
	
	::nDuration 	:= oServer:nSessionDuration		//	Default duration session time 3600
	::nGarbage 	:= oServer:nSessionGarbage		//	Default totals sessions executed for garbage 1000
	::nLifeDays		:= oServer:nSessionLifeDays		//	Default days stored for garbage 3
	::lCrypt		:= oServer:lSessionCrypt			//	Default crypt session .F.
	::cSID			:= ''				
	
	
	if hmtxSession == NIL
		hmtxSession	:= hb_mutexCreate()	
	endif 
	
retu Self

//	---------------------------------------------------------------------- //

METHOD Init() CLASS USessions
	
	local lNew 		:= .T.
	local cFile
	
	::cSID	:= UGetCookie( ::cName )

	if ! empty( ::cSID )

		cFile := ::cPath + '\' + ::cPrefix + ::cSID 
	
		lNew := if( ::Validate(), .F., .T. )	
		
		if lNew 	//	Try to delte old session		

			if file( cFile )			
				fErase( cFile )
			endif
			
		endif

	endif
	
	if lNew 
	
		::InitData()
	
	endif 	

retu nil

//	---------------------------------------------------------------------- //

METHOD ReadCookie() CLASS USessions

	::cSID := UGetCookie( ::cName ) 
	
	if empty( ::cSID )
		retu .f.
	endif	

retu ::Validate()

//	---------------------------------------------------------------------- //

METHOD Validate() CLASS USessions

	local lValidate := .f.
	local cFile, cSession	
	
	cFile := ::SessionFile()

	if ! file( cFile )
		retu .f.
	endif
	
	cSession := hb_Base64Decode( hb_Memoread( cFile ) )		
	
	if empty( cSession ) 
		retu .f.
	endif

	if ::lCrypt
		cSession := hb_blowfishDecrypt( hb_blowfishKey( ::cSeed ), cSession )					
	endif	

	::hSession := hb_deserialize( cSession )						

	if Valtype( ::hSession ) == 'H' 	
	
		//	Validaremos estructura
		
		if ( 	hb_HHasKey( ::hSession, 'ip'   	) .and. ;
				hb_HHasKey( ::hSession, 'sid'  	) .and. ;
				hb_HHasKey( ::hSession, 'expired' ) .and. ;
				hb_HHasKey( ::hSession, 'data' 	) )												
		
			if  ::hSession[ 'expired' ] >= seconds()  .and. ;
				::hSession[ 'ip' ] == UGetIp()
			
				lValidate 	:= .T.	
	
			endif							

		endif	
	
	endif	

RETU lValidate

//	---------------------------------------------------------------------- //

METHOD InitData( cSID ) CLASS USessions

	hb_default( @cSID, ::NewId() )

	::hSession := { => }
	
	::hSession[ 'ip'     ] := UGetIp()			//	La Ip no es fiable. Pueden usar proxy
	::hSession[ 'sid'    ] := cSID
	::hSession[ 'expired'] := seconds() + ::nDuration
	::hSession[ 'data'   ] := { => }	
	
	::cSID := cSID

retu nil

//	---------------------------------------------------------------------- //

METHOD End() CLASS USessions

	local cFile 

	if ! empty( ::cName )
		USetCookie( ::cName, '', -1 )
	endif

	if empty( ::cSID )
		retu .f.
	endif	

	cFile := ::SessionFile()	


	if File( cFile )
		fErase( cFile ) 
	endif 			

	::hSession  	:= {=>}
	::cSID			:= ''	
	::cName 		:= ''
			
retu nil

//	---------------------------------------------------------------------- //


METHOD Write() CLASS USessions

	local cData, cKey, lSave
	local lGarbage := .f.

		::hSession[ 'expired' ] 	:= seconds() + ::nDuration		
	
		cData 	:= hb_serialize( ::hSession )

		if ::lCrypt
			cKey 		:= hb_blowfishKey( ::cSeed )	
			cData 		:= hb_blowfishEncrypt( cKey, cData )	
		endif		

		lSave := hb_memowrit( ::SessionFile(), hb_Base64Encode( cData ) ) 
		
	

	// 	GARBAGE 
	
		hb_mutexLock(hmtxSession)
		
			nSessions_Executed++
			
			if nSessions_Executed > ::nGarbage		
				nSessions_Executed 	:= 0
				lGarbage				:= .t.
			endif

		hb_mutexUnlock(hmtxSession)		

		if lGarbage
			::Garbage()
		endif
		
	//	-------------------------
		
	USetCookie( ::cName, ::cSID, 0 )	//	Session live like browser	
	
retu nil

//	---------------------------------------------------------------------- //


METHOD Garbage() CLASS USessions

	local aFiles 	:= Directory( ::cPath + '/*.*' )
	local nFiles 	:= len( aFiles )
	local nSFiles 	:= 0
	local nSize 	:= 0
	local nTime 	:= hb_milliseconds()
	local nI, dMaxDate
	
	dMaxDate := date() - ::nLifeDays
	
	_d( 'Init Garbage Procces! => ' + dtoc(dMaxDate) )
	
	for nI := 1 to nFiles 
	
		if aFiles[nI][3] <= dMaxDate 			
			
			if fErase( ::cPath + '/' + aFiles[nI][1] ) == 0
				nSFiles++
				nSize 	+= aFiles[nI][2]
			endif
			
		endif
	next	
	
	_d( '=====================================' )
	_d( 'Session files deleted: ' 		+ ltrim(str( nSFiles )) )
	_d( 'Session size deleted: ' 		+ ltrim(str( nSize )) + ' kb.' )
	_d( 'Time proccess for garbage: ' 	+ ltrim(str( hb_milliseconds() - nTime )) + ' ms.' )
	_d( '=====================================' )
	
	
retu nil

//	---------------------------------------------------------------------- //

METHOD Data( cKey, uValue )  CLASS USessions

	hb_default( @cKey, '' )
	
	cKey := lower( cKey )		
	
	if ValType( uValue ) <> 'U' 		//	SETTER

		if !empty( cKey ) 						
			
			::hSession[ 'data' ][ cKey ] := uValue		
			
		endif
	
	else								// GETTER
		
		if ( hb_HHasKey( ::hSession[ 'data' ], cKey  ) )
			retu ::hSession[ 'data' ][ cKey ] 
		else
			retu ''
		endif
		
	endif 		

retu nil

//	---------------------------------------------------------------------- //


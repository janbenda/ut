#include "inkey.ch"
#include "hbclass.ch"
#include "hbsocket.ch"

#define UWS_VERSION		'1.00'

#xcommand CODE TO <var> [ PARAMS [<v1>] [,<vn>] ] => #pragma __stream|<var> += UReplaceBlocks( %s, '<$', "$>" [,<(v1)>][+","+<(vn)>] [, @<v1>][, @<vn>] )

#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY => ALWAYS

#xCommand _trace( [<uVar,...>] )  	=>	if( oWebServerSocket:lTrace,	_t( [<uVar>] ), nil )
#xCommand _traceSys( [<uVar,...>] )  =>	if( oWebServerSocket:lTraceSys,	_d( [<uVar>] ), nil )


#ifndef NO_SSL
	#include "hbssl.ch"
	#define IS_SSL 	.T.
	#define PORT   		8443
#else
	#define IS_SSL  	.F.
	#define PORT    	9000
#endif


#define TIMEOUT    50		//5000	
#define CRLF       Chr( 13 ) + Chr( 10 )

#define OPC_CONT   		0x00
#define OPC_TEXT   		0x01
#define OPC_BIN    		0x02
#define OPC_AUTH 			0x03
#define OPC_RECONNECT 		0x07
#define OPC_CLOSE  		0x08
#define OPC_PING   		0x09
#define OPC_PONG   		0x0A
#define OPC_ERROR 			0x0B

static oWebServerSocket

static hMutexUser 
static aSockets 


//----------------------------------------------------------------//

function UWebSocket()

	oWebServerSocket := UT_WebSocket():New()

retu oWebServerSocket

//----------------------------------------------------------------//

function UWS_Version()	; RETU UWS_VERSION 

//----------------------------------------------------------------//

CLASS UT_WebSocket 

	DATA cError					INIT ''
	DATA hInfoSSL					INIT {=>}
	DATA hSSLCtx
	DATA lInit						INIT .F.
	DATA bInit						INIT {|| Qout( 'WEBSOCKET Vrs. ' +  UWS_VERSION  ) }
	DATA hConfig					INIT {=>}
	DATA lTrace					INIT .F.
	DATA lTraceSys					INIT .F.
	DATA bValidate					INIT {|| .t. }
	DATA bMessage					INIT NIL
	
	METHOD New()    				CONSTRUCTOR
	METHOD Run()
	METHOD ValidConfig( hConfig )		
	
	METHOD SetAddress( cAddress )	INLINE ::hConfig[ 'address' ] := if( valtype( cAddress ) == 'C', cAddress, '0.0.0.0' )
	METHOD SetPort( nPort )			INLINE ::hConfig[ 'port' ] := if( valtype( nPort ) == 'N', nPort, if( IS_SSL, 8443 , 9000 ) )
 	METHOD SetSSL( lSSL )				INLINE ::hConfig[ 'ssl' ] := if( valtype( lSSL ) == 'L', lSSL, .F. )
	
 	METHOD SetTrace( lTrace )		INLINE ::lTrace := if( valtype( lTrace ) == 'L', lTrace, .F. )
 	METHOD SetTraceSys( lTrace )		INLINE ::lTraceSys := if( valtype( lTrace ) == 'L', lTrace, .F. )
	
	METHOD SetCertificate( PrivateKeyFilename, CertificateFilename )
	METHOD InfoSSL()					INLINE ::hInfoSSL

	
ENDCLASS

//----------------------------------------------------------------//

METHOD New() CLASS UT_WebSocket

	//	Init vars....
	
	::hConfig[ 'ssl' ] 	:= IS_SSL 
	::hConfig[ 'address'] 	:= '0.0.0.0' 
	::hConfig[ 'port' ]	:= IF( IS_SSL, 8443 , 9000 ) 

RETU SELF

//----------------------------------------------------------------//

METHOD SetCertificate( PrivateKeyFilename, CertificateFilename ) CLASS UT_WebSocket

	hb_default( @PrivateKeyFilename, 'cert/privatekey.pem' )
	hb_default( @CertificateFilename, 'cert/certificate.pem' )
	
	::hConfig[ 'PrivateKeyFilename' ] 	:= PrivateKeyFilename
	::hConfig[ 'CertificateFilename' ]	:= CertificateFilename
	
RETU NIL 

//----------------------------------------------------------------//

METHOD Run( hConfig ) CLASS UT_WebSocket

	local hListen, hSocket, lOk, nTries 
	
	_traceSys( '>> Run' )
	
	::ValidConfig( hConfig )				

	if ! hb_mtvm()
		::cError := "Multithread support required"
		_trace( ::cError )
		retu .f.
    endif  
	

	hMutexUser	:= HB_MutexCreate()  
	aSockets 	:= {=>}	
	
	#ifndef NO_SSL

		//SSL_INIT()		// Initiated en UHTTPD2
		
		::hSSLCtx := SSL_CTX_NEW(HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER)

		
		SSL_CTX_SET_OPTIONS( ::hSSLCtx, HB_SSL_OP_NO_TLSv1)		
		

		IF SSL_CTX_USE_PRIVATEKEY_FILE( ::hSSLCtx, ::hConfig[ 'PrivateKeyFilename' ], HB_SSL_FILETYPE_PEM) != 1
			::cError := "Invalid private key file: " + ::hConfig[ 'PrivateKeyFilename' ]
			_trace( ::cError )
			RETU .F.
		ENDIF

		

		IF SSL_CTX_USE_CERTIFICATE_FILE(::hSSLCtx, ::hConfig[ 'CertificateFilename' ], HB_SSL_FILETYPE_PEM) != 1
			::cError := "Invalid certificate file: " + ::hConfig[ 'CertificateFilename' ]
			_trace( ::cError )
			RETU .F.
		ENDIF	

		_traceSys( '>> Run > SSL OK' )

		
	#endif
	
	if Empty( hListen := hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, HB_SOCKET_IPPROTO_TCP ) )
		::cError := "Socket create error " + hb_ntos( hb_socketGetError() ) 
		_trace( ::cError )
		RETU .F.
	endif
	
	_traceSys( '>> Run > Listen OK')
	
	
	/*
	
		UWSVersion( UWS_VERSION )
		nTries := 0
		lOk := .f. 
		_d( 'Xec UServer...')
		
		
		while ! UIsRunning()
			nTries++
			_d( 'Esperant...' )
			if nTries > 5			
				retu .f.
			endif	
			hb_idleSleep( 0.1 )		
		end 
	*/
	
	if valtype( ::bInit ) == 'B'		
		//eval( ::bInit, ::aInfo )
		eval( ::bInit )
	endif				

	lOk := .f. 
	nTries := 0
	
	//_traceSys( '>> Run >> Init SocketBind > ' +  ::hConfig[ 'address'] + ' - ' + ltrim(str(::hConfig[ 'port' ])) )
	_traceSys( '>> Run >> Init SocketBind > ' +  '0.0.0.0' + ' - ' + ltrim(str(::hConfig[ 'port' ])) )
	
	while ! lOk
	

		//if ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, ::hConfig[ 'address'], ::hConfig[ 'port' ]  } )		
		if ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, '0.0.0.0', ::hConfig[ 'port' ]  } )		
		

			nTries++
			
			#ifndef NO_SSL	
				::cError := "Bind error " + hb_ntos( hb_socketGetError() )
//				_trace( ::cError )
			#endif						
			
			_traceSys( '>> Run > Attempt ' + ltrim(str( nTries ) ) + '. Error: ' + ::cError  )
			
		else 		

			lOk := .t. 
			
		endif		
		
		if nTries >= 5
			_traceSys( '>> Run > Max number attempts. Exit !' )			
			retu .f.
		endif	
	

		//if !lOk
			hb_idleSleep( 0.1 )			
		//endif
		
	end 	
	
	_traceSys( '>> Run > Binding done !' )

	if ! hb_socketListen( hListen )		
	
		#ifndef NO_SSL
			::cError :=  "Listen error " + hb_ntos( hb_socketGetError() )
			_trace( ::cError )
		#endif 
		
		RETU .F.			
	endif		
	
	//	Config UServer()	

		 USetServerInfo( 'websocket',  .t. )			
		 USetServerInfo( 'websocket_cfg', ::hConfig )		

		 
	_traceSys( '>> Run > Config UServerHttpd2 done' )
	
	ERRORBLOCK( {|o| _d( o )} )	

	_traceSys( '>> Run > Init loop accept socket...' )
	
	while .T.  		
 
		if Empty( hSocket := hb_socketAccept( hListen,, TIMEOUT ) )
		
			if hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
				// ? "loop"
			ELSE
				_trace( "Accept error " + hb_ntos( hb_socketGetError() ) )
			endif
			
		ELSE
			_trace( "Accept socket request"	 )
			hb_threadDetach( hb_threadStart( @ClientConnection(), hSocket, SELF ) )
		endif
		
		if Inkey() == K_ESC
			_trace( "Quitting - esc pressed" )
			EXIT
		endif	  	  
	  
    end

	_trace( "Close listening socket" )   
	
	hb_socketShutdown( hListen )
	hb_socketClose( hListen )		
	
RETU .T.

//----------------------------------------------------------------//

METHOD ValidConfig( hConfig ) CLASS UT_WebSocket

	local n, cKey, nLen	

	hb_default( @hConfig, {=>} )
	
	nLen := len( hConfig )
	
	HB_HCaseMatch( hConfig, .F. )

	for n := 1 to nLen
		
		cKey := HB_HKeyAt( ::hConfig, n )
		
		if HB_HHasKey( hConfig, cKey ) 
			::hConfig[ cKey ] := hConfig[ cKey ]
		endif
		
	next

	
	if ::hConfig[ 'ssl' ]
		::hConfig[ 'uri' ] 	:= 'wss://'
	else
		::hConfig[ 'uri' ] 	:= 'ws://'
	endif
	
	::hConfig[ 'uri' ] += if( ::hConfig[ 'address' ] == '0.0.0.0', 'localhost', ::hConfig[ 'address' ] ) + ':' + ltrim(str(::hConfig[ 'port' ]))

	_traceSys( '>> ValidConfig > ::hConfig',  ::hConfig )
	
RETU NIL

//----------------------------------------------------------------//
//	STATIC Functions ---------------------------------------------//
//----------------------------------------------------------------//

static function  ClientConnection( hSocket, oWS )

	local cRequest, cBuffer := Space( 4096 ), nLen, nOpcode
	local hSSL, cUser, cToken, hData
	local nErr, nTries
	local lExit := .T. 
	local nIni, nEnd, cParameters, cPart, hParameters, nI, lValidate
	local uValue, cType, cValType, lAuth, hClient, aI, cScope
	local hParClient := { 'scope' => '', 'token' => '' }

	
	#ifndef NO_SSL	
		local nTime
	#else
		local uDummy 
	#endif

	_traceSys( '>> ClienConnection' )
	
	#ifndef NO_SSL	

		hSSL := SSL_NEW( oWS:hSSLCtx )				

		SSL_SET_MODE(hSSL, hb_bitOr(SSL_GET_MODE(hSSL), HB_SSL_MODE_ENABLE_PARTIAL_WRITE))
		hb_socketSetBlockingIO(hSocket, .F.)
		SSL_SET_FD(hSSL, hb_socketGetFD(hSocket))
		

		//	Info SSL -------------------------------------------
		
			if empty( oWS:hInfoSSL )
				nErr := 0				
			
				_traceSys( '>> Run > Extract info SSL...' )

				oWS:hInfoSSL["SSL_CIPHER"] 				:= SSL_GET_CIPHER(hSSL)
				oWS:hInfoSSL["SSL_PROTOCOL"] 			:= SSL_GET_VERSION(hSSL)
				oWS:hInfoSSL["SSL_CIPHER_USEKEYSIZE"]	:= SSL_GET_CIPHER_BITS(hSSL, @nErr)
				oWS:hInfoSSL["SSL_CIPHER_ALGKEYSIZE"]	:= nErr
				oWS:hInfoSSL["SSL_VERSION_LIBRARY"] 	:= SSLEAY_VERSION(HB_SSLEAY_VERSION )
				oWS:hInfoSSL["SSL_SERVER_I_DN"] 		:= X509_NAME_ONELINE(X509_GET_ISSUER_NAME(SSL_GET_CERTIFICATE(hSSL)))
				oWS:hInfoSSL["SSL_SERVER_S_DN"] 		:= X509_NAME_ONELINE(X509_GET_SUBJECT_NAME(SSL_GET_CERTIFICATE(hSSL)))	
				
				_traceSys( oWS:hInfoSSL )
				
			endif
			
		//	----------------------------------------------------
	
        nTime := hb_MilliSeconds()
		nTries := 0
		
        WHILE .T.
            
            IF ( nErr := MY_SSL_ACCEPT( hSSL, hSocket, TIMEOUT ) ) == 0            

               EXIT
			   
            ELSE
			
				nTries++ 
				
				_traceSys( '> Attempt ' + ltrim(str( nTries )) + ',  Error: ' + ltrim(str( nErr ))  )
				
				if nTries >= 5

					_traceSys( '>> ClientConnection > Error , al carrer '   )
					
					IF nErr == HB_SOCKET_ERR_TIMEOUT			   
						
					  
						IF ( hb_MilliSeconds() - nTime ) > TIMEOUT
							
							_trace( "SSL accept timeout" )

							EXIT
						ENDIF
						
				   ELSE
		
						//Eval( oServer:hConfig[ "Trace" ], "SSL accept error:", nErr, hb_socketErrorString( nErr ) )

						EXIT
				   ENDIF
				   
				else
				   
                ENDIF
				
				hb_idleSleep( 0.1 )
			   
            ENDIF

        END				

		
		if nErr != 0	
		
			_traceSys( "> SSL accept error:" + ltrim(str(nErr)) + ' ' +  hb_socketErrorString( nErr ) + '. Exit!' )	

			ClientClose( hSocket, nil )

			retu nil 
		endif

		/* Intentaremos leer hasta 5 veces si no recibimos datos (nLen == -1 ) */

		nTries := 0
		
		_traceSys( '>> ClientConnection > Init loop socket for handshaking... ')
		
		
		while .t.
		
			nLen := MY_SSL_READ( hSSL, hSocket, @cBuffer, TIMEOUT , @nErr)


			if nLen > 0
				exit
			else

				if nErr != 0	
				
					_traceSys( '>> ClientConnection > nErr', nErr  )
	
					if nErr == 1001 

						ClientClose( hSocket, nil )		
						retu nil					
					else 
					
						nTries++ 						
						
						if nTries = 5		
							_traceSys( '>> ClientConnection > Max Tries'  )
							
							ClientClose( hSocket, nil )							
							retu nil 						
						else					
							hb_idleSleep( 0.1 )	
						endif
					endif
	
				else 
					exit 
				endif						
			endif						
			
		end

	#else

		uDummy := oWS:cError

		nTries := 0
		
		while .t.

			nLen := hb_socketRecv( hSocket, @cBuffer,,, TIMEOUT )

			
			if nLen > 0
				exit
			else
				nErr := hb_socketGetError()
				
				if nErr != 0
				
					_traceSys( '>> ClientConnection > nErr', nErr  )
					
					if nErr == 1001 

						ClientClose( hSocket, nil )											
						retu nil					
					else 
					
						nTries++ 						
						
						if nTries = 5	

							_traceSys( '>> ClientConnection > Max Tries'  )						
													
							ClientClose( hSocket, nil )							
							retu nil 						
						else					

							hb_idleSleep( 0.1 )	
						endif
					endif
	
				else 
					exit 
				endif								
			endif			
			
		end 
		
	#endif

	
	cBuffer := hb_BLeft( cBuffer, nLen )	//	 RTrim( cBuffer )
	
	
	//	HANDSHAKING ---------------------------------------

		UHandShaking( hSocket, cBuffer, hSSL )	
	
	
	//	Recover posible parameters ------------------------
	
		//	Check cBuffer := /?m=test&t=MyToken987 HTTP/1.1
		//	m = module
		//	t = token 
		
		nIni 			:= At( '/?', cBuffer )
		nEnd 			:= At( 'HTTP/1.1', cBuffer )
		hParameters 	:= {=>}				
		
		if nIni > 0 .and. nEnd > 0
		
			cParameters := Alltrim(Substr( cBuffer, nIni+2, (nEnd - nIni - 2 ) ))			
			
			FOR EACH cPart IN hb_aTokens(cParameters, "&")
				IF (nI := AT("=", cPart)) > 0
					hParameters[ UUrlDecode(LEFT( cPart, nI - 1)) ] := UUrlDecode(SUBSTR(cPart, nI + 1))
				ELSE
					hParameters[ UUrlDecode(cPart) ] := NIL
				ENDIF
			NEXT

			hParClient[ 'scope' ] := HB_HGetDef( hParameters, 'm', '' )
			hParClient[ 'token' ] := HB_HGetDef( hParameters, 't', '' )
			
		endif
	
		_traceSys( '>> ClientConnection > Parameters', hParClient )
		
		if Valtype( oWS:bValidate ) == 'B'
		
			_traceSys( '>> ClientConnection > Eval Validate' )
			
			lValidate := Eval( oWS:bValidate, hParClient )
			
			_traceSys( '>> ClientConnection > return', lValidate )

			if Valtype( lValidate ) != 'L' .or. !lValidate 
			
				_trace( 'Error Validate. Not return logic value' )				
				
				#ifndef NO_SSL				
					nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ), TIMEOUT, @nErr )								
				#ELSE
					hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ) )   // close handShake
				#ENDIF			
		
				ClientClose( hSocket, nil )				
				retu nil								
			endif		
		
		endif	

	//	---------------------------------------------------		


	//	Register Socket ----------------------------------------	
	
			_traceSys( '>> ClientConnection > Register Socket'  )
			
			cUser 	:= hb_NumToHex( hSocket )	
			_traceSys( '>> ClientConnection > cUser: ' + cUser )
			
			cToken	:= USetToken( cUser ) 			
			_traceSys( '>> ClientConnection > cToken: ' + cToken )

			//	CHARLY -> Si bloquejo casca a la 3 recàrrega
			//	Pendent de revisar
			
			//hb_mutexLock( hMutexUser )  						

				hClient := {=>}
				hClient[ 'pSocket' ] 	:= hSocket 
				hClient[ 'pSSL' ] 		:= hSSL 
				hClient[ 'scope' ] 	:= HB_HGetDef( hParameters, 'm', '' )	//	m = module
				hClient[ 'token' ] 	:= HB_HGetDef( hParameters, 't', '' )	//	t = token
				hClient[ 'in' ] 		:= dtoc( date() ) + ' ' + time()
				
				IF ! EMPTY( aI := hb_socketGetPeerName(hSocket))
				  hClient[ 'ip' ] := aI[2]				  
				  hClient[ 'port' ] := aI[3]
				ENDIF																
				
				aSockets[ cUser ] := hClient								

			//hb_mutexUnlock( hMutexUser )				

			_traceSys( '>> ClientConnection > Registered !', hClient  )
			
	//	--------------------------------------------------------  	
	
	//USocketGarbage()				//	IMPORTANTISIMA !!! Sino CPU almacena Sockets vacios que ocupan mucha memoria 		

	_traceSys( '>> ClientConnection > Init Loop accept socket request...'  )		
	
	while .T.
		cRequest = ""
		nLen = 1				

		while nLen > 0
		
			cBuffer := Space( 4096 )
		 
			#ifndef NO_SSL		
				nLen := MY_SSL_READ( hSSL, hSocket, @cBuffer, TIMEOUT , @nErr)
			#ELSE
				nLen := hb_socketRecv( hSocket, @cBuffer,,, TIMEOUT ) 		
			#ENDIF		 
			
			//_d( 'ERROR: ' + str( hb_socketGetError() ) )
			
         
			if  nLen > 0
				cRequest += Left( cBuffer, nLen )							    
			else
				if nLen == -1 .and. hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
					nLen = 0
				endif
			endif
			
		end

      
		if ! Empty( cRequest )		 

			/* OPCODE			
				1.- Normal
				8.- Exiting			
			*/
		
			cRequest:= UnMask( cRequest, @nOpcode )	  			
			
			_traceSys( '>> ClientConnection > OPCODE: ' + lTrim( hb_CSTR( nOpcode )) )
			_traceSys( '>> ClientConnection > REQUEST: ', cRequest  )
			
         
			do case							
			
				case cRequest == "exit"     // 1000 value in hex and bytes swapped 
			
					#ifndef NO_SSL				
						nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "exiting", OPC_CLOSE ), TIMEOUT, @nErr )
					#ELSE
						hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "exiting", OPC_CLOSE ) )   // close handShake
					#ENDIF
               
				case cRequest == I2Bin( 0xE803 ) + "exiting"        // client answered to close handShake							
			
					exit
               
				otherwise									
				
					// Aqui hauriem de tenir una validacio per cada peticio?										
					
					hData := hb_jsondecode( cRequest )				
				
					if valtype( hData ) == 'H'																									
						
						//	Validate system ------------------------------------------
						
							lAuth := .f.
						
							if ! HB_HHasKey( hData, 'msg' ) .or. ;
							   ! HB_HHasKey( hData, 'token' ) .or. ;
							   ! HB_HHasKey( hData, 'scope' ) 
							   
						
								#ifndef NO_SSL				
									nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ), TIMEOUT, @nErr )								
								#ELSE
									hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ) )   // close handShake
								#ENDIF												
							
								ClientClose( hSocket, cUser )
								retu nil												
							   
							   
							endif
							
							//	Autorizacion para la conexion...
							
							if hData[ 'msg' ] == 'UT Websocket Server'	
							
								//	Si hemos definido rutina de validacion, la ejecutamos
								//	y le pasamos los parametros token, scope 
								
								cScope := hData[ 'scope' ]
							
								if Valtype( oWS:bValidate ) == 'B'
								
									_traceSys( '>> ClientConnection > Eval Validate' )
									
									lValidate := Eval( oWS:bValidate, hParClient )
									
									_traceSys( '>> ClientConnection > return', lValidate )
									
									//	La rutina bValidate ha de devolver un logico
									
									if Valtype( lValidate ) != 'L'
									
										_trace( 'Error Validate. Not return logic value' )	

										#ifndef NO_SSL				
											nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ), TIMEOUT, @nErr )								
										#ELSE
											hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ) )   // close handShake
										#ENDIF

										ClientClose( hSocket, cUser )
										retu nil									
										
									else 
									
										if lValidate 
											cRequest :=  hb_jsonencode( { 'type' => 'uws_token', 'value' => cToken } )
										else
										
											#ifndef NO_SSL				
												nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ), TIMEOUT, @nErr )								
											#ELSE
												hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ) )   // close handShake
											#ENDIF			
										
											ClientClose( hSocket, cUser )
											retu nil								
										endif
										
									endif
									
								else 
								
									//	Si NO hay control de validacion lo damos por valido
								
									cRequest :=  hb_jsonencode( { 'type' => 'uws_token', 'value' => cToken } )																	
								
								endif						
								
							endif						
						
						//	---------------------------------------------------------
						
						
					else 		

						// Si  definimos nuestro gestor de mensajes, lo ejecutamos						
						//	Verificamos OpCode == 1 porque a veces llega 8
						
						if nOpcode == 1 .and. Valtype( oWS:bMessage ) == 'B'														
						
							//	uValue ha de ser un string o un hash 
							//	Si es un hash habra de tener   key: type y value 
							//	Si es un string el type == 'msg'														

							uValue 	:= eval( oWS:bMessage, cUser, cRequest, hParClient )							
							cValType 	:= ValType( uValue )											
							
							//	cValType == 'C'		-> type = 'msg', value = uValue
							//	cValType == 'H' 		-> Si exist type  
							//	Converrt value to string, Si hash -> jsonencode(value)
							//
							
							if !empty( uValue )

								do case
									case cValType == 'C'
										cRequest :=  hb_jsonencode( { 'type' => 'msg', 'value' => uValue } )
										
									case cValType == 'H'
									
										HB_HCaseMatch( uValue, .f. )
									
										cType 	:= HB_HGetDef( uValue, 'type', '*' )
										uValue	:= HB_HGetDef( uValue, 'value', '' )
										
										if ! empty( uValue )
											
											cValType := Valtype( uValue )
											
											do case
												case cValType == 'C' 
												case cValType == 'N' ; uValue := ltrim(str(uValue))
												case cValType == 'L' ; uValue := if( uValue, 'true', 'false' )
												case cValType == 'A' ; uValue := hb_jsonencode( uValue )
												case cValType == 'H' ; uValue := hb_jsonencode( uValue )																							
												otherwise
													uValue := hb_CStr( uValue)											
											endcase 
										endif									
										
										cRequest :=  hb_jsonencode( { 'type' => cType, 'value' => uValue } )
										
									otherwise 
									
										//	De moemnto si no es ni C ni H , error 
										
											#ifndef NO_SSL				
												nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "error 170", OPC_AUTH ), TIMEOUT, @nErr )								
											#ELSE
												hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "error 170", OPC_AUTH ) )   // close handShake
											#ENDIF
											
											ClientClose( hSocket, cUser )
											retu nil																													
										
								endcase
								
							else 
							
								cRequest := ''
							
							endif							
						
						endif																								
						
					endif					  
				
					if !empty(  cRequest )
					
						#ifndef NO_SSL
							nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( cRequest ), TIMEOUT, @nErr  )
						#ELSE			   
							hb_socketSend( hSocket, UMask( cRequest ) )
						#ENDIF	
					
					endif
					
					USocketGarbage()

					
			endcase
			
		endif				
		
		hb_idleSleep( 0.1 )
		
	end
	
    _trace( "Close socket" )

	_traceSys( '>> ClientConnection > Close socket' )
	
	ClientClose( hSocket, cUser )

return nil

//----------------------------------------------------------------//

static function ClientClose( hSocket, cUser )

	hb_socketShutdown( hSocket )	
	hb_socketClose( hSocket )	
	UDelClient( cUser )

retu .t. 

//----------------------------------------------------------------//

static function UHandShaking( hSocket, cHeaders, hSSL )   

	local aHeaders := hb_ATokens( cHeaders, CRLF )
	local hHeaders := {=>}, cLine 
	local cAnswer, nLen
	local hCfg 		

	hCfg  	:= UGetServerInfo()[ 'websocket_cfg' ]	
	


	#ifndef NO_SSL
	
	#else 
		hSSL := nil 
	#endif
	
	_traceSys( '>> UHandShaking >', cHeaders )	

	//hb_default( @hSSL, nil )	//	Crash

	//	No habria de llegar cHeader vacio... 
	
	
	for each cLine in aHeaders
		hHeaders[ SubStr( cLine, 1, At( ":", cLine ) - 1 ) ] = SubStr( cLine, At( ":", cLine ) + 2 )
	next


	cAnswer = "HTTP/1.1 101 Web Socket Protocol Handshake" + CRLF + ;
				"Upgrade: websocket" + CRLF + ;
				"Connection: Upgrade" + CRLF + ;
				"WebSocket-Origin: " + hCfg[ 'address'] + CRLF + ;
				"WebSocket-Location: " + hCfg[ 'uri'] + CRLF + ;
				"Sec-WebSocket-Accept: " + ;
				hb_Base64Encode( hb_SHA1( hHeaders[ "Sec-WebSocket-Key" ] + ;
								"258EAFA5-E914-47DA-95CA-C5AB0DC85B11", .T. ) ) + CRLF + CRLF 


	_traceSys( '>> UHandShaking > cAnswer', cAnswer )
								
    #ifndef NO_SSL			
		nLen := MY_SSL_WRITE( hSSL, hSocket, cAnswer, TIMEOUT )		
	#else	
		nLen := hb_socketSend( hSocket, cAnswer,,, TIMEOUT ) 
	#endif

return nil    

//----------------------------------------------------------------//
// PUBLIC Functions -> For UDom() Class
//----------------------------------------------------------------//

function  UMask( cText, nOPCode )

	local nLen := Len( cText )
	local nFirstByte := 0
	local cHeader 
					
	hb_default( @nOPCode, OPC_TEXT )
	
	nFirstByte = hb_bitSet( nFirstByte, 7 ) // 1000 0000
	
	nFirstByte := hb_bitOr( nFirstByte, nOPCode )  // 1000 XXXX -> is set
	
	do case
		case nLen <= 125
			cHeader = Chr( nFirstByte ) + Chr( nLen )   
	
		case nLen < 65536
			cHeader = Chr( nFirstByte ) + Chr( 126 ) + ;
					Chr( hb_BitShift( nLen, -8 ) ) + Chr( hb_BitAnd( nLen, 0xFF ) )
	
		otherwise 
			cHeader = Chr( nFirstByte ) + Chr( 127 ) + NetworkULL2Bin( nLen )   
	endcase

return cHeader + cText  

//----------------------------------------------------------------//

function  Unmask( cBytes, nOpcode )
   
	local lComplete := hb_bitTest( hb_bPeek( cBytes, 1 ), 7 )
	local nFrameLen := hb_bitAnd( hb_bPeek( cBytes, 2 ), 127 ) 
	local nLength, cMask, cData, cChar, cHeader := "", nCommaPos

	nOpcode := hb_bitAnd( hb_bPeek( cBytes, 1 ), 15 )

	do case
		case nFrameLen <= 125
			nLength = nFrameLen
			cMask = SubStr( cBytes, 3, 4 )
			cData = SubStr( cBytes, 7 )

		case nFrameLen = 126
			nLength = ( hb_bPeek( cBytes, 3 ) * 256 ) + hb_bPeek( cBytes, 4 )
			cMask   = SubStr( cBytes, 5, 4 )
			cData   = SubStr( cBytes, 9 )

		case nFrameLen = 127  
			nLength = NetworkBin2ULL( SubStr( cBytes, 3, 8 ) )  
			cMask   = SubStr( cBytes, 11, 4 )
			cData   = SubStr( cBytes, 15 )
	endcase 

	cBytes = ""
   
	for each cChar in cData
		cBytes += Chr( hb_bitXor( Asc( cChar ),;
						hb_bPeek( cMask, ( ( cChar:__enumIndex() - 1 ) % 4 ) + 1 ) ) ) 
	next   

	nCommaPos = At( ",", cBytes )
	cHeader = SubStr( cBytes, 1, nCommaPos - 1 )
	
	if Right( cHeader, 6 ) == "base64"
		cBytes = hb_base64Decode( SubStr( cBytes, nCommaPos + 1 ) )
	else
		cHeader = ""      
	endif

return cBytes 

//----------------------------------------------------------------//

static function  NetworkULL2Bin( n )

	local nBytesLeft := 64
	local cBytes := ""

	while nBytesLeft > 0
		nBytesLeft -= 8
		cBytes += Chr( hb_BitAnd( hb_BitShift( n, -nBytesLeft ), 0xFF ) )
	end

return cBytes

//----------------------------------------------------------------//

static function  NetworkBin2ULL( cBytes )

	local cByte, n := 0
   
	for each cByte in cBytes
		n += hb_BitShift( Asc( cByte ), 64 - cByte:__enumIndex() * 8 )
	next
   
return n

//----------------------------------------------------------------//

function UDelClient( cUser )

	//_d( '>> Cierre cliente ' + cUser )
	
	if valtype( cUser ) == 'C'
	
		if HB_HHasKey( aSockets, cUser )

			hb_mutexLock( hMutexUser ) 
				hb_HDel( aSockets, cUser )
			hb_mutexUnlock( hMutexUser )
		
		endif
	
	endif

retu nil 

//----------------------------------------------------------------//

function USocketGarbage( lTrace ) 
	
	local nLen 		:= len( aSockets )
	local nPos 		:= 1
	local nActive		:= 0
	local nDelete		:= 0
	local cDummy 		:= ' '
	local aPair, hSocket, cSocket, nErr
	
	hb_default( @lTrace, .f.)
	
	_trace( '>> USocketGarbage --------------' )
	
	hb_mutexLock( hMutexUser ) 			
	
	while nPos <= nLen 
	
		aPair := HB_HPairAt( aSockets, nPos )
		
		cSocket  	:= aPair[1] 		
		hSocket 	:= aPair[2][ 'pSocket' ]
		
		hb_socketRecv(hSocket, @cDummy,,, 1 )		//	VULL REBRE !!! SI CASCA == 15
		nErr := hb_socketGetError()
		
		if nErr == HB_SOCKET_ERR_TIMEOUT 			//	2	SOCKET EXIST	
		
			nPos++	
			nActive++
			
		elseif nErr == HB_SOCKET_ERR_CONNABORTED	//	15 Conexio KO		
		
			hb_socketShutdown( hSocket )
			hb_socketClose( hSocket )		

			//UDelClient( cSocket )	
			
			hb_HDel( aSockets, cSocket )
			
			nLen--
			nDelete++
			
		endif									
	
	end

	hb_mutexUnlock( hMutexUser )	
	
	_trace( 'Total Active: ' + ltrim(str(nActive)) )
	_trace( 'Total Recolleted: ' + ltrim(str(nDelete)) )
	
	_trace( '<< USocketGarbage --------------' )
	
retu nil 

//----------------------------------------------------------------//

function UWS_GetSockets(); retu aSockets
function UWS_InfoSSL(); retu oWebServerSocket:InfoSSL()

//----------------------------------------------------------------//

function UWS_GetInfoSockets()
	local nLen 	:= len( aSockets )
	local aInfo 	:= {}
	local n, aPair 
	
	for n := 1 to nLen 
	
		aPair := HB_HPairAt( aSockets, n )	
		
		Aadd( aInfo, { 'token' => aPair[1], 'ip' => aPair[2][ 'ip' ], 'scope' => aPair[2]['scope'], 'in' => aPair[2][ 'in' ] })
	next 

retu aInfo

//----------------------------------------------------------------//

function UWS_GetPSocket( cSocket )

	local pSocket := nil

	hb_default( @cSocket, '' )
	
	if HB_HHasKey( aSockets, cSocket ) 
		pSocket := aSockets[ cSocket ][ 'pSocket ']
	endif	

retu pSocket

//----------------------------------------------------------------//

function UWS_GetPSSL( cSocket )

	local pSSL := nil

	hb_default( @cSocket, '' )
	
	if HB_HHasKey( aSockets, cSocket ) 
		pSSL := aSockets[ cSocket ][ 'pSSL']
	endif	
	
retu pSSL

//----------------------------------------------------------------//

function UWS_Define( cScope, cToken, cOnOpen, cOnMessage, cOnClose, cOnError, lConsole )

	local hServerInfo, hCfg, cUri
	local cJS 			:= ''
	local cCfg
	
	
	hb_default( @cScope 		, '' )
	hb_default( @cToken 		, '' )
	hb_default( @cOnOpen 		, '' )
	hb_default( @cOnMessage 	, '' )
	hb_default( @cOnClose 	, '' )
	hb_default( @cOnError 	, '' )    		
	hb_default( @lConsole 	, .f. )    		

	hServerInfo	:= UGetServerInfo()
	
	if hServerInfo[ 'websocket' ] == .F.
		_trace( 'WebSocket not initialized' )
		
		CODE TO cJs 
			<script>								
				console.error( 'WS >> Error initializating...'  )										
			</script>
		ENDTEXT 				
		
		retu cJs
	endif	

	hCfg := hServerInfo[ 'websocket_cfg' ]
	
	if hCfg[ 'ssl' ] 
		cUri 	:= 'wss://'
	else
		cUri 	:= 'ws://'
	endif
	
	_traceSys( '>> UWS_Define > address > ' +  hCfg[ 'address'] )
	
	//	ADDRESS debe tener el nombre del dominio, p.e charles9000@wok.ef
	//	En el caso del default, lo pasaremos a 0.0.0.0 
	
		cUri 	+= if( hCfg[ 'address'] == '0.0.0.0', 'localhost', hCfg[ 'address'] ) + ':' + ltrim(str(hCfg[ 'port' ]))
	
	
	//cUri += '?m=test&t=mytoken666'
	
	if !empty( cScope ) .or. !empty( cToken )
		cUri += '?' 
		
		if !empty( cScope )
			cUri += 'm=' + cScope 
		endif
		
		if !empty( cToken )
		
			if !empty( cScope )
				cUri += '&'
			endif
			
			cUri += 't=' + cToken
		endif
		
	endif

	//	TOTI --> Ha de posarse el domini !!!! --> wss://charles9000.work.gd:8883?m=basic&t=ABC-1234
	//cUri := 'wss://188.84.11.11:8883?m=basic&t=ABC-1234'
	//cUri := 'wss://charles9000.work.gd:8883?m=basic&t=ABC-1234'
	
	_traceSys( '>> UWS_Define > cUri > ' + cUri )
	
	hCfg := {=>}
	
	hCfg[ 'scope' ] := cScope
	hCfg[ 'token' ] := cToken
	hCfg[ 'uri' ] := cUri 
	hCfg[ 'onopen' ] := cOnOpen
	hCfg[ 'onmessage' ] := cOnMessage	
	hCfg[ 'onclose' ] := cOnClose	
	hCfg[ 'onerror' ] := cOnError
	hCfg[ 'console' ] := lConsole
	
	cCfg := hb_jsonencode( hCfg )
	
	_traceSys( '>> UWS_Define > hCfg > ', hCfg )
	
	
CODE TO cJs PARAMS cCfg

<script>
	
	$( document ).ready(function() {							
		UWS_Define( '<$ cCfg $>' )
		UWS_Init();
	})	

</script>

ENDTEXT 

retu cJs

#ifndef NO_SSL	 

//----------------------------------------------------------------//
//	Mindauga's functions 
//----------------------------------------------------------------//
static function  MY_SSL_READ(hSSL, hSocket, cBuf, nTimeout, nError)

	LOCAL nErr, nLen

	nLen := SSL_READ(hSSL, @cBuf)
	IF nLen < 0
		nErr := SSL_GET_ERROR(hSSL, nLen)
		
		IF nErr == HB_SSL_ERROR_WANT_READ
		
			nErr := hb_socketSelectRead(hSocket, nTimeout)
			
			IF nErr < 0
				nError := hb_socketGetError()
			ELSE  // Both cases: data received and timeout
				nError := HB_SOCKET_ERR_TIMEOUT
			ENDIF
			
			RETURN -1
			
		ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE
		
			nErr := hb_socketSelectWrite(hSocket, nTimeout)
			
			IF nErr < 0
				nError := hb_socketGetError()
			ELSE  // Both cases: data sent and timeout
				nError := HB_SOCKET_ERR_TIMEOUT
			ENDIF
			
			RETURN -1
			
		ELSE
		
			nError := 1000 + nErr
			
			RETURN -1
			
		ENDIF
		
	ENDIF
	
RETURN nLen

//----------------------------------------------------------------//

static function  MY_SSL_WRITE(hSSL, hSocket, cBuf, nTimeout, nError)

	LOCAL nErr, nLen

	nLen := SSL_WRITE(hSSL, cBuf)
	
	IF nLen <= 0
	
		nErr := SSL_GET_ERROR(hSSL, nLen)
		
		IF nErr == HB_SSL_ERROR_WANT_READ
		
			nErr := hb_socketSelectRead(hSocket, nTimeout)
			
			IF nErr < 0
				nError := hb_socketGetError()
				RETURN -1
			ELSE  // Both cases: data received and timeout
				RETURN 0
			ENDIF
			
		ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE
		
			nErr := hb_socketSelectWrite(hSocket, nTimeout)
			
			IF nErr < 0
				nError := hb_socketGetError()
				RETURN -1
			ELSE  // Both cases: data sent and timeout
				RETURN 0
			ENDIF
			
		ELSE
		
			nError := 1000 + nErr
			RETURN -1
		ENDIF
		
	ENDIF
	
RETURN nLen

//----------------------------------------------------------------//

STATIC FUNC MY_SSL_ACCEPT(hSSL, hSocket, nTimeout)

	LOCAL nErr
	
	nErr := SSL_ACCEPT(hSSL)

	IF nErr > 0
	
		RETURN 0
	
	ELSEIF nErr < 0
	
		nErr := SSL_GET_ERROR(hSSL, nErr)
	
		IF nErr == HB_SSL_ERROR_WANT_READ
	
		nErr := hb_socketSelectRead(hSocket, nTimeout)
	
		IF nErr < 0
	
			nErr := hb_socketGetError()
		ELSE
	
			nErr := HB_SOCKET_ERR_TIMEOUT
		ENDIF
		
	ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE
	
		nErr := hb_socketSelectWrite(hSocket, nTimeout)
		
		IF nErr < 0
	
			nErr := hb_socketGetError()
		ELSE
	
			nErr := HB_SOCKET_ERR_TIMEOUT
		ENDIF
		
		ELSE
		//? "SSL_ACCEPT() error", nErr
		nErr := 1000 + nErr
		ENDIF
		
	ELSE /* nErr == 0 */
	
		nErr := SSL_GET_ERROR( hSSL, nErr )
		//? "SSL_ACCEPT() shutdown error", nErr
		nErr := 1000 + nErr
	ENDIF

RETURN nErr
#endif
//----------------------------------------------------------------//

function UWS_SendJS( cSocket, cFunction, uValue, cScope )

	local hParam := {=>}

	if empty( cFunction ) .or. empty( uValue )
		retu .f.
	endif

retu UWS_Send( cSocket, 'js', uValue, cFunction, cScope )



//----------------------------------------------------------------//

function UWS_Send( cSocket, cType, cMsg, cFunction, cScope  )

	local aSockets, hInfo, c, n, aPair, nLen
	local aData  	:= {=>}
	
	

	//hb_default( @cSocket, '' )
	hb_default( @cType, 'msg' )
	//hb_default( @cMsg, '' )			// No xuten be, donen errors
	hb_default( @cFunction, '' )			
	hb_default( @cScope, '' )			

	if empty( cSocket ) //.or. empty( cType )	
		retu .f.
	endif

	aSockets 	:= UWS_GetSockets()
	
	if empty( cScope )

		if HB_HHasKey( aSockets, cSocket )
				
			hInfo := aSockets[ cSocket ]
			
		endif
		
		if ! empty( hInfo )

			if empty( cMsg )
				c :=  hb_jsonencode( { 'type' => 'msg', 'value' => cType, 'function' => cFunction } )
			else
				c :=  hb_jsonencode( { 'type' => cType, 'value' => cMsg , 'function' => cFunction } )
			endif	
		

			#ifndef NO_SSL					
				MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )			//TIMEOUT ??
			#else
				hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) ) 		
			#endif
			
		else 
		
			retu .f.
			
		endif 
		
	else 						
		
		nLen := len( aSockets )
		
		for  n := 1 to nLen 
		
			cScope := lower( cScope )
	
			aPair := HB_HPairAt( aSockets, n )
			
			cSocket := aPair[1]
			hInfo   := aPair[2]
			
			if !empty( hInfo )
			
				if cScope == '*' .or. lower( hInfo[ 'cScope' ] ) == cScope
				
					if empty( cMsg )
						c :=  hb_jsonencode( { 'type' => 'msg', 'value' => cType, 'function' => cFunction } )
					else
						c :=  hb_jsonencode( { 'type' => cType, 'value' => cMsg , 'function' => cFunction } )
					endif					

					#ifndef NO_SSL					
						MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )			//TIMEOUT ??
					#else
						hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) ) 		
					#endif

				endif
			
			endif
			
		next		
	
	endif

RETU .t. 

//----------------------------------------------------------------//

function UWS_ErrorHandler( oErr )

	local hError 	:= {=>}
	local aStack 	:= {}
	local aArgs 	:= {}
	local nI

	IF !EMPTY(oErr:filename)
		hError[ 'filename' ] := oErr:filename 
	ENDIF
  
	IF !EMPTY(oErr:description)
		hError[ 'description' ] := oErr:description 
	ENDIF
  
	IF !EMPTY(oErr:operation)
		hError[ 'operation' ] := oErr:operation 
	ENDIF
  
	IF !EMPTY(oErr:osCode)
		hError[ 'error' ] := LTRIM(STR(oErr:osCode)) 
	ENDIF
  
	IF VALTYPE(oErr:args) == "A"		
		AEVAL( oErr:args, {|X| Aadd( aArgs, HB_CStr(X) )  })
		hError[ 'arguments' ] := aArgs
	ENDIF  

	nI := 2
	
	DO WHILE ! EMPTY(PROCNAME(++nI))
		Aadd( aStack, PROCNAME(nI) + "(" + LTRIM(STR(PROCLINE(nI))) + ")"  )
	ENDDO	
	
	if !empty( aStack )
		hError[ 'stack' ] := aStack
	endif
	
	hError[ 'version' ] 	:= OS()
	hError[ 'harbour' ] 	:= Version()
	hError[ 'ut' ] 		:= UVersion()

	if hb_isfunction( 'TWebVersion' ) 
		hError[ 'tweb' ] := eval( &( '{|| TWebVersion()}' ) )	
	endif	

RETU hError 



#include "inkey.ch"
#include "hbclass.ch"
#include "hbsocket.ch"

#define UWS_VERSION  '1.00'

#xcommand CODE TO <var> [ PARAMS [<v1>] [,<vn>] ] => #pragma __stream|<var> += UReplaceBlocks( %s, '<$', "$>" [,<(v1)>][+","+<(vn)>] [, @<v1>][, @<vn>] )

#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY => ALWAYS

#xcommand _trace( [<uVar,...>] )   => if( oWebServerSocket:lTrace, _t( [<uVar>] ), nil )
#xcommand _traceSys( [<uVar,...>] )  => if( oWebServerSocket:lTraceSys, _d( [<uVar>] ), nil )


#ifndef NO_SSL
#include "hbssl.ch"
#define IS_SSL  .T.
#define PORT     8443
#else
#define IS_SSL   .F.
#define PORT     9000
#endif


#define TIMEOUT    50  // 5000
#define CRLF       Chr( 13 ) + Chr( 10 )

#define OPC_CONT     0x00
#define OPC_TEXT     0x01
#define OPC_BIN      0x02
#define OPC_AUTH    0x03
#define OPC_RECONNECT   0x07
#define OPC_CLOSE    0x08
#define OPC_PING     0x09
#define OPC_PONG     0x0A
#define OPC_ERROR    0x0B

STATIC oWebServerSocket

STATIC hMutexUser
STATIC aSockets


// ----------------------------------------------------------------//

FUNCTION UWebSocket()

   oWebServerSocket := UT_WebSocket():New()

   RETU oWebServerSocket

// ----------------------------------------------------------------//

FUNCTION UWS_Version() ; RETU UWS_VERSION

// ----------------------------------------------------------------//

CLASS UT_WebSocket

   DATA cError     INIT ''
   DATA hInfoSSL     INIT { => }
   DATA hSSLCtx
   DATA lInit      INIT .F.
   DATA bInit      INIT {|| QOut( 'WEBSOCKET Vrs. ' +  UWS_VERSION  ) }
   DATA hConfig     INIT { => }
   DATA lTrace     INIT .F.
   DATA lTraceSys     INIT .F.
   DATA bValidate     INIT {|| .T. }
   DATA bMessage     INIT NIL

   METHOD New()        CONSTRUCTOR
   METHOD Run()
   METHOD ValidConfig( hConfig )

   METHOD SetAddress( cAddress ) INLINE ::hConfig[ 'address' ] := if( ValType( cAddress ) == 'C', cAddress, '0.0.0.0' )
   METHOD SetPort( nPort )   INLINE ::hConfig[ 'port' ] := if( ValType( nPort ) == 'N', nPort, if( IS_SSL, 8443, 9000 ) )
   METHOD SetSSL( lSSL )    INLINE ::hConfig[ 'ssl' ] := if( ValType( lSSL ) == 'L', lSSL, .F. )

   METHOD SetTrace( lTrace )  INLINE ::lTrace := if( ValType( lTrace ) == 'L', lTrace, .F. )
   METHOD SetTraceSys( lTrace )  INLINE ::lTraceSys := if( ValType( lTrace ) == 'L', lTrace, .F. )

   METHOD SetCertificate( PrivateKeyFilename, CertificateFilename )
   METHOD InfoSSL()     INLINE ::hInfoSSL

ENDCLASS

// ----------------------------------------------------------------//

METHOD New() CLASS UT_WebSocket

// Init vars....

   ::hConfig[ 'ssl' ]  := IS_SSL
   ::hConfig[ 'address' ]  := '0.0.0.0'
   ::hConfig[ 'port' ] := IF( IS_SSL, 8443, 9000 )

   RETU SELF

// ----------------------------------------------------------------//

METHOD SetCertificate( PrivateKeyFilename, CertificateFilename ) CLASS UT_WebSocket

   hb_default( @PrivateKeyFilename, 'cert/privatekey.pem' )
   hb_default( @CertificateFilename, 'cert/certificate.pem' )

   ::hConfig[ 'PrivateKeyFilename' ]  := PrivateKeyFilename
   ::hConfig[ 'CertificateFilename' ] := CertificateFilename

   RETU NIL

// ----------------------------------------------------------------//

METHOD Run( hConfig ) CLASS UT_WebSocket

   LOCAL hListen, hSocket, lOk, nTries

   _traceSys( '>> Run' )

   ::ValidConfig( hConfig )

   IF ! hb_mtvm()
      ::cError := "Multithread support required"
      _trace( ::cError )
      RETU .F.
   ENDIF


   hMutexUser := hb_mutexCreate()
   aSockets  := { => }

#ifndef NO_SSL

// SSL_INIT()  // Initiated en UHTTPD2

   ::hSSLCtx := SSL_CTX_NEW( HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER )


   SSL_CTX_SET_OPTIONS( ::hSSLCtx, HB_SSL_OP_NO_TLSv1 )


   IF SSL_CTX_USE_PRIVATEKEY_FILE( ::hSSLCtx, ::hConfig[ 'PrivateKeyFilename' ], HB_SSL_FILETYPE_PEM ) != 1
      ::cError := "Invalid private key file: " + ::hConfig[ 'PrivateKeyFilename' ]
      _trace( ::cError )
      RETU .F.
   ENDIF



   IF SSL_CTX_USE_CERTIFICATE_FILE( ::hSSLCtx, ::hConfig[ 'CertificateFilename' ], HB_SSL_FILETYPE_PEM ) != 1
      ::cError := "Invalid certificate file: " + ::hConfig[ 'CertificateFilename' ]
      _trace( ::cError )
      RETU .F.
   ENDIF

   _traceSys( '>> Run > SSL OK' )


#endif

   IF Empty( hListen := hb_socketOpen( HB_SOCKET_AF_INET, HB_SOCKET_PT_STREAM, HB_SOCKET_IPPROTO_TCP ) )
      ::cError := "Socket create error " + hb_ntos( hb_socketGetError() )
      _trace( ::cError )
      RETU .F.
   ENDIF

   _traceSys( '>> Run > Listen OK' )


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

   IF ValType( ::bInit ) == 'B'
// eval( ::bInit, ::aInfo )
      Eval( ::bInit )
   ENDIF

   lOk := .F.
   nTries := 0

// _traceSys( '>> Run >> Init SocketBind > ' +  ::hConfig[ 'address'] + ' - ' + ltrim(str(::hConfig[ 'port' ])) )
   _traceSys( '>> Run >> Init SocketBind > ' +  '0.0.0.0' + ' - ' + LTrim( Str( ::hConfig[ 'port' ] ) ) )

   WHILE ! lOk


// if ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, ::hConfig[ 'address'], ::hConfig[ 'port' ]  } )
      IF ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, '0.0.0.0', ::hConfig[ 'port' ]  } )


         nTries++

#ifndef NO_SSL
         ::cError := "Bind error " + hb_ntos( hb_socketGetError() )
// _trace( ::cError )
#endif

         _traceSys( '>> Run > Attempt ' + LTrim( Str( nTries ) ) + '. Error: ' + ::cError  )

      ELSE

         lOk := .T.

      ENDIF

      IF nTries >= 5
         _traceSys( '>> Run > Max number attempts. Exit !' )
         RETU .F.
      ENDIF


// if !lOk
      hb_idleSleep( 0.1 )
// endif

   END

   _traceSys( '>> Run > Binding done !' )

   IF ! hb_socketListen( hListen )

#ifndef NO_SSL
      ::cError :=  "Listen error " + hb_ntos( hb_socketGetError() )
      _trace( ::cError )
#endif

      RETU .F.
   ENDIF

// Config UServer()

   USetServerInfo( 'websocket',  .T. )
   USetServerInfo( 'websocket_cfg', ::hConfig )


   _traceSys( '>> Run > Config UServerHttpd2 done' )

   ErrorBlock( {| o | _d( o ) } )

   _traceSys( '>> Run > Init loop accept socket...' )

   WHILE .T.

      IF Empty( hSocket := hb_socketAccept( hListen,, TIMEOUT ) )

         IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
// ? "loop"
         ELSE
            _trace( "Accept error " + hb_ntos( hb_socketGetError() ) )
         ENDIF

      ELSE
         _trace( "Accept socket request"  )
         hb_threadDetach( hb_threadStart( @ClientConnection(), hSocket, SELF ) )
      ENDIF

      IF Inkey() == K_ESC
         _trace( "Quitting - esc pressed" )
         EXIT
      ENDIF

   END

   _trace( "Close listening socket" )

   hb_socketShutdown( hListen )
   hb_socketClose( hListen )

   RETU .T.

// ----------------------------------------------------------------//

METHOD ValidConfig( hConfig ) CLASS UT_WebSocket

   LOCAL n, cKey, nLen

   hb_default( @hConfig, { => } )

   nLen := Len( hConfig )

   hb_HCaseMatch( hConfig, .F. )

   FOR n := 1 TO nLen

      cKey := hb_HKeyAt( ::hConfig, n )

      IF hb_HHasKey( hConfig, cKey )
         ::hConfig[ cKey ] := hConfig[ cKey ]
      ENDIF

   NEXT


   IF ::hConfig[ 'ssl' ]
      ::hConfig[ 'uri' ]  := 'wss://'
   ELSE
      ::hConfig[ 'uri' ]  := 'ws://'
   ENDIF

   ::hConfig[ 'uri' ] += if( ::hConfig[ 'address' ] == '0.0.0.0', 'localhost', ::hConfig[ 'address' ] ) + ':' + LTrim( Str( ::hConfig[ 'port' ] ) )

   _traceSys( '>> ValidConfig > ::hConfig',  ::hConfig )

   RETU NIL

// ----------------------------------------------------------------//
// STATIC Functions ---------------------------------------------//
// ----------------------------------------------------------------//

STATIC FUNCTION  ClientConnection( hSocket, oWS )

   LOCAL cRequest, cBuffer := Space( 4096 ), nLen, nOpcode
   LOCAL hSSL, cUser, cToken, hData
   LOCAL nErr, nTries
   LOCAL lExit := .T.
   LOCAL nIni, nEnd, cParameters, cPart, hParameters, nI, lValidate
   LOCAL uValue, cType, cValType, lAuth, hClient, aI, cScope
   LOCAL hParClient := { 'scope' => '', 'token' => '' }


#ifndef NO_SSL
   LOCAL nTime
#else
   LOCAL uDummy
#endif

   _traceSys( '>> ClienConnection' )

#ifndef NO_SSL

   hSSL := SSL_NEW( oWS:hSSLCtx )

   SSL_SET_MODE( hSSL, hb_bitOr( SSL_GET_MODE( hSSL ), HB_SSL_MODE_ENABLE_PARTIAL_WRITE ) )
   hb_socketSetBlockingIO( hSocket, .F. )
   SSL_SET_FD( hSSL, hb_socketGetFD( hSocket ) )


// Info SSL -------------------------------------------

   IF Empty( oWS:hInfoSSL )
      nErr := 0

      _traceSys( '>> Run > Extract info SSL...' )

      oWS:hInfoSSL[ "SSL_CIPHER" ]     := SSL_GET_CIPHER( hSSL )
      oWS:hInfoSSL[ "SSL_PROTOCOL" ]    := SSL_GET_VERSION( hSSL )
      oWS:hInfoSSL[ "SSL_CIPHER_USEKEYSIZE" ] := SSL_GET_CIPHER_BITS( hSSL, @nErr )
      oWS:hInfoSSL[ "SSL_CIPHER_ALGKEYSIZE" ] := nErr
      oWS:hInfoSSL[ "SSL_VERSION_LIBRARY" ]  := SSLEAY_VERSION( HB_SSLEAY_VERSION )
      oWS:hInfoSSL[ "SSL_SERVER_I_DN" ]   := X509_NAME_ONELINE( X509_GET_ISSUER_NAME( SSL_GET_CERTIFICATE( hSSL ) ) )
      oWS:hInfoSSL[ "SSL_SERVER_S_DN" ]   := X509_NAME_ONELINE( X509_GET_SUBJECT_NAME( SSL_GET_CERTIFICATE( hSSL ) ) )

      _traceSys( oWS:hInfoSSL )

   ENDIF

// ----------------------------------------------------

   nTime := hb_MilliSeconds()
   nTries := 0

   WHILE .T.

      IF ( nErr := MY_SSL_ACCEPT( hSSL, hSocket, TIMEOUT ) ) == 0

         EXIT

      ELSE

         nTries++

         _traceSys( '> Attempt ' + LTrim( Str( nTries ) ) + ',  Error: ' + LTrim( Str( nErr ) )  )

         IF nTries >= 5

            _traceSys( '>> ClientConnection > Error , al carrer '   )

            IF nErr == HB_SOCKET_ERR_TIMEOUT


               IF ( hb_MilliSeconds() - nTime ) > TIMEOUT

                  _trace( "SSL accept timeout" )

                  EXIT
               ENDIF

            ELSE

// Eval( oServer:hConfig[ "Trace" ], "SSL accept error:", nErr, hb_socketErrorString( nErr ) )

               EXIT
            ENDIF

         ELSE

         ENDIF

         hb_idleSleep( 0.1 )

      ENDIF

   END


   IF nErr != 0

      _traceSys( "> SSL accept error:" + LTrim( Str( nErr ) ) + ' ' +  hb_socketErrorString( nErr ) + '. Exit!' )

      ClientClose( hSocket, NIL )

      RETU NIL
   ENDIF

/* Intentaremos leer hasta 5 veces si no recibimos datos (nLen == -1 ) */

   nTries := 0

   _traceSys( '>> ClientConnection > Init loop socket for handshaking... ' )


   WHILE .T.

      nLen := MY_SSL_READ( hSSL, hSocket, @cBuffer, TIMEOUT, @nErr )


      IF nLen > 0
         EXIT
      ELSE

         IF nErr != 0

            _traceSys( '>> ClientConnection > nErr', nErr  )

            IF nErr == 1001

               ClientClose( hSocket, NIL )
               RETU NIL
            ELSE

               nTries++

               IF nTries = 5
                  _traceSys( '>> ClientConnection > Max Tries'  )

                  ClientClose( hSocket, NIL )
                  RETU NIL
               ELSE
                  hb_idleSleep( 0.1 )
               ENDIF
            ENDIF

         ELSE
            EXIT
         ENDIF
      ENDIF

   END

#else

   uDummy := oWS:cError

   nTries := 0

   WHILE .T.

      nLen := hb_socketRecv( hSocket, @cBuffer,,, TIMEOUT )


      IF nLen > 0
         EXIT
      ELSE
         nErr := hb_socketGetError()

         IF nErr != 0

            _traceSys( '>> ClientConnection > nErr', nErr  )

            IF nErr == 1001

               ClientClose( hSocket, NIL )
               RETU NIL
            ELSE

               nTries++

               IF nTries = 5

                  _traceSys( '>> ClientConnection > Max Tries'  )

                  ClientClose( hSocket, NIL )
                  RETU NIL
               ELSE

                  hb_idleSleep( 0.1 )
               ENDIF
            ENDIF

         ELSE
            EXIT
         ENDIF
      ENDIF

   END

#endif


   cBuffer := hb_BLeft( cBuffer, nLen ) // RTrim( cBuffer )


// HANDSHAKING ---------------------------------------

   UHandShaking( hSocket, cBuffer, hSSL )


// Recover posible parameters ------------------------

// Check cBuffer := /?m=test&t=MyToken987 HTTP/1.1
// m = module
// t = token

   nIni    := At( '/?', cBuffer )
   nEnd    := At( 'HTTP/1.1', cBuffer )
   hParameters  := { => }

   IF nIni > 0 .AND. nEnd > 0

      cParameters := AllTrim( SubStr( cBuffer, nIni + 2, ( nEnd - nIni - 2 ) ) )

      FOR EACH cPart IN hb_ATokens( cParameters, "&" )
         IF ( nI := At( "=", cPart ) ) > 0
            hParameters[ UUrlDecode( Left( cPart, nI - 1 ) ) ] := UUrlDecode( SubStr( cPart, nI + 1 ) )
         ELSE
            hParameters[ UUrlDecode( cPart ) ] := NIL
         ENDIF
      NEXT

      hParClient[ 'scope' ] := hb_HGetDef( hParameters, 'm', '' )
      hParClient[ 'token' ] := hb_HGetDef( hParameters, 't', '' )

   ENDIF

   _traceSys( '>> ClientConnection > Parameters', hParClient )

   IF ValType( oWS:bValidate ) == 'B'

      _traceSys( '>> ClientConnection > Eval Validate' )

      lValidate := Eval( oWS:bValidate, hParClient )

      _traceSys( '>> ClientConnection > return', lValidate )

      IF ValType( lValidate ) != 'L' .OR. !lValidate

         _trace( 'Error Validate. Not return logic value' )

#ifndef NO_SSL
         nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ), TIMEOUT, @nErr )
#else
         hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ) )   // close handShake
#endif

         ClientClose( hSocket, NIL )
         RETU NIL
      ENDIF

   ENDIF

// ---------------------------------------------------


// Register Socket ----------------------------------------

   _traceSys( '>> ClientConnection > Register Socket'  )

   cUser  := hb_NumToHex( hSocket )
   _traceSys( '>> ClientConnection > cUser: ' + cUser )

   cToken := USetToken( cUser )
   _traceSys( '>> ClientConnection > cToken: ' + cToken )

// CHARLY -> Si bloquejo casca a la 3 recàrrega
// Pendent de revisar

// hb_mutexLock( hMutexUser )

   hClient := { => }
   hClient[ 'pSocket' ]  := hSocket
   hClient[ 'pSSL' ]   := hSSL
   hClient[ 'scope' ]  := hb_HGetDef( hParameters, 'm', '' ) // m = module
   hClient[ 'token' ]  := hb_HGetDef( hParameters, 't', '' ) // t = token
   hClient[ 'in' ]   := DToC( Date() ) + ' ' + Time()

   IF ! Empty( aI := hb_socketGetPeerName( hSocket ) )
      hClient[ 'ip' ] := aI[ 2 ]
      hClient[ 'port' ] := aI[ 3 ]
   ENDIF

   aSockets[ cUser ] := hClient

// hb_mutexUnlock( hMutexUser )

   _traceSys( '>> ClientConnection > Registered !', hClient  )

// --------------------------------------------------------

// USocketGarbage()    // IMPORTANTISIMA !!! Sino CPU almacena Sockets vacios que ocupan mucha memoria

   _traceSys( '>> ClientConnection > Init Loop accept socket request...'  )

   WHILE .T.
      cRequest = ""
      nLen = 1

      WHILE nLen > 0

         cBuffer := Space( 4096 )

#ifndef NO_SSL
         nLen := MY_SSL_READ( hSSL, hSocket, @cBuffer, TIMEOUT, @nErr )
#else
         nLen := hb_socketRecv( hSocket, @cBuffer,,, TIMEOUT )
#endif

// _d( 'ERROR: ' + str( hb_socketGetError() ) )


         IF  nLen > 0
            cRequest += Left( cBuffer, nLen )
         ELSE
            IF nLen == -1 .AND. hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
               nLen = 0
            ENDIF
         ENDIF

      END


      IF ! Empty( cRequest )

   /* OPCODE
    1.- Normal
    8.- Exiting
   */

         cRequest := UnMask( cRequest, @nOpcode )

         _traceSys( '>> ClientConnection > OPCODE: ' + LTrim( hb_CStr( nOpcode ) ) )
         _traceSys( '>> ClientConnection > REQUEST: ', cRequest  )


         DO CASE

         CASE cRequest == "exit"     // 1000 value in hex and bytes swapped

#ifndef NO_SSL
            nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "exiting", OPC_CLOSE ), TIMEOUT, @nErr )
#else
            hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "exiting", OPC_CLOSE ) )   // close handShake
#endif

         CASE cRequest == I2Bin( 0xE803 ) + "exiting"        // client answered to close handShake

            EXIT

         OTHERWISE

// Aqui hauriem de tenir una validacio per cada peticio?

            hData := hb_jsonDecode( cRequest )

            IF ValType( hData ) == 'H'

// Validate system ------------------------------------------

               lAuth := .F.

               IF ! hb_HHasKey( hData, 'msg' ) .OR. ;
                     ! hb_HHasKey( hData, 'token' ) .OR. ;
                     ! hb_HHasKey( hData, 'scope' )


#ifndef NO_SSL
                  nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ), TIMEOUT, @nErr )
#else
                  hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ) )   // close handShake
#endif

                  ClientClose( hSocket, cUser )
                  RETU NIL


               ENDIF

// Autorizacion para la conexion...

               IF hData[ 'msg' ] == 'UT Websocket Server'

// Si hemos definido rutina de validacion, la ejecutamos
// y le pasamos los parametros token, scope

                  cScope := hData[ 'scope' ]

                  IF ValType( oWS:bValidate ) == 'B'

                     _traceSys( '>> ClientConnection > Eval Validate' )

                     lValidate := Eval( oWS:bValidate, hParClient )

                     _traceSys( '>> ClientConnection > return', lValidate )

// La rutina bValidate ha de devolver un logico

                     IF ValType( lValidate ) != 'L'

                        _trace( 'Error Validate. Not return logic value' )

#ifndef NO_SSL
                        nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ), TIMEOUT, @nErr )
#else
                        hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_AUTH ) )   // close handShake
#endif

                        ClientClose( hSocket, cUser )
                        RETU NIL

                     ELSE

                        IF lValidate
                           cRequest :=  hb_jsonEncode( { 'type' => 'uws_token', 'value' => cToken } )
                        ELSE

#ifndef NO_SSL
                           nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ), TIMEOUT, @nErr )
#else
                           hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "not auth", OPC_CLOSE ) )   // close handShake
#endif

                           ClientClose( hSocket, cUser )
                           RETU NIL
                        ENDIF

                     ENDIF

                  ELSE

// Si NO hay control de validacion lo damos por valido

                     cRequest :=  hb_jsonEncode( { 'type' => 'uws_token', 'value' => cToken } )

                  ENDIF

               ENDIF

// ---------------------------------------------------------


            ELSE

// Si  definimos nuestro gestor de mensajes, lo ejecutamos
// Verificamos OpCode == 1 porque a veces llega 8

               IF nOpcode == 1 .AND. ValType( oWS:bMessage ) == 'B'

// uValue ha de ser un string o un hash
// Si es un hash habra de tener   key: type y value
// Si es un string el type == 'msg'

                  uValue  := Eval( oWS:bMessage, cUser, cRequest, hParClient )
                  cValType  := ValType( uValue )

// cValType == 'C'  -> type = 'msg', value = uValue
// cValType == 'H'   -> Si exist type
// Converrt value to string, Si hash -> jsonencode(value)
//

                  IF !Empty( uValue )

                     DO CASE
                     CASE cValType == 'C'
                        cRequest :=  hb_jsonEncode( { 'type' => 'msg', 'value' => uValue } )

                     CASE cValType == 'H'

                        hb_HCaseMatch( uValue, .F. )

                        cType  := hb_HGetDef( uValue, 'type', '*' )
                        uValue := hb_HGetDef( uValue, 'value', '' )

                        IF ! Empty( uValue )

                           cValType := ValType( uValue )

                           DO CASE
                           CASE cValType == 'C'
                           CASE cValType == 'N' ; uValue := LTrim( Str( uValue ) )
                           CASE cValType == 'L' ; uValue := if( uValue, 'true', 'false' )
                           CASE cValType == 'A' ; uValue := hb_jsonEncode( uValue )
                           CASE cValType == 'H' ; uValue := hb_jsonEncode( uValue )
                           OTHERWISE
                              uValue := hb_CStr( uValue )
                           ENDCASE
                        ENDIF

                        cRequest :=  hb_jsonEncode( { 'type' => cType, 'value' => uValue } )

                     OTHERWISE

// De moemnto si no es ni C ni H , error

#ifndef NO_SSL
                        nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( I2Bin( 0xE803 ) + "error 170", OPC_AUTH ), TIMEOUT, @nErr )
#else
                        hb_socketSend( hSocket, UMask( I2Bin( 0xE803 ) + "error 170", OPC_AUTH ) )   // close handShake
#endif

                        ClientClose( hSocket, cUser )
                        RETU NIL

                     ENDCASE

                  ELSE

                     cRequest := ''

                  ENDIF

               ENDIF

            ENDIF

            IF !Empty(  cRequest )

#ifndef NO_SSL
               nLen := MY_SSL_WRITE( hSSL, hSocket, UMask( cRequest ), TIMEOUT, @nErr  )
#else
               hb_socketSend( hSocket, UMask( cRequest ) )
#endif

            ENDIF

            USocketGarbage()


         ENDCASE

      ENDIF

      hb_idleSleep( 0.1 )

   END

   _trace( "Close socket" )

   _traceSys( '>> ClientConnection > Close socket' )

   ClientClose( hSocket, cUser )

   RETURN NIL

// ----------------------------------------------------------------//

STATIC FUNCTION ClientClose( hSocket, cUser )

   hb_socketShutdown( hSocket )
   hb_socketClose( hSocket )
   UDelClient( cUser )

   RETU .T.

// ----------------------------------------------------------------//

STATIC FUNCTION UHandShaking( hSocket, cHeaders, hSSL )

   LOCAL aHeaders := hb_ATokens( cHeaders, CRLF )
   LOCAL hHeaders := { => }, cLine
   LOCAL cAnswer, nLen
   LOCAL hCfg

   hCfg   := UGetServerInfo()[ 'websocket_cfg' ]



#ifndef NO_SSL

#else
   hSSL := nil
#endif

   _traceSys( '>> UHandShaking >', cHeaders )

// hb_default( @hSSL, nil ) // Crash

// No habria de llegar cHeader vacio...


   FOR EACH cLine in aHeaders
      hHeaders[ SubStr( cLine, 1, At( ":", cLine ) - 1 ) ] = SubStr( cLine, At( ":", cLine ) + 2 )
   NEXT


   cAnswer = "HTTP/1.1 101 Web Socket Protocol Handshake" + CRLF + ;
      "Upgrade: websocket" + CRLF + ;
      "Connection: Upgrade" + CRLF + ;
      "WebSocket-Origin: " + hCfg[ 'address' ] + CRLF + ;
      "WebSocket-Location: " + hCfg[ 'uri' ] + CRLF + ;
      "Sec-WebSocket-Accept: " + ;
      hb_base64Encode( hb_SHA1( hHeaders[ "Sec-WebSocket-Key" ] + ;
      "258EAFA5-E914-47DA-95CA-C5AB0DC85B11", .T. ) ) + CRLF + CRLF


   _traceSys( '>> UHandShaking > cAnswer', cAnswer )

#ifndef NO_SSL
   nLen := MY_SSL_WRITE( hSSL, hSocket, cAnswer, TIMEOUT )
#else
   nLen := hb_socketSend( hSocket, cAnswer,,, TIMEOUT )
#endif

   RETURN NIL

// ----------------------------------------------------------------//
// PUBLIC Functions -> For UDom() Class
// ----------------------------------------------------------------//

FUNCTION  UMask( cText, nOPCode )

   LOCAL nLen := Len( cText )
   LOCAL nFirstByte := 0
   LOCAL cHeader

   hb_default( @nOPCode, OPC_TEXT )

   nFirstByte = hb_bitSet( nFirstByte, 7 ) // 1000 0000

   nFirstByte := hb_bitOr( nFirstByte, nOPCode )  // 1000 XXXX -> is set

   DO CASE
   CASE nLen <= 125
      cHeader = Chr( nFirstByte ) + Chr( nLen )

   CASE nLen < 65536
      cHeader = Chr( nFirstByte ) + Chr( 126 ) + ;
         Chr( hb_bitShift( nLen, - 8 ) ) + Chr( hb_bitAnd( nLen, 0xFF ) )

   OTHERWISE
      cHeader = Chr( nFirstByte ) + Chr( 127 ) + NetworkULL2Bin( nLen )
   ENDCASE

   RETURN cHeader + cText

// ----------------------------------------------------------------//

FUNCTION  Unmask( cBytes, nOpcode )

   LOCAL lComplete := hb_bitTest( hb_BPeek( cBytes, 1 ), 7 )
   LOCAL nFrameLen := hb_bitAnd( hb_BPeek( cBytes, 2 ), 127 )
   LOCAL nLength, cMask, cData, cChar, cHeader := "", nCommaPos

   nOpcode := hb_bitAnd( hb_BPeek( cBytes, 1 ), 15 )

   DO CASE
   CASE nFrameLen <= 125
      nLength = nFrameLen
      cMask = SubStr( cBytes, 3, 4 )
      cData = SubStr( cBytes, 7 )

   CASE nFrameLen = 126
      nLength = ( hb_BPeek( cBytes, 3 ) * 256 ) + hb_BPeek( cBytes, 4 )
      cMask   = SubStr( cBytes, 5, 4 )
      cData   = SubStr( cBytes, 9 )

   CASE nFrameLen = 127
      nLength = NetworkBin2ULL( SubStr( cBytes, 3, 8 ) )
      cMask   = SubStr( cBytes, 11, 4 )
      cData   = SubStr( cBytes, 15 )
   ENDCASE

   cBytes = ""

   FOR EACH cChar in cData
      cBytes += Chr( hb_bitXor( Asc( cChar ), ;
         hb_BPeek( cMask, ( ( cChar:__enumIndex() - 1 ) % 4 ) + 1 ) ) )
   NEXT

   nCommaPos = At( ",", cBytes )
   cHeader = SubStr( cBytes, 1, nCommaPos - 1 )

   IF Right( cHeader, 6 ) == "base64"
      cBytes = hb_base64Decode( SubStr( cBytes, nCommaPos + 1 ) )
   ELSE
      cHeader = ""
   ENDIF

   RETURN cBytes

// ----------------------------------------------------------------//

STATIC FUNCTION  NetworkULL2Bin( n )

   LOCAL nBytesLeft := 64
   LOCAL cBytes := ""

   WHILE nBytesLeft > 0
      nBytesLeft -= 8
      cBytes += Chr( hb_bitAnd( hb_bitShift( n, - nBytesLeft ), 0xFF ) )
   END

   RETURN cBytes

// ----------------------------------------------------------------//

STATIC FUNCTION  NetworkBin2ULL( cBytes )

   LOCAL cByte, n := 0

   FOR EACH cByte in cBytes
      n += hb_bitShift( Asc( cByte ), 64 - cByte:__enumIndex() * 8 )
   NEXT

   RETURN n

// ----------------------------------------------------------------//

FUNCTION UDelClient( cUser )

// _d( '>> Cierre cliente ' + cUser )

   IF ValType( cUser ) == 'C'

      IF hb_HHasKey( aSockets, cUser )

         hb_mutexLock( hMutexUser )
         hb_HDel( aSockets, cUser )
         hb_mutexUnlock( hMutexUser )

      ENDIF

   ENDIF

   RETU NIL

// ----------------------------------------------------------------//

FUNCTION USocketGarbage( lTrace )

   LOCAL nLen   := Len( aSockets )
   LOCAL nPos   := 1
   LOCAL nActive  := 0
   LOCAL nDelete  := 0
   LOCAL cDummy   := ' '
   LOCAL aPair, hSocket, cSocket, nErr

   hb_default( @lTrace, .F. )

   _trace( '>> USocketGarbage --------------' )

   hb_mutexLock( hMutexUser )

   WHILE nPos <= nLen

      aPair := hb_HPairAt( aSockets, nPos )

      cSocket   := aPair[ 1 ]
      hSocket  := aPair[ 2 ][ 'pSocket' ]

      hb_socketRecv( hSocket, @cDummy,,, 1 )  // VULL REBRE !!! SI CASCA == 15
      nErr := hb_socketGetError()

      IF nErr == HB_SOCKET_ERR_TIMEOUT    // 2 SOCKET EXIST

         nPos++
         nActive++

      ELSEIF nErr == HB_SOCKET_ERR_CONNABORTED // 15 Conexio KO

         hb_socketShutdown( hSocket )
         hb_socketClose( hSocket )

// UDelClient( cSocket )

         hb_HDel( aSockets, cSocket )

         nLen--
         nDelete++

      ENDIF

   END

   hb_mutexUnlock( hMutexUser )

   _trace( 'Total Active: ' + LTrim( Str( nActive ) ) )
   _trace( 'Total Recolleted: ' + LTrim( Str( nDelete ) ) )

   _trace( '<< USocketGarbage --------------' )

   RETU NIL

// ----------------------------------------------------------------//

FUNCTION UWS_GetSockets(); RETU aSockets

FUNCTION UWS_InfoSSL(); RETU oWebServerSocket:InfoSSL()

// ----------------------------------------------------------------//

FUNCTION UWS_GetInfoSockets()

   LOCAL nLen  := Len( aSockets )
   LOCAL aInfo  := {}
   LOCAL n, aPair

   FOR n := 1 TO nLen

      aPair := hb_HPairAt( aSockets, n )

      AAdd( aInfo, { 'token' => aPair[ 1 ], 'ip' => aPair[ 2 ][ 'ip' ], 'scope' => aPair[ 2 ][ 'scope' ], 'in' => aPair[ 2 ][ 'in' ] } )
   NEXT

   RETU aInfo

// ----------------------------------------------------------------//

FUNCTION UWS_GetPSocket( cSocket )

   LOCAL pSocket := nil

   hb_default( @cSocket, '' )

   IF hb_HHasKey( aSockets, cSocket )
      pSocket := aSockets[ cSocket ][ 'pSocket ' ]
   ENDIF

   RETU pSocket

// ----------------------------------------------------------------//

FUNCTION UWS_GetPSSL( cSocket )

   LOCAL pSSL := nil

   hb_default( @cSocket, '' )

   IF hb_HHasKey( aSockets, cSocket )
      pSSL := aSockets[ cSocket ][ 'pSSL' ]
   ENDIF

   RETU pSSL

// ----------------------------------------------------------------//

FUNCTION UWS_Define( cScope, cToken, cOnOpen, cOnMessage, cOnClose, cOnError, lConsole )

   LOCAL hServerInfo, hCfg, cUri
   LOCAL cJS    := ''
   LOCAL cCfg

   hb_default( @cScope, '' )
   hb_default( @cToken, '' )
   hb_default( @cOnOpen, '' )
   hb_default( @cOnMessage, '' )
   hb_default( @cOnClose, '' )
   hb_default( @cOnError, '' )
   hb_default( @lConsole, .F. )

   hServerInfo := UGetServerInfo()

   IF hServerInfo[ 'websocket' ] == .F.
      _trace( 'WebSocket not initialized' )

      CODE TO cJs
      <script >
      CONSOLE.error( 'WS >> Error initializating...'  )
      </script>
      ENDTEXT

      RETU cJs
   ENDIF

   hCfg := hServerInfo[ 'websocket_cfg' ]

   IF hCfg[ 'ssl' ]
      cUri  := 'wss://'
   ELSE
      cUri  := 'ws://'
   ENDIF

   _traceSys( '>> UWS_Define > address > ' +  hCfg[ 'address' ] )

// ADDRESS debe tener el nombre del dominio, p.e charles9000@wok.ef
// En el caso del default, lo pasaremos a 0.0.0.0

   cUri  += if( hCfg[ 'address' ] == '0.0.0.0', 'localhost', hCfg[ 'address' ] ) + ':' + LTrim( Str( hCfg[ 'port' ] ) )


// cUri += '?m=test&t=mytoken666'

   IF !Empty( cScope ) .OR. !Empty( cToken )
      cUri += '?'

      IF !Empty( cScope )
         cUri += 'm=' + cScope
      ENDIF

      IF !Empty( cToken )

         IF !Empty( cScope )
            cUri += '&'
         ENDIF

         cUri += 't=' + cToken
      ENDIF

   ENDIF

// TOTI --> Ha de posarse el domini !!!! --> wss://charles9000.work.gd:8883?m=basic&t=ABC-1234
// cUri := 'wss://188.84.11.11:8883?m=basic&t=ABC-1234'
// cUri := 'wss://charles9000.work.gd:8883?m=basic&t=ABC-1234'

   _traceSys( '>> UWS_Define > cUri > ' + cUri )

   hCfg := { => }

   hCfg[ 'scope' ] := cScope
   hCfg[ 'token' ] := cToken
   hCfg[ 'uri' ] := cUri
   hCfg[ 'onopen' ] := cOnOpen
   hCfg[ 'onmessage' ] := cOnMessage
   hCfg[ 'onclose' ] := cOnClose
   hCfg[ 'onerror' ] := cOnError
   hCfg[ 'console' ] := lConsole

   cCfg := hb_jsonEncode( hCfg )

   _traceSys( '>> UWS_Define > hCfg > ', hCfg )


   CODE TO cJs PARAMS cCfg

   <script >

   $( document ).ready( function() {
   UWS_Define( '<$ cCfg $>' )
   UWS_Init();
      } )

   </script>

   ENDTEXT

   RETU cJs

#ifndef NO_SSL

// ----------------------------------------------------------------//
// Mindauga's functions
// ----------------------------------------------------------------//

STATIC FUNCTION  MY_SSL_READ( hSSL, hSocket, cBuf, nTimeout, nError )

   LOCAL nErr, nLen

   nLen := SSL_READ( hSSL, @cBuf )
   IF nLen < 0
      nErr := SSL_GET_ERROR( hSSL, nLen )

      IF nErr == HB_SSL_ERROR_WANT_READ

         nErr := hb_socketSelectRead( hSocket, nTimeout )

         IF nErr < 0
            nError := hb_socketGetError()
         ELSE  // Both cases: data received and timeout
            nError := HB_SOCKET_ERR_TIMEOUT
         ENDIF

         RETURN -1

      ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE

         nErr := hb_socketSelectWrite( hSocket, nTimeout )

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

// ----------------------------------------------------------------//

STATIC FUNCTION  MY_SSL_WRITE( hSSL, hSocket, cBuf, nTimeout, nError )

   LOCAL nErr, nLen

   nLen := SSL_WRITE( hSSL, cBuf )

   IF nLen <= 0

      nErr := SSL_GET_ERROR( hSSL, nLen )

      IF nErr == HB_SSL_ERROR_WANT_READ

         nErr := hb_socketSelectRead( hSocket, nTimeout )

         IF nErr < 0
            nError := hb_socketGetError()
            RETURN -1
         ELSE  // Both cases: data received and timeout
            RETURN 0
         ENDIF

      ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE

         nErr := hb_socketSelectWrite( hSocket, nTimeout )

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

// ----------------------------------------------------------------//

STATIC FUNCTION MY_SSL_ACCEPT( hSSL, hSocket, nTimeout )

   LOCAL nErr

   nErr := SSL_ACCEPT( hSSL )

   IF nErr > 0

      RETURN 0

   ELSEIF nErr < 0

      nErr := SSL_GET_ERROR( hSSL, nErr )

      IF nErr == HB_SSL_ERROR_WANT_READ

         nErr := hb_socketSelectRead( hSocket, nTimeout )

         IF nErr < 0

            nErr := hb_socketGetError()
         ELSE

            nErr := HB_SOCKET_ERR_TIMEOUT
         ENDIF

      ELSEIF nErr == HB_SSL_ERROR_WANT_WRITE

         nErr := hb_socketSelectWrite( hSocket, nTimeout )

         IF nErr < 0

            nErr := hb_socketGetError()
         ELSE

            nErr := HB_SOCKET_ERR_TIMEOUT
         ENDIF

      ELSE
// ? "SSL_ACCEPT() error", nErr
         nErr := 1000 + nErr
      ENDIF

   ELSE /* nErr == 0 */

      nErr := SSL_GET_ERROR( hSSL, nErr )
// ? "SSL_ACCEPT() shutdown error", nErr
      nErr := 1000 + nErr
   ENDIF

   RETURN nErr
#endif
// ----------------------------------------------------------------//

FUNCTION UWS_SendJS( cSocket, cFunction, uValue, cScope )

   LOCAL hParam := { => }

   IF Empty( cFunction ) .OR. Empty( uValue )
      RETU .F.
   ENDIF

   RETU UWS_Send( cSocket, 'js', uValue, cFunction, cScope )



// ----------------------------------------------------------------//

FUNCTION UWS_Send( cSocket, cType, cMsg, cFunction, cScope  )

   LOCAL aSockets, hInfo, c, n, aPair, nLen
   LOCAL aData   := { => }

// hb_default( @cSocket, '' )
   hb_default( @cType, 'msg' )
// hb_default( @cMsg, '' )   // No xuten be, donen errors
   hb_default( @cFunction, '' )
   hb_default( @cScope, '' )

   IF Empty( cSocket ) // .or. empty( cType )
      RETU .F.
   ENDIF

   aSockets  := UWS_GetSockets()

   IF Empty( cScope )

      IF hb_HHasKey( aSockets, cSocket )

         hInfo := aSockets[ cSocket ]

      ENDIF

      IF ! Empty( hInfo )

         IF Empty( cMsg )
            c :=  hb_jsonEncode( { 'type' => 'msg', 'value' => cType, 'function' => cFunction } )
         ELSE
            c :=  hb_jsonEncode( { 'type' => cType, 'value' => cMsg, 'function' => cFunction } )
         ENDIF


#ifndef NO_SSL
         MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )   // TIMEOUT ??
#else
         hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) )
#endif

      ELSE

         RETU .F.

      ENDIF

   ELSE

      nLen := Len( aSockets )

      FOR  n := 1 TO nLen

         cScope := Lower( cScope )

         aPair := hb_HPairAt( aSockets, n )

         cSocket := aPair[ 1 ]
         hInfo   := aPair[ 2 ]

         IF !Empty( hInfo )

            IF cScope == '*' .OR. Lower( hInfo[ 'cScope' ] ) == cScope

               IF Empty( cMsg )
                  c :=  hb_jsonEncode( { 'type' => 'msg', 'value' => cType, 'function' => cFunction } )
               ELSE
                  c :=  hb_jsonEncode( { 'type' => cType, 'value' => cMsg, 'function' => cFunction } )
               ENDIF

#ifndef NO_SSL
               MY_SSL_WRITE( hInfo[ 'pSSL' ], hInfo[ 'pSocket' ], UMask( c ) )   // TIMEOUT ??
#else
               hb_socketSend( hInfo[ 'pSocket' ], UMask( c ) )
#endif

            ENDIF

         ENDIF

      NEXT

   ENDIF

   RETU .T.

// ----------------------------------------------------------------//

FUNCTION UWS_ErrorHandler( oErr )

   LOCAL hError  := { => }
   LOCAL aStack  := {}
   LOCAL aArgs  := {}
   LOCAL nI

   IF !Empty( oErr:filename )
      hError[ 'filename' ] := oErr:filename
   ENDIF

   IF !Empty( oErr:description )
      hError[ 'description' ] := oErr:description
   ENDIF

   IF !Empty( oErr:operation )
      hError[ 'operation' ] := oErr:operation
   ENDIF

   IF !Empty( oErr:osCode )
      hError[ 'error' ] := LTrim( Str( oErr:osCode ) )
   ENDIF

   IF ValType( oErr:args ) == "A"
      AEval( oErr:args, {| X | AAdd( aArgs, hb_CStr( X ) )  } )
      hError[ 'arguments' ] := aArgs
   ENDIF

   nI := 2

   DO WHILE ! Empty( ProcName( ++nI ) )
      AAdd( aStack, ProcName( nI ) + "(" + LTrim( Str( ProcLine( nI ) ) ) + ")"  )
   ENDDO

   IF !Empty( aStack )
      hError[ 'stack' ] := aStack
   ENDIF

   hError[ 'version' ]  := OS()
   hError[ 'harbour' ]  := Version()
   hError[ 'ut' ]   := UVersion()

   IF hb_IsFunction( 'TWebVersion' )
      hError[ 'tweb' ] := Eval( &( '{|| TWebVersion()}' ) )
   ENDIF

   RETU hError

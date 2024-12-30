/*
 * HTTPD2 
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * Adapted 2022 Carles Aubia <carles9000 at gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
request hb_threadId
request __vmCountThreads
 
#include "hbclass.ch"
#include "common.ch"
#include "fileio.ch"
#include "error.ch"
#include "hbsocket.ch"
#include "hbthread.ch"
#include "FileIO.ch"

#ifndef NO_SSL
#include "hbssl.ch"
#endif

#pragma -kM+

/*
  Docs:

  RFC 1945 - Hypertext Transfer Protocol -- HTTP/1.0
  RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1
  HTTP Made Really Easy (http://www.jmarshall.com/easy/http/)
*/

#define HTTPD2_VERSION			 	'2.00'

#define THREAD_COUNT_PREALLOC   		10
#define THREAD_COUNT_MAX       		200
#define TIME_KEEP_ALIVE        		60

#define SESSION_NAME 	 			'USESSID'
#define SESSION_PREFIX  			'sess_'
#define SESSION_DURATION 			3600

#define CR_LF                       (CHR(13)+CHR(10))

#xcommand TEXT <into:TO,INTO> <v> => #pragma __cstream|<v>+=%s


//	_d esta definit al meu modul...
//#xcommand ? [<explist,...>] => _d( [,<explist>] )
//#xcommand ? [<explist,...>] => if( lDebug, _d( [,<explist>] ), nil )
//	--------------------------------

// No debug
// #xtranslate QOUT([<x,...>]) =>




STATIC oServer
STATIC hCfg
STATIC nFiles_Size	:= 0

//THREAD STATIC s_aSessionData
THREAD STATIC s_cResult, s_nStatusCode, s_aHeader
THREAD STATIC cProcError 
THREAD STATIC cFilePrg, cFileHtml


//THREAD STATIC s_oSession

MEMVAR server, get, post, files, cookie, session, httpd

FUNC Httpd2() 		
	oServer := UHttpd2():New()
RETURN oServer

FUNC UHttpd2New()	
	oServer := UHttpd2():New()
RETURN oServer

FUNC UVersion()	; RETURN HTTPD2_VERSION 

CLASS UHttpd2 MODULE FRIENDLY

EXPORTED:
  METHOD New()    				CONSTRUCTOR
  METHOD Run(aConfig)
  METHOD Stop()
  
  METHOD SetPort( nPort )		INLINE ::aConfig[ 'Port' ] := if( valtype( nPort ) == 'N', nPort, 80 )
  METHOD SetSSL( lSSL )		INLINE ::aConfig[ 'SSL' ] := if( valtype( lSSL ) == 'L', lSSL, .F. )
  METHOD Route( cRoute, bAction, cMethod )
  METHOD RouteDelete( cRoute ) 
  METHOD SetDirFiles( cRoute )	
  METHOD SetCertificate( PrivateKeyFilename, CertificateFilename )
  METHOD SetErrorStatus( nStatus, cPage, cAjax )
  
  
  METHOD Statistics()
  METHOD SetInfo( cKey, uValue )	
  
  METHOD Dbg(...)   					INLINE if( ::lDebug, _d(...), nil )
  METHOD Time()   					INLINE time() 
  METHOD IsInit()   					INLINE ::lInit



  DATA 	cError       						INIT ""
  DATA 	cPathHtml     						INIT ""
  DATA 	cPathLog      						INIT ""
  DATA 	cPathTmp							INIT ""
  
  DATA 	nTime_Keep_Alive					INIT TIME_KEEP_ALIVE
  
  //	Sessions
  DATA 	cSessionPath  						INIT ""
  DATA 	cSessionName  						INIT SESSION_NAME
  DATA 	cSessionPrefix						INIT SESSION_PREFIX
  DATA 	cSessionSeed  						INIT 'UhHttPPd2@2022!'
  DATA 	nSessionDuration  					INIT SESSION_DURATION
  DATA 	nSessionLifeDays					INIT 3
  DATA 	nSessionGarbage					INIT 1000
  DATA 	lSessionCrypt						INIT .F.
 
 
 
  //	Files Upload 
  DATA 	nfiles_size_garbage_inspector		INIT 2000000	//	kb.
  DATA 	nfiles_lifeDays					INIT 1
  DATA 	nfile_max_size						INIT 100000		//	kb.
  DATA  aFilesAllowed						INIT {}
 
  
  DATA 	bInit 
  DATA 	lDebug 								INIT .f. 
  DATA 	lDbfAutoClose						INIT .t. 
  
  DATA 	lUtf8								INIT .F. 
  
  DATA 	bRX									INIT {|| 'RX not exist' }
  DATA 	bPostRun 
  DATA  aInfo								INIT {=>}
    DATA lInit							INIT .F.
  
		 
 
  

HIDDEN:
  DATA aConfig					INIT {=>}
  DATA aErrorRoutes				INIT {}

  DATA aFirewallFilter

  DATA hAccessLog
  DATA hErrorLog
  DATA hReq
  //DATA hSession
  

  DATA hmtxQueue
  DATA hmtxLog
  DATA hmtxSession		// Necesary?
  DATA hmtxReq
  DATA hmtxFiles

  DATA hListen
  DATA hSSLCtx
//  DATA aSession

  DATA lStop


  METHOD LogAccess()
  METHOD LogError(cError)
ENDCLASS




METHOD New() CLASS UHttpd2

	::aConfig := {"SSL"                  => .F., ;
                   "Port"                 => 80, ;
                   "BindAddress"          => "0.0.0.0", ;
                   "AccessLogFilename"    => "access.log", ;
                   "ErrorLogFilename"     => "error.log", ;
                   "ErrorBloc"            => {|| NIL}, ;
                   "Idle"                 => {|| NIL}, ;
                   "Mount"                => {=>}, ;
				   "ErrorStatus"          => {}, ;
                   "PrivateKeyFilename"   => "", ;
                   "CertificateFilename"  => "", ;
                   "FirewallFilter"		  => "" ; 		//	"192.168.0.0/16";		//	 "FirewallFilter"       => "0.0.0.0/0" ;
                  }                   				  
	
	::aConfig[ 'Mount' ][ '/api' ]		:= { 'action' => {|hSrv| UWebApi( hSrv, self ) }, 'method' => 'POST', 'regexp' => NIL , 'cargo' => '' }
	::aConfig[ 'Mount' ][ '/files/*' ]	:= { 'action' => {|hSrv| UProcFiles(HB_DIRBASE() + "/files/" + hSrv[ 'path' ], .F.)}, 'method' => 'GET,POST,PUT,DELETE,OPTIONS', 'regexp' => NIL, 'cargo' => '' }		

	
	::cPathHtml 	:= HB_DIRBASE() + 'html'
	::cPathLog 	:= HB_DIRBASE() + 'log'
	::cSessionPath	:= HB_DIRBASE() + '.sessions'
	::cPathTmp		:= HB_DIRBASE() + '.tmp'
	
	::bInit			:= {|| Qout( 'HTTPD2 Vrs. ' +  HTTPD2_VERSION + ' - ' + HB_COMPILER() + CR_LF + 'Escape for exit...' ) } 
	
	s_cResult := ''

	cFilePrg	:= ''
	
retu SELF 

METHOD SetDirFiles( cRoute, lIndex ) CLASS UHttpd2

	hb_default( @cRoute, '' )
	hb_default( @lIndex, .F. )
	
	if empty( cRoute )
		retu nil
	endif
	
	if substr( cRoute, 1, 1 ) != '/'
		cRoute := '/' + cRoute 
	endif

	::aConfig[ 'Mount' ][ cRoute + '/*' ]	:= { 'action' => {|hSrv| UProcFiles(HB_DIRBASE() + cRoute + "/" + hSrv[ 'path' ], lIndex )}, 'method' => 'GET,POST,PUT,DELETE,OPTIONS', 'regexp' => NIL, 'cargo' => ''  }
	
retu cRoute 

METHOD SetCertificate( PrivateKeyFilename, CertificateFilename ) CLASS UHttpd2

	hb_default( @PrivateKeyFilename, '' )
	hb_default( @CertificateFilename, '' )
	
	::aConfig[ 'PrivateKeyFilename' ] 	:= PrivateKeyFilename
	::aConfig[ 'CertificateFilename' ] 	:= CertificateFilename

retu nil 

METHOD SetErrorStatus( nStatus, cPage, cAjax ) CLASS UHttpd2

	hb_default( @nStatus, 0 )		//	404
	hb_default( @cPage, '' )		//	error\404.html
	hb_default( @cAjax, '' )		//	'page not found !'
	
	Aadd( ::aConfig[ 'ErrorStatus' ], { 'status' => nStatus, 'page' => cPage, 'ajax' => cAjax } )
	
retu nil 

METHOD Route( cRoute, bAction, cMethod, uCargo ) CLASS UHttpd2

	local cHtml, cExt, pRoute

	hb_default( @cRoute, '' )	
	hb_default( @cMethod, 'GET,POST,PUT,DELETE,OPTIONS' )	
	hb_default( @uCargo, '' )	
	
	if at( 'OPTIONS', cMethod ) == 0
		cMethod += ',OPTIONS'
	endif
	
	if empty( cRoute )
		retu nil
	endif
	
	//	Valid route
		
		pRoute := HB_RegexComp( cRoute + '$' )
			
		if pRoute == NIL 	

			Aadd( ::aErrorRoutes, cRoute )			
			return nil 
		endif
		
	//	-----------------------------				
	
	if valtype( bAction ) == 'C'
	
		cHtml 	:= bAction
		cExt 	:= lower( hb_fnameext( cHtml ) )
	
		do case
			case cExt == '.html' .or. cExt == '.htm' 

				bAction := {|hSrv| ULoadPage( hSrv, cHtml ) }
				 
			case cExt == '.prg'

				bAction := {|hSrv| ULoadPrg( hSrv, cHtml ) }	

			case cExt == '.rx'
			
				uCargo 	:= cHtml
				bAction	:= ::bRX
				 
			//	
			//	if hb_isfunction( 'rx_testcode' )
			//		bAction := &( "{|o,oSrv| rx_testcode(o,oSrv) }" )
			//	endif
				 
			case cExt == ''			
				
				if hb_isfunction( cHtml ) 
		
					bAction := &( "{|o,oSrv| " + cHtml + "(o,oSrv) }" )																	
					
				else
				
					//	For example if you define Route( 'examples'	, 'examples/*' ) 
					//	Objetivo, listar indice del directorio
				
					if substr( cHtml, len(cHtml), 1 ) == '*'					
						bAction := {|| URedirect( cHtml ) }
					else								
						bAction := {|| UDo_Error( "Api function doesn't exist. => " + cHtml, nil, 100 ) }					
					endif
				
				endif 
				 
		endcase
		
	elseif valtype( bAction ) == 'B'
		//	at moment nothing....
	else
		retu nil
	endif
	
	if !( cRoute == '/' ) 	
		if Substr( cRoute, 1, 1 ) != '/'
			cRoute := '/' + cRoute 
		endif
	endif
	
	::aConfig[ 'Mount' ][ cRoute ] := { 'action' => bAction, 'method' => cMethod, 'regexp' => pRoute, 'cargo' => uCargo }

retu nil 

METHOD RouteDelete( cRoute ) CLASS UHttpd2

	hb_default( @cRoute, '' )

	if !empty( cRoute )
	
		cRoute := '/' + cRoute
	
		if HB_HHasKey( ::aConfig[ 'Mount' ], cRoute )

			HB_HDel( ::aConfig[ 'Mount' ], cRoute )
		endif
	
	endif

retu nil 

METHOD SetInfo( cKey, uValue ) CLASS UHttpd2

	WHILE ! ::lInit 
		hb_idleSleep( 0.1 )
	END 		
	
	::aInfo[ cKey ] := uValue

RETU NIL 



METHOD Run(aConfig) CLASS UHttpd2

	LOCAL hSocket, nI, aI, xValue, aThreads, nJobs, nWorkers

	hb_default( @aConfig, {} )	

	IF ! HB_MTVM()
		Self:cError := "Multithread support required"
		RETURN .F.
	ENDIF

	if len( ::aErrorRoutes ) > 0
		_t( 'Warning: Error config routes' )
		for nI := 1 to len( ::aErrorRoutes )
			_t( '  => ' + ::aErrorRoutes[nI] )
		next
		_t( '----------------------------' )
    endif
  
	if !hb_DirExists( ::cPathHtml )
		HB_DirBuild( ::cPathHtml )
	endif 
	
	if !hb_DirExists( ::cPathLog )
		HB_DirBuild( ::cPathLog )
	endif 
	
	if !hb_DirExists( ::cSessionPath )
		HB_DirBuild( ::cSessionPath )		
	endif 	
	
	if !hb_DirExists( ::cPathTmp )
		HB_DirBuild( ::cPathTmp )			
	endif 	
	
	UFilesTmpCollector( .T. )		//	Del temporally files...

  FOR EACH xValue IN aConfig
    IF ! HB_HHasKey(Self:aConfig, xValue:__enumKey) .OR. VALTYPE(xValue) != VALTYPE(Self:aConfig[xValue:__enumKey])
      Self:cError := "Invalid config option '" + xValue:__enumKey + "'"
      RETURN .F.
    ENDIF
    Self:aConfig[xValue:__enumKey] := xValue
  NEXT

  IF Self:aConfig["SSL"]
#ifndef NO_SSL

	if ! file( Self:aConfig["PrivateKeyFilename"]	)
		Self:cError := "Certificate - Privetekey is missing !"
		_t( Self:cError )
		RETURN .F.		
	endif
	
	if ! file( Self:aConfig["CertificateFilename"]	)
		Self:cError := "Certificate is missing !"
		_t( Self:cError )
		RETURN .F.		
	endif	



    SSL_INIT()
    DO WHILE RAND_STATUS() != 1
      RAND_add(STR(hb_random(), 18, 15) + STR(hb_milliseconds(), 20), 1)
    ENDDO

    Self:hSSLCtx := SSL_CTX_NEW(HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER)
    SSL_CTX_SET_OPTIONS(Self:hSSLCtx, HB_SSL_OP_NO_TLSv1)

    IF SSL_CTX_USE_PRIVATEKEY_FILE(Self:hSSLCtx, Self:aConfig["PrivateKeyFilename"], HB_SSL_FILETYPE_PEM) != 1
      Self:cError := "Invalid private key file"
      RETURN .F.
    ENDIF
	
    IF SSL_CTX_USE_CERTIFICATE_FILE(Self:hSSLCtx, Self:aConfig["CertificateFilename"], HB_SSL_FILETYPE_PEM) != 1
      Self:cError := "Invalid certificate file"
      RETURN .F.
    ENDIF
#else    
    Self:cError := "SSL not supported"
    RETURN .F.
#endif
  ENDIF
  


  IF Self:aConfig["Port"] < 1 .OR. Self:aConfig["Port"] > 65535
    Self:cError := "Invalid port number"
    RETURN .F.
  ENDIF

  IF ParseFirewallFilter(Self:aConfig["FirewallFilter"], @aI)
    Self:aFirewallFilter := aI
  ELSE
    Self:cError := "Invalid firewall filter"
    RETURN .F.
  ENDIF
  
  
  Self:hReq 		:= { 'ip' => {=>}, 'proc' => {=>} , 'func' => {=>} }
  
  /*
  hCfg	:= { 'sessionpath' 		=> Self:cSessionPath ,;			 
			 'sessionduration' 	=> Self:nSessionDuration ,;
			 'sessionlifedays' 	=> Self:nSessionLifeDays ,;
			 'sessiongarbage' 	=> Self:nSessionGarbage ;
			} 				
	*/

  IF (Self:hAccessLog := FOPEN(Self:cPathLog + '/' + Self:aConfig["AccessLogFilename"], FO_CREAT + FO_WRITE)) == -1
    Self:cError :=  "Access log file open error " + LTRIM(STR(FERROR()))
    RETURN .F.
  ENDIF
  FSEEK(Self:hAccessLog, 0, FS_END)

  IF (Self:hErrorLog := FOPEN(Self:cPathLog + '/' + Self:aConfig["ErrorLogFilename"], FO_CREAT + FO_WRITE)) == -1
    Self:cError :=  "Error log file open error " + LTRIM(STR(FERROR()))
    FCLOSE(Self:hAccessLog)
    RETURN .F.
  ENDIF
  FSEEK(Self:hErrorLog, 0, FS_END)

  Self:hmtxQueue   	:= hb_mutexCreate()
  Self:hmtxLog     	:= hb_mutexCreate()
  Self:hmtxSession		:= hb_mutexCreate()	//NEcesary?
  Self:hmtxReq 		:= hb_mutexCreate()
  Self:hmtxFiles 		:= hb_mutexCreate()

  IF EMPTY(Self:hListen := hb_socketOpen()) == NIL
    Self:cError :=  "Socket create error: " + hb_socketErrorString()
    FCLOSE(Self:hErrorLog)
    FCLOSE(Self:hAccessLog)
    RETURN .F.
  ENDIF
	
  IF ! hb_socketBind(Self:hListen, {HB_SOCKET_AF_INET, Self:aConfig["BindAddress"], Self:aConfig["Port"]})
    Self:cError :=  "Bind error: " + hb_socketErrorString()
    hb_socketClose(Self:hListen)
    FCLOSE(Self:hErrorLog)
    FCLOSE(Self:hAccessLog)
    RETURN .F.
  ENDIF

  IF ! hb_socketListen(Self:hListen)
    Self:cError :=  "Listen error: " + hb_socketErrorString()
    hb_socketClose(Self:hListen)
    FCLOSE(Self:hErrorLog)
    FCLOSE(Self:hAccessLog)
    RETURN .F.
  ENDIF

  
  aThreads := {}
  FOR nI := 1 TO THREAD_COUNT_PREALLOC
    AADD(aThreads, hb_threadStart(HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self))
  NEXT

	_t( '================' )
	_t( 'HTTPD2 Vrs. ' +  HTTPD2_VERSION )
	_t( '================' )  

	::aInfo[ 'sessionpath'] 	:= Self:cSessionPath 	
	::aInfo[ 'sessionduration']	:=  Self:nSessionDuration 
	::aInfo[ 'sessionlifedays']	:=  Self:nSessionLifeDays 
	::aInfo[ 'sessiongarbage']	:=  Self:nSessionGarbage  
	::aInfo[ 'port' ]			:= ::aConfig[ 'Port']
	::aInfo[ 'ssl'] 			:= ::aConfig[ 'SSL']
	::aInfo[ 'version'] 		:= HTTPD2_VERSION
	::aInfo[ 'start'] 			:= dtoc(date()) + ' ' + time()
	::aInfo[ 'debug'] 			:= ::lDebug
	::aInfo[ 'utf8'] 			:= ::lUtf8	
	::aInfo[ 'errorstatus'] 	:= ::aConfig[ 'ErrorStatus']
	::aInfo[ 'websocket'] 		:= .F.

  //::aInfo := hCfg
  
  if valtype( Self:bInit ) == 'B'		
	eval( Self:bInit, ::aInfo )
  endif  

  ::lInit := .T.  


  Self:lStop := .F.
//  Self:aSession := {=>}

  DO WHILE .T.
  
  	
    IF EMPTY(hSocket := hb_socketAccept(Self:hListen,, 1000))
      IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
        EVAL(Self:aConfig["Idle"], Self)
        IF Self:lStop
			EXIT
        ENDIF
      ELSE
        Self:LogError("[error] Accept error: " + hb_socketErrorString())
      ENDIF
    ELSE
	
      Self:Dbg( "New connection => " + hb_NumToHex( hSocket ) )         
	  
      IF hb_mutexQueueInfo(Self:hmtxQueue, @nWorkers, @nJobs) .AND. ;
         LEN(aThreads) < THREAD_COUNT_MAX .AND. ;
         nJobs >= nWorkers
        AADD(aThreads, hb_threadStart(HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self))
      ENDIF
      hb_mutexNotify(Self:hmtxQueue, hSocket)
    ENDIF
  ENDDO
  hb_socketClose(Self:hListen)
	
  // End child threads
  AEVAL(aThreads, {|| hb_mutexNotify(Self:hmtxQueue, NIL)})
  AEVAL(aThreads, {|h| hb_threadJoin(h)})

  FCLOSE(Self:hErrorLog)
  FCLOSE(Self:hAccessLog)
  
  	
RETURN .T.


METHOD Stop() CLASS UHttpd2
  ::Dbg( "stopping" )
  Self:lStop := .T.
RETURN NIL


METHOD LogError(cError) CLASS UHttpd2
  hb_mutexLock(Self:hmtxLog)
  FWRITE(Self:hErrorLog, ">>> " + DTOS(DATE()) + " " + TIME() + " " + cError + " " + HB_OSNewLine())
  hb_mutexUnlock(Self:hmtxLog)
RETURN NIL


METHOD LogAccess() CLASS UHttpd2
LOCAL cDate := DTOS(DATE()), cTime := TIME()
  hb_mutexLock(Self:hmtxLog)
  FWRITE(Self:hAccessLog, ;
         server["REMOTE_ADDR"] + " - - [" + RIGHT(cDate, 2) + "/" + ;
         {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}[VAL(SUBSTR(cDate, 5, 2))] + ;
         "/" + LEFT(cDate, 4) + ":" + cTime + ' +0000] "' + server["REQUEST_ALL"] + '" ' + ;
         LTRIM(STR(s_nStatusCode)) + " " + LTRIM(STR(LEN(s_cResult))) + ;
         ' "' + server["HTTP_REFERER"] + '" "' + server["HTTP_USER_AGENT"] + ;
         '"' + HB_OSNewLine())
  hb_mutexUnlock(Self:hmtxLog)
RETURN NIL

METHOD Statistics() CLASS UHttpd2

	local hReq := {=>}

	hb_mutexLock(Self:hmtxReq)
	
		hReq := Self:hReq
	
		//_d( 'STATISTICS' )
		//_d( Self:hReq )
		//_d( Self:hReqIP )

	hb_mutexUnlock(Self:hmtxReq)	
	
RETURN hReq


STATIC FUNC IPAddr2Num(cIP)
LOCAL aA, n1, n2, n3, n4
  aA := hb_regex("^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$", cIP)
  IF LEN(aA) == 5 .AND. (n1 := VAL(aA[2])) <= 255 .AND. (n2 := VAL(aA[3])) <= 255 .AND. ;
                        (n3 := VAL(aA[4])) <= 255 .AND. (n4 := VAL(aA[5])) <= 255
    RETURN (((n1 * 256) + n2) * 256 + n3) * 256 + n4
  ENDIF
RETURN NIL


STATIC FUNC ParseFirewallFilter(cFilter, aFilter)
LOCAL cExpr, nI, cI, nPrefix, nAddr, nAddr2, nPos, nPos2, lDeny, aDeny, aI

  aFilter := {=>}
  aDeny := {}
  FOR EACH cExpr IN hb_aTokens(cFilter, " ")
    IF ! EMPTY(cExpr)
      IF lDeny := (LEFT(cExpr, 1) == "!")
        cExpr := SUBSTR(cExpr, 2)
      ENDIF
      IF (nI := AT("/", cExpr)) > 0
        cI := SUBSTR(cExpr, nI + 1)
        cExpr := LEFT(cExpr, nI - 1)
        IF "." $ cI
          IF (nI := IPAddr2Num(cI)) == NIL
            RETURN .F.
          ENDIF
          nPrefix := 32
          DO WHILE hb_bitAnd(nI, 1) == 0
            nPrefix--
            nI := hb_bitShift(nI, -1)
          ENDDO
          IF nI + 1 != hb_bitShift(1, nPrefix)
            RETURN .F.
          ENDIF
        ELSE
          nPrefix := VAL(cI)
          IF nPrefix < 0 .OR. nPrefix > 32 .OR. ! (HB_NTOS(nPrefix) == cI)
            RETURN .F.
          ENDIF
        ENDIF
      ELSE
        nPrefix := 32
      ENDIF
      IF (nAddr := IPAddr2Num(cExpr)) == NIL
        RETURN .F.
      ENDIF
      nPrefix := 0x100000000 - hb_bitShift(1, 32 - nPrefix)

      // Remove unnecessary network address part
      nAddr := hb_bitAnd(nAddr, nPrefix)
      nAddr2 := hb_bitOr(nAddr, hb_bitXor(nPrefix, 0xFFFFFFFF))

      IF lDeny
        AADD(aDeny, {nAddr, nAddr2})
      ELSE
        // Add to filter
        HB_HHasKey(aFilter, nAddr, @nPos)
        IF nPos == 0 .OR. HB_HValueAt(aFilter, nPos) + 1 < nAddr
          // Does not overlap/glue with nPos
          // So, add new interval
          aFilter[nAddr] := nAddr2
          nPos++
        ENDIF
        HB_HHasKey(aFilter, nAddr2 + 1, @nPos2)
        // Merge and delete inner subintervals
        aFilter[HB_HKeyAt(aFilter, nPos)] := MAX(HB_HValueAt(aFilter, nPos2), nAddr2)
        DO WHILE nPos2-- > nPos
          HB_HDelAt(aFilter, nPos + 1)
        ENDDO
      ENDIF
    ENDIF
  NEXT

  FOR EACH aI IN aDeny
    nAddr := aI[1]
    nAddr2 := aI[2]

    // Delete from filter
    HB_HHasKey(aFilter, nAddr, @nPos)
    IF nPos == 0 .OR. HB_HValueAt(aFilter, nPos) < nAddr
      nPos++
    ENDIF 
    IF nPos > LEN(aFilter)
      LOOP
    ENDIF
   
    HB_HHasKey(aFilter, nAddr2, @nPos2) 
    IF nPos2 > 0 .AND. HB_HValueAt(aFilter, nPos2) > nAddr2
      aFilter[nAddr2 + 1] := HB_HValueAt(aFilter, nPos2)
    ENDIF
    IF nAddr > HB_HKeyAt(aFilter, nPos)
      aFilter[HB_HKeyAt(aFilter, nPos)] := nAddr - 1
      nPos++
    ENDIF
    DO WHILE nPos2-- >= nPos
      HB_HDelAt(aFilter, nPos)
    ENDDO
  NEXT
RETURN .T.



#ifndef NO_SSL

//STATIC FUNC MY_SSL_READ(hSSL, hSocket, cBuf, nTimeout, nError)
FUNC MY_SSL_READ(hSSL, hSocket, cBuf, nTimeout, nError)
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
      //? "SSL_READ() error", nErr
      nError := 1000 + nErr
      RETURN -1
    ENDIF
  ENDIF
RETURN nLen


//STATIC FUNC MY_SSL_WRITE(hSSL, hSocket, cBuf, nTimeout, nError)
FUNC MY_SSL_WRITE(hSSL, hSocket, cBuf, nTimeout, nError)
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
      //? "SSL_WRITE() error", nErr
      nError := 1000 + nErr
      RETURN -1
    ENDIF
  ENDIF
RETURN nLen


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

STATIC FUNC ProcessConnection(oServer)
LOCAL hSocket, cRequest, aI, nLen, nErr, nTime, nReqLen, cBuf, aServer
LOCAL aMount, cMount, aMethods, lPassMethod, n, nRoutes, aPair, aRoute, aMatch, cMethod
#ifndef NO_SSL
LOCAL hSSL
#endif

  ERRORBLOCK({|o| UErrorHandler(o, oServer)})

  PRIVATE server, get, post, files, cookie, session, httpd

  httpd := oServer

  /* main worker thread loop */
  DO WHILE .T.
    hb_mutexSubscribe(oServer:hmtxQueue,, @hSocket)
    IF hSocket == NIL
      EXIT
    ENDIF

    /* Prepare server variable and clone it for every query, 
       because request handler script can ruin variable value */
    aServer := {=>}
    aServer["HTTPS"] := oServer:aConfig["SSL"]
    IF ! EMPTY(aI := hb_socketGetPeerName(hSocket))
      aServer["REMOTE_ADDR"] := aI[2]
      aServer["REMOTE_HOST"] := aServer["REMOTE_ADDR"]  // no reverse DNS
      aServer["REMOTE_PORT"] := aI[3]
    ENDIF
    IF ! EMPTY(aI := hb_socketGetSockName(hSocket))
      aServer["SERVER_ADDR"] := aI[2]
      aServer["SERVER_PORT"] := aI[3]
    ENDIF
	


    /* Firewall */
    nLen := IPAddr2Num(aServer["REMOTE_ADDR"])
    HB_HHasKey(oServer:aFirewallFilter, nLen, @nErr)
    IF nErr > 0 .AND. nLen <= HB_HValueAt(oServer:aFirewallFilter, nErr)
      oServer:Dbg( "Firewall denied", aServer["REMOTE_ADDR"] )
      hb_socketShutdown(hSocket)
      hb_socketClose(hSocket)
      LOOP
    ENDIF

#ifndef NO_SSL
    IF oServer:aConfig["SSL"]
      hSSL := SSL_NEW(oServer:hSSLCtx)
      SSL_SET_MODE(hSSL, hb_bitOr(SSL_GET_MODE(hSSL), HB_SSL_MODE_ENABLE_PARTIAL_WRITE))
      hb_socketSetBlockingIO(hSocket, .F.)
      SSL_SET_FD(hSSL, hb_socketGetFD(hSocket))

      nTime := hb_milliseconds()
      DO WHILE .T.
        IF (nErr := MY_SSL_ACCEPT(hSSL, hSocket, 1000)) == 0
          EXIT
        ELSE
          IF nErr == HB_SOCKET_ERR_TIMEOUT
            //IF (hb_milliseconds() - nTime) > 1000 * 30 .OR. oServer:lStop
            //IF (hb_milliseconds() - nTime) > 1000 * SESSION_TIMEOUT .OR. oServer:lStop
            IF (hb_milliseconds() - nTime) > ( 1000 * oServer:nTime_Keep_Alive ) .OR. oServer:lStop
              oServer:Dbg( "SSL accept timeout", hSocket )
              EXIT
            ENDIF
          ELSE
            oServer:Dbg( "SSL accept error:", nErr, hb_socketErrorString(nErr) )
            EXIT
          ENDIF
        ENDIF
      ENDDO

      IF nErr != 0
        oServer:Dbg( "Close connection.. => " + hb_NumToHex( hSocket ) )
		
		DbCloseAll()	//	CAF				
		
        hb_socketShutdown(hSocket)
        hb_socketClose(hSocket)
        LOOP
      ENDIF

      aServer["SSL_CIPHER"] 			:= SSL_GET_CIPHER(hSSL)
      aServer["SSL_PROTOCOL"] 			:= SSL_GET_VERSION(hSSL)
      aServer["SSL_CIPHER_USEKEYSIZE"] := SSL_GET_CIPHER_BITS(hSSL, @nErr)
      aServer["SSL_CIPHER_ALGKEYSIZE"] := nErr
      aServer["SSL_VERSION_LIBRARY"] 	:= SSLEAY_VERSION(HB_SSLEAY_VERSION )
      aServer["SSL_SERVER_I_DN"] 		:= X509_NAME_ONELINE(X509_GET_ISSUER_NAME(SSL_GET_CERTIFICATE(hSSL)))
      aServer["SSL_SERVER_S_DN"] 		:= X509_NAME_ONELINE(X509_GET_SUBJECT_NAME(SSL_GET_CERTIFICATE(hSSL)))
    ENDIF
#endif

    /* loop for processing connection */

    /* Set cRequest to empty string here. This enables request pipelining */
    cRequest := ""
    DO WHILE ! oServer:lStop

      /* receive query header */
      nLen := 1
      nTime := hb_milliseconds()
      cBuf := SPACE(4096)
      DO WHILE AT(CR_LF + CR_LF, cRequest) == 0
#ifndef NO_SSL
        IF oServer:aConfig["SSL"]
          nLen := MY_SSL_READ(hSSL, hSocket, @cBuf, 1000, @nErr)
        ELSE
#endif
          nLen := hb_socketRecv(hSocket, @cBuf,,, 1000)
          IF nLen < 0
            nErr := hb_socketGetError()
          ENDIF
#ifndef NO_SSL
        ENDIF
#endif
        IF nLen > 0
          cRequest += LEFT(cBuf, nLen)
        ELSEIF nLen == 0
          /* connection closed */
          EXIT
        ELSE
          /* nLen == -1  socket error */
          IF nErr == HB_SOCKET_ERR_TIMEOUT            
		  
            IF (hb_milliseconds() - nTime) > ( 1000 * oServer:nTime_Keep_Alive ) .OR. oServer:lStop
              oServer:Dbg( "Receive timeout => " + hb_NumToHex( hSocket ) )
              EXIT
            ENDIF
          ELSE
            oServer:Dbg( "Receive error: " + ltrim(str(nErr)) + ' ' + hb_socketErrorString(nErr) )
            EXIT
          ENDIF
        ENDIF
      ENDDO

      IF nLen <= 0 .OR. oServer:lStop
        EXIT
      ENDIF

      // PRIVATE
      server := HB_HCLONE(aServer)
      get := {=>}
      post := {=>}
      files := {}
      cookie := {=>}	 
      session := NIL	
      s_cResult := ''
      s_aHeader := {}
      s_nStatusCode := 200
//      s_aSessionData := NIL		//	Necesary ?      
	  
      nReqLen := ParseRequestHeader(@cRequest)
	  
      IF nReqLen == NIL
        USetStatusCode(400)
        UAddHeader("Connection", "close")
      ELSE

        /* receive query body */

		//	Search Route
		
			cMount		:= server["SCRIPT_NAME"]	
			aMount 		:= oServer:aConfig["Mount"]
			nRoutes		:= len( aMount )
			aRoute 		:= {}	
			lPassMethod := .T.			




			IF cMount == '/api'
		
				lPassMethod := .t.
				Aadd( aRoute, { 'route' => '/api', 'config' => aMount[ '/api' ], 'match' => {} } )			
				
			else
			
				FOR n :=  1 to nRoutes
				
					aPair := HB_HPairAt( aMount, n )												
					
					if ( aPair[2][ 'regexp' ] != NIL ) 
					
						aMatch := HB_Regex( aPair[2][ 'regexp' ], cMount )

						if !empty( aMatch )   

							//	Check Method						
				
								cMethod 	:= aPair[2][ 'method' ]
								aMethods 	:= hb_Atokens( cMethod, ',' )													
		
							if AScan( aMethods, {| x | upper( x ) ==  server["REQUEST_METHOD"]} ) > 0
								lPassMethod := .t.
								Aadd( aRoute, { 'route' => aPair[1], 'config' => aPair[2], 'match' => aMatch } )									
							endif
							
						endif
						
					endif
				
				NEXT 
			
			ENDIF

		//	Validate Route													

			IF len( aRoute ) == 1   		//	Perfect. match Route
				//_d( 'Perfect. We execute: ' + aRoute[1]['route'] )
			ELSEIF len(aRoute) > 1 			//	More than 1 casuistic. 
				//	At the moment in this case, we can select First coincidence	and show warning for dbg			
				_t( 'Warning! More than 1 route' )
				for n = 1 to len( aRoute )
					_t( '  >> ' + aRoute[n][ 'route'] )
				next
				_t( '--------------------------' )				
			ELSE 
				//_d( 'Ni idea...' )
			ENDIF
		
        nLen := 1
        nTime := hb_milliseconds()
        cBuf := SPACE(4096)				
	
        DO WHILE LEN(cRequest) < nReqLen
#ifndef NO_SSL
          IF oServer:aConfig["SSL"]
            nLen := MY_SSL_READ(hSSL, hSocket, @cBuf, 1000, @nErr)
          ELSE
#endif
            nLen := hb_socketRecv(hSocket, @cBuf,,, 1000)
            IF nLen < 0
              nErr := hb_socketGetError()
            ENDIF
#ifndef NO_SSL
          ENDIF
#endif
          IF nLen > 0
            cRequest += LEFT(cBuf, nLen)
          ELSEIF nLen == 0
            /* connection closed */
            EXIT
          ELSE
            /* nLen == -1  socket error */
            IF nErr == HB_SOCKET_ERR_TIMEOUT
              IF (hb_milliseconds() - nTime) > 1000 * 120 .OR. oServer:lStop
                oServer:Dbg( "Receive timeout => " + hb_NumToHex( hSocket ))
                EXIT
              ENDIF
            ELSE
              oServer:Dbg( "Receive error: " + ltrim(str(nErr)) + ' ' + hb_socketErrorString(nErr) )
              EXIT
            ENDIF
          ENDIF
        ENDDO

        IF nLen <= 0 .OR. oServer:lStop
          EXIT
        ENDIF

        //? cRequest
        ParseRequestBody(LEFT(cRequest, nReqLen), oServer )
        cRequest := SUBSTR(cRequest, nReqLen + 1)			

        /* Deal with supported protocols and methods */
        IF ! (LEFT(server["SERVER_PROTOCOL"], 5) == "HTTP/")
          USetStatusCode(400) /* Bad request */
          UAddHeader("Connection", "close")
        ELSEIF ! (SUBSTR(server["SERVER_PROTOCOL"], 6) $ "1.0 1.1")
          USetStatusCode(505) /* HTTP version not supported */
//      ELSEIF !(server["REQUEST_METHOD"] $ "GET POST")				// Check for Lautaro. Check method ! only GET POST 
      ELSEIF ! lPassMethod																					
			USetStatusCode(404) /* Not implemented */		  		  
        ELSE
          IF server["SERVER_PROTOCOL"] == "HTTP/1.1"
            IF LOWER(server["HTTP_CONNECTION"]) == "close"
              UAddHeader("Connection", "close")
            ELSE
              UAddHeader("Connection", "keep-alive")
            ENDIF
          ENDIF

          /* Do the job */	
  
          ProcessRequest(oServer, aRoute )

        ENDIF
      ENDIF /* request header ok */

      // Send response
      cBuf := MakeResponse()

      DO WHILE LEN(cBuf) > 0 .AND. ! oServer:lStop
#ifndef NO_SSL
        IF oServer:aConfig["SSL"]
          nLen := MY_SSL_WRITE(hSSL, hSocket, cBuf, 1000, @nErr)
        ELSE
#endif
          nLen := hb_socketSend(hSocket, cBuf,,, 1000)
          IF nLen < 0
            nErr := hb_socketGetError()
          ENDIF
#ifndef NO_SSL
        ENDIF
#endif
        IF nLen < 0
          oServer:Dbg( "send error:", nErr, hb_socketErrorString(nErr) )
          EXIT
        ELSEIF nLen > 0
          cBuf := SUBSTR(cBuf, nLen + 1)
        ENDIF
      ENDDO

      IF oServer:lStop
        EXIT
      ENDIF

      oServer:LogAccess()

      IF HB_IsString( UGetHeader("Connection") ) .and. LOWER(UGetHeader("Connection")) == "close" .OR. server["SERVER_PROTOCOL"] == "HTTP/1.0"
        EXIT
      ENDIF
    ENDDO

#ifndef NO_SSL
    hSSL := NIL
#endif
    oServer:Dbg( "Close connection. => " + hb_NumToHex( hSocket )  )
    hb_socketShutdown(hSocket)
    hb_socketClose(hSocket)
					
	DbCloseAll()	//	CAF		
	
  ENDDO
RETURN 0

FUNCTION USetFilePrg( cFile )
	cFilePrg := cFile
RETU NIL

FUNCTION UGetFilePrg(); RETU cFilePrg
FUNCTION UGetFileHtml(); RETU cFileHtml

STATIC FUNCTION ProcessRequest( oServer, aRoute )
LOCAL nI, aMount, cMount, cPath, bEval, xRet, nT := HB_MILLISECONDS()
local h := {=>}
local aMethods
local cError := '',  cDbg
local nIni, oError, n, aParams
local cInfoCode := ''
local lIsRoute := .t.
local hMtr 		:= {=>}

//local oUDom
//* Error vars


	oError := NIL
	
  // Search mounting table
  aMount := oServer:aConfig["Mount"]


	if len( aRoute ) > 0	
		cPath  := ''
		cMount := aRoute[1][ 'route' ]

	else 

		cMount := server["SCRIPT_NAME"]


		nI := LEN(cMount)
		DO WHILE (nI := HB_RAT("/", cMount,, nI)) > 0
		  IF HB_HHasKey(aMount, LEFT(cMount, nI) + "*")
			//? "HAS", LEFT(cMount, nI) + "*"
			cMount := LEFT(cMount, nI) + "*"
			cPath := SUBSTR(server["SCRIPT_NAME"], nI + 1)
			EXIT
		  ENDIF
		  IF --nI == 0
			EXIT
		  ENDIF
		ENDDO
		
	endif
	

  IF cPath != NIL .or. server["REQUEST_METHOD"] == 'OPTIONS' 
  
	server['route'] := cMount

	h[ 'path' ] 		:= cPath					//	Request file like .../proto.dos/files/favicon.ico
	h[ 'pathhtml' ]	 	:= oServer:cPathHtml		//	Root path where will be html files
	h[ 'pathlog' ] 		:= oServer:cPathLog
	h[ 'debug' ] 		:= oServer:lDebug	
	h[ 'server' ] 		:= server					//	Environment files server
	h[ 'get' ] 			:= get						//	
	h[ 'post' ] 		:= post						//	
	h[ 'files' ] 		:= files
	h[ 'route' ] 		:= if( len(aRoute) > 0 , aRoute[1], {} )
	

	//	Parameters if you used friendly url

		if len( aRoute ) > 0 .and. valtype( h[ 'get' ] ) == 'H'		//	Sure!
		
			aParams := aRoute[1][ 'match' ]
		
			for n := 2 to len( aParams )
			
				h[ 'get' ][ '_value' + ltrim(str(n-1)) ] := aParams[n]
			
			next 
		
		endif 

	//	----------------------------------------
	
	if valtype( aMount[cMount] ) == 'B'
	
		bEval := aMount[cMount]
		
	elseif valtype( aMount[cMount] ) == 'H'	

		if HB_HHasKey( aMount[cMount], 'action' )
	
	
			bEval := aMount[cMount][ 'action' ]
			
		endif
		
		if HB_HHasKey( aMount[cMount], 'method' )

			aMethods := hb_Atokens( aMount[cMount][ 'method'] , ',' )		
	
		
			if server["REQUEST_METHOD"] != 'OPTIONS' .and. AScan( aMethods, {| x | upper( x ) ==  server["REQUEST_METHOD"]} ) == 0
			
				if UIsAjax()

					UAddHeader("Content-Type", "application/json")
					
					xRet := {=>}
					xRet[ 'success' ] := .f.
					xRet[ 'html' ] := ''
					xRet[ 'msg' ] := 'Error route...' +  cMount + ', dont declarated method ' + server["REQUEST_METHOD"]
					
					
					UWrite( hb_jsonEncode( xRet ) )
					
				else								
					USetStatusCode( 404 )					
					_t( '>> Error route: ' +  cMount + ', dont declarated method ' + server["REQUEST_METHOD"] )
				endif

				retu ''
			endif			
		
		endif 			
		
	endif


	nIni := hb_milliseconds()		// CAF

    //BEGIN SEQUENCE WITH {|oErr| UErrorHandler( oErr, oServer, @cError )}
    BEGIN SEQUENCE WITH {|oErr| oError := oErr, Break( oErr ) }
    
	  

		/* Hem d'explorar aquesta opció de pasar primer el oDoms
		   Ho vaig porbar però no xutava bé. Això és el que fem 
		   a UWebApi
		   
			oUDom := UDom():New( h[ 'post' ], h[ 'files' ] )
			xRet := EVAL(bEval, oUDom, h )
		*/
		
	    xRet := EVAL(bEval, h )
		
		//	If exist session, we can save data values...	

			//	Charly -> Pendiente de chequear por que a veces
			//	se evalua UWebApi y realiza el USessionWrite() y despues le USessionDelte().
			//	y viene aqui y lo vuelve a ejecutar. no pasa nada
			//  porque como oSession == NIL ni escribe ni nada.
			//	Pero pendiente de revisar
				
				USessionWrite()	
				UsessionDelete()
			
			//	-------------------------------------------------
			
		

      IF VALTYPE(xRet) == "C"

        UWrite(xRet)		

      ELSEIF VALTYPE(xRet) == "H"
        
		if UIsAjax()

			UAddHeader("Content-Type", "application/json")
			
			UWrite( hb_jsonEncode( xRet ) )
			
		else		

			if xRet[ 'success' ]									
				UWrite( xRet[ 'html' ] )
			else 									
				//UWrite( UMsgError(  xRet[ 'msg' ] ) )
				UDo_ErrorMsg( xRet[ 'msg' ] )
			endif
			
		endif
			
		
		
		//	Hauriem de mir de fer un hb_jsondecode(xRet) 
		//	Despres ... no de som agafar-ho
      ENDIF
	  
	  //	Quizas tendriamos de meter el cierre de session en 
	  //	este punto...
	  

    //RECOVER 
    RECOVER USING oError

		cInfoCode 	:= UGetInfoCode( oError )
		
		cError 		+= UErrorWeb()			

		cError 		+= UErrorGetDescription( oError, cInfoCode )									
		
		cError 		+= UErrorGetSys()						

		if UIsAjax()		
			UWrite( cError )
		else			
			UWrite( UMsgError( cError ) )
		endif
		
		retu ''

	  
    END SEQUENCE
	
    //oServer:Dbg( 'Lap. ' + ltrim(str(hb_milliseconds()-nIni )) + 'ms.' ) 	//CAF	

	if oServer:lDbfAutoClose	// CAF
		//oServer:Dbg( 'DbCloseAll(1)' )
		DBCLOSEALL()		
	endif

    // Unlock session					//	Necesary?
	/*
    IF s_aSessionData != NIL
      session := NIL
      hb_mutexUnlock(s_aSessionData[1])
      s_aSessionData := NIL
    ENDIF
	*/
  ELSE
	
	USetStatusCode(404)

	
	/*No chuta
	if UIsAjax()
		UWrite( "Route don't exist => " +  cMount )
	endif
	*/
  ENDIF                    

	cDbg := server["REMOTE_HOST"] + ':' + ltrim(str(server["REMOTE_PORT"])) 


	//	Metrics... <<-----------------------------------	
	
	hMtr[ 'key' ] := ''
	hMtr[ 'run' ] := 0
	hMtr[ 'time' ]:= 0
	hMtr[ 'max' ] := 0
	hMtr[ 'min' ] := 999999
	hMtr[ 'lapsus' ] := HB_MILLISECONDS() - nT
	
	hMtr[ 'metric' ]  := ''


  if hb_HHaskey( h, 'post' ) .and. hb_HHaskey( h[ 'post' ], 'api' )  	
	
	cDbg += ' >> Api ' + h[ 'post' ][ 'api' ] + ' => ' + h[ 'post' ][ 'proc' ] + ' : ' + LTRIM(STR(hMtr[ 'lapsus' ])) +  "ms" 
		
	hMtr[ 'key' ] := h[ 'post' ][ 'api' ] + ':' + h[ 'post' ][ 'proc' ] 	
	
	hb_mutexLock(oServer:hmtxReq)
	
		if HB_HHasKey( oServer:hReq[ 'ip' ], server["REMOTE_ADDR"] )		
			hMtr[ 'run' ] 	:=  oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"]  ][ 'run' ] + 1
			oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"] ] := { 'run' => hMtr[ 'run' ], 'last' => DToC(date()) + ' ' + time() }
		else
			oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"] ] := { 'run' => 1, 'last' => DToC(date()) + ' ' + time() } 
		endif
		
		if HB_HHasKey( oServer:hReq[ 'proc' ], hMtr[ 'key' ] ) 
		
			hMtr[ 'run' ] 	:= oServer:hReq[ 'proc'][ hMtr[ 'key' ] ][ 'run' ] + 1
			hMtr[ 'time' ] 	:= oServer:hReq[ 'proc'][ hMtr[ 'key' ] ][ 'time' ] + hMtr[ 'lapsus' ]
			hMtr[ 'max' ] 	:= Max( oServer:hReq[ 'proc'][ hMtr[ 'key' ] ][ 'max' ], hMtr[ 'lapsus' ] )
			hMtr[ 'min' ] 	:= Min( oServer:hReq[ 'proc'][ hMtr[ 'key' ] ][ 'min' ], hMtr[ 'lapsus' ] )
		
			oServer:hReq[ 'proc'][ hMtr[ 'key' ] ] := { 'run' => hMtr[ 'run' ], 'time' => hMtr[ 'time' ], 'min' => hMtr[ 'min' ], 'max' => hMtr[ 'max' ] } 
		else 
			oServer:hReq[ 'proc'][ hMtr[ 'key' ] ] := { 'run' => 1, 'time' => hMtr[ 'lapsus' ], 'min' => hMtr[ 'lapsus' ], 'max' => hMtr[ 'lapsus' ] } 
		endif				
	
	hb_mutexUnlock(oServer:hmtxReq)	
	
	
  ELSE			
	
	cDbg += ' >> ' + cMount + " : " + LTRIM(STR(hMtr[ 'lapsus' ])) + "ms" 
	
	if cMount != '/files/*' .and. !( cMount == '/' ) .and. lisRoute
	
		hb_mutexLock(oServer:hmtxReq)
		
			if HB_HHasKey( oServer:hReq[ 'ip' ], server["REMOTE_ADDR"] )		
				hMtr[ 'run' ] 	:=  oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"]  ][ 'run' ] + 1
				oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"] ] := { 'run' => hMtr[ 'run' ], 'last' => DToC(date()) + ' ' + time() }
			else
				oServer:hReq[ 'ip' ][ server["REMOTE_ADDR"] ] := { 'run' => 1, 'last' => DToC(date()) + ' ' + time() } 
			endif
			
			if HB_HHasKey( oServer:hReq[ 'func' ], cMount ) 
			
				hMtr[ 'run' ] 	:= oServer:hReq[ 'func'][ cMount ][ 'run' ] + 1
				hMtr[ 'time' ] 	:= oServer:hReq[ 'func'][ cMount ][ 'time' ] + hMtr[ 'lapsus' ]
				hMtr[ 'max' ] 	:= Max( oServer:hReq[ 'func'][ cMount ][ 'max' ], hMtr[ 'lapsus' ] )
				hMtr[ 'min' ] 	:= Min( oServer:hReq[ 'func'][ cMount ][ 'min' ], hMtr[ 'lapsus' ] )
			
				oServer:hReq[ 'func'][ cMount ] := { 'run' => hMtr[ 'run' ], 'time' => hMtr[ 'time' ], 'min' => hMtr[ 'min' ], 'max' => hMtr[ 'max' ] } 
			else 
				oServer:hReq[ 'func'][ cMount ] := { 'run' => 1, 'time' => hMtr[ 'lapsus' ], 'min' => hMtr[ 'lapsus' ], 'max' => hMtr[ 'lapsus' ] } 
			endif				
		
		hb_mutexUnlock(oServer:hmtxReq)						
		
	endif
	
	oServer:Dbg( cDbg )
	
  endif 
 
  
RETURN ''

function DoMetric()


retu nil 


function UGetInfoCode( oError, cCode, cCodePP )

	local n, nL, nLineError, nOffset, nLin, nPos, cLine
	local aTagLine := {}
	local aLines := {}
	local cText := ''
	local cInfoCode := ''

	hb_default( @cCode, '' )
	hb_default( @cCodePP, '' )

	//	En el código preprocesado, buscamos tags #line (#includes,#commands,...)

		aLines = hb_ATokens( cCodePP, chr(10) )

		for n = 1 to Len( aLines )   

			cLine := aLines[ n ] 
		  
			if substr( cLine, 1, 5 ) == '#line' 

				nLin := Val(Substr( cLine, 6 ))				

				Aadd( aTagLine, { n, (nLin-n-1) } )
				
				
			endif 	  

		next 
	
	
	//	Buscamos si oError nos da Linea
	
		nL 			:= 0					
		
		if ! Empty( oError:operation )
	  
			nPos := AT(  'line:', oError:operation )

			if nPos > 0 				
				nL := Val( Substr( oError:operation, nPos + 5 ) ) 
			endif	  	  
		  
		endif 


	//	Procesamos Offset segun linea error					
	
		nLineError := nL
		
		if nL > 0					
		
			//	Xec vectors 	
			//	{ nLine, nOffset }
			//	{ 1, 5 }, { 39, 8 }
			
			for n := 1  to len( aTagLine ) 
				
				if aTagLine[n][1] < nL 
					nOffset 	:= aTagLine[n][2]
					nLineError	:= nL + nOffset 
				endif		
			
			next 
	
			nLineError	-= 1	//	__Inline() + CRLF
			
		else 
		

			
		endif	


		if At(  'line:', oError:operation ) > 0
			oError:operation := 'Line: ' + ltrim(str(nLineError))
		endif
		
		if ! empty( cCode )
			cInfoCode += UErrorGetCode( cCode, nLineError )	
		endif
		

retu cInfoCode 

FUNC UIsAjax()

	local lIsAjax := .f.

	if HB_HHasKey( server, 'HTTP_X_REQUESTED_WITH' )
		lIsAjax := lower( server[ 'HTTP_X_REQUESTED_WITH' ] ) == 'xmlhttprequest'
	endif

RETU lIsAjax 

function USetErrorStatus( nStatus, cPage, cAjax )
	oServer:SetErrorStatus( nStatus, cPage, cAjax )
retu nil


STATIC FUNC ParseRequestHeader(cRequest)
LOCAL aRequest, aLine, nI, nJ, cI, nK, nL, nContentLength := 0
LOCAL aCookies 


  nI := AT(CR_LF + CR_LF, cRequest)
  aRequest := hb_aTokens(LEFT(cRequest, nI - 1), CR_LF)
  cRequest := SUBSTR(cRequest, nI + 4)

  aLine := hb_aTokens(aRequest[1], " ")

  server["REQUEST_ALL"] := aRequest[1]
  IF LEN(aLine) == 3 .AND. LEFT(aLine[3], 5) == "HTTP/"
    server["REQUEST_METHOD"] := aLine[1]
    server["REQUEST_URI"] := aLine[2]
    server["SERVER_PROTOCOL"] := aLine[3]
  ELSE
    server["REQUEST_METHOD"] := aLine[1]
    server["REQUEST_URI"] := IIF(LEN(aLine) >= 2, aLine[2], "")
    server["SERVER_PROTOCOL"] := IIF(LEN(aLine) >= 3, aLine[3], "")
    RETURN NIL
  ENDIF

  // Fix invalid queries: bind to root
  IF ! (LEFT(server["REQUEST_URI"], 1) == "/")
    server["REQUEST_URI"] := "/" + server["REQUEST_URI"]
  ENDIF

  IF (nI := AT("?", server["REQUEST_URI"])) > 0
    server["SCRIPT_NAME"] := LEFT(server["REQUEST_URI"], nI - 1)
    server["QUERY_STRING"] := SUBSTR(server["REQUEST_URI"], nI + 1)
  ELSE
    server["SCRIPT_NAME"] := server["REQUEST_URI"]
    server["QUERY_STRING"] := ""
  ENDIF

  server["HTTP_ACCEPT"] := ""
  server["HTTP_ACCEPT_CHARSET"] := ""
  server["HTTP_ACCEPT_ENCODING"] := ""
  server["HTTP_ACCEPT_LANGUAGE"] := ""
  server["HTTP_CONNECTION"] := ""
  server["HTTP_HOST"] := ""
  server["HTTP_KEEP_ALIVE"] := ""
  server["HTTP_REFERER"] := ""
  server["HTTP_USER_AGENT"] := ""
  

  FOR nI := 2 TO LEN(aRequest)
    IF aRequest[nI] == "";  EXIT
    ELSEIF (nJ := AT(":", aRequest[nI])) > 0
      cI := ALLTRIM(SUBSTR(aRequest[nI], nJ + 1))
      SWITCH UPPER(LEFT(aRequest[nI], nJ - 1))
        CASE "COOKIE"
	
          server["HTTP_COOKIE"] := cI
		  
		  aCookies = HB_ATOKENS( cI, ';' )
		  
		  for nL = 1 to len( aCookies )
			nK := At( '=', aCookies[nL] )
			
			if nK > 0 			
				cookie[ ALLTRIM(UPPER(LEFT(aCookies[nL], nK - 1))) ] := Alltrim( SUBSTR(aCookies[nL], nK + 1) )
			endif 			
		  next 

          EXIT
        CASE "CONTENT-LENGTH"
          nContentLength := VAL(cI)
          EXIT
        CASE "CONTENT-TYPE"
          server["CONTENT_TYPE"] := cI
          EXIT  
		  
        OTHERWISE
          server["HTTP_" + STRTRAN(UPPER(LEFT(aRequest[nI], nJ - 1)), "-", "_")] := cI	  
          EXIT
      ENDSWITCH
    ENDIF
  NEXT
  IF !(server["QUERY_STRING"] == "")
    FOR EACH cI IN hb_aTokens(server["QUERY_STRING"], "&")
      IF (nI := AT("=", cI)) > 0
        get[UUrlDecode(LEFT(cI, nI - 1))] := UUrlDecode(SUBSTR(cI, nI + 1))
      ELSE
        get[UUrlDecode(cI)] := NIL
      ENDIF
    NEXT
  ENDIF
  

  
  
RETURN nContentLength



STATIC FUNC ParseRequestBody(cRequest, oServer )
	LOCAL nI, cPart
	local cEncoding := ''

  IF HB_HHasKey(server, "CONTENT_TYPE") 

		IF (nI := AT("CHARSET=", UPPER(server["CONTENT_TYPE"]))) > 0
		  cEncoding := UPPER(SUBSTR(server["CONTENT_TYPE"], nI + 8))
		ENDIF		
		
		DO CASE
  
			CASE LEFT(server["CONTENT_TYPE"], 33) == "application/x-www-form-urlencoded"				

				IF ! (cRequest == "")
				  IF cEncoding == "UTF-8" .AND. oServer:lUtf8 == .F. 		//	Transform UTF8toStr
					 FOR EACH cPart IN hb_aTokens(cRequest, "&")
					   IF (nI := AT("=", cPart)) > 0
						 post[HB_UTF8TOSTR(UUrlDecode(LEFT(cPart, nI - 1)))] := HB_UTF8TOSTR(UUrlDecode(SUBSTR(cPart, nI + 1)))
					   ELSE
						 post[HB_UTF8TOSTR(UUrlDecode(cPart))] := NIL
					   ENDIF
					 NEXT
				  ELSE
					 FOR EACH cPart IN hb_aTokens(cRequest, "&")
					   IF (nI := AT("=", cPart)) > 0
						 post[UUrlDecode(LEFT(cPart, nI - 1))] := UUrlDecode(SUBSTR(cPart, nI + 1))
					   ELSE
						 post[UUrlDecode(cPart)] := NIL
					   ENDIF
					 NEXT
				  ENDIF
				  
				ENDIF
				  
			CASE LEFT(server["CONTENT_TYPE"], 16 ) == "application/json"	//	Workaround of Quim
			
				if hb_jsonDecode( cRequest, @post ) == 0
					post := {=>}
				endif			
				  
			CASE LEFT(server["CONTENT_TYPE"], 19 ) == "multipart/form-data"
			
				post 	:= {=>}
				files	:= {}
				
				UParseMultipart( @cRequest, @Post, @files )	

			OTHERWISE 
			
				if oServer:lUtf8 
					post["RAW"] := hb_UTF8ToStr( UUrlDecode( cRequest ) )
				else
					post["RAW"] := UUrlDecode( cRequest ) 
				endif			
				
		ENDCASE 
	
   ELSE
   
		//	Workaround of Rafa

		if !empty( cRequest )
	
			if oServer:lUtf8 
				post["RAW"] := hb_UTF8ToStr( UUrlDecode( cRequest ) )
			else
				post["RAW"] := UUrlDecode( cRequest ) 
			endif
		endif
	
  ENDIF
RETURN NIL

// Check--> https://www.geeksforgeeks.org/how-to-upload-files-asynchronously-using-jquery/ 

//	------------------------------------------------------------------- //

FUNCTION UGetServer(); RETU oServer
FUNCTION UGetServerInfo(); RETU oServer:aInfo
FUNCTION USetServerInfo( cKey, uValue ); RETU  oServer:SetInfo( cKey, uValue )	
FUNCTION UGetParams(); retu server
FUNCTION UIsRunning(); retu oServer:lInit 


//FUNCTION UGetStatistics(); RETU oServer
FUNCTION UGetRoutes(); retu oServer:aConfig[ 'Mount' ]
FUNCTION UGetMethod(); RETU server['REQUEST_METHOD']


FUNCTION UGetMethodsRoute()	//	Methods defined in route:  GET, POST

	local cMethods := ''
	local c			:= ''
	local aRoutes 	:= UGetRoutes() 
	local cRoute	:= UGetParams()[ 'route' ]
	local nI, nLen 
	local aMethods

	if HB_HHasKey( aRoutes, cRoute )
	
		c  := aRoutes[cRoute][ 'method' ]
	
		aMethods 	:= hb_Atokens( c , ',' )
		nLen 		:= len( aMethods )
		
		for nI := 1 to nLen 		
			
			if aMethods[nI] != '*'
				if ! empty( cMethods )
					cMethods += ', '
				endif
				cMethods += aMethods[nI]
			endif
			
		next 

	endif

RETU cMethods 
  
FUNCTION UGetRoutesList( uCargo )
	local hRoutes 	:= UGetRoutes()
	local aRoutes	:= {}
	local nLen 		:= len(hRoutes)
	local nI, cId, aPair, hData
	
	hb_default( @uCargo, '' )
	
	if empty( uCargo )
		for nI := 1 to nLen 
			Aadd( aRoutes, HB_HKeyAt( hRoutes, nI ) )		
		next 	
	else
		for nI := 1 to nLen 
			aPair  	:= HB_HPairAt( hRoutes, nI )
			cId 	:= aPair[1]
			hData	:= aPair[2]
			if hData[ 'cargo' ] == uCargo
				Aadd( aRoutes, cId )		
			endif
		next 		
	endif

retu aRoutes

FUNCTION UIsRoute( cKey )

	local nPos
	
	hb_default( @cKey, '' )
	
	cKey := lower( cKey )
	
	nPos := Ascan( oServer:aConfig[ 'Mount' ], {|x| lower(x) == cKey } )
	
retu nPos > 0


//	------------------------------------------------------------------- //

FUNCTION UGet( cKey, uDefault )

	hb_default( @cKey, '' )
	hb_default( @uDefault, '' )
	
	if empty( cKey )
		retu get 
	endif	

	HB_HCaseMatch( get, .f. )
	
retu HB_HGetDef( get, cKey, uDefault ) 

//	------------------------------------------------------------------- //

FUNCTION UPost( cKey, uDefault )

	hb_default( @cKey, '' )
	hb_default( @uDefault, '' )
	
	if empty( cKey )
		retu post
	endif	

	HB_HCaseMatch( post, .f. )
	
retu HB_HGetDef( post, cKey, uDefault ) 
	
//	------------------------------------------------------------------- //

FUNCTION UCookie( cKey, uDefault )

	hb_default( @cKey, '' )
	hb_default( @uDefault, '' )
	
	if empty( cKey )
		retu cookie
	endif	

	HB_HCaseMatch( cookie, .f. )
	
retu HB_HGetDef( cookie, cKey, uDefault ) 
	
//	------------------------------------------------------------------- //

FUNCTION UFiles()

retu files

//	------------------------------------------------------------------- //

FUNCTION UServer( cKey, uDefault )

	hb_default( @cKey, '' )
	hb_default( @uDefault, '' )
	
	if empty( cKey )
		retu server 
	endif

	HB_HCaseMatch( server, .f. )
	
retu HB_HGetDef( server, cKey, uDefault ) 
	
//	------------------------------------------------------------------- //

STATIC FUNC MakeResponse()
LOCAL cRet, cStatus
LOCAL  aErrorStatus, nPos, cFile

  IF UGetHeader("Content-Type") == NIL
    UAddHeader("Content-Type", "text/html")
  ENDIF
  UAddHeader("Date", HttpDateFormat(HB_DATETIME()))

  cRet := IIF(server["SERVER_PROTOCOL"] == "HTTP/1.0", "HTTP/1.0 ", "HTTP/1.1 ")
  SWITCH s_nStatusCode
    CASE 100 ;  cStatus := "100 Continue"                        ;  EXIT
    CASE 101 ;  cStatus := "101 Switching Protocols"             ;  EXIT
    CASE 200 ;  cStatus := "200 OK"                              ;  EXIT
    CASE 201 ;  cStatus := "201 Created"                         ;  EXIT
    CASE 202 ;  cStatus := "202 Accepted"                        ;  EXIT
    CASE 203 ;  cStatus := "203 Non-Authoritative Information"   ;  EXIT
    CASE 204 ;  cStatus := "204 No Content"                      ;  EXIT
    CASE 205 ;  cStatus := "205 Reset Content"                   ;  EXIT
    CASE 206 ;  cStatus := "206 Partial Content"                 ;  EXIT
    CASE 300 ;  cStatus := "300 Multiple Choices"                ;  EXIT
    CASE 301 ;  cStatus := "301 Moved Permanently"               ;  EXIT
    CASE 302 ;  cStatus := "302 Found"                           ;  EXIT
    CASE 303 ;  cStatus := "303 See Other"                       ;  EXIT
    CASE 304 ;  cStatus := "304 Not Modified"                    ;  EXIT
    CASE 305 ;  cStatus := "305 Use Proxy"                       ;  EXIT
    CASE 307 ;  cStatus := "307 Temporary Redirect"              ;  EXIT
    CASE 400 ;  cStatus := "400 Bad Request"                     ;  EXIT
    CASE 401 ;  cStatus := "401 Unauthorized"                    ;  EXIT
    CASE 402 ;  cStatus := "402 Payment Required"                ;  EXIT
    CASE 403 ;  cStatus := "403 Forbidden"                       ;  EXIT
    CASE 404 ;  cStatus := "404 Not Found"                       ;  EXIT
    CASE 405 ;  cStatus := "405 Method Not Allowed"              ;  EXIT
    CASE 406 ;  cStatus := "406 Not Acceptable"                  ;  EXIT
    CASE 407 ;  cStatus := "407 Proxy Authentication Required"   ;  EXIT
    CASE 408 ;  cStatus := "408 Request Timeout"                 ;  EXIT
    CASE 409 ;  cStatus := "409 Conflict"                        ;  EXIT
    CASE 410 ;  cStatus := "410 Gone"                            ;  EXIT
    CASE 411 ;  cStatus := "411 Length Required"                 ;  EXIT
    CASE 412 ;  cStatus := "412 Precondition Failed"             ;  EXIT
    CASE 413 ;  cStatus := "413 Request Entity Too Large"        ;  EXIT
    CASE 414 ;  cStatus := "414 Request-URI Too Long"            ;  EXIT
    CASE 415 ;  cStatus := "415 Unsupprted Media Type"           ;  EXIT                                                       
    CASE 416 ;  cStatus := "416 Requested Range Not Satisfiable" ;  EXIT
    CASE 417 ;  cStatus := "417 Expectation Failed"              ;  EXIT
    CASE 500 ;  cStatus := "500 Internal Server Error"           ;  EXIT
    CASE 501 ;  cStatus := "501 Not Implemented"                 ;  EXIT
    CASE 502 ;  cStatus := "502 Bad Gateway"                     ;  EXIT
    CASE 503 ;  cStatus := "503 Service Unavailable"             ;  EXIT
    CASE 504 ;  cStatus := "504 Gateway Timeout"                 ;  EXIT
    CASE 505 ;  cStatus := "505 HTTP Version Not Supported"      ;  EXIT
    OTHERWISE
      cStatus := "500 Internal Server Error"
  ENDSWITCH

  cRet += cStatus + CR_LF
  
  IF s_nStatusCode != 200
  
	aErrorStatus := uGetServerInfo()[ 'errorstatus' ]
	
	nPos := Ascan( aErrorStatus, {|u| u['status'] == s_nStatusCode } )
 
	IF nPos > 0
	
		s_cResult := ''
		cFile := ''
	
		 IF UIsAjax()			
			cFile := aErrorStatus[ nPos ][ 'ajax' ] 			
		 else 		 
			cFile := aErrorStatus[ nPos ][ 'page' ] 			
		 endif 
		 
		if file( UGetServer():cPathHtml + '\'  + cFile )
			s_cResult := ULoadHtml( cFile )
		else 
			s_cResult := cFile
		endif		 

	ELSE
		s_cResult := "<html><body><h1>" + cStatus + "</h1></body></html>"
	ENDIF
  ENDIF
  UAddHeader("Content-Length", LTRIM(STR(LEN(s_cResult))))
  AEVAL(s_aHeader, {|x| cRet += x[1] + ": " + x[2] + CR_LF})
  
  

  cRet += CR_LF
  //? cRet
  cRet += s_cResult
  
  
  
  
RETURN cRet


STATIC FUNC HttpDateFormat(tDate)
  tDate -= HB_UTCOFFSET() / (3600 * 24)
RETURN {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"}[DOW(tDate)] + ", " + ;
       PADL(DAY(tDate), 2, "0") + " " + ;
       {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}[MONTH(tDate)] + ;
       " " + PADL(YEAR(tDate), 4, "0") + " " + HB_TTOC(tDate, "", "HH:MM:SS") + " GMT" // TOFIX: time zone


STATIC FUNC HttpDateUnformat(cDate, tDate)
LOCAL nMonth, tI
  // TODO: support outdated compatibility format RFC2616
  IF LEN(cDate) == 29 .AND. RIGHT(cDate, 4) == " GMT" .AND. SUBSTR(cDate, 4, 2) == ", "
    nMonth := ASCAN({"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", ;
                     "Oct", "Nov", "Dec"}, SUBSTR(cDate, 9, 3))
    IF nMonth > 0
      tI := HB_STOT(SUBSTR(cDate, 13, 4) + PADL(nMonth, 2, "0") + SUBSTR(cDate, 6, 2) + STRTRAN(SUBSTR(cDate, 18, 8), ":", ""))
      IF ! EMPTY(tI)
        tDate := tI + HB_UTCOFFSET() / (3600 * 24)
        RETURN .T.
      ENDIF
    ENDIF
  ENDIF
RETURN .F.


/********************************************************************
  Public functions
********************************************************************/
PROC USetStatusCode(nStatusCode)
  s_nStatusCode := nStatusCode
RETURN



FUNC UGetHeader(cType)
	LOCAL nI
	
	hb_default( @cType, '' )
	
	if !empty(cType) 
		IF (nI := ASCAN(s_aHeader, {|x| UPPER(x[1]) == UPPER(cType)})) > 0
			RETURN s_aHeader[nI, 2]
		ENDIF
	else	
		retu s_aHeader	
	endif
	
RETURN NIL


FUNC UAddHeader(cType, cValue)
LOCAL nI

	IF pCount() == 0
		retu s_aHeader 
	ENDIF

  IF (nI := ASCAN(s_aHeader, {|x| UPPER(x[1]) == UPPER(cType)})) > 0
    s_aHeader[nI, 2] := cValue
  ELSE
    AADD(s_aHeader, {cType, cValue})
  ENDIF
  
RETURN NIL 


PROC URedirect(cURL, nCode)

	local xRet
	
	IF nCode == NIL;  nCode := 303
	ENDIF


	if UIsAjax()

		UAddHeader("Content-Type", "application/json")
		
		xRet := {=>}
		
		xRet[ 'url' ] := cURL
		xRet[ 'target' ] := '_self'				
		
		UWrite( hb_jsonEncode( { 'url' => xRet } ) )
		
	else

		USetStatusCode(nCode)
		UAddHeader("Location", cURL)

	endif
  
RETURN

FUNCTION UWrite(...)

    LOCAL aParams 	:= hb_AParams()
    LOCAL n    		:= Len( aParams )
    LOCAL i

    IF n == 0
       RETURN NIL
    ENDIF

	FOR i = 1 TO n - 1
	  s_cResult += UValtoChar( aParams[ i ] ) + ' '
 
	NEXT

	s_cResult += UValtoChar( aParams[ n ] )

   
retu nil 


FUNC UOsFileName(cFileName)
  IF HB_OSPathSeparator() != "/"
    RETURN STRTRAN(cFileName, "/", HB_OSPathSeparator())
  ENDIF
RETURN cFileName


FUNC UHtmlEncode(cString)
   local cChar, cResult := "" 

   for each cChar in cString
      do case
      case cChar == "<"
            cChar = "&lt;"

      case cChar == '>'
            cChar = "&gt;"     
            
      case cChar == "&"
            cChar = "&amp;"     

      case cChar == '"'
            cChar = "&quot;"    
            
      case cChar == " "
            cChar = "&nbsp;"               
      endcase
      cResult += cChar 
   next
    
return cResult 


FUNC UUrlEncode(cString)
  LOCAL nI, cI, cRet := ""

  FOR nI := 1 TO LEN(cString)
    cI := SUBSTR(cString, nI, 1)
    IF cI == " "
      cRet += "+"
    ELSEIF ASC(cI) >= 127 .OR. ASC(cI) <= 31 .OR. cI $ '=&%+'
      cRet += "%" + HB_StrToHex(cI)
    ELSE
      cRet += cI
    ENDIF
  NEXT
RETURN cRet


FUNC UUrlDecode(cString)
LOCAL nI
  cString := STRTRAN(cString, "+", " ")
  nI := 1
  DO WHILE nI <= LEN(cString)
    nI := HB_AT("%", cString, nI)
    IF nI == 0;  EXIT
    ENDIF
    IF UPPER(SUBSTR(cString, nI + 1, 1)) $ "0123456789ABCDEF" .AND. ;
       UPPER(SUBSTR(cString, nI + 2, 1)) $ "0123456789ABCDEF"
      cString := STUFF(cString, nI, 3, HB_HexToStr(SUBSTR(cString, nI + 1, 2)))
    ENDIF
    nI++
  ENDDO
RETURN cString


FUNC ULink(cText, cUrl)
RETURN '<a href="' + cUrl + '">' + UHtmlEncode(cText) + '</a>'


FUNC UUrlCheckSum(cUrl)
RETURN cUrl + IIF("?" $ cUrl, "&", "?") + "_ucs=" + HB_MD5(session["_unique"] + cUrl + session["_unique"])


FUNC UUrlValidate(cUrl)
LOCAL nI
  IF cUrl == NIL;  cUrl := server["REQUEST_URI"]
  ENDIF
  IF (nI := AT("?_ucs=", cUrl)) == 0
    nI := AT("&_ucs=", cUrl)
  ENDIF
RETURN HB_MD5(session["_unique"] + LEFT(cUrl, nI - 1) + session["_unique"]) == SUBSTR(cUrl, nI + 6)




PROC UProcFiles(cFileName, lIndex )
LOCAL aDir, aF, nI, cI, tDate, tHDate


  IF lIndex == NIL;  lIndex := .F.
  ENDIF


  cFileName := STRTRAN(cFileName, "//", "/")


	if right( cFilename, 1 ) == '*' 		
		cFileName := Substr( cFilename, 1, len( cFilename)-1 )
	endif
  



  // Security
  IF "/../" $ cFileName
    USetStatusCode(403)
    RETURN
  ENDIF



  IF HB_FileExists(uOSFileName(cFileName))

    IF HB_HHasKey(server, "HTTP_IF_MODIFIED_SINCE") .AND. ;
       HttpDateUnformat(server["HTTP_IF_MODIFIED_SINCE"], @tHDate) .AND. ;
       HB_FGETDATETIME(UOsFileName(cFileName), @tDate) .AND. ;
       ( tDate <= tHDate )
      USetStatusCode(304)

    ELSEIF HB_HHasKey(server, "HTTP_IF_UNMODIFIED_SINCE") .AND. ;
       HttpDateUnformat(server["HTTP_IF_UNMODIFIED_SINCE"], @tHDate) .AND. ;
       HB_FGETDATETIME(UOsFileName(cFileName), @tDate) .AND. ;
       ( tDate > tHDate )

      USetStatusCode(412)
    ELSE


      IF (nI := RAT(".", cFileName)) > 0
	  
	  
        SWITCH LOWER(SUBSTR(cFileName, nI + 1))  
		
          CASE "prg"
	  
			UExecutePrg( cFileName )
			retu          
			
          CASE "htm";   CASE "html"
		  
			UExecuteHtml( cFileName )
			retu 
			
          CASE "css";                                 cI := "text/css";  EXIT
          //CASE "htm";   CASE "html";                  cI := "text/html";  EXIT
          CASE "txt";   CASE "text";  CASE "asc"
          CASE "c";     CASE "h";     CASE "cpp"
          CASE "hpp";   CASE "log";                   cI := "text/plain";  EXIT
          CASE "rtf";                                 cI := "text/rtf";  EXIT
          CASE "xml";                                 cI := "text/xml";  EXIT
          CASE "bmp";                                 cI := "image/bmp";  EXIT
          CASE "gif";                                 cI := "image/gif";  EXIT
          CASE "jpg";   CASE "jpe";   CASE "jpeg";    cI := "image/jpeg";  EXIT
          CASE "png";                                 cI := "image/png";   EXIT
          CASE "tif";   CASE "tiff";                  cI := "image/tiff";  EXIT
          CASE "djv";   CASE "djvu";                  cI := "image/vnd.djvu";  EXIT
          CASE "ico";                                 cI := "image/x-icon";  EXIT
          CASE "xls";                                 cI := "application/excel";  EXIT
          CASE "doc";                                 cI := "application/msword";  EXIT
          CASE "pdf";                                 cI := "application/pdf";  EXIT
          CASE "ps";    CASE "eps";                   cI := "application/postscript";  EXIT
          CASE "ppt";                                 cI := "application/powerpoint";  EXIT
          CASE "bz2";                                 cI := "application/x-bzip2";  EXIT
          CASE "gz";                                  cI := "application/x-gzip";  EXIT
          CASE "tgz";                                 cI := "application/x-gtar";  EXIT
          CASE "js";                                  cI := "application/x-javascript";  EXIT
          CASE "tar";                                 cI := "application/x-tar";  EXIT
          CASE "tex";                                 cI := "application/x-tex";  EXIT
          CASE "zip";                                 cI := "application/zip";  EXIT
          CASE "midi";                                cI := "audio/midi";  EXIT
          CASE "mp3";                                 cI := "audio/mpeg";  EXIT
          CASE "wav";                                 cI := "audio/x-wav";  EXIT
          CASE "qt";    CASE "mov";                   cI := "video/quicktime";  EXIT
          CASE "avi";                                 cI := "video/x-msvideo";  EXIT
          OTHERWISE
            cI := "application/octet-stream"
        ENDSWITCH
      ELSE

        cI := "application/octet-stream"
      ENDIF
      UAddHeader("Content-Type", cI)

      IF HB_FGETDATETIME(UOsFileName(cFileName), @tDate)
        UAddHeader("Last-Modified", HttpDateFormat(tDate))
      ENDIF

      UWrite(HB_MEMOREAD(UOsFileName(cFileName)))
	  	  
	  
    ENDIF

  ELSEIF HB_DirExists(UOsFileName(cFileName))
  

    IF RIGHT(cFileName, 1) != "/"
       URedirect("http://" + server["HTTP_HOST"] + server["SCRIPT_NAME"] + "/")
       RETURN
    ENDIF

    IF ASCAN({"index.html", "index.htm"}, ;
             {|x| IIF(HB_FileExists(UOSFileName(cFileName + X)), (cFileName += X, .T.), .F.)}) > 0
      UAddHeader("Content-Type", "text/html")
      UWrite(HB_MEMOREAD(UOsFileName(cFileName)))
      RETURN
    ENDIF

    IF ! lIndex
       USetStatusCode(403)
       RETURN
    ENDIF

    UAddHeader("Content-Type", "text/html")

    aDir := DIRECTORY(UOsFileName(cFileName), "D")



    IF HB_HHasKey(get, "s")
      IF get["s"] == "s"
        ASORT(aDir,,, {|X,Y| IF(X[5] == "D", IF(Y[5] == "D", X[1] < Y[1], .T.), ;
                                IF(Y[5] == "D", .F., X[2] < Y[2]))})
      ELSEIF get["s"] == "m"
        ASORT(aDir,,, {|X,Y| IF(X[5] == "D", IF(Y[5] == "D", X[1] < Y[1], .T.), ;
                                IF(Y[5] == "D", .F., DTOS(X[3]) + X[4] < DTOS(Y[3]) + Y[4]))})
      ELSE
        ASORT(aDir,,, {|X,Y| IF(X[5] == "D", IF(Y[5] == "D", X[1] < Y[1], .T.), ;
                                IF(Y[5] == "D", .F., X[1] < Y[1]))})
      ENDIF
    ELSE
      ASORT(aDir,,, {|X,Y| IF(X[5] == "D", IF(Y[5] == "D", X[1] < Y[1], .T.), ;
                              IF(Y[5] == "D", .F., X[1] < Y[1]))})
    ENDIF

    UWrite('<html><body><h1>Index of ' + server["SCRIPT_NAME"] + '</h1><pre>      ')
    UWrite('<a href="?s=n">Name</a>                                                  ')
    UWrite('<a href="?s=m">Modified</a>             ')
    UWrite('<a href="?s=s">Size</a>' + CR_LF + '<hr>')
    FOR EACH aF IN aDir
      IF LEFT(aF[1], 1) == "."
      ELSEIF "D" $ aF[5]
        UWrite('[DIR] <a href="' + aF[1] + '/">'+ aF[1] + '</a>' + SPACE(50 - LEN(aF[1])) + ;
               DTOC(aF[3]) + ' ' + aF[4] + CR_LF)
      ELSE
        UWrite('      <a href="' + aF[1] + '">'+ aF[1] + '</a>' + SPACE(50 - LEN(aF[1])) + ;
               DTOC(aF[3]) + ' ' + aF[4] + STR(aF[2], 12) + CR_LF)
      ENDIF
    NEXT
    UWrite("<hr></pre></body></html>")
  ELSE

    USetStatusCode(404)
  ENDIF

RETURN

//	Difference between UExecuteHtml and ULoadHtml is:
//	UExecuteHtml cFileName is with all path
//	ULoadHtml cFileName is only filename and path is UGetServer():cPathHtml

function UExecuteHtml( cFileName )

	local cCode 
	
	if !file( cFileName )
		retu nil
	endif
	
	cCode :=  hb_memoread( UOsFileName( cFileName ) ) 

	UReplaceBlocks( @cCode, "{{", "}}", nil )

	cFilePrg := hb_FNameNameExt(cFileName)
	
	UInlinePRG( @cCode )
	
	UWrite( cCode )

retu nil

function UExecutePrg( cFileName, lWrite, cCode, cCodePP )
	
	local oHrb, pSym
	local cResult := ''
	
	
	hb_default( @lWrite, .t. )
	hb_default( @cCode, '' )
	hb_default( @cCodePP, '' )
	

	if !file( cFileName )
		retu nil
	endif
	
	cCode :=  hb_memoread( UOsFileName( cFileName ) ) 		
	
	oHrb := UCompile( cCode, @cCodePP )
	
	
	IF ! Empty( oHrb )
   
		//mh_startmutex()

		//pSym := hb_hrbLoad( HB_HRB_BIND_OVERLOAD, oHrb )	  
		pSym := hb_hrbLoad( 0x1, oHrb )	  		//	HB_HRB_BIND_LOCAL 

		//mh_endmutex()

		cResult := hb_hrbDo( pSym )

   ENDIF		
	
	if lWrite
		UWrite( cResult )
	endif

retu if( lWrite, nil, cResult )


PROC UProcInfo()
LOCAL cI
  UWrite('<h1>Info</h1>')

  UWrite('<h2>Platform</h2>')
  UWrite('<table border=1 cellspacing=0>')
  UWrite('<tr><td>OS</td><td>' + UHtmlEncode(OS()) + '</td></tr>')
  UWrite('<tr><td>Harbour</td><td>' + UHtmlEncode(VERSION()) + '</td></tr>')
  UWrite('<tr><td>Build date</td><td>' + UHtmlEncode(HB_BUILDDATE()) + '</td></tr>')
  UWrite('<tr><td>Compiler</td><td>' + UHtmlEncode(HB_COMPILER()) + '</td></tr>')
  UWrite('</table>')

  UWrite('<h2>Capabilities</h2>')
  UWrite('<table border=1 cellspacing=0>')
  cI := ""
  AEVAL(RDDLIST(), {|X| cI += IIF(EMPTY(cI), "", ", ") + X})
  UWrite('<tr><td>RDD</td><td>' + UHtmlEncode(cI) + '</td></tr>')
  UWrite('</table>')

  UWrite('<h2>Variables</h2>')

  UWrite('<h3>server</h3>')
  UWrite('<table border=1 cellspacing=0>')
  AEVAL(ASORT(HB_HKeys(server)), {|X| UWrite('<tr><td>' + X + '</td><td>' + UHtmlEncode(HB_CStr(server[X])) + '</td></tr>')})
  UWrite('</table>')

  IF !EMPTY(get)
    UWrite('<h3>get</h3>')
    UWrite('<table border=1 cellspacing=0>')
    AEVAL(ASORT(HB_HKeys(get)), {|X| UWrite('<tr><td>' + X + '</td><td>' + UHtmlEncode(HB_CStr(get[X])) + '</td></tr>')})
    UWrite('</table>')
  ENDIF

  IF !EMPTY(post)
    UWrite('<h3>post</h3>')
    UWrite('<table border=1 cellspacing=0>')
    AEVAL(ASORT(HB_HKeys(post)), {|X| UWrite('<tr><td>' + X + '</td><td>' + UHtmlEncode(HB_CStr(post[X])) + '</td></tr>')})
    UWrite('</table>')
  ENDIF
RETURN


//----------------------------------------------------------------------------//
/*
	nSecs = -1 Delete cookie
	nSecs =  0 Cookie live during session browse
	nSecs >  0 Seconds of cookie
*/


function USetCookie( cKey, cValue, nSecs, cPath, cDomain, lHttps, lOnlyHttp, cSameSite )

	local cCookie := ''
	
   hb_default( @cKey, 'SESSID' )
   hb_default( @cValue, '' )
   hb_default( @nSecs, 0 )   // Session will expire in Seconds 60 * 60 = 3600 1hour
   hb_default( @cPath, '/' )
   hb_default( @cDomain	, '' )
   hb_default( @lHttps, .F. )
   hb_default( @lOnlyHttp, .T. )
   hb_default( @cSameSite, 'Lax' )		//	none, Strict
   
   
   // we build the cookie
   cCookie += cKey + '=' + cValue + ';'
   
   do case
		case nSecs == -1    
			cCookie += 'expires=' + 'Thu, 01 Jan 1970 00:00:00 GMT;'
		case nSecs == 0
			//cCookie -> No existe tiempo
		otherwise
			//cCookie += 'expires=' + 'Thu, 10 Oct 2023 13:00:00 GMT;'
			cCookie += 'expires=' + UCookieExpires( nSecs ) + ' GMT;'
	endcase		         	
   
	cCookie += 'path=' + cPath + ';'
   
	if ! Empty( cDomain )
      cCookie += 'domain=' + cDomain + ';'
	endif
	
	if lOnlyHttp
		cCookie += 'HttpOnly;'
	endif
	
    cCookie += 'SameSite=' + cSameSite + ';'

	
	if UServer()[ 'HTTPS' ]
		cCookie += 'Secure'
	endif
	
	//cCookie += 'Secure'
	
	
	UAddHeader("Set-Cookie", cCookie )		
	
retu nil

function UGetCookie( cKey ) 

	LOCAL cSID := ''
	
	hb_default( @cKey, 'SESSID' )

	cSID := HB_HGetDef( cookie, cKey, ''  ) 		


retu cSID

//----------------------------------------------------------------//

function UGetIP() 

retu if( valtype( server ) == 'H', server["REMOTE_ADDR"], '' )

//----------------------------------------------------------------//
// CookieExpire( nSecs ) builds the time format for the cookie
// Using this model: 'Sun, 09 Jun 2019 16:14:00'


function UCookieExpires( nSecs )

   local tNow := hb_datetime()	
   local tExpire   // TimeStampp 
   local cExpire   // TimeStamp to String
	
   hb_default( @nSecs, 60 ) // 60 seconds for this test
   
   tExpire = hb_ntot( ( hb_tton( tNow ) * 86400 - hb_utcoffset() + nSecs ) / 86400 )

   cExpire = cdow( tExpire ) + ', ' 
	     cExpire += AllTrim( Str( Day( hb_TtoD( tExpire ) ) ) ) + ;
	     ' ' + cMonth( tExpire ) + ' ' + AllTrim( Str( Year( hb_TtoD( tExpire ) ) ) ) + ' ' 
   cExpire += AllTrim( Str( hb_Hour( tExpire ) ) ) + ':' + AllTrim( Str( hb_Minute( tExpire ) ) ) + ;
              ':' + AllTrim( Str( hb_Sec( tExpire ) ) )

return cExpire


//----------------------------------------------------------------------------//

function UMsgError( cMsg )
	
	local cHtml := '<h2 class="uerrortitletop" >=> System Error</h2><hr>'			

retu cMsg

//----------------------------------------------------------------------------//

function ULoadPrg( hSrv, cPrg )
	
	local cFile 	:= hSrv[ 'pathhtml' ] + '/'  + cPrg
	local aRequest	:= {=>}
	local cResponse	:= ''
	local oError
	local lError 	:= .f.
	local cInfoCode := ''
	local cError := ''
	local cCode := ''
	local cCodePP := ''
	local cArgs
	
	
	cFileHtml := cPrg

	if file( cFile )	
	
		BEGIN SEQUENCE WITH {|oErr| oError := oErr, Break( oErr ) }
    	
			cResponse := UExecutePrg( cFile, .f., @cCode, @cCodePP )	

		RECOVER USING oError			
		
			lError := .t.
			
			/*
			
				cInfoCode 	:= UGetInfoCode( oError, cCode ,cCodePP )			
				
				_d( 'INFOCODE->', cInfoCode )
				_d( 'CODE->', cCode )
				_d( 'CODEPP->', cCodePP )
				_d( oError )
			*/
			
			
			cError      := '<b>File: </b>' + hb_FNameNameExt( cPrg )
			cError      += '<br><b>Error: </b>' + oError:description
			
			IF !EMPTY(oError:operation)
				cError += '<br><b>Operation: </b>' + oError:operation
			ENDIF

			cArgs := ''
			
			IF VALTYPE(oError:args) == "A"
				AEVAL(oError:args, {|X, Y| cArgs += ltrim(STR(Y)) + ": " + Alltrim(HB_CStr(X)) + '<br>'})
				cError += '<br><b>Arguments: </b><br>' +  cArgs 
			ENDIF													
		
		END SEQUENCE
		
		if ! lError 
		
			aRequest[ 'success' ] 	:= .t.		
			aRequest[ 'html' ] 	:= cResponse 
			aRequest[ 'msg' ] 		:= ''
		
		else 
		
			aRequest[ 'success' ] 	:= .f.		
			aRequest[ 'html' ] 	:= ''
			aRequest[ 'msg' ] 		:= cError
		
		endif
		
	else 
	
		aRequest[ 'success' ] 	:= .f.
		aRequest[ 'html' ] 	:= ''
		aRequest[ 'msg' ] 		:= "Don't exist prg file => " + cPrg
		
		
	endif 
	
	cFileHtml := ''

	//	retu cHtml				
	
retu aRequest

//----------------------------------------------------------------------------//

function ULoadPage( hSrv, cHtml )
	
	local cFile 	:= hSrv[ 'pathhtml' ] + '/'  + cHtml
	local aRequest	:= {=>}
	local cCode 	:= ''
	
	
	cFileHtml := cHtml 

	if file( cFile )	
	
		cCode := hb_memoread( cFile ) 
	
		UReplaceBlocks( @cCode, "{{", "}}" )						
				
		cFilePrg := hb_FNameNameExt(cFile)
	
		UInlinePrg( @cCode )				

		aRequest[ 'success' ] 	:= .t.		
		aRequest[ 'html' ] 	:= cCode 
		aRequest[ 'msg' ] 		:= ''
		
	else 
	
		aRequest[ 'success' ] 	:= .f.
		aRequest[ 'html' ] 	:= ''
		aRequest[ 'msg' ] 		:= "Don't exist html file => " + cHtml
		
		
	endif 
	
	cFileHtml := ''



	//	retu cHtml				
	
retu aRequest

//----------------------------------------------------------------------------//


function ULoadHtml( cFileHtml, ... )
	
	local cFile 	
	local aRequest	:= {=>}
	local cCode 	:= ''
	
	//hb_default( @lPathRelative, .T. )
	
	//if lPathRelative 	
		cFile 	:= UGetServer():cPathHtml + '\'  + cFileHtml
	//else
	//	cFile 	:= cFileHtml 
	//endif
	

	if file( cFile )

		UDefError()
	
		cCode := hb_memoread( cFile ) 
	
		UReplaceBlocks( @cCode, "{{", "}}", nil, ... )						
				
		cFilePrg := hb_FNameNameExt(cFile)
		
		UInlinePrg( @cCode,nil, ... )
	
		
		if !empty( UGetError() )

			cCode := ''
	
		endif
			
		
	else 	
		
		UDo_Error( 'ULoadHtml() File not found ' + cFileHtml, nil, 100 )

	endif
	
retu cCode 

//----------------------------------------------------------------------------//

FUNCTION UDefError(u) 
	hb_default( @u, '' )
	cProcError := u
retu nil 

FUNCTION UGetError() ; retu cProcError	

//----------------------------------------------------------------------------//

FUNCTION UGetCfg()	;	retu hCfg

//----------------------------------------------------------------------------//
//--- ERROR SYSTEM 
//----------------------------------------------------------------------------//
FUNCTION UDo_ErrorMsg( cDescription, cSubsystem )
	UDo_Error( cDescription, cSubsystem, 666 )
RETU NIL

FUNCTION UDo_Error( cDescription, cSubsystem, nSubCode )

   LOCAL oError := ErrorNew()

   hb_default( @cSubsystem, "httpd2" )
   hb_default( @nSubCode, 0 )

   oError:Subsystem   		:= cSubsystem
   oError:SubCode 			:= nSubCode
   oError:Severity    		:= 2 // ES_ERROR
   oError:Description 	:= cDescription
   Eval( ErrorBlock(), oError )

RETURN NIL

//----------------------------------------------------------------------------//



FUNC UErrorHandler(oErr, oServer, cError )
  
  cError := ''


 
  IF     oErr:genCode == EG_ZERODIV;  RETURN 0
  ELSEIF oErr:genCode == EG_LOCK;     RETURN .T.
  ELSEIF (oErr:genCode == EG_OPEN .AND. oErr:osCode == 32 .OR. ;
          oErr:genCode == EG_APPENDLOCK) .AND. oErr:canDefault
    NETERR(.T.)
    RETURN .F.
  ENDIF
		cError := UErrorWeb()			

		cError += UErrorGetDescription( oErr )								

		cError += UErrorGetSys()	

 
  oServer:LogError(UGetErrorLog(oErr))

  
  if valtype( oServer:aConfig[ 'ErrorBloc' ] ) == 'B'


		eval( oServer:aConfig[ 'ErrorBloc' ], oErr, UGetErrorLog(oErr) ) 

  ENDIF

  IF oErr != NIL  // Dummy check to avoid unreachable code warning for RETURN NIL

    BREAK(oErr)

  ENDIF

RETURN ''


//----------------------------------------------------------------------------//

STATIC FUNC UGetErrorLog(oErr, lWeb )
LOCAL cRet, nI, cNewLine
//LOCAL ci, apar,nj, xi

	hb_default( @cNewLine, HB_OSNewLine() )
	hb_default( @lWeb, .F. )
	
	cNewLine := if( lWeb, '<br>', HB_OSNewLine())

	
  cRet := "ERRORLOG =========================================" + cNewLine
  cRet += "Error: " + oErr:subsystem + "/" + ErrDescCode(oErr:genCode) + "(" + LTRIM(STR(oErr:genCode)) + ") " + ;
                      LTRIM(STR(oErr:subcode)) + cNewLine
					  
  IF !EMPTY(oErr:filename);      cRet += "File: " + oErr:filename + cNewLine
  ENDIF
  
  IF !EMPTY(oErr:description);   cRet += "Description: " + oErr:description + cNewLine
  ENDIF
  
  IF !EMPTY(oErr:operation);     cRet += "Operation: " + oErr:operation + cNewLine
  ENDIF
  
  IF !EMPTY(oErr:osCode);        cRet += "OS error: " + LTRIM(STR(oErr:osCode)) + cNewLine
  ENDIF
  
  IF VALTYPE(oErr:args) == "A"
    cRet += "Arguments:" + cNewLine
    AEVAL(oErr:args, {|X, Y| cRet += STR(Y, 5) + ": " + HB_CStr(X) + cNewLine})
	cRet += cNewLine
  ENDIF  

  cRet += "Stack:" + cNewLine
  
  nI := 2
  
//#if 0
  DO WHILE ! EMPTY(PROCNAME(++nI))
    cRet += "    " + PROCNAME(nI) + "(" + LTRIM(STR(PROCLINE(nI))) + ")" + cNewLine
  ENDDO
/*
#else
  DO WHILE ! EMPTY(PROCNAME(++nI))
    cI := "    " + PROCNAME(nI) + "(" + LTRIM(STR(PROCLINE(nI))) + ")"
    cI := PADR(cI, MAX(32, LEN(cI) + 1))
    cI += "("
    aPar := __dbgvmParLList(nI)
    FOR nJ := 1 TO LEN(aPar)
      cI += cvt2str(aPar[nJ])
      IF nJ < LEN(aPar);  cI += ", "
      ENDIF
    NEXT
    cI += ")"
    nJ := LEN(aPar)
    DO WHILE VALTYPE(xI := __dbgvmVarLGet(nI, ++nJ)) != "S"
      cI += ", " + cvt2str(xI)
    ENDDO
    xI := NIL
    cRet += cI + cNewLine
  ENDDO
#endif
*/

  cRet += cNewLine

  //cRet += "Executable:  " + HB_PROGNAME() + cNewLine
  cRet += "Versions:" + cNewLine
  cRet += "  OS: " + OS() + cNewLine
  cRet += "  Harbour: " + VERSION() + ", " + HB_BUILDDATE() + cNewLine
  cRet += "  UHttpd2: " + UVersion()   + cNewLine

	if hb_isfunction( 'TWebVersion' ) 
		cRet += '<br><b>TWeb: </b>' + eval( &( '{|| TWebVersion()}' ) )	+ cNewLine
	endif
  
  
  cRet += cNewLine

  IF oErr:genCode != EG_MEM
    cRet += "Database areas:" + cNewLine
    cRet += "    Current: " + LTRIM(STR(SELECT())) + "  " + ALIAS() + cNewLine

    BEGIN SEQUENCE WITH {|o| BREAK(o)}
      IF USED()
        cRet += "    Filter: " + DBFILTER() + cNewLine
        cRet += "    Relation: " + DBRELATION() + cNewLine
        cRet += "    Index expression: " + ORDKEY(ORDSETFOCUS()) + cNewLine
        cRet += cNewLine
        BEGIN SEQUENCE WITH {|o| BREAK(o)}
          FOR nI := 1 to FCOUNT()
            cRet += STR(nI, 6) + " " + PADR(FIELDNAME(nI), 14) + ": " + HB_VALTOEXP(FIELDGET(nI)) + cNewLine
          NEXT
        RECOVER
          cRet += "!!! Error reading database fields !!!" + cNewLine
        END SEQUENCE
        cRet += cNewLine
      ENDIF
    RECOVER
      cRet += "!!! Error accessing current workarea !!!" + cNewLine
    END SEQUENCE

    FOR nI := 1 to 250
      BEGIN SEQUENCE WITH {|o| BREAK(o)}
        IF USED()
          DBSELECTAREA(nI)
          cRet += STR(nI, 6) + " " + RDDNAME() + " " + PADR(ALIAS(), 15) + ;
                  STR(RECNO()) + "/" + STR(LASTREC()) + ;
                  IIF(EMPTY(ORDSETFOCUS()), "", " Index " + ORDSETFOCUS() + "(" + LTRIM(STR(ORDNUMBER())) + ")") + cNewLine
          DBCLOSEAREA()
        ENDIF
      RECOVER
        cRet += "!!! Error accessing workarea number: " + STR(nI, 4) + "!!!" + cNewLine
      END SEQUENCE
    NEXT
    cRet += cNewLine
  ENDIF
RETURN cRet

//----------------------------------------------------------------------------//

function UErrorWeb()
	local cStyle := ''
		
	
	
TEXT TO cStyle	
<html>
<head>
	<title>UT Error</title>
	<link rel="icon" type="image/x-icon" href="files/uhttpd2/images/favicon.ico">
	
<!--<link href="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css" rel="stylesheet">-->
</head>
<style>

	.uerrorcontent {
		padding: 5px;
		padding-top: 0;
	}
	
	.uerrorcontent pre {
		margin-top: 0px;
	}
	


	.uerrorcode {
		border: 1px solid black;
		overflow:auto;
		box-shadow: 5px 5px 5px gray;
	}
	
	.uerrortable {
		width:100%;
	}
	
	.uerrortablelabel {
		width: 0;
		min-width: fit-content;
		padding-right: 10px;
		font-weight: bold !important;
	}
	
	.uerrortablefont {
		font-family: system-ui,-apple-system,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans","Liberation Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";
		font-size: 1rem;
		font-weight: 400;	
	}
	
	.uerrortitle {
		padding-left: 5px;
		background-color: gray;
		color: white;		
		border-bottom: 1px solid black;	
		font-family: arial;
		font-size: 14px;		
	}
	
	.uerrorsource {		
		position: relative;
		bottom: 10px;
		top: 0px;
		height: 100%;		
		padding: 5px;
		left: 0;
		right: 0;
	}
	
	.uerrorsource pre {
		margin-top:0;
		margin-bottom:0;
	}
	
	
	.uerrorsys {	
		/*position: absolute;*/
		bottom: 0;
		border-top: 1px solid gray;
		width: 100%;
		padding: 5px;
		font-size: small;		
		box-sizing: border-box;
		background-color: white;
		left: 0;		
	}
	
	.uerrortitletop {
		font-family: Verdana;
		margin-bottom:5px;
	}
	
	.uerrorimg {
		width: 30px;
		margin-top: -4px;
		margin-right: 5px;
		float: left;
	}

</style>
<body>
ENDTEXT 

	
	if ! UIsAjax()		
		//cStyle += '<h3 class="uerrortitletop"><img class="uerrorimg" src="files\uhttpd2\images\error.png">System Error</h3><hr>'
		cStyle += '<h3 class="uerrortitletop"><img class="uerrorimg" src="data:image/png;base64, ' + UImgAlert() + '">System Error</h3><hr>'
		cStyle += '<div class="uerrorcontent" style="padding:5px;">'
	else
		cStyle += '<div class="uerrorcontent" style="padding:0px;">'
	endif
		


	
RETU cStyle 

function UErrorLin( cLabel, cValue )

retu '<tr><td class="uerrortablelabel uerrortablefont">' + cLabel + '</td><td class="uerrortablefont">' + cValue + '</td></tr>'

function UErrorGetDescription( oErr, cInfoCode, cCargo )

	local cRet := ''
	local cNewLine := '<br>'
	local nI
	local cStack := ''
	local cI := ''
	local cProc := ''
	local lExit := .f.
	local cArgs := ''

	hb_default( @cCargo, '' )	
	
	if oErr:subcode == 666
	
		cRet += '<table class="uerrortable">'
		
		IF !EMPTY(oErr:description);   cRet += UErrorLin( 'Description', oErr:description )
		ENDIF
		
		cRet += '</table>'
		cRet += '</div>'
	
		retu cRet
	endif 
	
	cRet += '<table class="uerrortable">'	
	
	IF !EMPTY(cFilePrg);      		 cRet += UErrorLin( 'Execute', cFilePrg  )
	ENDIF
	
	if !empty( cCargo )	
		cRet += '<tr><td colspan="2">' + cCargo + '</td></tr>' 
	endif	
	
	cRet += UErrorLin( 'Error', oErr:subsystem + "/" + ErrDescCode(oErr:genCode) + "(" + LTRIM(STR(oErr:genCode)) + ") " + LTRIM(STR(oErr:subcode)) )
			  
	IF !EMPTY(oErr:filename);      cRet += UErrorLin( 'File', oErr:filename )
	ENDIF
	
	IF !EMPTY(oErr:description);   cRet += UErrorLin( 'Description', oErr:description )
	ENDIF
	
	IF !EMPTY(oErr:operation);     cRet += UErrorLin( 'Operation', oErr:operation )
	ENDIF
	
	
	IF VALTYPE(oErr:args) == "A"
		AEVAL(oErr:args, {|X, Y| cArgs += ltrim(STR(Y)) + ": " + Alltrim(HB_CStr(X)) + cNewLine})
		cRet += UErrorLin( 'Arguments', cArgs )
	ENDIF	

	IF !EMPTY(oErr:osCode);        cRet += UErrorLin( 'OS Error',  LTRIM(STR(oErr:osCode)) ) 
	ENDIF
	  
	nI := 2
	
	
	DO WHILE ! EMPTY(PROCNAME(++nI)) .and. !lExit		
	
		cProc := PROCNAME(nI)
	
		if At( '(b)UWEBAPI', cProc ) > 0 .OR. ;		
		   At( '(b)UHTTPD2_ROUTE', cProc ) > 0
			lExit := .t. 
		else
			cStack += '&nbsp;&nbsp;' + cProc + "(" + LTRIM(STR(PROCLINE(nI))) + ")" + cNewLine		
		endif
		
	ENDDO
	
	if !empty(cStack) .and. oErr:subcode != 100
		cRet += UErrorLin( 'Stack', cStack )
	endif
	
	//	---------------------------------------------------
	
	if !empty( cInfoCode ) 
		cRet += '<tr><td colspan="2">' + cInfoCode + '</td></tr>'
	endif
	
	cRet += '</table>'
	cRet += '</div>'
	
	if oErr:subcode == 100 
		cRet += '<hr>'
	endif
	
retu cRet	
	

//----------------------------------------------------------------------------//

function UErrorGetSys()

	local cRet := ''

	cRet += '</div>'
	cRet += '<div class="uerrorsys" '
	cRet += 'style="position:' + IF( UIsAjax(), 'unset;', 'fixed;' ) + '" '
	cRet += '>'
	//cRet += '<b>OS: </b>' + OS()
	cRet += '<b>Harbour: </b>' + VERSION() //+ ", " + HB_BUILDDATE() 
	cRet += '<br><b>UHttpd2: </b>' + UVersion()
	
	if hb_isfunction( 'TWebVersion' ) 		
		cRet += '<br><b>TWeb: </b>' + eval( &( '{|| TWebVersion()}' ) )
	endif
	
	
	cRet += '</div>'	
			
retu cRet

//----------------------------------------------------------------------------//

function UErrorGetCode( cCode, nLineError )

	local cRet := ''
	local aLines :=  hb_ATokens( cCode, chr(10) )
	local cInfo 	:= ''
	local cLine 	:= ''
	local n 

	
	for n = 1 to Len( aLines )

		cLine := aLines[ n ] 

		cLine := UHtmlEncode( cLine )
		cLine := StrTran( cLine, chr(9), '&nbsp;&nbsp;&nbsp;' )	

	  if nLineError == n 		
		cInfo += '<b>' + StrZero( n, 4 ) + ' <span class="mc_line_error"  style="background-color:#e15959;color:white;">' + cLine + '</span></b>'
	  else				  
		cInfo += StrZero( n, 4 ) + ' ' + cLine 
	  endif 		  

	next	

	cRet += '<div class="uerrorcode">'
	cRet += ' <div class="uerrortitle">' + 'Source Code' + '</div>'
	cRet += ' <div class="uerrorsource"><pre>' + cInfo + '</pre></div>'
	cRet += '</div>' 	

retu cRet


//----------------------------------------------------------------------------//


STATIC FUNC ErrDescCode(nCode)
LOCAL cI := NIL

  IF nCode > 0 .AND. nCode <= 41
    cI := {"ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
           NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
           "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
           "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
           "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
           "LOCK"    }[nCode]                                                                                            // 41
  ENDIF
 
RETURN IF(cI == NIL, "", "EG_" + cI)

//----------------------------------------------------------------------------//


STATIC FUNC cvt2str(xI, lLong)
LOCAL cValtype, cI, xJ

  cValtype := VALTYPE(xI)
  lLong := ! EMPTY(lLong)
  IF     cValtype == "U"
    RETURN IF(lLong, "[U]:NIL", "NIL")
  ELSEIF cValtype == "N"
    RETURN IF(lLong, "[N]:" + STR(xI), LTRIM(STR(xI)))
  ELSEIF cValtype $ "CM"
    IF LEN(xI) <= 260
      RETURN IF(lLong, "[" + cValtype + LTRIM(STR(LEN(xI))) + "]:", "") + '"' + xI + '"'
    ELSE
      RETURN IF(lLong, "[" + cValtype + LTRIM(STR(LEN(xI))) + "]:", "") + '"' + LEFT(xI, 100) + '"...'
    ENDIF
  ELSEIF cValtype == "A"
    RETURN "[A" + LTRIM(STR(LEN(xI))) + "]"
  ELSEIF cValtype == "H"
    RETURN "[H" + LTRIM(STR(LEN(xI))) + "]"
  ELSEIF cValtype == "O"
    cI := ""
    IF __objHasMsg(xI, "ID")
      xJ := xI:ID
      IF VALTYPE(xJ) != "O";  cI += ",ID=" + cvt2str(xJ)
      ENDIF
    ENDIF
    IF __objHasMsg(xI, "nID")
      xJ := xI:nID
      IF VALTYPE(xJ) != "O";  cI += ",NID=" + cvt2str(xJ)
      ENDIF
    ENDIF
    IF __objHasMsg(xI, "xValue")
      xJ := xI:xValue
      IF VALTYPE(xJ) != "O";  cI += ",XVALUE=" + cvt2str(xJ)
      ENDIF
    ENDIF
    RETURN "[O:" + xI:ClassName + cI + "]"
  ELSEIF cValtype == "D"
    RETURN IF(lLong, "[D]:", "") + DTOC(xI)
  ELSEIF cValtype == "L"
    RETURN IF(lLong, "[L]:", "") + IF(xI, ".T.", ".F.")
  ELSEIF cValtype == "P"
    RETURN IF(lLong, "[P]:", "") + "0p" + HB_NumToHex(xI)
  ELSE
    RETURN  "[" + cValtype + "]"   // BS,etc
  ENDIF
RETURN NIL

//	--- MULTIPART -------------------------------------------------------------- //
//	https://developer.mozilla.org/es/docs/Web/HTTP/Basics_of_HTTP/MIME_types#multipartform-data 

static function UParseMultipart( cRaw, hPost, aFiles )

	local nStart 	:= 0
	local hServer 	:= UServer()
	local lExit 	:= .f.
	local cBoundary, nPos, cBlock, cTag, nBoundary, nIni, nEnd,nOffset 
	
	hPost 	:= {=>}
	aFiles 	:= {}

	//	Buscaremos la bandera

		IF ! HB_HHasKey( hServer, "CONTENT_TYPE" )  	
			retu nil
		endif 		

		cTag := 'multipart/form-data; boundary='

		if ( nPos := At( cTag, hServer[ 'CONTENT_TYPE' ]  ) ) == 0 
			retu nil 
		endif							

		nPos := nPos + len( cTag )		
	
		cBoundary := '--' + Substr( hServer[ 'CONTENT_TYPE' ], nPos )
		nBoundary := len( cBoundary )
		
	//	Procesamos todo el RAW y buscamos la/s bandera/s		
		

		WHILE ( nStart := hb_At( cBoundary, cRaw ) ) != 0 .AND. !lExit 
		
			nIni := nStart + nBoundary 
			nEnd := hb_At( cBoundary, cRaw, nIni )
			
			if nEnd > 0
				
				nOffset := nEnd - nIni 								
				cBlock 	:= Substr( cRaw, nIni, nOffSet )
				
				//	Cada Bloque del multipart lo procesaremos								
				
				UParseMultipartItem( cBlock, @hPost, @aFiles )
				
				cRaw := Substr( cRaw, nEnd )								
			
			else 
			
				lExit := .t.

			endif
		
		end	

	
retu nil 

static function UParseMultipartItem( cBlock, hPost, aFiles )
 
 
	local cTag 		:= 'Content-Type:'	
	local lIsFile
	
	//	Chequearemos los tags del bloque. Si tiene el tag Content-Type: es que se
	//	trata de una fichero.
	//	Cada bloque podra contener un valor dato o un valor de tipo fichero. 
	//	Los datos los pondremos en el Hash UPost y el fichero en un array UFiles
	
	cBlock 	:= alltrim( cBlock )	
	lIsFile := hb_at( cTag, cBlock ) > 0

	if lIsFile 	
		UParseMultiPartFile( @cBlock, @aFiles )		//	Aqui pasarem de param aFiles						
	else 	
		UParseMultiPartData( @cBlock, @hPost )		// Aqui pasarem de param hPost
	endif
	

retu nil 


/* Block example to parse.... */
/*
Content-Disposition: form-data; name="file"; filename="readme2.txt"

Content-Type: text/plain

Hi readme [2].

*/

static function UParseMultiPartFile( cBlock, aFiles )

	local nEnd := hb_at( Chr(13), cBlock )
	local uValue 
	local cInfo, cMime, aInfo, nPos, nHandle	
	local hFile := {=>}
	local oServer 
	local lError := .f.
	
	
	if nEnd > 0 
	
		//	Separamos la parte de cabecera de tags y la de valor
	
		cInfo := Substr( cBlock, 1, nEnd - 1 )
		
			aInfo := hb_atokens( cInfo, ';')
			
			//hFile[ 'Content-Disposition' ] 	:= UInfo2Tag( aInfo, 'Content-Disposition:' )
			//hFile[ 'name' ]			:= UInfo2Tag( aInfo, ' name=' )			//	Alert space
			hFile[ 'success' ]			:= .f.
			hFile[ 'filename' ]		:= UInfo2Tag( aInfo, ' filename=' )		//	Alert space
			hFile[ 'ext' ]				:= lower( hb_fnameext( hFile[ 'filename' ] ) )
			hFile[ 'tmp_name' ]		:= ''
			hFile[ 'error' ]			:= ''
			hFile[ 'size' ]			:= 0
			
			if ( nPos := at( '.', hFile[ 'ext' ] ) ) > 0
				hFile[ 'ext' ] := Substr( hFile[ 'ext' ], nPos + 1 )
			endif
			
			
			
		//	Recuperamos MIME y valor
		
			cMime := Substr( cBlock, nEnd+1 )					
		
			nEnd := hb_at( Chr(13), cMime )
		
			if nEnd > 0 
			
				//	Recupera info tags 
			
				cInfo := Alltrim( Substr( cMime, 1, nEnd - 1 ) )
				
				hFile[ 'Content-Type' ] := UInfo2Tag( cInfo, 'Content-Type:' )
				

					
				//	Recupera value raw
				
				uValue	:= Alltrim( Substr( cMime, nEnd + 1 ) )

				if right( uValue, 2 ) == chr(13)+chr(10) 
					uValue := Substr( uValue, 1, len( uValue)-2 )
				endif
				
				//	Valid files ------------------------------------------
				
					//	Es un fichero permitido ? /
					
						if !empty(  UGetServer():aFilesAllowed )
							if Ascan( UGetServer():aFilesAllowed, {|x| lower(x) == hFile[ 'ext' ] } ) == 0 
								hFile[ 'error' ] := 'Forbidden file type: ' +  hFile[ 'ext' ]
								lError := .t.				
							endif				
						endif				
					
					//	Exceded Size ?
					
						if  ! lError .and. ( len( uValue ) /  1024 ) > UGetServer():nfile_max_size
							hFile[ 'size' ]  := len( uValue )
							hFile[ 'error' ] := 'File exceeds the maximum allowed: ' + ltrim(str(len(uValue))) 
							lError := .t.				
						endif
				
				//	------------------------------------------------------
		
				if ! lError 

					hFile[ 'tmp_name' ] := TempFile( UGetServer():cPathTmp, hFile[ 'ext' ] )	

					
					//	Save to tmp folder --------------------								
		
						nHandle := FCreate( hFile[ 'tmp_name' ]  )
			
						FWrite( nHandle, uValue )
						
						hFile[ 'size' ] := FSeek( nHandle, 0, FS_END )

						FClose( nHandle )	
						
						hFile[ 'success' ]			:= .t.
						
					
					//	Check for Garbage Collector -----------
					
						oServer := UGetServer()

						hb_mutexLock( oServer:hmtxFiles )					


							nFiles_Size += ( hFile[ 'size' ] / 1024 )
						
							if nFiles_Size > oServer:nfiles_size_garbage_inspector
							
								UFilesTmpCollector()
								nFiles_Size := 0
								
							endif

						hb_mutexUnlock(oServer:hmtxFiles)						
					
					//	---------------------------------------
				
				endif
		
			endif						
			
			Aadd( aFiles, hFile )

	endif		

retu nil 



/* Block example to parse.... */
/*
Content-Disposition: form-data; name="name"

Charly Brown

*/

static function UParseMultiPartData( cBlock, hPost )

	local nEnd := hb_at( Chr(13), cBlock )
	local uValue 
	local cInfo, aInfo, cKey, nPos
	local hFile := {=>}
	
	if nEnd > 0 
	
		cInfo 	:= Substr( cBlock, 1, nEnd - 1 )				
		
		aInfo 	:= hb_atokens( cInfo, ';')
		
		cKey 	:= UInfo2Tag( aInfo, ' name=' )	
		
		if !empty( cKey )							
		
			uValue 	:= alltrim( Substr( cBlock, nEnd+1 ) )	
			nPos 	:= at( chr(13), uValue )
			
			if nPos > 0 
				uValue := Substr( uValue, 1, nPos-1 )
			endif 
			
			hPost[ cKey ] 	:= uValue 
			
		endif		
	
	endif

retu nil 


static function UInfo2Tag( uInfo, cTag )

	local n, nPos, nLen
	local cValue := ''
	
	if valtype( uInfo ) == 'A'
	
		nLen := len( uInfo )
	
		for n := 1 to nLen 
		
			nPos := at( cTag, uInfo[n] ) 
			
			if nPos > 0
			
				cValue := Substr( uInfo[n], nPos + len(cTag) )	

				if LEFT( cValue, 1 ) == '"'
					cValue := Substr( cValue, 2 )
				endif
				
				if RIGHT( cValue, 1 ) == '"'
					cValue := Substr( cValue, 1, len(cValue)-1 )
				endif				
			
			endif
		
		next
	
	else 
	
		nPos := at( cTag, uInfo ) 
		
		if nPos > 0
		
			cValue := Substr( uInfo, nPos + len(cTag) )	

			if LEFT( cValue, 1 ) == '"'
				cValue := Substr( cValue, 2 )
			endif
			
			if RIGHT( cValue, 1 ) == '"'
				cValue := Substr( cValue, 1, len(cValue)-1 )
			endif			
		
		endif			
	
	endif

retu cValue


static function UFilesTmpCollector( lDelAll )

	local cPath 	:= UGetServer():cPathTmp 
	local aFiles 	:= Directory( cPath + '/*.*' )
	local nFiles 	:= len( aFiles )
	local nSFiles 	:= 0
	local nSize 	:= 0
	local nTime 	:= hb_milliseconds()	
	local nLifeDays := UGetServer():nfiles_lifeDays 	
	local nI, dMaxDate
	
	hb_default( @lDelAll, .F. )	//	.T. when start server
	
	dMaxDate := date() - nLifeDays 
	
	if lDelAll
		oServer:Dbg( 'Init Garbage Tmp Files Procces!' )
	else
		oServer:Dbg( 'Init Garbage Tmp Files Procces! => ' + dtoc(dMaxDate) )
	endif
	
	for nI := 1 to nFiles 
	
		if aFiles[nI][3] <= dMaxDate .or. lDelAll 
			
			if fErase( cPath + '/' + aFiles[nI][1] ) == 0
				nSFiles++
				nSize 	+= aFiles[nI][2]
			endif
			
		endif
	next	
	
	oServer:Dbg( '=====================================' )
	oServer:Dbg( 'Tmp files deleted: ' + ltrim(str( nSFiles )) )
	oServer:Dbg( 'Tmp size deleted: ' + ltrim(str( nSize )) + ' kb.' )
	oServer:Dbg( 'Time proccess for garbage: ' + ltrim(str( hb_milliseconds() - nTime )) + ' ms.' )
	oServer:Dbg( '=====================================' )
	

retu nil 

function UImgAlert()

	local cImg := ''

TEXT TO cImg	
iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAACKJJREFUeF7tm3WsHFUUxn/FneLursU1uAV3d9fgWry4e9ECxT04xYMTijsUDU5xlyL5NWeayWT37czuzL6y5fzzmvd2Z+797pHvfOe2ByO49RjB9093AjASMDrgz5+76yC6C4BJgIWBGYG/gbeAN4FP2g1EdwAwAbAhcAAwM/AHMAi4EugHfNVOENoNwBjAysBRwLTAK+EBcwBfA8cBNwJ/tQuEdgMwC9AbWB+4HTgtNrs9sAnwEHA48E4nAjBObNzT/xw4CHg0NroocHyExDnA+e1KjO3yAN+zQLj+4sB5wJGpUx4f2DxO/43wkqfb4QXtAmAiYGvgYOB5YD/gtcwGzQOHASsClwKnAN9UDUI7ABgVWBboA0wFnA6cWWNjowHrxMZNiEcAd3YCAGb7vYAdgduA/YEv6mxsGmBPYCvglgDtsypBqNoDxgTWAI4GhkQOcGNd2TJRHcYFTgauAP6sCoSqAUjiWhAkOQcGEF3tZ2LAsrgP8GTkhdf/iwB4ghvFqX8UZe+xnBuxYpgz5ouSeDbwY87vFvpYVR4wcnB9XX/BGmWv0SLlDBsDxwDvA4ekOEOj7xb6e1UATArsAOwLWM/l/db3IiZrlCytBfQHTqqiT6gCAMuZtVwXnjDKmsyuqI0CrB4J0XZZb2qUQIu+oxI9YIYoddsAN8e/v8ysTOAFyiqh/RJdYXYDUwK7Rgm9K9jjx4V32cUXyvaAsYC147R+jQXfmnm/xEiQ5P8SI00d4MXQBWyP07ZEuP8UQaJkib+VBULZAMwdDG4V4OIoewoeidkOm9klRbK+8aIb1AMGRMZ/KlMq1Q8kRtLo56IsClYpViYAbmazaGjejYbm8dQqdXnLm0xwzcjuzwL/RKWYDLg3cofqUNrmSTVSFwFnAN+XgUBZAJiwdFVb3TmBs4ATMgu0MuwWJylANjvmCPPABhHrPWNz9gtpM7T8jC2z4aKm8ODwBIDxabLaA3gk6nb2FC1ruvF6ER6Hpiju1MH87ANuCKCyJ2zesKxKrq4NMLLJtTAmZXiAcW3MW/Y8TU/+shor0Y3VAASiVklTD7gKeCAaoixvkFytFP2BSrLy2fUhqRXeePKFMgBQ2PRkNwWuC/JSS9hsFQDXbJ4wge4eIWDL/F7Tu4eWecDY4dKe6LeAbm02r2VlAOBzFwn3nwmwR7gweERTOLTiAX7XkqZbLweYne32zOpVApDIZ4Jtl+jPZ5rafYsekK7Pxqtylu1rPSvLA3x+0mavEG32qeGBhXFo1gNkc0tHMps+GFq2dGUXUyYAJluJlAlX+Uzw7ym8+xY8wLJlyTIhmbV1w7cbLKBMAHxVIp9tEU2SrbNyeyFrxgNEf7Uoe37/WOCaHG8tGwBLofKZspniy4nA1UXls2YAmC1OfN3YuKxMN2xkZQPg+xyybhei6xMRCg5ac1tRAETawab199N4oeOsPFYFAL43kc96hfJ0LvBTngX5mSIA6HILBd+3FmenO43eWRUANmHOFRNSpHymN+SyIgAkaq183OmOQ0y7ubw2V3xn/ojXNF2WTm8L9A0iZU9hw5TXEvlM9Vn5zLyQJyxze4Ct7PKR+Iw7664eUMRkbmp8NjNq/QKYNDw2OpIo5XD5vVT3hwIP96aJm7csqi1IzhzCNLS8HjBdzPOc790d7ualhiLmfNCEZcn8ADBWZXC/A0tFB6iXOTK3nS5qqkt2pL7DkZqtuXmqS8sDgL24AoZ8X7nKBzcjTppDdH9PWganzv9CJCx/7wbs8fWMbCvdaB/J35cM+cymSS+9PACu+/08AChwmGBWjWss/rvZqa2xbtZWMLV/MJw0539Oi43fO/Lutsbn9CC91DxlfpIhegulaQCSDOuDvLXh6eTOsF28VxbnVZlZo3l6NRJrGSOweSMHLBadopPouvmkKw9QgPAhur5e4IPMrsO7JSM5162Ebll8uN6iuwLAONolMrKnbmZ9uaTdqyPYT9hR2j4roFi2vivp+VYcxVcbJuUzq8PgWs+uB4BlRRdV5nKx9WSuZtZrNXByZD9hJ+mNMGu+krfDDweprZplW5lOEVWzUbqplnxWDwAvMCYyl8qtmd/S1aoZ+0rnW8ZwxIsSAmDyMk5VdxRWyrgrqFC7c3Ss94cHf5jdQC0APHEbHV1el3TznkyrpuRtjfa5ymf2EPIAL044QdYjnCap+UuGyggHKbveK9Eyhzms8R3DrBYAZlE3reDhGEo3KmMxblIl15pvfVZASa7K6BnODMw5VgK5QhnVxuGsUyWn05ZDSZihVhcAT0l5Wve3NFnzB7Z69PF9c4rsT/rrQu7LPFc2KHlR8xN06XIZ5rjO8u2BXhJMc9iBpj3A6Y6LsHx4scnFuOCyzIzsAtQPPZHsPUCHpbqr+afMdxvSTpVMhHqcnMYR3FBLAyAVtQnZKWQuY7WQuNAAKa/K6fYqSoaCQ5DEZIhOlT157wI0S7frLcEcsHdoGSZ1Vayh4ZcA4AKkupY9Obs/TURlWtINOkBxdmDra8mzCtgqe6NEicty5W2QMu8L6932HwJvb6OnyQ+GJABISWVMzu2c7ngCZd/PU0l2tCW49u/e/ZGvewXOBKlgosDq38vKO+kDnDxKoh2j80vz2yAB8EKSMWJs2D66+bwyV1EPMR49ZcNMCUtSlDBBs785IlcfX/TF8XkVLb1g9shvfQXAXxrvnsIFEae5NbUmFmKPYTjoDcn/GFFbsDWu4uTTS5R6K6Pr7ea33gLgxWXbR1+uW5TF95vApi1fsbFTyVbh6i8A0kQ5uYlB4tPpltxjsiwOFgA5viNmr6a+1Om7j/0lCnUvATAb24pag72glL7U1Il4WPKVzuQFPQXAcZIcwNO3OansZvZwgqYql0KPFHmAAHgnVx3ehkR0Ot30cO8ZWnn6CYAdkyXQ2ig6nW623zZkTrMH5lGFOxqQ/wHo6OPNsbl/Aa2y86hAqj7XAAAAAElFTkSuQmCC
ENDTEXT 

retu cImg 

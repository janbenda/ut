/*	---------------------------------------------------------
	File.......: UJWT.prg
	Description: Support to Json Web Token. 
	Author.....: Carles Aubia Floresvi
	Date:......: 10/07/2019
	Usage......: 
		oJWT:Encode() -> Create a new JWT
		oJWT:Decode( cJWT ) -> Decode Token. Return .T./.F.
		oJWT:SetVar( cVar, cValue ) -> Create a new var
		oJWT:GetVar( cVar ) -> Recover value var
		oJWT:GetData() -> Recover data
		oJWT:SetKey( cKey ) -> Set default server key 
	--------------------------------------------------------- */
#include 'hbclass.ch'
#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY => ALWAYS

#define MSG_1	'Token error' 
#define MSG_2	'Signature verify was wrong'
#define MSG_3	'Token was expired'
#define MSG_4	'Algorithm not allowed: ' 

CLASS UJWT

	DATA aAlgorithm					INIT { 'JWT', 'HS256' }
	DATA aHeader 						INIT {=>}
	DATA aPayload 						INIT {=>}
	DATA lClaims						INIT .t.
	DATA lValid							INIT .f.
	DATA cError							INIT ''
	DATA nLapsus						INIT 3600			//	Lapsus in seconds...	
	DATA cKey 							INIT 'UT!2019@v1'
	
	METHOD New() 						CONSTRUCTOR
	METHOD Reset()
	
	METHOD Encode()
	METHOD Decode()
	METHOD Valid()
	METHOD Refresh()						INLINE ::Encode()
	
	METHOD SetKey( cKey )				
	
	METHOD SetData( hData )
	METHOD SetVar( cVar, cValue )
	METHOD SetAlgorithm( cType, cAlg )   INLINE ( ::aAlgorithm[1] := cType, ::aAlgorithm[2] := cAlg )

	METHOD SetTimeValidate( nLapsus )	INLINE if( valtype(nLapsus) == 'N' , ::nLapsus := nLapsus, nil)
	METHOD GetTimeValidate()				INLINE HB_HGetDef( ::aPayLoad, 'lap', ::nLapsus )
	
	METHOD GetVar( cVar, uDefault )
	METHOD GetExp()						//	Exp = Expires 
	
	METHOD MakeSignature( cHeader, cPayload )
	
	METHOD GetData()					
	METHOD GetClaims()					
	METHOD GetError()						INLINE ::cError

ENDCLASS

// ----------------------------------------------- //

METHOD New( cKey ) CLASS UJWT

	::SetKey( cKey )

	::aPayload[ 'iss'  ]:= 'runnerxbase'				//	Emisor

RETU SELF

// ----------------------------------------------- //

METHOD SetKey( cKey ) CLASS UJWT

	hb_default( @cKey, '' )
	
	if !empty( cKey )
		::cKey := cKey 
	endif
	
retu nil 

// ----------------------------------------------- //

METHOD Encode( hData ) CLASS UJWT

	LOCAL cHeader, cPayload, cSignature
	LOCAL cJWT, nDateTime
	
	//	Valid Algorithm. At the moment we're working in this method...
		
		::aHeader[ 'typ' ] := ::aAlgorithm[1]
		::aHeader[ 'alg' ] := ::aAlgorithm[2]	

		if ! ( ::aHeader[ 'typ' ] == 'JWT' .and. ::aHeader[ 'alg' ] == 'HS256' )  
			::cError := MSG_4 + ::aHeader[ 'typ' ] + '/' + ::aHeader[ 'alg' ]
			retu ''
		endif				
		
	//	Set data 
	
		::SetData( hData )

	//	Actualizamos fecha de expiracion (CLAIMS)
	
		if ::lClaims 			

			nDateTime 	:= hb_datetime()
			
			::aPayload[ 'iat'  ]:= hb_TtoSec( nDateTime )
			
			if ::nLapsus == 0
				::aPayload[ 'exp'  ]:= 0						//	Expire				
			else								
				::aPayload[ 'exp'  ]:= hb_TtoSec( nDateTime ) + ::nLapsus 	//	Expire								
			endif
			
			::aPayload[ 'lap'  ]:= ::nLapsus					//	lapsus
		endif
		

	//	Codificamos Header y Payload
	
		cHeader   		:= hb_jsonEncode( ::aHeader ) 
		cHeader 	  	:= hb_base64Encode( cHeader )
		cHeader		:= hb_StrReplace( cHeader , '+/=', { '-', '_', '' } )
		
		//cPayload 		:= hb_serialize( ::aPayload ) 
		cPayload 		:= hb_jsonEncode( ::aPayload ) 
		cPayload 	 	:= hb_base64Encode( cPayload )	
		cPayload 		:= hb_StrReplace( cPayload, '+/=', { '-', '_', '' } )

	//	Make signature	
	
		cSignature := ::MakeSignature( cHeader, cPayload )
		
	//	Make JWT	
		
		cJWT := cHeader + '.' + cPayload + '.' + cSignature
		
RETU cJWT

// ----------------------------------------------- //

METHOD Reset() CLASS UJWT

	::aHeader 	:= {=>}
	::aPayload := {=>}	
	::cError	:= ''
	
RETU NIL 

// ----------------------------------------------- //

METHOD Valid( cJWT ) CLASS UJWT
	
	LOCAL aJWT, cSignature, cNewSignature
	
	
	::lValid  := .f.
	
	//	Antes de decodificar reseteamos datas
	
		::Reset()

	//	Una firma JWT consta de 3 parte separadas por "."	

		aJWT := HB_ATokens( cJWT, '.' )		

		IF !( len(aJWT) ==  3 )
			::cError := MSG_1 
			RETU .F.
		ENDIF

	//	Recuperamos datos del Header	

		::aHeader 	:= hb_StrReplace( aJWT[1] , "-_", "+/" )
		::aHeader 	:= hb_base64Decode( ::aHeader )
		::aHeader 	:= hb_jsonDecode( ::aHeader )		

		if !hb_isHash( ::aHeader ) .or. ! HB_HHasKey( ::aHeader, 'typ' ) .or. ! HB_HHasKey( ::aHeader, 'alg' )
			::cError := MSG_1 
			RETU .F.
		endif 
	

	//	Recuperamos datos del PayLoad

		::aPayload := hb_StrReplace( aJWT[2] , "-_", "+/" )							
		::aPayload := hb_base64Decode( ::aPayload )
		::aPayload	:= hb_jsonDecode( ::aPayload )

	//	Recuperamos Firma
	
		cSignature  := aJWT[3] 		
	
	//	Creamos una firma nueva para validar que es la misma que hemos recuperado	
		
		cNewSignature := ::MakeSignature( aJWT[1], aJWT[2] )		

		::lValid := ( cSignature == cNewSignature )
		
		if ! ::lValid
			::cError := MSG_2 
			RETU .F.
		ENDIF

	//	Validamos 'exp' (JWT Claims)

		if ::lClaims .and. HB_HHasKey( ::aPayLoad, 'exp' )

			if ::aPayLoad[ 'exp' ] > 0 

				if  hb_TtoSec(  hb_datetime() ) > ::aPayLoad[ 'exp' ]
					::cError := MSG_3 					
					RETU .F.		
				endif
			endif
		endif	

RETU ::lValid 

// ----------------------------------------------- //

METHOD Decode( cJWT ) CLASS UJWT
	
	if ! ::Valid( cJWT )
		retu nil 
	endif

RETU ::GetData()

// ----------------------------------------------- //

METHOD GetData( lClaims ) CLASS UJWT

	local hData 

	__defaultNIL( @lClaims, .f. )
	
	if !::lValid
		retu nil
	endif
	
	if lClaims 
		retu ::aPayLoad
	else 
		hData := HB_HClone( ::aPayLoad )
		HB_HDel( hData, 'exp' )
		HB_HDel( hData, 'iss' )
		HB_HDel( hData, 'lap' )
		HB_HDel( hData, 'iat' )
		retu hData
	endif 
	
retu nil 

// ----------------------------------------------- //

METHOD SetData( hData ) CLASS UJWT
	LOCAL nI, h 
	
	if valtype( hData ) == 'H'

		FOR nI := 1 TO len( hData )
			h := HB_HPairAt( hData, nI )
			::SetVar( h[1], h[2] )		
		NEXT			
	
	endif

RETU NIL

// ----------------------------------------------- //

METHOD GetClaims() CLASS UJWT

	local hData := {=>}
	
	hData[ 'iss' ] := ::aPayLoad[ 'iss' ]
	hData[ 'exp' ] := ::aPayLoad[ 'exp' ]
	hData[ 'iat' ] := ::aPayLoad[ 'iat' ]
	hData[ 'lap' ] := ::aPayLoad[ 'lap' ]
	
retu hData


// ----------------------------------------------- //

METHOD SetVar( cVar, uValue ) CLASS UJWT

	__defaultNIL( @cVar, '' )
	
	cVar := alltrim(lower( cVar ))

	//	Xec Claims var	
		
		//IF  cVar == 'exp' .or. cVar == 'iss' .or. cVar == 'lap' .or. cVar == 'iat'
		IF  cVar == 'exp' .or. cVar == 'lap' .or. cVar == 'iat'
			RETU NIL
		ENDIF
	
	//	Set var
	
		::aPayload[ cVar ] := uValue 	
		
RETU NIL

// ----------------------------------------------- //

METHOD GetVar( cVar ) CLASS UJWT

	LOCAL uValue := ''

	__defaultNIL( @cVar, '' )
	
	cVar := alltrim(lower(cVar))			

RETU HB_HGetDef( ::aPayLoad, cVar, '' )

// ----------------------------------------------- //

METHOD GetExp() CLASS UJWT				//	Exp = Expire.... Por efecto devolveremos el default de la clase
RETU ::Getvar( 'exp', ::nLapsus )

// ----------------------------------------------- //

METHOD MakeSignature( cHeader, cPayload ) CLASS UJWT

	local cSignature := ''

	//	Make signature	

		do case
			case ::aHeader[ 'typ' ] == 'JWT' .and. ::aHeader[ 'alg' ] == 'HS256'
				cSignature	:= HB_HMAC_SHA256( ( cHeader + '.' + cPayload ), ::cKey )
			
		endcase
		
		cSignature 	:= ToString( cSignature )
		cSignature 	:= hb_base64Encode( cSignature )		
		cSignature 	:= hb_StrReplace( cSignature, '+/=',{ '-', '_', '' } )
		
retu cSignature

// ----------------------------------------------- //

static function ToString( cData )

   local cString 	:= ""
   local nLen 		:= len(cData)
   local nX, nNum

   cData := Upper( cData )
   
   for nX := 1 to nLen step 2
      nNum 		:= (At(SubStr(cData, nX, 1), "0123456789ABCDEF") - 1) * 16
      nNum 		+= At(SubStr(cData, nX + 1, 1), "0123456789ABCDEF") - 1
      cString 	+= Chr(nNum)
   next

RETU cString


		
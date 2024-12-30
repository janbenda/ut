#include 'hbclass.ch'

CLASS UTokenData
	
	DATA cKey 							INIT 'UT!2019@v1'			
	DATA lValid							INIT .f.
	DATA uData 		
	DATA cError							INIT ''

	METHOD New() 						CONSTRUCTOR
	
	METHOD Encode( uData )
	METHOD Decode()
	METHOD Valid( cToken )
	METHOD GetData()					INLINE ::uData
	METHOD SetData( uData )			INLINE ::uData := uData
	
	METHOD SetKey( cKey )			

ENDCLASS

// ----------------------------------------------- //

METHOD New( cKey ) CLASS UTokenData

	::SetKey( cKey )
	
	::uData := nil 

RETU SELF

// ----------------------------------------------- //

METHOD SetKey( cKey ) CLASS UTokenData

	hb_default( @cKey, '' )
	
	if !empty( cKey )
		::cKey := cKey 
	endif
	
retu nil 

// ----------------------------------------------- //

METHOD Encode( uData ) CLASS UTokenData

	local cToken 
	local cKey 		:= hb_blowfishKey( ::cKey )

	if uData == nil 
		uData := ::uData
	endif

	//uData	:= hb_jsonencode( uData )	
	uData	:= hb_serialize( uData )	
	
	cToken 	:= hb_base64Encode( hb_blowfishEncrypt( cKey, uData )	)
	cToken 	:= hb_StrReplace( cToken, '+/', '-_' )		
		
RETU cToken

// ----------------------------------------------- //

METHOD Valid( cToken ) CLASS UTokenData

	
	local cRealToken, cData 
	
	hb_default( @cToken, '' )
	
	::lValid 	:= .f.
	::uData 	:= nil 
	::cError   := ''
	
	if  empty( cToken )
		retu .f.
	endif
	
	cRealToken := hb_StrReplace( cToken, '-_',  '+/' )			
	cRealToken := hb_base64Decode( cRealToken )		
	
	cData := hb_blowfishDecrypt( hb_blowfishKey( ::cKey ), cRealToken )	
	
	::lValid := if( cData == nil, .f., .t. )
	
	if ::lValid				
		//::uData := hb_jsondecode( cData )					
		::uData := hb_deserialize( cData )	
	else
		::cError   := 'Verificacion de firma ha fallado'
	endif			

RETU ::lValid 

// ----------------------------------------------- //

METHOD Decode( cToken ) CLASS UTokenData

	if ! ::Valid( cToken )
		retu nil 
	endif

RETU ::uData

// ----------------------------------------------- //
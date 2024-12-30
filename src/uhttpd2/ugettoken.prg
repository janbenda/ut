/*
    Credentials recover: token, user/psw
    
    - UGetTokenApiKey( cKey )   --> cToken
    - UGetTokenBearer()         --> cToken
    - UGetTokenBasic()          --> aPair -> aPair[ 'user' ], aPair[ 'password' ]   
*/		
// ----------------------------------------------------------- //

function UGetTokenBearer()

    local hParams 	:= UGetParams()
	local cName     := 'HTTP_AUTHORIZATION'
	local hAuth     := nil
	local nPos, cToken

	
	cToken 	:= HB_HGetDef( hParams, cName, '' )			
	nPos 	:= At( 'Bearer', cToken )
			
	if nPos > 0 
	
		cToken 	:= alltrim(Substr( cToken, 7 ))
		
	endif

retu cToken

// ----------------------------------------------------------- //

function UGetTokenBasic()

    local hParams 	:= UGetParams()
	local cName     := 'HTTP_AUTHORIZATION'
	local hAuth     := nil
	local nPos, aTokens, cToken

	cToken 	:= HB_HGetDef( hParams, cName, '' )			
	nPos 	:= At( 'Basic', cToken )
			
	if nPos > 0 
	
		cToken 	:= alltrim(Substr( cToken, 6 ))	
		cToken 	:= hb_base64Decode(cToken) 
		
		aTokens	:= hb_Atokens( cToken, ':' )
		
		if valtype( aTokens ) == 'A' .and. len( aTokens ) == 2 
		    hAuth := {=>}
			hAuth[ 'username']	:= aTokens[1]
			hAuth[ 'password']	:= aTokens[2]
			
		endif
		
	endif

retu hAuth

// ----------------------------------------------------------- //

function UGetTokenApiKey( cKey )

    local hParams 	:= UGetParams()
	local cName
	
	hb_default( @cKey, '' )
	
	cName	:= 'HTTP_' + Upper( cKey )
	
retu HB_HGetDef( hParams, cName, nil )

// ----------------------------------------------------------- //
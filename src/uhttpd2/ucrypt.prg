#define U_PSW  'mEmorY@2022!'

static _cPsw := U_PSW 

function USetPsw( cNewPsw ) 
	_cPsw := cNewPsw
retu nil 

function UGetPsw() ; retu _cPsw

function USetToken( uData, cPsw ) 
	
	local cKey 	:= hb_blowfishKey( if( valtype(cPsw) == 'C', cPsw, UGetPsw()) )		
	local hData 	:= {=>}
	local cToken, cData
	
	hData[ 'data' ] := uData

	cData	:= hb_jsonencode( hData )	
	
	cToken 	:= hb_base64Encode( hb_blowfishEncrypt( cKey, cData )	)
	cToken 	:= hb_StrReplace( cToken, '+/', '-_' )
	
retu cToken 

// lAuth by reference 

function UGetToken( cToken, lAuth, cPsw  )

	local cRealToken 	:= hb_StrReplace( cToken, '-_',  '+/' )	
	local hData 		:= {=>}
	local cData
	
	cRealToken := hb_base64Decode( cRealToken )			

	cData := hb_blowfishDecrypt( hb_blowfishKey( if( valtype(cPsw) == 'C', cPsw, UGetPsw()) ), cRealToken )					
	
	lAuth := if( cData == nil, .f., .t. )
	
	if lAuth 
		hData	:= hb_jsondecode( cData )
	endif	

retu if( !empty( hData ), hData[ 'data' ], nil )

#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY => ALWAYS

THREAD STATIC hPool


function UDataKeepAlive( bInit, cId ) 

	local cStamp := HB_HGetDef( UGetParams(), 'HTTP_UT_STAMP', '0' )
	local uValue, oError 
	
	hb_default( @bInit, {|| time() } )
	hb_default( @cId, 'default' )
	
	if hPool == NIL 
		hPool := {=>}
	endif 
	
	HB_HCaseMatch( hPool, .f. )
	
	if ! HB_HHasKey( hPool, cId ) 
		hPool[ cId ] := {=>}			
	endif	
	
	if HB_HHasKey( hPool[ cId ], cStamp ) 

		uValue := hPool[ cId ][ cStamp ]
		
	else 		
	
		TRY 
			uValue := Eval( bInit )
			hPool[ cId ][ cStamp ] := uValue 
		CATCH oError 
			_d( ProcName(1) + ' (' + ltrim(str(ProcLine(1))) + ') ' )
			_d( ProcName(2) + ' (' + ltrim(str(ProcLine(2))) + ') ' )			
			_d( '>> Error UDataKeepAlive', oError )
		END 

	endif 
	
retu uValue
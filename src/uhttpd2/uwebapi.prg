

function UWebApi( oServer, o )
	
	local oUDom, bCode
	local cHtml

	
	//	Controles 
	
		if hb_HHaskey( oServer[ 'post' ], 'controls' )		   	
			oServer[ 'post' ][ 'controls' ] := hb_jsondecode( oServer[ 'post' ][ 'controls' ] )
		else
			oServer[ 'post' ][ 'controls' ] := {=>}		
		endif	
		
		if ! hb_HHaskey( oServer[ 'post' ], 'api' )
			UDo_Error( 'data-api not defined', nil, 100 )
			retu nil
		endif
	
		/*
		if hb_HHaskey( oServer[ 'server' ], 'HTTP_COOKIE' )
			_d( 'HAY COOKIE => ' + oServer[ 'server' ]['HTTP_COOKIE'] )
		else
			_d( 'NO COOKIE*!' )
		endif
		*/

	
	//	Function API		


		if hb_isfunction( oServer[ 'post' ][ 'api' ] )					

			USetFilePrg( 'API ' + oServer[ 'post' ][ 'api' ] )
			
			oUDom := UDom():New( oServer[ 'post' ], oServer[ 'files' ] )

			bCode := &( "{|o,oSrv| " + oServer[ 'post' ][ 'api' ] + "(o,oSrv) }" )														
			
			//_d( 'UWebApi INI', oServer )			
			//_d( 'UWebApi INI' )
			
			cHtml := eval( bCode, oUDom, oServer  )		

			//	If exist session, we can save data values...			
				
				USessionWrite()	//	Write Session if exist
				
				USessionDelete()	//	Delete static Session if exist

				if valtype( o:bPostRun ) == 'B' 
					eval( o:bPostRun )
				endif									
			
			//_d( 'WEBAPI END' )

		else 	
		
			cHtml := "Api function doesn't exist.... => " + oServer[ 'post' ][ 'api' ]									
		
		endif 		
	
retu cHtml

//----------------------------------------------------------------------------//

function UMyError( oErr, cErr )


	uWrite( cErr )
	uWrite( oErr )

retu nil 
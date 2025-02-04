function MyController()

	local hGet := UGet()
	local cHtml, cData, cToken
	
	//	My process 
	
		cData := hGet[ 'first' ] + ' ' + hGet[ 'last' ]
		cToken := USetToken( cData )
		
	//	Now I load page and pass parameters...
	
	cHtml := ULoadHtml( 'action_page.html', cData, cToken )			
	
retu cHtml

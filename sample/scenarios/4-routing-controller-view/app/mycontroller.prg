#include 'lib/tweb/tweb.ch' 


function MyController()

	local hGet := UGet()
	local cHtml, cData, cToken
	
	//	My process 
	
		cData := hGet[ 'first' ] + ' ' + hGet[ 'last' ]
		cToken := USetToken( cData )
		
	//	Now I load page and pass parameters...
	
	cHtml := View_ActionPage( cData, cToken )			
	
retu cHtml

static function View_ActionPage( cData, cToken ) 

	local cHtml := ''
	
	
	TEXT TO cHtml PARAMS cData, cToken
<!DOCTYPE html>
<html>
<body>
<h2>Request Data...</h2>
<hr>

User Name: <$ cData $>
<br>Token: <$ cToken $>		

<hr>
<a href="/">Index</a>

</body>
</html>				
	
	ENDTEXT

retu cHtml 

/*
**  Original code from modHarbour
**
** Developed by Antonio Linares & Carles Aubia 
** MIT license https://github.com/FiveTechSoft/mod_harbour/blob/master/LICENSE
*/

#include "common.ch"

//	------------------------------------------------------------- //

#define HB_HRB_BIND_DEFAULT      0x0   /* do not overwrite any functions, ignore
                                          public HRB functions if functions with
                                          the same names already exist in HVM */

#define HB_HRB_BIND_LOCAL        0x1   /* do not overwrite any functions
                                          but keep local references, so
                                          if module has public function FOO and
                                          this function exists also in HVM
                                          then the function in HRB is converted
                                          to STATIC one */

#define HB_HRB_BIND_OVERLOAD     0x2   /* overload all existing public functions */

#define HB_HRB_BIND_FORCELOCAL   0x3   /* convert all public functions to STATIC ones */

#define HB_HRB_BIND_MODEMASK     0x3   /* HB_HRB_BIND_* mode mask */

#define HB_HRB_BIND_LAZY         0x4   /* lazy binding with external public
                                          functions allows to load .hrb files
                                          with unresolved or cross function
                                          references */


#define HB_HRB_FUNC_PUBLIC       0x1   /* locally defined public functions */
#define HB_HRB_FUNC_STATIC       0x2   /* locally defined static functions */
#define HB_HRB_FUNC_LOCAL        0x3   /* locally defined functions */
#define HB_HRB_FUNC_EXTERN       0x4   /* external functions used in HRB module */

//	------------------------------------------------------------- //


FUNCTION UAddPPRules()

	LOCAL cOs := OS()
	   
	thread static hPP 

    IF hPP != nil
		retu hPP 
	ENDIF 
	
    hPP = __pp_Init()

    DO CASE
		CASE "Windows" $ cOs  ; __pp_Path( hPP, "c:\harbour\include" )
		CASE "Linux" $ cOs   ; __pp_Path( hPP, "~/harbour/include" )
    ENDCASE

    IF ! Empty( hb_GetEnv( "HB_INCLUDE" ) )
        __pp_Path( hPP, hb_GetEnv( "HB_INCLUDE" ) )
    ENDIF
	
   __pp_addRule( hPP, "#xcommand ? [<explist,...>] => UWrite( '<br>' [,<explist>] )" )
   __pp_addRule( hPP, "#xcommand ?? [<explist,...>] => UWrite( [<explist>] )" )	
   
	__pp_addRule( hPP, "#xcommand TEXT [TO] <var> => #pragma __stream|<var> += %s")
	__pp_addRule( hPP, "#xcommand TEXT [TO] <var> [ PARAMS [<v1>] [,<vn>] ] => " +;
	      "#pragma __stream|<var> += UInlinePrg( UReplaceBlocks( %s, '<$', '$>' [,<(v1)>][+','+<(vn)>] [, @<v1>][, @<vn>] ) )")
   
/*	
					  //	InitProcess .ch files
	for n := 1 to len(mh_HashModules())
	
		aPair 	:= HB_HPairAt( mh_HashModules(), n )		
		cExt 	:= lower( hb_FNameExt( aPair[1] ) )

		if cExt == '.ch' 	
			__pp_AddRule( hPP, aPair[2] )			
		endif 
		
	next 
*/
	

RETURN hPP

//	------------------------------------------------------------- //

FUNCTION UReplaceBlocks( cCode, cStartBlock, cEndBlock, cParams, ... )

	LOCAL nStart, nEnd, cBlock
	LOCAL lReplaced := .F.   
	LOCAL uValue 
	local cError := ''
	local oError
	local cCodeInitial := cCode 
	local cInfoCode := ''
			
	hb_default( @cEndBlock, "}}" )
	hb_default( @cParams, "" )  
	
	
	BEGIN SEQUENCE WITH {|oErr| oError := oErr, Break( oErr ) }

		WHILE ( nStart := At( cStartBlock, cCode ) ) != 0 .AND. ;
			 ( nEnd := At( cEndBlock, cCode ) ) != 0
			 
			cBlock = SubStr( cCode, nStart + Len( cStartBlock ), nEnd - nStart - Len( cEndBlock ) )	  
			
			uValue := Eval( &( "{ |" + cParams + "| " + cBlock + " }" ), ... )

			cCode = SubStr( cCode, 1, nStart - 1 ) + ;
			UValToChar( uValue ) + ;
			SubStr( cCode, nEnd + Len( cEndBlock ) )		 

			lReplaced = .T.
		END 

    RECOVER USING oError
_d( 'Error en prepro--->' )	
		cInfoCode 	:= UGetInfoCode( oError, cCode )
		
		cError 		+= UErrorWeb()			
		
		cError 		+= UErrorGetDescription( oError, cInfoCode )			
		
		cError 		+= UErrorGetSys()	
	
/*	
	
		cType := '<b>Error type: </b>Replace Block ' +  cStartBlock + '...' + cEndBlock + '<br>'
		cType += '<b>Block: </b>' +  cBlock + '<br>'
	
		cError += UErrorWeb()			
		cError += UErrorGetDescription( oError, cType )						
		cError += UErrorGetCode( cCodeInitial, cBlock )	

			
		cError += UErrorGetSys()	

*/		
		
		cCode := ''
		//	Aqui he de montar el missatge de Error.....		

		if UIsAjax()		
			UWrite( cError )
		else			
			UWrite( UMsgError( cError ) )
		endif
		
		retu ''

    END SEQUENCE		

RETURN If( hb_PIsByRef( 1 ), lReplaced, cCode )
//RETURN cCode 


//	------------------------------------------------------------- //

function UInlinePRG( cText, cParams, ... )

	LOCAl BlocA, BlocB
	LOCAL nStart, nEnd, cCode, cResult
	local cPreCode 	:= cText
	local cError := ''
	local oError
	local cCodeInitial := cText 	
	local cCodePP, cPrg, pSym, oHrb
	local lExit := .f.

	local cInfoCode := ''
	
	
	DEFAULT cParams	TO ''	
	
	//bError := ERRORBLOCK()
	
	//BEGIN SEQUENCE WITH {|oErr| UErrorInlinePrg(oErr, @cError, @cCode ) }
	BEGIN SEQUENCE WITH {|oErr| oError := oErr, Break( oErr ) }
	
		WHILE ( nStart := At( "<?prg", cText ) ) != 0 .and. !lExit
		
			nEnd  := At( "?>", SubStr( cText, nStart + 5 ) )
		  
			BlocA := SubStr( cText, 1, nStart - 1 )
			cCode := SubStr( cText, nStart + 5, nEnd - 1 )
			BlocB := SubStr( cText, nStart + nEnd + 6 )	


			//--------------------
			
				//	--> OK cResult := UExecInline( cCodePP, cParams, ... ) 
				//	----------------------
				//hPP 	:= UAddPPRules()   			
				//cCodePP	:= __pp_Process( hPP, cCode )	

				cPrg := "function __Inline( " + cParams + " )" + hb_osNewLine() + cCode
				oHrb := UCompile( cPrg, @cCodePP )
				
				IF ! Empty( oHrb )
			   
					//mh_startmutex()

					//pSym := hb_hrbLoad( HB_HRB_BIND_OVERLOAD, oHrb )	  
					pSym := hb_hrbLoad( HB_HRB_BIND_LOCAL, oHrb )	  

					//mh_endmutex()

					cResult := hb_hrbDo( pSym, ... )

			   ENDIF										
			
			//--------------------
			
			IF Valtype( cResult ) != 'C' 
				//lExit := .t.
				cResult := UValToChar( cResult )
			ENDIF		
		
			cText 	:= BlocA + cResult + BlocB              
	 
		END	

    RECOVER USING oError

		//	_d( 'Procname 1: ' + procname(1) )		// D'on ve ?

		cText := ''

		if oError:subcode == 666		//	Error provocat
		
			cError 		:= UBuildError( oError, UGetFileHtml() )
	
		else 
			cInfoCode 	:= UGetInfoCode( oError, cCode, cCodePP )
			
			cError 		+= UErrorWeb()			
			
			cError 		+= UErrorGetDescription( oError, cInfoCode )			
			
			cError 		+= UErrorGetSys()						

		endif 
		
		if UIsAjax()		
			UWrite( cError )
		else			
			UWrite( UMsgError( cError ) )
		endif
		
		retu nil


    END SEQUENCE	

	if lExit
		return nil //	No podemos usar QUIT 
	endif	
	
RETU cText 

//	------------------------------------------------------------- //

function UBuildError( oError,  cSource )

	local cError 		:= UErrorWeb()	
	
	hb_default( @cSource, '' )
	
	cError 		+= '<table class="uerrortable">'
	cError 		+= UErrorLin( 'System', 'UHttpd2 ' + UVersion() ) 
	cError 		+= IF ( !EMPTY(oError:subsystem), UErrorLin( 'Subsystem', oError:subsystem ), '') 
	
	if hb_isfunction( 'TWebVersion' )
		UErrorLin( 'Version Tweb', eval( &( '{|| TWebVersion()}' ) ) )  		
	endif
	
	cError 		+= IF ( !EMPTY(cSource), UErrorLin( 'Source', cSource ), '') 
	cError 		+= IF ( !EMPTY(oError:description), UErrorLin( 'Description', oError:description ), '') 
	
	cError 		+= '</table>'
	cError 		+= '</div><hr>'
			
retu cError

//	------------------------------------------------------------- //

function UErrorInlinePrg(oErr, cError, cCode )

	_d( 'UErrorInlinePrg------------->>' )
	_d( oErr )
	_d( cCode )
	
	cError := oErr:description + '<br>'
	
	//	HE DE TREURE POSIBLE TAGS DELS CODE !!!! <li>,<ul>,...
	
	cError += 'Code: ' + '<br>' + cCode
	
	//UDefError( oErr:description )
	

	IF oErr != NIL  // Dummy check to avoid unreachable code warning for RETURN NIL
		BREAK(oErr)
	ENDIF
	
RETURN nil


//	------------------------------------------------------------- //

FUNCTION UExecInline( cCode, cParams, ... )

   IF cParams == nil
      cParams = ""
   ENDIF


RETURN UExecute( "function __Inline( " + cParams + " )" + hb_osNewLine() + cCode, ... )
 
//	------------------------------------------------------------- //

FUNCTION UExecute( cCode, ... )

	local oHrb := UCompile( cCode, ... )	
	local pSym, uRet 	

	IF ! Empty( oHrb )
   
		//mh_startmutex()

		//pSym := hb_hrbLoad( HB_HRB_BIND_OVERLOAD, oHrb )	  
		pSym := hb_hrbLoad( HB_HRB_BIND_LOCAL, oHrb )	  

		//mh_endmutex()

		uRet := hb_hrbDo( pSym, ... )

   ENDIF

retu uRet 

//	------------------------------------------------------------- //

function UCompile( cCode, cCodePP )

	local oHrb
	local hPP
	LOCAL cOs   		:= OS()
	LOCAL cHBHeader  	:= ''		

	DEFAULT cCodePP TO ''

	DO CASE
		CASE "Windows" $ cOs  ; cHBHeader := "c:\harbour\include"
		CASE "Linux" $ cOs   ; cHBHeader := "~/harbour/include"
	ENDCASE   

	hPP := UAddPPRules()   

	UReplaceBlocks( @cCode, "{%", "%}" )   

	cCodePP := __pp_Process( hPP, cCode )

	oHrb = HB_CompileFromBuf( cCodePP, .T., "-n", "-q2", "-I" + cHBheader, ;
			"-I" + hb_GetEnv( "HB_INCLUDE" ), hb_GetEnv( "HB_USER_PRGFLAGS" ) )	


retu oHrb

//	------------------------------------------------------------- //

function Uvaltochar( u ) 
retu HB_VALTOSTR( u )
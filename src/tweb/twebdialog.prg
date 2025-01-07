#include 'hbclass.ch'
#include 'common.ch'
#include 'error.ch'

// ----------------------------------------------------------



CLASS TWebDialog

   DATA cTitle      INIT ''
   DATA cRootRelative    INIT ''
   DATA aInclude     INIT {}
   DATA aControls     INIT {}

   METHOD New()      CONSTRUCTOR
   METHOD Activate()
   METHOD AddControl( uValue )  INLINE AAdd( ::aControls, uValue )

   METHOD AddJs( cFile, lAbsolute )
   METHOD AddCss( cFile, lAbsolute )

ENDCLASS

METHOD New( cTitle ) CLASS TWebDialog

   DEFAULT cTitle    TO 'Dialog'

   ::cTitle  := cTitle

   RETU SELF

METHOD Activate() CLASS TWebDialog

   LOCAL cHtml  := ''
   LOCAL nI

   hb_SetEnv( "ROOTRELATIVE", ::cRootRelative )

   FOR nI := 1 TO Len( ::aInclude )
      cHtml += ::aInclude[ nI ] + CRLF
   NEXT

   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'
         cHtml += ::aControls[ nI ]:Activate()
      ELSE
         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT

   UReplaceBlocks( @cHtml, "{{", "}}" )


   RETU cHtml

METHOD AddJs( cFile, lAbsolute ) CLASS TWebDialog

   hb_default( @lAbsolute, .F. )

   IF lAbsolute
      AAdd( ::aInclude, '<script src="' + cFile + '"></script>' )
   ELSE
      AAdd( ::aInclude, '<script src="{{ hb_GetEnv( "ROOTRELATIVE") }}' + cFile + '"></script>' )
   ENDIF

   RETU NIL

METHOD AddCss( cFile, lAbsolute ) CLASS TWebDialog

   hb_default( @lAbsolute, .F. )

   IF lAbsolute
      AAdd( ::aInclude, '<link rel="stylesheet" href="' + cFile + '">' )
   ELSE
      AAdd( ::aInclude, '<link rel="stylesheet" href="{{ hb_GetEnv( "ROOTRELATIVE") }}' + cFile + '">' )
   ENDIF

   RETU NIL

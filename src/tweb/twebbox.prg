// -------------------------------------------------------------

CLASS TWebBox FROM TWebForm

   DATA oParent

   METHOD New()      CONSTRUCTOR
   METHOD Activate()


   METHOD End()     INLINE ::Html( '</div>' )

ENDCLASS

METHOD New( oParent, cId, nGrid, nHeight, cClass ) CLASS TWebBox

   DEFAULT cId    TO ''
   DEFAULT nGrid TO 12
   DEFAULT cClass TO ''
   DEFAULT nHeight TO 0


   ::oParent   := oParent
   ::cId   := cId
   ::nGrid   := nGrid
   ::nHeight   := nHeight
   ::cClass   := cClass


   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )

      ::lDessign := oParent:lDessign

   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebBox

   LOCAL cHtml := ''
   LOCAL nI

 /*
 local cType := ::oParent:cType

 IF !Empty( cType )
  cPrefix := cType + '-'
 ELSE
  cPrefix := IF( empty(::cType), '', ::cType + '-' )
 ENDIF
 //cHtml += '<div class="col-' + cPrefix + ltrim(str(::nGrid))
 */

   cHtml += '<div class="col-' + LTrim( Str( ::nGrid ) )


// cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '')

   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   END

   cHtml += '" '
   cHtml += 'style="'
   cHtml += IF( ::oParent:lDessign, 'border:3px solid yellow;', '' )

   IF !Empty( ::nHeight )
      IF ValType( ::nHeight ) == 'N'
         IF ::nHeight > 0
            cHtml += 'height:' + LTrim( Str( ::nHeight ) ) + 'px;'
         ENDIF
      ELSE
         cHtml += 'height:' + ::nHeight + ';' // don't work
      ENDIF
   ENDIF

   cHtml += '">'

   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'
         cHtml += ::aControls[ nI ]:Activate()
      ELSE
         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT


   RETU cHtml

// -------------------------------------------------------------

CLASS TWebSay FROM TWebControl

   DATA cLink       INIT ''

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cCaption, nGrid, cAlign, cClass, cFont, cLink, cStyle, cAction ) CLASS TWebSay

   DEFAULT cId TO ::GetId()
   DEFAULT cCaption TO ''
   DEFAULT nGrid TO 4
   DEFAULT cAlign TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cLink TO ''
   DEFAULT cStyle TO ''
   DEFAULT cAction TO ''

   ::oParent   := oParent
   ::cId   := cId
   ::uValue  := cCaption
   ::nGrid   := nGrid
   ::cAlign   := Lower( cAlign )
   ::cClass   := cClass
   ::cFont   := cFont
   ::cLink   := cLink
   ::cStyle  := cStyle
   ::cAction  := cAction

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebSay

   LOCAL cHtml
   LOCAL cSize  := ''
   LOCAL cIdPrefix
   LOCAL cGrid

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM' ; cSize   := 'form-control-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG' ; cSize   := 'form-control-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

// cHtml := '<div class="col-' + ltrim(str(::nGrid))

   IF ValType( ::nGrid ) == 'N'
      cGrid := LTrim( Str( ::nGrid ) )
   ELSE
      cGrid := ::nGrid
   ENDIF

   cHtml := '<div class="col-' + cGrid

   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )
   cHtml += ' tweb_say'

   DO CASE
   CASE ::cAlign == 'center' ; cHtml += ' text-center'
   CASE ::cAlign == 'right'  ; cHtml += ' text-right'
   ENDCASE



   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF


   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF

   cHtml += '" '
   cHtml += IF( ::oParent:lDessign, 'style="border:1px solid brown;"', '' )
   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'


   IF !Empty( ::cLink )
      cHtml += '<a href="' + ::cLink + '">'

   ENDIF


   cHtml += '<span id="' + cIdPrefix + ::cId + '" '


   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cAction )
      IF At( '(', ::cAction ) >  0   // Exist function ?
         cHtml += 'onclick="' + ::cAction + '" '
      ELSE
         cHtml += ' data-onclick="' + ::cAction + '" '
      ENDIF
   ENDIF

   cHtml += ' data-live '

   cHtml += '>' + ::uValue + '</span>'

   IF !Empty( ::cLink )
      cHtml += '</a>'
   ENDIF

   cHtml += '</div>'

   RETU cHtml

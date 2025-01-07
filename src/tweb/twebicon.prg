// -------------------------------------------------------------

CLASS TWebIcon FROM TWebControl

   DATA cLink       INIT ''

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cSrc, nGrid, cAlign, cClass, cFont, cLink, cStyle ) CLASS TWebIcon

   DEFAULT cId TO ::GetId()
   DEFAULT cSrc TO ''
   DEFAULT nGrid TO 1
   DEFAULT cAlign TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cLink TO ''
   DEFAULT cStyle TO ''

   ::oParent   := oParent
   ::cId   := cId
   ::cSrc   := cSrc
   ::nGrid   := nGrid
   ::cAlign   := Lower( cAlign )
   ::cClass   := cClass
   ::cFont   := cFont
   ::cLink   := cLink
   ::cStyle   := cStyle

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebIcon

   LOCAL cHtml
   LOCAL cSize  := ''
   LOCAL cIdPrefix

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM' ; cSize   := 'form-control-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG' ; cSize   := 'form-control-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   cHtml := '<div id="' + cIdPrefix + ::cId + '" class="col-' + LTrim( Str( ::nGrid ) )

   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )
   cHtml += ' tweb_icon'

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

// cHtml += IF( ::oParent:lDessign, 'style="border:1px solid brown;"', '' )
   IF ::oParent:lDessign
      ::cStyle += "border:1px solid brown;"
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'


   IF !Empty( ::cLink )
      cHtml += '<a href="' + ::cLink + '">'
   ENDIF

// cHtml += '<span id="' + ::cId + '">' + ::uValue + '</span>'
   cHtml += ::cSrc

   IF !Empty( ::cLink )
      cHtml += '</a>'
   ENDIF

   cHtml += '</div>'

   RETU cHtml

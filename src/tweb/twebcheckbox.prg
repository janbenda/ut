// -------------------------------------------------------------

CLASS TWebCheckbox FROM TWebControl

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New(  oParent, cId, lValue, cLabel, nGrid, cAction, cClass, cFont, cStyle, cProp, lReadonly, lHidden ) CLASS TWebCheckbox

   DEFAULT cId TO ::GetId()
   DEFAULT lValue TO .F.
   DEFAULT nGrid TO 4
   DEFAULT cLabel TO ''
   DEFAULT cAction TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lReadonly TO .F.
   DEFAULT lHidden TO .F.

   ::oParent   := oParent
   ::cId   := cId
   ::uValue   := lValue
   ::cLabel   := cLabel
   ::nGrid   := nGrid
   ::cAction  := cAction
   ::cClass   := cClass
   ::cFont   := cFont
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lReadOnly  := lReadOnly
   ::lHidden  := lHidden

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebCheckbox

   LOCAL cHtml  := ''
   LOCAL cChecked := ''
   LOCAL cIdPrefix
   LOCAL cSt := ''

   IF ::uValue
      cChecked := 'checked="checked"'
   ENDIF

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF


   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) ) + ' tweb_checkbox" ' + IF( ::oParent:lDessign, 'style="border:1px solid blue;"', '' )


   IF  ::oParent:lDessign
      cSt += 'border:1px solid blue;'
   ENDIF

   IF ::lHidden
      cSt += 'display:none;'
   ENDIF

   IF !Empty( cSt )
      cHtml += ' style="' + cSt + '" '
   ENDIF


   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'

   IF !Empty( ::cStyle )
      cHtml += '<div  style="' + ::cStyle + '" >'
   ENDIF



   cHtml += '<div class="custom-control custom-checkbox " >'
   cHtml += '<input type="checkbox" class="custom-control-input tweb_pointer" id="' + cIdPrefix + ::cId + '" '


   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF

   cHtml += ' data-live '

   IF !Empty( ::cAction )
      IF At( '(', ::cAction ) >  0   // Exist function ?
         cHtml += ' onchange="' + ::cAction + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cAction + '" '
      ENDIF

   ENDIF

   cHtml += cChecked

   IF ::lReadOnly
      cHtml += ' disabled '
   ENDIF

   cHtml += '> '

   IF !Empty( ::cLabel )

      cHtml += '<label class="custom-control-label tweb_pointer '

      IF !Empty( ::cClass )
         cHtml += ' ' + ::cClass
      ENDIF

      IF !Empty( ::cFont )
         cHtml += ' ' + ::cFont
      ENDIF

      cHtml += '" '


      cHtml += 'for="' + cIdPrefix + ::cId + '">' + ::cLabel + '</label>'

   ENDIF

   IF !Empty( ::cStyle )
      cHtml += '</div>'
   ENDIF

   cHtml += '</div>'
   cHtml += '</div>'

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )

   RETU cHtml

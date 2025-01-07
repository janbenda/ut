// -------------------------------------------------------------

CLASS TWebGetNumber FROM TWebControl

   DATA cType       INIT 'text'
   DATA cPlaceHolder     INIT ''

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, uValue, nGrid, cLabel, cAlign, lReadOnly, cPlaceHolder, lRequired, cChange, cClass, cFont, cStyle ) CLASS TWebGetNumber

   DEFAULT cId TO ::GetId()
   DEFAULT uValue TO ''
   DEFAULT nGrid TO 6
   DEFAULT cLabel TO ''
   DEFAULT cAlign TO 'center'
   DEFAULT lReadOnly TO .F.
   DEFAULT cPlaceHolder TO ''
   DEFAULT lRequired TO .F.
   DEFAULT cChange TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cStyle TO ''

   ::oParent   := oParent
   ::cId   := cId
   ::uValue  := uValue
   ::nGrid   := nGrid
   ::cLabel   := cLabel
   ::cAlign   := cAlign
   ::lReadOnly  := lReadOnly
   ::lRequired  := lRequired
   ::cChange   := cChange
   ::cClass   := cClass
   ::cFont   := cFont
   ::cStyle   := cStyle


   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebGetNumber

   LOCAL cHtml
   LOCAL cSize   := ''
   LOCAL cAlign   := ''
   LOCAL cSizeLabel := 'col-form-label'
   LOCAL cBtnSize   := ''
   LOCAL cIdPrefix

   DO CASE
   CASE ::cAlign == 'center' ; cAlign := 'text-center'
   CASE ::cAlign == 'right'  ; cAlign := 'text-right'
   ENDCASE

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM'
      cSize   := 'form-control-sm'
      cSizeLabel := 'col-form-label-sm'
      cBtnSize  := 'btn-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG'
      cSize   := 'form-control-lg'
      cSizeLabel := 'col-form-label-lg'
      cBtnSize  := 'btn-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) )

   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )
   cHtml += '" '
   chtml += IF( ::oParent:lDessign, 'style="border:1px solid blue;"', '' )
   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'

   IF !Empty( ::cLabel )

      cHtml += '<label class="' + cSizeLabel + ' ' + ::cFontLabel + ' " for="' + cIdPrefix + ::cId + '">' + ::cLabel + '</label>'

   ENDIF


   cHtml += '<div class="input-group" '

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   cHtml += '>'

   cHtml += '<div class="input-group-prepend">'

   cHtml += '<button id="btn_minus_' + cIdPrefix + ::cId + '" class="btn btn-outline-secondary ' + cBtnSize + '" type="button" '

   cHtml += ' ><i class="fas fa-minus"></i></button>'

   cHtml += '</div>'




   cHtml += '<input type="tlf" class="form-control ' + cSize + ' ' + cAlign

   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF

   cHtml += '" '
   cHtml += 'id="' + cIdPrefix + ::cId + '"  name="' + cIdPrefix + ::cId + '" '
   cHtml += 'placeholder="' + ::cPlaceHolder + '" '

   IF ::lReadOnly
      cHtml += ' readonly '
   ENDIF

   IF ::lRequired
      cHtml += ' required '
   ENDIF

   cHtml += ' data-live '

   IF !Empty( ::cChange )
      IF At( '(', ::cChange ) >  0   // Exist function ?
         cHtml += ' onchange="' + ::cChange + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cChange + '" '
      ENDIF

   ENDIF

// if !empty( ::cStyle )
// cHtml += ' style="' + ::cStyle + '" '
// endif

   cHtml += ' value="' + ::uValue + '">'

   cHtml += '<div class="input-group-append">'

   cHtml += '<button id="btn_plus_' + cIdPrefix + ::cId + '" class="btn btn-outline-secondary ' + cBtnSize + '" type="button" '

   cHtml += ' ><i class="fas fa-plus"></i>'
   cHtml += '</button>'
   cHtml += '</div>'


   cHtml += ' </div>'
   cHtml += '</div>'



   cHtml += '<script>'

   cHtml += "$('#btn_minus_" + cIdPrefix + ::cId + "').click(function(e){"
   cHtml += "    e.preventDefault();"
   cHtml += "    var quantity = parseInt($('#" + cIdPrefix + ::cId + "').val());"
   cHtml += "    $('#" + cIdPrefix + ::cId + "').val(quantity - 1);"
// cHtml += "    $('#" + cIdPrefix + ::cId + "').trigger('change');"
   cHtml += "    UOnChange( '" + cIdPrefix + ::cId + "') "
   cHtml += "});"

   cHtml += "$('#btn_plus_" + cIdPrefix + ::cId + "').click(function(e){"
   cHtml += "    e.preventDefault();"
   cHtml += "    var quantity = parseInt($('#" + cIdPrefix + ::cId + "').val());"
   cHtml += "    $('#" + cIdPrefix + ::cId + "').val(quantity + 1);"
// cHtml += "    $('#" + cIdPrefix + ::cId + "').trigger('change');"
   cHtml += "    UOnChange( '" + cIdPrefix + ::cId + "') "
   cHtml += "});"

   cHtml += '</script>'


   RETU cHtml

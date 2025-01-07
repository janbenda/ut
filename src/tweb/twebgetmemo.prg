// -------------------------------------------------------------

CLASS TWebGetMemo FROM TWebControl

   DATA cType       INIT 'text'
   DATA cPlaceHolder     INIT ''
   DATA cBtnLabel      INIT ''
   DATA cBtnAction     INIT ''
   DATA nRows      INIT 3


   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, uValue, nGrid, cLabel, cAlign, lReadOnly, nRows, cClass, cFont, cChange, cStyle, cProp, lHidden  ) CLASS TWebGetMemo

   DEFAULT cId TO ::GetId()
   DEFAULT uValue TO ''
   DEFAULT nGrid TO 4
   DEFAULT cLabel TO ''
   DEFAULT cAlign TO ''
   DEFAULT lReadOnly TO .F.
   DEFAULT nRows TO 3
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cChange TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.


   ::oParent   := oParent
   ::cId   := cId
   ::uValue  := uValue
   ::nGrid   := nGrid
   ::cLabel   := cLabel
   ::cAlign   := cAlign
   ::lReadOnly  := lReadOnly
   ::nRows   := nRows
   ::cClass   := cClass
   ::cFont   := cFont
   ::cChange   := cChange
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lHidden   := lHidden


   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebGetMemo

   LOCAL cHtml
   LOCAL cSize := ''
   LOCAL cIdPrefix
   LOCAL cSt := ''
   LOCAL cSizeLabel := ''

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM'
      cSize   := 'form-control-sm'
      cSizeLabel  := 'col-form-label-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG'
      cSize   := 'form-control-lg'
      cSizeLabel  := 'col-form-label-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) ) + IF( ::oParent:lDessign, ' tweb_dessign', '' ) + '" '


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

   IF !Empty( ::cLabel )

      cHtml += '<label class="' + cSizeLabel + '" for="' + cIdPrefix + ::cId + '">' + ::cLabel + '</label>'

   ENDIF

   cHtml += '<textarea class="form-control '

   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF

   cHtml += '" rows="' + LTrim( Str( ::nRows ) ) + '" '
   cHtml += 'id="' + cIdPrefix + ::cId + '"  name="' + cIdPrefix + ::cId + '" '
// cHtml += 'placeholder="' + ::cPlaceHolder + '" '

   IF ::lReadOnly
      cHtml += ' readonly '
   ENDIF


   cHtml += ' data-live '

   IF !Empty( ::cChange )
      IF At( '(', ::cChange ) >  0   // Exist function ?
         cHtml += ' onchange="' + ::cChange + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cChange + '" '
      ENDIF

   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF


// cHtml += ' value="' + ::uValue + '">'
   cHtml += ' >'
   cHtml += ::uValue

   cHtml += '</textarea>'

   cHtml += '</div>'

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )

   RETU cHtml

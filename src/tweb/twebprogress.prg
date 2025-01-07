// -------------------------------------------------------------

CLASS TWebProgress FROM TWebControl

   DATA lPercentage     INIT .T.
   DATA nHeight      INIT 0

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New(  oParent, cId, nValue, cLabel, lPercentage, nGrid, cClass, cFont, cStyle, cProp, lHidden, nHeight ) CLASS TWebProgress

   DEFAULT cId TO ::GetId()
   DEFAULT nValue TO 0
   DEFAULT nGrid TO 4
   DEFAULT cLabel TO ''
   DEFAULT lPercentage TO .F.
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.
   DEFAULT nHeight TO 0

   ::oParent   := oParent
   ::cId   := cId
   ::uValue   := nValue
   ::cLabel   := cLabel
   ::lPercentage := lPercentage
   ::nGrid   := nGrid
   ::cClass   := cClass
   ::cFont   := cFont
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lHidden  := lHidden
   ::nHeight  := nHeight

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebProgress

   LOCAL cHtml  := ''
   LOCAL cIdPrefix
   LOCAL cSt := ''

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF


   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) ) + ' tweb_progress" '



   IF  ::oParent:lDessign
      cSt += 'border:1px solid blue;'
   ENDIF

   IF ::lHidden
      cSt += 'display:none;'
   ENDIF

   IF !Empty( cSt )
      cHtml += ' style="' + cSt + '" '
   ENDIF

   cHtml += '>'


   IF !Empty( ::cLabel )
      cHtml += '<label id="' + cIdPrefix + ::cId + '_label"' + ' class="col-form-label  '

// if !empty( ::cClass )
// cHtml += ' ' + ::cClass
// endif

      IF !Empty( ::cFont )
         cHtml += ' ' + ::cFont
      ENDIF

      cHtml += '" >' + ::cLabel + '</label>'
   ENDIF

   cHtml += '<div class="progress " '

   cHtml += ' style="'
   IF ::nHeight > 0
      cHtml += 'height: ' + LTrim( Str( ::nHeight ) ) + 'px;'
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ::cStyle
   ENDIF

   cHtml += '" >'

   cHtml += '<div id="' + cIdPrefix + ::cId + '" '  + 'class="progress-bar ' + ::cClass + ' "'
   cHtml += 'role="progressbar" '
   cHtml += 'style="width: ' + LTrim( Str( ::uValue ) ) + '%;"'
   cHtml += ' data-percentage="'  + if( ::lPercentage, 'yes', 'no' ) + '" '
   cHtml += ' >'

   IF ::lPercentage
      cHtml += LTrim( Str( ::uValue ) ) + '%'
   ENDIF

   cHtml += '</div>'

 /*
  if !empty( ::cProp )
   cHtml += ' ' + ::cProp + ' '
  endif

  cHtml += ' data-live '
 */


// if !empty( ::cStyle )
// cHtml += '</div>'
// endif

   cHtml += '  </div>'
   cHtml += '</div>'

   RETU cHtml

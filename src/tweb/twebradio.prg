// -------------------------------------------------------------

CLASS TWebRadio FROM TWebControl

   DATA lInline     INIT .F.
   DATA aItems      INIT {}
   DATA aValues     INIT {}

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cLabel, uValue, aItems, aValues, lReadOnly, nGrid, cAction, lInline, cClass, cFont, cStyle, cProp, lHidden ) CLASS TWebRadio

   DEFAULT cId TO ::GetId()
   DEFAULT cLabel TO ''
   DEFAULT uValue TO ''
   DEFAULT aItems TO {}
   DEFAULT aValues TO {}
   DEFAULT nGrid TO 4
   DEFAULT cAction TO ''
   DEFAULT lInline TO .F.
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lReadOnly TO .F.
   DEFAULT lHidden TO .F.


   ::oParent   := oParent
   ::cId   := cId
   ::cLabel  := cLabel
   ::uValue  := uValue
   ::aItems   := aItems // IF( valtype( aItems ) == 'A', aItems, {} )
   ::aValues  := aValues // IF( valtype( aValues ) == 'A' .AND. len( aValues ) == len( aItems ), aValues, aItems )
   ::nGrid   := nGrid
   ::cAction  := cAction
   ::lInline  := lInline
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

METHOD Activate() CLASS TWebRadio

   LOCAL cHtml  := ''
   LOCAL cChecked := ''
   LOCAL cSt  := ''
   LOCAL nI, cIdPrefix

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

// IF lValue
// cChecked := 'checked=false'
   cChecked := ''
// ENDIF

   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) ) + ' custom-control " '

   IF ::oParent:lDessign
      cSt += "border:1px solid blue;"
   ENDIF

   IF ::lHidden
      cSt += 'display:none;'
   ENDIF



   cSt += "padding-left: 15px;"

   cHtml += ' style="' + cSt + '" '

   cHtml += ' data-control="radio">'

   IF !Empty( ::cStyle )
      cHtml += '<div style="' + ::cStyle + '" >'
   ENDIF

   IF !Empty( ::cLabel )

      cHtml += '<label class="custom-control-label tweb_pointer '

      IF !Empty( ::cClass )
         cHtml += ' ' + ::cClass
      ENDIF

      IF !Empty( ::cFont )
         cHtml += ' ' + ::cFont
      ENDIF

      cHtml += '" '


      cHtml +=  '">' + ::cLabel + '&nbsp;</label> '

      IF ! ::lInline
         cHtml += '<br>'
      ENDIF

   ENDIF

   FOR nI := 1 TO Len( ::aItems )

      cHtml += '<div class="custom-control custom-radio ' + IF( ::lInline, 'custom-control-inline', '' ) + '">'
      cHtml += ' <input type="radio" class="custom-control-input tweb_pointer" id="' + cIdPrefix + ::cId + '_' + LTrim( Str( ni ) ) + '" name="' +  cIdPrefix + ::cId  + '" value="' +  ::aValues[ nI ] + '" ' + IF( ::lDisabled, 'disabled', '' )
      cHtml += ' data-live '

      IF !Empty( ::cProp )
         cHtml += ' ' + ::cProp + ' '
      ENDIF

      IF !Empty( ::cAction )
         IF At( '(', ::cAction ) >  0   // Exist function ?
            cHtml += ' onchange="' + ::cAction + '" '
         ELSE
            cHtml += ' data-onchange="' + ::cAction + '" '
         ENDIF

      ENDIF
// cHtml += '  onchange="' + ::cAction + '" '

      IF ::uValue == ::aValues[ nI ]
         cHtml += ' checked '
      ENDIF

      IF ::lReadOnly
         cHtml += ' disabled '
      ENDIF


      cHtml += '  >'
      cHtml += ' <label class="custom-control-label tweb_pointer '

      IF !Empty( ::cClass )
         cHtml += ' ' + ::cClass
      ENDIF

      IF !Empty( ::cFont )
         cHtml += ' ' + ::cFont
      ENDIF

      cHtml += '" '

      cHtml += ' for="'  + cIdPrefix + ::cId + '_' + LTrim( Str( ni ) ) + '" >' + ::aItems[ nI ] + '</label>'
      cHtml += '</div>'

   NEXT

   IF !Empty( ::cStyle )
      cHtml += '</div>'
   ENDIF

   cHtml += '</div>'

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )

   RETU cHtml

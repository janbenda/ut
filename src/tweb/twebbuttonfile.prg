// -------------------------------------------------------------

CLASS TWebButtonFile FROM TWebControl

   DATA cType       INIT 'text'
   DATA cPlaceHolder     INIT ''
   DATA cConfirm      INIT ''
   DATA lOutline      INIT .T.
   DATA lSubmit     INIT .F.

   DATA lMultiple     INIT .F.


   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cLabel, cName, cAction, cValue, nGrid, cAlign, cIcon, lDisabled, lSubmit, cClass, cFont, nWidth, cConfirm, cStyle, cProp, lMultiple ) CLASS TWebButtonFile

   DEFAULT cId TO ::GetId()
   DEFAULT cLabel TO ''
   DEFAULT cAction TO ''
   DEFAULT cName TO ''
   DEFAULT cValue TO ''
   DEFAULT nGrid TO 4
   DEFAULT cAlign TO ''
   DEFAULT cIcon TO ''  // '<i class="fas fa-check"></i>'
   DEFAULT cClass TO 'btn-warning'
   DEFAULT lDisabled TO .F.
   DEFAULT lSubmit TO .F.

   DEFAULT cClass TO ''
   DEFAULT cFont TO ''

   DEFAULT nWidth TO ''
   DEFAULT cConfirm TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lMultiple TO .F.


   IF Empty( cClass )
      cClass := if( ::lOutline, 'btn btn-warning', 'btn-primary' )
   ENDIF


   ::oParent   := oParent
   ::cId   := cId
   ::nGrid   := nGrid
   ::cAlign   := Lower( cAlign )
   ::cLabel   := cLabel
   ::cAction   := cAction

   ::cName   := cName
   ::uValue  := cValue
   ::cClass  := cClass
   ::cIcon   := cIcon
   ::lDisabled := lDisabled
   ::lSubmit  := lSubmit

   ::cClass   := cClass
   ::cFont   := cFont

   ::nWidth   := nWidth
   ::cConfirm   := cConfirm
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lMultiple  := lMultiple

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebButtonFile

   LOCAL cHtml := ''
   LOCAL cSize := ''
   LOCAL cType := 'button'
   LOCAL cIdPrefix

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM' ; cSize := 'btn-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG' ; cSize := 'btn-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   IF ::lSubmit
      cType := 'submit'
   ENDIF


   IF Empty( ::cName )
      ::cName := ::cId
   ENDIF


   cHtml += '<div class="col-' + LTrim( Str( ::nGrid ) )
   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )

   DO CASE
   CASE ::cAlign == 'center' ; cHtml += ' text-center'
   CASE ::cAlign == 'right'  ; cHtml += ' text-right'
   ENDCASE


   cHtml += '" '

   cHtml += IF( ::oParent:lDessign, 'style="border:1px solid blue;"', '' )
   cHtml += '>'

   cHtml += '<input type="file" id="_' + cIdPrefix + ::cId + '" style="display:none;" '

   IF !Empty( ::cAction )

      IF At( '(', ::cAction ) >  0   // Exist function ?
         cHtml += 'onchange="' + ::cAction + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cAction + '" '
      ENDIF
   ENDIF

   cHtml += IF( ::lMultiple, ' multiple ', '' )

// cHtml += ' data-live '
   cHtml += ' data-live >'

   cHtml += '<button type="' + cType + '" '

   cHtml += 'id="' + cIdPrefix + ::cId + '" name="' + ::cName + '" value="' + ::uValue + '" '

   cHtml += 'class="btn ' + cSize


   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF


   cHtml += '" '

   IF !Empty( ::nWidth )
      ::cStyle += 'width: '  + ::nWidth + '; '
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF

   cHtml += ' data-live '

   IF !Empty( ::cConfirm )
      cHtml += ' data-confirm="' + ::cConfirm + '" '
   ENDIF



   cHtml += ' onclick="TDoClick(' + "'_" + cIdPrefix + ::cId + "' )" + '" '

   cHtml += IF( ::lDisabled, 'disabled', '' ) + ' >'
   cHtml += ::cIcon + ::cLabel
   cHtml += '</button>'
   cHtml += '</div>'


   RETU cHtml

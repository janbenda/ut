// -------------------------------------------------------------

CLASS TWebButton FROM TWebControl

   DATA cType       INIT 'text'
   DATA cPlaceHolder     INIT ''
   DATA cConfirm      INIT ''
   DATA lOutline      INIT .T.
   DATA lSubmit     INIT .F.
   DATA cLink      INIT ''
   DATA cId_Btn_Files    INIT ''
   DATA cPBS      INIT ''

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cLabel, cAction, cName, cValue, nGrid, cAlign, cIcon, lDisabled, lSubmit, cLink, cClass, cFont, cId_Btn_Files, nWidth, cConfirm, cStyle, cProp, lHidden, cPBS ) CLASS TWebButton

   DEFAULT cId TO ::GetId()
   DEFAULT cLabel TO ''
   DEFAULT cAction TO ''
   DEFAULT cName TO ''
   DEFAULT cValue TO ''
   DEFAULT nGrid TO 4
   DEFAULT cAlign TO ''
   DEFAULT cIcon TO ''  // '<i class="fas fa-check"></i>'
   DEFAULT lDisabled TO .F.

   DEFAULT lSubmit TO .F.
   DEFAULT cLink TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cId_Btn_Files TO ''

   DEFAULT nWidth TO ''
   DEFAULT cConfirm TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.
   DEFAULT cPBS TO ''




   ::oParent   := oParent
   ::cId   := cId
   ::nGrid   := nGrid
   ::cAlign   := Lower( cAlign )
   ::cLabel   := cLabel
   ::cAction  := cAction
   ::cName   := cName
   ::uValue  := cValue
   ::cClass  := cClass
   ::cIcon   := cIcon
   ::lDisabled := lDisabled
   ::lSubmit  := lSubmit
   ::cLink   := cLink
   ::cFont   := cFont
   ::cId_Btn_Files := cId_Btn_Files
   ::nWidth   := nWidth
   ::cConfirm   := cConfirm
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lHidden   := lHidden
   ::cPBS    := cPBS

   IF Empty( ::cClass )
      ::cClass := if( ::lOutline, 'btn-outline-secondary', 'btn-secondary' )
   ENDIF

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebButton

   LOCAL cHtml := ''
   LOCAL cSize := ''
   LOCAL cType := 'button'
   LOCAL cIdPrefix
   LOCAL cSt := ''

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

   IF !Empty( ::cLink )
      ::cAction := "location.href='" + ::cLink + "' "
   ENDIF

   IF Empty( ::cName )
      ::cName := ::cId
   ENDIF


   IF ::nGrid > 0
      cHtml += '<div class="col-' + LTrim( Str( ::nGrid ) )
      cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )

      DO CASE
      CASE ::cAlign == 'center' ; cHtml += ' text-center'
      CASE ::cAlign == 'right'  ; cHtml += ' text-right'
      ENDCASE


      cHtml += '" '

      cHtml += IF( ::oParent:lDessign, 'style="border:1px solid blue;"', '' )

      cHtml += '>'

   ENDIF

   IF !Empty( ::cLink )
      cHtml += '<a href="' + ::cLink + '">'
   ENDIF


   cHtml += '<button type="' + cType + '" '
// cHtml += 'id="' + cIdPrefix + ::cId + '" '
   cHtml += 'class="btn ' + cSize


   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF

 /*
 if ::nGrid > 0
  cHtml += ' col-' + ltrim(str(::nGrid))
 endif
 */

   cHtml += '" '

   IF !Empty( ::nWidth )
// cHtml += 'style="width: '  + ::nWidth + '; " '
      ::cStyle += 'width: '  + ::nWidth + '; '
   ENDIF


   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF


   IF ::lHidden
      ::cStyle += 'display:none;'
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   cHtml += ' data-live '

   IF !Empty( ::cConfirm )
      cHtml += ' data-confirm="' + ::cConfirm + '" '
   ENDIF

   IF !Empty( ::cPBS )
      cHtml += ' data-pbs="' + ::cPBS + '" '
   ENDIF

   IF !Empty( ::cId_Btn_Files )
      cHtml += ' data-upload="_' + cIdPrefix + ::cId_Btn_Files + '" '
   ENDIF

   IF Empty( ::cLink )
      IF !Empty( ::cAction )

         IF At( '(', ::cAction ) >  0   // Exist function ?
            cHtml += 'onclick="' + ::cAction + '" '
         ELSE
            cHtml += ' data-onclick="' + ::cAction + '" '
         ENDIF
      ENDIF
   ENDIF


   cHtml += 'id="' + cIdPrefix + ::cId + '" name="' + ::cName + '" value="' + ::uValue + '" '




   cHtml += IF( ::lDisabled, 'disabled', '' ) + ' >'
   cHtml += ::cIcon + ::cLabel
   cHtml += '</button>'

   IF !Empty( ::cLink )
      cHtml += '</a>'
   ENDIF


   IF ::nGrid > 0
      cHtml += '</div>'
   ENDIF

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )

   RETU cHtml

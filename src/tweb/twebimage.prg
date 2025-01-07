// -------------------------------------------------------------



CLASS TWebImage FROM TWebControl

   DATA cSrc      INIT ''
   DATA cBigSrc     INIT ''
   DATA cGallery     INIT ''
   DATA lZoom      INIT .T.

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cSrc, cBigSrc, nGrid, cAlign, cClass, nWidth, cGallery, lNoZoom, cStyle, cProp, lHidden ) CLASS TWebImage

   DEFAULT cId TO ::GetId()
   DEFAULT cSrc TO ''
   DEFAULT cBigSrc TO ''
   DEFAULT nGrid TO 4
   DEFAULT cAlign TO ''
   DEFAULT cClass TO ''
   DEFAULT nWidth TO 0
   DEFAULT cGallery TO ''
   DEFAULT lNoZoom TO .F.
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.

   ::oParent   := oParent
   ::cId   := cId
   ::cSrc   := cSrc
   ::cBigSrc  := cBigSrc
   ::nGrid   := nGrid
   ::cAlign   := Lower( cAlign )
   ::cClass   := cClass
   ::nWidth   := nWidth
   ::cGallery  := cGallery
   ::lZoom   := !lNoZoom
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lHidden   := lHidden


   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebImage

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

   cHtml := '<div class="col-' + LTrim( Str( ::nGrid ) )

   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )
   cHtml += ' tweb_image'


   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF



   cHtml += '" '
   cHtml += IF( ::oParent:lDessign, 'style="border:1px solid black;"', '' )
   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'

   cHtml += '<div class="input-group" '
 /*
 if !empty( ::cStyle )
  cHtml += ' style="' + ::cStyle + '" '
 endif
 */

   cHtml += '>'

   IF ( !Empty( ::cBigSrc ) .OR. ::lZoom  )

      IF Empty( ::cBigSrc )
         ::cBigSrc = ::cSrc
      ENDIF

      cHtml += '<a id="twebimg_' + ::cId + '" href="' + ::cBigSrc + '" '

      IF  Empty( ::cGallery )
         cHtml += 'data-lightbox="twebimg_' + cIdPrefix + ::cId + '" '
      ELSE
         cHtml += 'data-lightbox="' + ::cGallery + '" '
      ENDIF

      IF !Empty( ::uValue )
         cHtml += 'data-title="' + ::cCaption + '" '
      ENDIF

      DO CASE
      CASE ::cAlign == 'center' ; cHtml += ' style="margin:auto;" '
      CASE ::cAlign == 'right'  ; cHtml += ' style="margin-left:auto;" '
      ENDCASE

      cHtml += ' >'

   ENDIF


   cHtml += '<img id="' + cIdPrefix + ::cId + '" src="' + ::cSrc + '" class="rounded " '

   IF !Empty( ::nWidth )

      IF ValType( ::nWidth ) == 'N'
         IF ::nWidth > 0
// cHtml += ' style="width:' + ltrim(str(::nWidth)) + 'px; '
            ::cStyle += ';width:' + LTrim( Str( ::nWidth ) ) + 'px; '
         ENDIF
      ELSE
         ::cStyle += ';width:' + ::nWidth + '; '
      ENDIF

   ENDIF

   IF ::lHidden
      ::cStyle += 'display:none;'
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF

   cHtml += ' alt="...">'

   IF ( !Empty( ::cBigSrc ) .OR.  ::lZoom  )
      cHtml += '</a>'
   ENDIF

   cHtml += '</div>'
   cHtml += '</div>'

   RETU cHtml

#include "hbclass.ch"

#pragma -kM+

MEMVAR server

//============================================================
CLASS UWBrowse
  DATA aColumns   INIT {}
  DATA nPageSize  INIT 0
  DATA nPos       INIT 0

  METHOD AddColumn(nID, cTitle, cField, lRaw)
  METHOD Output()
ENDCLASS


FUNC UWBrowseNew() 
LOCAL oW := UWBrowse()
RETURN oW


METHOD AddColumn(nID, cTitle, cField, lRaw) CLASS UWBrowse
  AADD(Self:aColumns, {nID, cTitle, cField, !EMPTY(lRaw)})
RETURN Self


METHOD Output() CLASS UWBrowse
LOCAL cRet := "", nI, xI, xField, nPos, cUrl, cI, lValidate

  cRet += '<table class="ubr"><tr>'

  // Header
  cRet += '<tr>'
  FOR nI := 1 TO LEN(Self:aColumns)
     cRet += '<th>' + UHtmlEncode(Self:aColumns[nI, 2]) + '</th>'
  NEXT
  cRet += '</tr>'

  // Body
  nPos := 0
  DBGOTOP()
  IF Self:nPageSize > 0 .AND. Self:nPos > 0
    DBSKIP(Self:nPos)
  ENDIF
  DO WHILE ! EOF()
    cRet += '<tr>'
    FOR nI := 1 TO LEN(Self:aColumns)
      xField := Self:aColumns[nI, 3]
      IF VALTYPE(xField) == "C"
        xI := FIELDGET(FIELDPOS(xField))
      ELSEIF VALTYPE(xField) == "B"
        xI := EVAL(xField)
      ENDIF
      IF     VALTYPE(xI) == "C";  xI := TRIM(xI)
      ELSEIF VALTYPE(xI) == "N";  xI := STR(xI)
      ELSEIF VALTYPE(xI) == "D";  xI := DTOC(xI)
      ELSE ;  xI := "VALTYPE()==" + VALTYPE(xI)
      ENDIF
      IF ! Self:aColumns[nI, 4]
        xI := UHtmlEncode(xI)
      ENDIF
      cRet += '<td><nobr>' + xI + '</nobr></td>'
    NEXT
    cRet += '</tr>'
    DBSKIP()
    IF ++nPos >= Self:nPageSize
      EXIT
    ENDIF
  ENDDO
  cRet += '</table>'
  IF ! EOF() .OR. Self:nPos > 0
    cUrl := server["REQUEST_URI"]
    IF (nI := AT("?_ucs=", cUrl)) == 0  
      nI := AT("&_ucs=", cUrl)
    ENDIF
    IF (lValidate := nI > 0)
      cUrl := LEFT(cUrl, nI - 1)
    ENDIF
    IF (nI := AT("?_pos=", cUrl)) == 0  
      nI := AT("&_pos=", cUrl)
    ENDIF
    IF nI > 0
      cUrl := LEFT(cUrl, nI - 1)
    ENDIF
    cUrl += IIF("?" $ cUrl, "&", "?") + "_pos="
    cRet := '<br>' + cRet
    IF ! EOF()
      cI := cUrl + LTRIM(STR(Self:nPos + Self:nPageSize))
      cRet := '<a href="' + IIF(lValidate, UUrlChecksum(cI), cI) + '">&gt;&gt;</a>' + cRet
    ENDIF
    IF Self:nPos > 0
      cI := cUrl + LTRIM(STR(MAX(0, Self:nPos - Self:nPageSize)))
      cRet := '<a href="' + IIF(lValidate, UUrlChecksum(cI), cI) + '">&lt;&lt;</a>&nbsp;&nbsp;' + cRet
    ENDIF
  ENDIF
RETURN cRet


//============================================================
CLASS UWOption
  DATA aOption   INIT {}
  DATA cValue

  METHOD Add(cTitle, cCode, lRaw)
  METHOD Output()
ENDCLASS


FUNC UWOptionNew() 
LOCAL oW := UWOption()
RETURN oW


METHOD Add(cTitle, cCode, lRaw) CLASS UWOption
  AADD(Self:aOption, {IIF(! EMPTY(lRaw), cTitle, UHtmlEncode(cTitle)), cCode})
RETURN Self


METHOD Output() CLASS UWOption
LOCAL cRet := ""
  AEVAL(Self:aOption, {|X| cRet += HB_STRFORMAT('<option value="%s"%s>%s</option>', UHtmlEncode(X[2]), IIF(X[2] == Self:cValue, " selected", ""), X[1])})
RETURN cRet



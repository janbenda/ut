
FUNC UParse(aData, cFileName)
RETURN parse_data(aData, compile_file(cFileName))


STATIC FUNC parse_data(aData, aCode)
LOCAL aInstr, aData2, cRet, xValue, aValue, cExtend := ""

  DO WHILE cExtend != NIL
    cExtend := NIL
    cRet := ""
    FOR EACH aInstr IN aCode
      SWITCH aInstr[1]
        CASE "txt"
          cRet += aInstr[2]
          EXIT

        CASE "="
          IF HB_HHasKey(aData, aInstr[2])
            xValue := aData[aInstr[2]]
            IF HB_ISCHAR(xValue)
              cRet += UHtmlEncode(xValue)
            ELSEIF HB_ISNUMERIC(xValue)
              cRet += UHtmlEncode(STR(xValue))
            ELSEIF HB_ISDATE(xValue)
              cRet += UHtmlEncode(DTOC(xValue))
            ELSEIF HB_ISTIMESTAMP(xValue)
              cRet += UHtmlEncode(HB_TTOC(xValue))
            ELSEIF HB_ISOBJECT(xValue)
              cRet += UHtmlEncode(xValue:Output())
            ELSE
              ? HB_STRFORMAT("Template error: invalid type '%s'", VALTYPE(xValue))
            ENDIF
          ELSE
            ? HB_STRFORMAT("Template error: variable '%s' not found", aInstr[2])
          ENDIF
          EXIT

        CASE ":"
          IF HB_HHasKey(aData, aInstr[2])
            xValue := aData[aInstr[2]]
            IF HB_ISCHAR(xValue)
              cRet += xValue
            ELSEIF HB_ISNUMERIC(xValue)
              cRet += STR(xValue)
            ELSEIF HB_ISDATE(xValue)
              cRet += DTOC(xValue)
            ELSEIF HB_ISTIMESTAMP(xValue)
              cRet += HB_TTOC(xValue)
            ELSEIF HB_ISOBJECT(xValue)
              cRet += xValue:Output()
            ELSE
              ? HB_STRFORMAT("Template error: invalid type '%s'", VALTYPE(xValue))
            ENDIF
          ELSE
            ? HB_STRFORMAT("Template error: variable '%s' not found", aInstr[2])
          ENDIF
          EXIT

        CASE "if"
          xValue := IIF(HB_HHasKey(aData, aInstr[2]), aData[aInstr[2]], NIL)
          IF ! EMPTY(xValue)
            cRet += parse_data(aData, aInstr[3])
          ELSE
            cRet += parse_data(aData, aInstr[4])
          ENDIF
          EXIT

        CASE "loop"
          IF HB_HHasKey(aData, aInstr[2]) .AND. VALTYPE(aValue := aData[aInstr[2]]) == "A"
            FOR EACH xValue IN aValue
              aData2 := HB_HCLONE(aData)
              HB_HEVAL(xValue, {|k,v| aData2[aInstr[2] + "." + k] := v})
              aData2[aInstr[2] + ".__index"] := xValue:__enumIndex
              cRet += parse_data(aData2, aInstr[3])
              aData2 := NIL
            NEXT
          ELSE
            ? HB_STRFORMAT("Template error: loop variable '%s' not found", aInstr[2])
          ENDIF
          EXIT

        CASE "extend"
          cExtend := aInstr[2]
          EXIT

        CASE "include"
          cRet += parse_data(aData, compile_file(aInstr[2]))
          EXIT
      ENDSWITCH
    NEXT
    IF cExtend != NIL
      aData[""] := cRet
      cRet := ""
      aCode := compile_file(cExtend)
    ENDIF
  ENDDO
RETURN cRet


STATIC FUNC compile_file(cFileName)
LOCAL nPos, cTpl, aCode := {}

  IF cFileName == NIL
    cFileName := MEMVAR->server["SCRIPT_NAME"]
  ENDIF
  cFileName := UOsFileName(HB_DIRBASE() + "/tpl/" + cFileName + ".tpl")
  IF HB_FILEEXISTS(cFileName)
    cTpl := HB_MEMOREAD(cFileName)
    BEGIN SEQUENCE
      IF (nPos := compile_buffer(cTpl, 1, aCode)) < LEN(cTpl) + 1
        BREAK(nPos)
      ENDIF
    RECOVER USING nPos
      ? HB_STRFORMAT("Template error: syntax at %s(%d,%d)", cFileName, SUBSTRCOUNT(CHR(10), cTpl,, nPos) + 1, nPos - HB_RAT(CHR(10), LEFT(cTpl, nPos - 1)))
      aCode := {}
    END SEQUENCE
  ELSE
      ? HB_STRFORMAT("Template error: file '%s' not found", cFileName)
  ENDIF
RETURN aCode


STATIC FUNC compile_buffer(cTpl, nStart, aCode)
LOCAL nI, nS, nE, cTag, cParam

   DO WHILE (nS := HB_AT("{{", cTpl, nStart)) > 0
     IF nS > nStart
       AADD(aCode, {"txt", SUBSTR(cTpl, nStart, nS - nStart)})
     ENDIF
     nE := HB_AT("}}", cTpl, nS)
     IF nE > 0
       IF (nI := HB_AT(" ", cTpl, nS, nE)) == 0
         nI := nE
       ENDIF
       cTag := SUBSTR(cTpl, nS + 2, nI - nS - 2)
       cParam := SUBSTR(cTpl, nI + 1, nE - nI - 1)

       SWITCH cTag
         CASE "="
         CASE ":"
           AADD(aCode, {cTag, cParam})
           nStart := nE + 2
           EXIT

         CASE "if"
           AADD(aCode, {"if", cParam, {}, {}})
           nI := compile_buffer(cTpl, nE + 2, ATAIL(aCode)[3])
           IF SUBSTR(cTpl, nI, 8) == "{{else}}"
             nI := compile_buffer(cTpl, nI + 8, ATAIL(aCode)[4])
           ENDIF
           IF SUBSTR(cTpl, nI, 9) == "{{endif}}"
             nStart := nI + 9
           ELSE
             BREAK(nI)
           ENDIF
           EXIT

         CASE "loop"
           AADD(aCode, {"loop", cParam, {}})
           nI := compile_buffer(cTpl, nE + 2, ATAIL(aCode)[3])
           IF SUBSTR(cTpl, nI, 11) == "{{endloop}}"
             nStart := nI + 11
           ELSE
             BREAK(nI)
           ENDIF
           EXIT

         CASE "extend"
           AADD(aCode, {"extend", cParam})
           nStart := nE + 2
           EXIT

         CASE "include"
           AADD(aCode, {"include", cParam})
           nStart := nE + 2
           EXIT

         OTHERWISE
           RETURN nS

       ENDSWITCH
     ELSE
       BREAK(nS)
     ENDIF
   ENDDO
   IF nStart < LEN(cTpl)
     AADD(aCode, {"txt", SUBSTR(cTpl, nStart)})
   ENDIF
RETURN LEN(cTpl) + 1


STATIC FUNC SUBSTRCOUNT(cSub, cString, nStart, nEnd)
LOCAL nCount := 0
  IF nStart == NIL;  nStart := 1
  ENDIF
  DO WHILE (nStart := HB_AT(cSub, cString, nStart, nEnd)) > 0
    nCount++
    nStart++
  ENDDO
RETURN nCount

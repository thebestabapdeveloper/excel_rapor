*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_BY_ODN_TLB_M01
*&---------------------------------------------------------------------*

DEFINE yukleniyor.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING percentage = &1
              text       = &2.
END-OF-DEFINITION.

DEFINE set_icon.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING  name                  = &1
               info                  = &2
    IMPORTING  result                = &3
    EXCEPTIONS icon_not_found        = 1
               outputfield_too_short = 2
               OTHERS                = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*

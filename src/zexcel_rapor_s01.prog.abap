*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_BY_ODN_TLB_S01
*&---------------------------------------------------------------------*

*- Seçim ekranları
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.

PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000' OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.

PARAMETERS: p_gjahr TYPE gjahr OBLIGATORY.
SELECT-OPTIONS:s_poper FOR faglflexa-poper DEFAULT '1' TO '12' OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_sel_out.

*&---------------------------------------------------------------------*

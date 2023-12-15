*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_BY_ODN_TLB_TOP
*&---------------------------------------------------------------------*

*- Sabitler
CONSTANTS: BEGIN OF gc_gui_status,
             gs_0100 TYPE char10 VALUE 'GS_0100',
           END   OF gc_gui_status,
           BEGIN OF gc_racct,
             b   TYPE racct VALUE '1910100001',
             c   TYPE racct VALUE '1910200001',
             d   TYPE racct VALUE '1910300001',
             f   TYPE racct VALUE '3910200001',
             g   TYPE racct VALUE '3910400001',
             h   TYPE racct VALUE '3910500001',
             i   TYPE racct VALUE '3910500002',
             d56 TYPE racct VALUE '3600305002',
           END OF gc_racct.

*- Sözlük Yapıları
TABLES: faglflexa.

*- Tipler
TYPES: BEGIN OF gty_poper,
         poper TYPE poper,
         begda TYPE begda,
         endda TYPE endda,
       END OF gty_poper.

*- Nesneler
DATA: go_excel TYPE REF TO zcl_excel.

*- İç tablolar
DATA: gt_out         TYPE faglflexa_t_type,
      gt_kdvrpias    TYPE TABLE OF zhvl_fi_kdvrpias,
      gt_vergi_liste TYPE zhvl_fi_gm_tt_vergi_list,
      gt_poper       TYPE TABLE OF gty_poper,
      gt_bkpf_d57    TYPE TABLE OF bkpf.

*- Yapılar
DATA: gs_out TYPE faglflexa,
      BEGIN OF gs_screen_init,
        screen_0100 TYPE char1,
      END   OF gs_screen_init.
*&---------------------------------------------------------------------*

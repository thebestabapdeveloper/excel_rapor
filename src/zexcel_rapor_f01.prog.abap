*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_BY_ODN_TLB_F01
*&---------------------------------------------------------------------*

FORM get_data.
*- Aralıklar
  RANGES: lr_poper FOR faglflexa-poper,
          lr_racct FOR faglflexa-racct.
  DATA: lr_rbusa TYPE RANGE OF gsber,
        lr_budat TYPE RANGE OF budat,
        lr_hkont TYPE RANGE OF hkont.

*- Değişkenler
  DATA: lv_ayin_son_gunu TYPE datum.

*- Lojik
  PERFORM evrenselleri_temizle.

  " Sütunları dolduracak ana hesaplar belirleniyor
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-b ). " B
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-c ). " C
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-d ). " D
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-f ). " F
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-g ). " G
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-h ). " H
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-i ). " I
  APPEND lr_racct.
  lr_racct = VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = gc_racct-d56 ). " D56
  APPEND lr_racct.

  yukleniyor 10 'Uyarlama tabloları sorgulanıyor'.
  SELECT * FROM zhvl_fi_kdvrpias
    INTO TABLE gt_kdvrpias.

  lr_rbusa = VALUE #( BASE lr_rbusa FOR ls_kdvrpias IN gt_kdvrpias
                      ( sign   = 'I'
                        option = 'EQ'
                        low    = ls_kdvrpias-rbusa ) ).

*- Defteri kebir: Fiili münferit kalemleri çekiliyor
  yukleniyor 20 'Defteri kebir: Fiili münferit kalemleri çekiliyor'.
  SELECT * FROM faglflexa
    INTO TABLE @DATA(lt_faglflexa)
    WHERE ryear  EQ @p_gjahr
      AND rbukrs EQ @p_bukrs
      AND rldnr  EQ @zhvl_fi_if_constants=>defterler-tcdd_ana_defter_tms
      AND poper  IN @s_poper
      AND racct  IN @lr_racct
      AND rbusa  IN @lr_rbusa.

*- Vergi Listesi çekiliyor
  yukleniyor 50 'Vergi Listesi çekiliyor'.
  APPEND VALUE #( poper = '001' ) TO gt_poper.
  APPEND VALUE #( poper = '002' ) TO gt_poper.
  APPEND VALUE #( poper = '003' ) TO gt_poper.
  APPEND VALUE #( poper = '004' ) TO gt_poper.
  APPEND VALUE #( poper = '005' ) TO gt_poper.
  APPEND VALUE #( poper = '006' ) TO gt_poper.
  APPEND VALUE #( poper = '007' ) TO gt_poper.
  APPEND VALUE #( poper = '008' ) TO gt_poper.
  APPEND VALUE #( poper = '009' ) TO gt_poper.
  APPEND VALUE #( poper = '010' ) TO gt_poper.
  APPEND VALUE #( poper = '011' ) TO gt_poper.
  APPEND VALUE #( poper = '012' ) TO gt_poper.

  DELETE gt_poper WHERE poper NOT IN s_poper.

  LOOP AT gt_poper ASSIGNING FIELD-SYMBOL(<lfs_poper>).
    CLEAR lv_ayin_son_gunu.
    <lfs_poper>-begda = |{ p_gjahr }{ <lfs_poper>+1(2) }01|.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = <lfs_poper>-begda
      IMPORTING
        last_day_of_month = lv_ayin_son_gunu
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      <lfs_poper>-endda = lv_ayin_son_gunu.
      APPEND VALUE #( sign   = 'I'
                      option = 'BT'
                      low    = <lfs_poper>-begda
                      high   = <lfs_poper>-endda ) TO lr_budat.
    ENDIF.
  ENDLOOP.
  DELETE gt_poper WHERE endda IS INITIAL.

  APPEND VALUE #( sign   = 'I'
                  option = 'EQ'
                  low    = '1910100001' ) TO lr_hkont.

  SUBMIT zhvl_fi_gm_vergi_liste
         WITH p_bukrs EQ p_bukrs
         WITH p_gjahr EQ p_gjahr
         WITH s_budat IN lr_budat
         WITH s_bldat IN lr_budat
         WITH s_hkont IN lr_hkont
         WITH p_call EQ abap_true
          AND RETURN.

  IMPORT gt_list = gt_vergi_liste FROM MEMORY.
  DELETE gt_vergi_liste WHERE sgtxt CP '*DÖNEMİ KDV.LERİN MAHSUBU'.

*- 1004 iş alanına ait ÖdncekÜŞh KDVTvkfat hesaplanıyor (D57)
  yukleniyor 75 '1004 iş alanına ait ÖdncekÜŞh KDVTvkfat hesaplanıyor.'.

  SELECT *
    FROM bseg
    INTO TABLE @DATA(lt_bseg_d57)
   WHERE bukrs EQ @zhvl_fi_if_constants=>bukrs_default
     AND gjahr EQ @p_gjahr
     AND shkzg EQ @zhvl_fi_if_constants=>shkzg_s_borc
     AND gsber EQ '1004'
     AND hkont EQ @gc_racct-d56.

  DELETE lt_bseg_d57 WHERE belnr CP '99*'.

  SELECT *
    FROM bkpf
     FOR ALL ENTRIES IN @lt_bseg_d57
   WHERE bukrs EQ @lt_bseg_d57-bukrs
     AND belnr EQ @lt_bseg_d57-belnr
     AND gjahr EQ @lt_bseg_d57-gjahr
     AND stblg EQ @space
    INTO TABLE @gt_bkpf_d57.

  SORT gt_bkpf_d57 BY bukrs belnr gjahr.

*-Veri tablosu oluşturuluyor.
  yukleniyor 90 'Rapor oluşturuluyor.'.
  LOOP AT lt_faglflexa INTO DATA(ls_faglflexa).
    CLEAR gs_out.
    gs_out = ls_faglflexa.
    APPEND gs_out TO gt_out.
  ENDLOOP.

  yukleniyor 99 'Tamamlandı.'.
ENDFORM.

FORM show_data.
  PERFORM excel_template_al.
  PERFORM excele_yaz.
ENDFORM.

FORM init_status_0100.
  SET PF-STATUS gc_gui_status-gs_0100.
  IF gs_screen_init-screen_0100 IS NOT INITIAL.
    RETURN.
  ENDIF.

  PERFORM excel_template_al.
  PERFORM excele_yaz.
  gs_screen_init-screen_0100 = abap_true.
ENDFORM.

FORM init.
ENDFORM.

FORM evrenselleri_temizle.
  REFRESH: gt_out,
           gt_kdvrpias,
           gt_vergi_liste,
           gt_poper,
           gt_bkpf_d57.
  CLEAR: gs_out.
ENDFORM.

FORM excel_template_al.
*- Nesneler
  DATA: lo_reader TYPE REF TO zif_excel_reader.

*- Değişkenler
  DATA: lv_string TYPE string.

*- Lojik
  lo_reader = NEW zcl_excel_reader_2007( ).
  go_excel = NEW #( ).

  lv_string = 'ZHVL_FI_GM_KDV_DOKUM_CETVELI'.

  TRY.
      go_excel = lo_reader->load_file_from_mime( i_template = lv_string ).
    CATCH zcx_excel INTO DATA(lo_error). " Exceptions for ABAP2XLSX
      MESSAGE lo_error->error TYPE 'E'.
  ENDTRY.
ENDFORM.

FORM excele_yaz.
*- Yapılar
  TYPES: BEGIN OF lty_collect,
           rbusa TYPE gsber,
           racct TYPE racct,
           hsl   TYPE vlcur12,
         END OF lty_collect,
         BEGIN OF lty_vt_collect,
           mwskz  TYPE mwskz,
           fwste  TYPE fwste,
           fwste1 TYPE fwste,
         END OF lty_vt_collect.

*- Nesneler
  DATA: lo_style            TYPE REF TO zcl_excel_style,
        lo_border           TYPE REF TO zcl_excel_style_border,
        lo_writer           TYPE REF TO zif_excel_writer,
        lo_worksheet        TYPE REF TO zcl_excel_worksheet,
        lo_column_dimension TYPE REF TO zcl_excel_worksheet_columndime.

*- Aralıklar
  DATA: lr_budat TYPE RANGE OF budat.

*- İç tablolar
  DATA: lt_xtab       TYPE cpt_x255,
        lt_collect    TYPE TABLE OF lty_collect,
        lt_vt_collect TYPE TABLE OF lty_vt_collect.

*- Yapılar
  DATA: ls_collect    TYPE lty_collect,
        ls_vt_collect TYPE lty_vt_collect.

*- Değişkenler
  DATA: lv_style   TYPE zexcel_cell_style,
        lv_d56_hsl TYPE vlcur12,
        lv_d57_hsl TYPE vlcur12.

*- Lojik
  lo_border = NEW #( ).
  lo_border->border_color-rgb = zcl_excel_style_color=>c_black.
  lo_border->border_style     = zcl_excel_style_border=>c_border_thin.

  " Veri satır stili
  lo_style = go_excel->add_new_style( ).
  lo_style->font->size = 11.
  lo_style->alignment->wraptext   = abap_true.
  lo_style->alignment->vertical   = zcl_excel_style_alignment=>c_vertical_top.
  lo_style->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_left.
  lo_style->borders->allborders = lo_border.
  lv_style = lo_style->get_guid( ).

  SORT gt_out BY poper.

  LOOP AT gt_out INTO DATA(ls_out) GROUP BY ls_out-poper.

    " REFRESH:lt_collect. "Collect özellikle temizlenmedi çünkü hesaplar ay - ay eklenerek toplanıyor
    CLEAR: lv_d57_hsl.
    go_excel->set_active_sheet_index( CONV #( ls_out-poper ) ).
    lo_worksheet = go_excel->get_active_worksheet( ).

    " Excel Dosyasından PDF alınmak istenirse düzgün çıktı üretilmesi için set edildi.
    lo_worksheet->sheet_setup->paper_size   = zcl_excel_sheet_setup=>c_papersize_a3.
    lo_worksheet->sheet_setup->fit_to_page  = abap_true.
    lo_worksheet->sheet_setup->fit_to_width = 1.

    LOOP AT GROUP ls_out INTO DATA(ls_out_mmbr). " GROUP BY ls_out_mmbr-rbusa.
*      ADD 1 TO lv_group_size.
      CLEAR ls_collect.
      MOVE-CORRESPONDING ls_out_mmbr TO ls_collect.
      COLLECT ls_collect INTO lt_collect.
      IF     ls_out_mmbr-docnr NP '99*'
         AND ls_out_mmbr-drcrk EQ 'S'
         AND ls_out_mmbr-racct EQ gc_racct-d56.
        READ TABLE gt_bkpf_d57 TRANSPORTING NO FIELDS WITH KEY bukrs = ls_out_mmbr-rbukrs
                                                               belnr = ls_out_mmbr-docnr
                                                               gjahr = ls_out_mmbr-gjahr
                                                               BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_d57_hsl = lv_d57_hsl + ls_out_mmbr-hsl.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " RBUSA bazında toplam değer.
    LOOP AT lt_collect INTO DATA(ls_colgrp) GROUP BY ls_colgrp-rbusa.

      READ TABLE gt_kdvrpias INTO DATA(ls_kdvrpias) WITH KEY rbusa = ls_colgrp-rbusa.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      LOOP AT GROUP ls_colgrp INTO DATA(ls_colgrpmem).
        CASE ls_colgrpmem-racct.
          WHEN gc_racct-b.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'B'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-c.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'C'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-d.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'D'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-f.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'F'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-g.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'G'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-h.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'H'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN gc_racct-i.
            lo_worksheet->set_cell( ip_row    = CONV #( ls_kdvrpias-satir_no )
                                    ip_column = 'I'
                                    ip_value  = ls_colgrpmem-hsl
                                    ip_style  = lv_style ).
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    " D56 toplamı clear edilmeyecek aylar toplanarak hesaplanacak.
    LOOP AT lt_collect INTO ls_collect WHERE racct = gc_racct-d56.
      lv_d56_hsl = lv_d56_hsl + ls_collect-hsl.
    ENDLOOP.
    lo_worksheet->set_cell( ip_row    = 56
                            ip_column = 'D'
                            ip_value  = lv_d56_hsl
                            ip_style  = lv_style ).

    " D57
    lo_worksheet->set_cell( ip_row    = 57
                            ip_column = 'D'
                            ip_value  = lv_d57_hsl
                            ip_style  = lv_style ).

    lo_column_dimension = lo_worksheet->get_column_dimension( ip_column = 'B' ).
    lo_column_dimension->set_auto_size( ip_auto_size = abap_true ).

    lo_column_dimension = lo_worksheet->get_column_dimension( ip_column = 'D' ).
    lo_column_dimension->set_width( ip_width = 65 ).

*- Vergi listesi
    DATA(lt_vergi_liste) = gt_vergi_liste.

    REFRESH lt_vt_collect.

    READ TABLE gt_poper INTO DATA(ls_poper) WITH KEY poper = ls_out-poper.
    IF sy-subrc EQ 0.
      REFRESH lr_budat.
      CLEAR lr_budat.
      APPEND VALUE #( sign   = 'I'
                      option = 'BT'
                      low    = ls_poper-begda
                      high   = ls_poper-endda ) TO lr_budat.
      DELETE lt_vergi_liste WHERE budat NOT IN lr_budat.
      LOOP AT lt_vergi_liste INTO DATA(ls_vergi_liste).
        MOVE-CORRESPONDING ls_vergi_liste TO ls_vt_collect.
        COLLECT ls_vt_collect INTO lt_vt_collect.
      ENDLOOP.
    ENDIF.

    " B65
    lo_worksheet->set_cell( ip_row    = 65
                            ip_column = 'B'
                            ip_value  = ( VALUE fwste( lt_vt_collect[ mwskz = 'V4' ]-fwste DEFAULT 0 )
                                         + VALUE fwste( lt_vt_collect[ mwskz = 'V4' ]-fwste1 DEFAULT 0 ) )
                            ip_style  = lv_style ).

    " B67
    lo_worksheet->set_cell( ip_row    = 67
                            ip_column = 'B'
                            ip_value  = ( VALUE fwste( lt_vt_collect[ mwskz = 'V1' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V9' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V9' ]-fwste DEFAULT 0 ) )
                            ip_style  = lv_style ).

    " B66
    lo_worksheet->set_cell( ip_row    = 66
                            ip_column = 'B'
                            ip_value  = ( VALUE fwste( lt_vt_collect[ mwskz = 'V2' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V2' ]-fwste DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V8' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V8' ]-fwste DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = '00' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = '00' ]-fwste DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'KD' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'KD' ]-fwste DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V0' ]-fwste1 DEFAULT 0 )
                                          + VALUE fwste( lt_vt_collect[ mwskz = 'V0' ]-fwste DEFAULT 0 ) )
                            ip_style  = lv_style ).

  ENDLOOP.

  go_excel->set_active_sheet_index( 1 ).

  " Export to Excel
  lo_writer = NEW zcl_excel_writer_2007( ).
  DATA(lv_xstring) = lo_writer->write_file( go_excel ).

  cl_scp_change_db=>xstr_to_xtab( EXPORTING im_xstring = lv_xstring
                                  IMPORTING ex_xtab    = lt_xtab ).

  IF sy-batch EQ 'X'.
    PERFORM tabloyu_gos_a_gonder USING 'KDV_DC'
                                       lv_xstring.
  ELSE.

    cl_gui_frontend_services=>show_document( EXPORTING  document_name         = 'KDV_DC.xlsx'               " Default document file name
                                                        mime_type             = 'Excel'             "'BIN' " MIME Type
                                                        data_length           = xstrlen( lv_xstring )      " File Length
                                             CHANGING   document_data         = lt_xtab                    " Transfer table
                                             EXCEPTIONS cntl_error            = 1                          " Error when calling front-end control or internal error
                                                        error_no_gui          = 2                          " No SAPGUI available (background mode)
                                                        bad_parameter         = 3                          " Invalid input value
                                                        error_writing_data    = 4                          " Error when downloading document content
                                                        error_starting_viewer = 5                          " Cannot launch display application
                                                        unknown_mime_type     = 6                          " Front end does not recognize specified MIME typ
                                                        not_supported_by_gui  = 7                          " Method not supported by client
                                                        access_denied         = 8                          " Operation rejected by front end
                                                        no_authority          = 9                          " Missing authority
                                                        OTHERS                = 10 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.

FORM call_gos.
  CALL FUNCTION 'ZHVL_BC_RAPORLAR_ICIN_GOS'.
ENDFORM.

FORM tabloyu_gos_a_gonder USING iv_dosya_adi        TYPE string
                                iv_excel_as_xstring TYPE xstring.

*- Sabitler
  CONSTANTS: lc_objtyp TYPE swo_objtyp VALUE 'ZGOS'.

*- Değişkenler
  DATA: lv_objkey TYPE swo_typeid.

*- Lojik
  SELECT SINGLE name FROM trdir
    INTO lv_objkey
    WHERE name EQ sy-repid.

  CALL FUNCTION 'ZHVL_CA_SAVE_DOC_GOS'
    EXPORTING
      iv_file_name      = CONV string( iv_dosya_adi && '_' && sy-datum && '_' && sy-uzeit && '.xlsx' )
      iv_objkey         = lv_objkey
      iv_bus            = lc_objtyp
      iv_media_value    = iv_excel_as_xstring
    EXCEPTIONS
      file_cannot_saved = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Excel, GOS servisine yazılamadı!' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM at_sel_out.
*- Lojik
  PERFORM call_gos.
  LOOP AT SCREEN.
    IF screen-name CP '*S_POPER-LOW*'.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Rapor  ZHVL_FI_BY_R_ODN_TLB
*&---------------------------------------------------------------------*
*& Yazan                   : Tuna CENGİZKANER
*& Tasarlayan              : Esra TAŞKIN
*& Çağrı Numarası          : 8000019225
*& Açıklama                : KDV Döküm Cetveli
*&---------------------------------------------------------------------*
REPORT zexcel_rapor.

INCLUDE ZEXCEL_RAPOR_TOP.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_top. " Global tanımlamalar
INCLUDE ZEXCEL_RAPOR_S01.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_s01. " Seçim ekranları
INCLUDE ZEXCEL_RAPOR_M01.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_m01. " Makrolar
INCLUDE ZEXCEL_RAPOR_F01.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_f01. " Alt rutinler
INCLUDE ZEXCEL_RAPOR_PBO.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_pbo. " PBO modülleri
INCLUDE ZEXCEL_RAPOR_PAI.
*INCLUDE zhvl_fi_gm_i_kdv_dokum_cet_pai. " PAI modülleri

*---Programın başlatılması---------------------------------------------*
INITIALIZATION.
  PERFORM init.

*---Seçimin başlangıcı-------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

*---Seçimin bitişi-----------------------------------------------------*
END-OF-SELECTION.
  PERFORM show_data.

*&---------------------------------------------------------------------*

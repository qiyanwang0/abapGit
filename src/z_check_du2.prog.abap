*&---------------------------------------------------------------------*
*& Report Z_CHECK_DU
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_CHECK_DU2.

  DATA: lt_search_attribute       TYPE usmd_ts_value,
        lt_data                   TYPE usmd_t_search_result,
        lt_message                TYPE usmd_t_message,
        ls_duplicate              TYPE bss_duplicate,
        ls_bapiret2               TYPE bapiret2,
        lv_duplicate_exists       TYPE usmd_flg.

    data  ls_search_attribute      like LINE OF lt_search_attribute  .
   ls_search_attribute-fieldname = 'MATERIAL'.
    ls_search_attribute-value = 'TESTWANG'.
    APPEND  ls_search_attribute TO lt_search_attribute.
  CALL METHOD cl_mdg_duplicate_check=>duplicate_check
    EXPORTING
      iv_model            = 'MM'
      iv_entity           = 'MATERIAL'
      it_search_attribute = lt_search_attribute
      iv_update_mode      = 'S'    "Search only, no update
    IMPORTING
      ev_duplicate_exists = lv_duplicate_exists
      et_message          = lt_message
      et_data             = lt_data.
  BREAK-POINT
  .

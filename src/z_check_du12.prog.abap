*&---------------------------------------------------------------------*
*& Report Z_CHECK_DU1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_CHECK_DU12.
DATA:   lo_model     TYPE REF TO if_usmd_model_ext,
        ls_sel       TYPE usmd_s_sel,
        lt_sel       TYPE usmd_ts_sel,
        lt_objlist   TYPE usmd_t_crequest_entity,
        ls_objlist   TYPE usmd_s_crequest_entity,
        lv_structure TYPE REF TO data,
        " lt_data TYPE USMD_TS_DATA_ENTITY,
        lt_message TYPE USMD_T_MESSAGE.


FIELD-SYMBOLS : <lt_data> TYPE ANY TABLE,
                <lt_data_all> TYPE ANY TABLE.

CALL METHOD cl_usmd_model_ext=>get_instance
  EXPORTING
    i_usmd_model = 'BP'
  IMPORTING
    eo_instance  = lo_model.

CLEAR: ls_sel, lt_sel.
ls_sel-fieldname = 'BP_HEADER'.
ls_sel-option = 'EQ'.
ls_sel-sign = 'I'.
ls_sel-low = 'WANG123'.
INSERT ls_sel INTO TABLE lt_sel.
*

CALL METHOD lo_model->create_data_reference
  EXPORTING
   i_fieldname = 'BP_HEADER'
   i_struct    =  lo_model->GC_STRUCT_KEY_ATTR
  IMPORTING
   er_data     = lv_structure.

ASSIGN lv_structure->* TO <lt_data>.

CALL METHOD lo_model->read_char_value
  EXPORTING
   i_fieldname = 'BP_HEADER'
   it_sel      = lt_sel
   i_readmode  = '4'
  IMPORTING
   et_data     = <lt_data>.


*DATA lo_if_usmd_model TYPE REF TO if_usmd_model.
*DATA lo_cl_usmd_model TYPE REF TO cl_usmd_model.
*DATA lo_data TYPE REF TO data.
*DATA lv_model TYPE usmd_model VALUE 'BP'.
*DATA lv_pl_flg TYPE wdy_boolean.
*DATA lv_querymode TYPE usmd_querymode VALUE '1'.
*DATA ls_sel TYPE usmd_s_sel.
*DATA lt_messages TYPE usmd_t_message.
*DATA lt_sel TYPE usmd_ts_sel.
*FIELD-SYMBOLS: <fs_t_data> TYPE ANY TABLE,
* <fs_s_data> TYPE any,
* <fs_v_val> TYPE any.
*PARAMETERS: p_acc TYPE BU_PARTNER.
*CALL METHOD cl_usmd_model=>get_instance
* EXPORTING
* i_usmd_model = lv_model
* IMPORTING
* eo_instance = lo_if_usmd_model
* et_message = lt_messages.
*lo_cl_usmd_model ?= lo_if_usmd_model.
*CALL METHOD lo_cl_usmd_model->if_usmd_model_delta~create_data_reference
* EXPORTING
* i_fieldname = 'BP_HEADER'
* i_struct = 'KATTR'
* if_table = abap_true
* i_tabtype = 'S'
* RECEIVING
* er_data = lo_data.
*ASSIGN lo_data->* TO <fs_t_data>.
**BP
*ls_sel-fieldname = 'BP_HEADER'.
*ls_sel-low = p_acc.
*ls_sel-option = 'EQ'.
*ls_sel-sign = 'I'.
*APPEND ls_sel TO lt_sel.
*CALL METHOD lo_cl_usmd_model->if_usmd_model~query
* EXPORTING
* i_fieldname = 'BP_HEADER'
* it_sel = lt_sel
* i_querymode = lv_querymode
* if_use_edtn_slice = abap_false
* IMPORTING
* et_data = <fs_t_data>
* et_message = lt_messages.

BREAK-POINT.

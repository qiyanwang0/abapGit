*&---------------------------------------------------------------------*
*& Report Z_GET_USER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_get_user1.



CONSTANTS: lc_bor_bus2250 TYPE swo_objtyp VALUE 'BUS2250',
           lc_index       TYPE swc_index VALUE '000000',
           BEGIN OF lcs_witype,
             w TYPE sww_witype VALUE 'W', "Dialog task
             b TYPE sww_witype VALUE 'B', "Background tasks
             f TYPE sww_witype VALUE 'F', "Subworkflow
           END OF lcs_witype.
FIELD-SYMBOLS: <ls_items> TYPE swr_wihdr.
DATA: lt_items            TYPE TABLE OF swr_wihdr,
      lt_message_struct   TYPE usmd4_t_swr_mstruc,
      lt_simple_container TYPE usmd4_t_swr_cont,
      ls_wf_result        TYPE usmd4_s_alv_wf_result,
      ls_message_struct   TYPE swr_mstruc,
      ls_workitem_detail  TYPE swr_widtl,
      l_action            TYPE usmd_crequest_action,
      ls_simple_container TYPE swr_cont,
      l_objkey            TYPE swo_typeid,
      l_return_code       TYPE sysubrc.
DATA: lt_container LIKE TABLE OF swr_cont .
DATA: lt_worklist TYPE swr_wihdr OCCURS 0 WITH HEADER LINE. "PH9K008190
DATA: lt_message_lines            LIKE  TABLE OF swr_messag,
      lt_subcontainer_bor_objects LIKE  TABLE OF swr_cont,
      lt_subcontainer_all_objects LIKE  TABLE OF swr_cont.
* Build object key of BOR object
CONCATENATE '00000000' '3411' lc_index INTO l_objkey.
* Get list of workflow(s) to current change request
CALL METHOD cl_usmd4_crequest_protocol=>workitems_to_object
  EXPORTING
    i_objtype         = lc_bor_bus2250
    i_objkey          = l_objkey
    i_top_level_items = ''
  IMPORTING
    e_return_code     = l_return_code
  CHANGING
    t_worklist        = lt_items.

LOOP AT lt_items ASSIGNING <ls_items> WHERE wi_type = lcs_witype-w.
  CLEAR : ls_wf_result ,
  ls_workitem_detail.
  CALL METHOD cl_usmd4_crequest_protocol=>get_workitem_detail
    EXPORTING
      i_workitem_id     = <ls_items>-wi_id
    IMPORTING
      e_workitem_detail = ls_workitem_detail
      e_return_code     = l_return_code
    CHANGING
      t_message_struct  = lt_message_struct.
  check l_return_code eq 0.
  clear : l_return_code,
  lt_message_struct,
  lt_simple_container.
  CALL METHOD cl_usmd4_crequest_protocol=>read_container
    EXPORTING
      i_workitem_id      = <ls_items>-wi_id
    IMPORTING
      e_return_code      = l_return_code
    CHANGING
      t_simple_container = lt_simple_container.

  BREAK-POINT.
ENDLOOP.






*CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*  EXPORTING
*    username =  wa_user "lt_users-UNAME
*    cache_results = space
*  IMPORTING
*   address = ls_user_data
*   TABLES
*   return = lt_return.
*   ev_mail_address = ls_user_data-e_mail.

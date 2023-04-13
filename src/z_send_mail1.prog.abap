
REPORT Z_SEND_MAIL1.
*DATA ls_user_data type  bapiaddr3.
*DATA lt_return TYPE bapirettab.
*DATA ev_mail_address TYPE string.
*DATA rv_user type USMD_T_USER.
*DATA wa_user LIKE LINE OF rv_user.
*DATA lo_wf type ref to cl_usmd_wf_service.
*
*lo_wf = CL_USMD_WF_SERVICE=>GET_INSTANCE( ).
*
*CALL METHOD lo_wf->GET_CR_WF_PROCESSORS
* exporting
*   ID_CREQUEST = '3411'
* IMPORTING
*   et_uname = rv_user.
*
*loop at rv_user into wa_user.
* WRITE wa_user.
*endloop.
*BREAK-POINT.

cONSTANTS: lc_bor_bus2250 TYPE swo_objtyp VALUE 'BUS2250',
lc_index TYPE swc_index VALUE '000000',
BEGIN OF lcs_witype,
w TYPE sww_witype VALUE 'W', "Dialog task
b TYPE sww_witype VALUE 'B', "Background tasks
f TYPE sww_witype VALUE 'F', "Subworkflow
END OF lcs_witype.
FIELD-SYMBOLS: <ls_items> TYPE swr_wihdr.
DATA: lt_items TYPE TABLE of SWR_WIHDR,
lt_message_struct TYPE usmd4_t_swr_mstruc,
lt_simple_container TYPE usmd4_t_swr_cont,
ls_wf_result TYPE usmd4_s_alv_wf_result,
ls_message_struct TYPE swr_mstruc,
ls_workitem_detail TYPE swr_widtl,
l_action TYPE usmd_crequest_action,
ls_simple_container TYPE swr_cont,
l_objkey TYPE swo_typeid,
l_return_code TYPE sysubrc.
* Build object key of BOR object
CONCATENATE '3411' lc_index INTO l_objkey.
* Get list of workflow(s) to current change request
*CALL METHOD cl_usmd4_crequest_protocol=>workitems_to_object
*EXPORTING
*i_objtype = lc_bor_bus2250
*i_objkey = l_objkey
*i_top_level_items = ''
*IMPORTING
*e_return_code = l_return_code
*CHANGING
*t_worklist = lt_items.
*

DATA ls_user_data type  bapiaddr3.
DATA lt_return TYPE bapirettab.
DATA ev_mail_address TYPE string.
DATA rv_user type USMD_T_USER.
DATA wa_user LIKE LINE OF rv_user.
DATA lo_wf type ref to cl_usmd_wf_service.

lo_wf = CL_USMD_WF_SERVICE=>GET_INSTANCE( ).

CALL METHOD lo_wf->GET_CR_WF_PROCESSORS
 exporting
   ID_CREQUEST = '3723'
 IMPORTING
   et_uname = rv_user.

loop at rv_user into wa_user.
 WRITE wa_user.
endloop.



CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
 EXPORTING
*   OBJECT_POR                     =
  OBJTYPE                        = lc_bor_bus2250
   OBJKEY                         = '000000003411000000'
   TOP_LEVEL_ITEMS                = ''
   SELECTION_STATUS_VARIANT       = 0000
*   TIME                           =
*   TEXT                           = 'X'
*   OUTPUT_ONLY_TOP_LEVEL          = ' '
*   LANGUAGE                       = SY-LANGU
*   DETERMINE_TASK_FILTER          = 'X'
*   REMOVED_OBJECTS                = ' '
 IMPORTING
   RETURN_CODE                    = l_return_code
  TABLES
*   TASK_FILTER                    =
    worklist                       = lt_items.
*   MESSAGE_LINES                  =
*   MESSAGE_STRUCT                 =
          .



DATA: lt_container LIKE swr_cont OCCURS 0 WITH HEADER LINE.
DATA: lt_worklist TYPE swr_wihdr OCCURS 0 WITH HEADER LINE. "PH9K008190
DATA: lt_message_lines  LIKE  swr_messag OCCURS 0 WITH HEADER LINE,
*      lt_message_struct LIKE  swr_mstruc OCCURS 0 WITH HEADER LINE,
      lt_subcontainer_bor_objects LIKE  swr_cont OCCURS 0 WITH HEADER
LINE,
      lt_subcontainer_all_objects LIKE  swr_cont OCCURS 0 WITH HEADER
LINE.

lt_worklist-wi_id = '298126'.

CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
  EXPORTING
    workitem_id                    = lt_worklist-wi_id
*   LANGUAGE                       = SY-LANGU
*   USER                           = SY-UNAME
* IMPORTING
*   RETURN_CODE                    =
  TABLES
    simple_container               = lt_container
   message_lines                   = lt_message_lines
   message_struct                  = lt_message_struct
   subcontainer_bor_objects        = lt_subcontainer_bor_objects
   subcontainer_all_objects        = lt_subcontainer_all_objects.
BREAK-POINT.

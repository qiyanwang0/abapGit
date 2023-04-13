*&---------------------------------------------------------------------*
*& Report ZPOST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPOST1.
DATA: lw_ballog TYPE bal_s_log,
      lv_balloghndl TYPE balloghndl,
      lw_statistics TYPE bal_s_scnt,
      lw_message_handle TYPE balmsghndl,
      lw_message TYPE bal_s_msg,
      lv_messagetext TYPE string.

DATA:
  GT_LOGHANDLE TYPE BAL_T_LOGH,
  G_S_LOG      TYPE BAL_S_LOG,
  G_DUMMY      TYPE C,
  L_LOG_HANDLE TYPE BALLOGHNDL.
*  G_S_LOG-EXTNUMBER  = 'Z_SET_DAC_FLG_TO_FDO'.
  G_S_LOG-OBJECT     = 'ZWANG'.


CALL FUNCTION 'BAL_LOG_CREATE'
  EXPORTING
    i_s_log      = G_S_LOG
  IMPORTING
    e_log_handle = lv_balloghndl
  EXCEPTIONS
    OTHERS       = 0.
* Add some messages
CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
  EXPORTING
    i_log_handle = lv_balloghndl
    i_msgty      = 'E' "Error
    i_text       = 'This went wrong here !'.

CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
  EXPORTING
    i_log_handle = lv_balloghndl
    i_msgty      = 'E' "Error
    i_text       = 'This went wrong as well, over there'.



  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      I_SAVE_ALL = 'X'
*     I_T_LOG_HANDLE  = gt_loghandle
    EXCEPTIONS
      OTHERS     = 1.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
exceptions
others                 = 1.

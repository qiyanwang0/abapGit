*&---------------------------------------------------------------------*
*& Report Z_CREATE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_create1.




DATA :lo_mdg_conv TYPE REF TO if_usmd_conv_som_gov_api,
      lo_gov_api  TYPE REF TO if_usmd_gov_api,
      lo_s_entity TYPE REF TO data,
      lo_t_entity TYPE REF TO data.

DATA : lv_crequest     TYPE usmd_crequest,
       lv_temp_key     TYPE /mdgbp/_s_bp_ky_bp_header,
       lit_keyfields   TYPE usmd_t_fieldname ##NEEDED,
       lit_entity_keys TYPE usmd_gov_api_ts_ent_tabl,
       lw_entity_keys  TYPE usmd_gov_api_s_ent_tabl,
       lit_entity_data TYPE usmd_gov_api_ts_ent_data,
       lw_entity_data  TYPE usmd_gov_api_s_ent_data.

FIELD-SYMBOLS: <lw_0g_cctr>  TYPE any,
               <lit_0g_cctr> TYPE ANY TABLE,
               <lw_value>    TYPE any.
DATA lo_conv_error TYPE REF TO cx_usmd_conv_som_gov_api.
DATA lo_error TYPE REF TO cx_usmd_gov_api.
DATA:lo_gov_edition_api TYPE REF TO if_usmd_edition_api.

*
CALL METHOD cl_usmd_edition_api=>get_instance
  RECEIVING
    ro_edition_api = lo_gov_edition_api.


CONSTANTS: cs_edi    TYPE usmd_edition VALUE '2022.09w',
           cs_edtype TYPE usmd_edition_type VALUE '0G_ALL'.

*CALL METHOD LO_GOV_EDITION_API->CREATE_EDITION
*  EXPORTING
*    IV_EDITION             = CS_EDI
*    IV_EDITION_TYPE        = CS_EDTYPE
*    IV_DATE_FROM           = SY-DATUM
*    IV_DISTRIBUTION_PER_CR = 'X'
*    IV_DESCRIPTION         = 'Test Edition'
*    IV_TEST                = ''
*  IMPORTING
*    ET_INFO_MESSAGE        = DATA(ET_MESSAGE).
*COMMIT WORK AND WAIT.

*CALL METHOD LO_GOV_EDITION_API->ENQUEUE_EDITION
*  EXPORTING
*    IV_EDITION = CS_EDI.

lo_mdg_conv = cl_usmd_conv_som_gov_api=>get_instance(
 iv_model_name = 'BP' ).


lo_mdg_conv->set_environment(
EXPORTING
 iv_crequest_type   = 'BP1P1'
 iv_create_crequest = abap_true
).
*LO_MDG_CONV->set_crequest_attributes(
*        EXPORTING
*          iv_crequest_text   = |CR { sy-datum }|
*      ).

lv_crequest = lo_mdg_conv->get_crequest_id( ).


CALL METHOD lo_mdg_conv->get_entity_key_fields(
  EXPORTING
    iv_entity_name = 'BP_HEADER'
    iv_struct      = lo_mdg_conv->gc_struct_key
  RECEIVING
    rt_key_fields  = lit_keyfields ).

CALL METHOD lo_mdg_conv->get_entity_structure(
  EXPORTING
    iv_entity_name = 'BP_HEADER'
    iv_struct_type = lo_mdg_conv->gc_struct_key_attr
    iv_edition     = abap_false
  IMPORTING
    er_structure   = lo_s_entity
    er_table       = lo_t_entity ).

ASSIGN lo_s_entity->* TO <lw_0g_cctr>.
ASSIGN lo_t_entity->* TO <lit_0g_cctr>.

ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = 'A1402'.
ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = cl_system_uuid=>create_uuid_x16_static( ).
ASSIGN COMPONENT 'bu_type' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = '2'.
ASSIGN COMPONENT 'txtlg' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = 'testfordu'.
INSERT <lw_0g_cctr> INTO TABLE <lit_0g_cctr>.
CLEAR: lw_entity_keys, lit_entity_keys.
lw_entity_keys-entity = 'BP_HEADER'.
lw_entity_keys-tabl   = lo_t_entity.
INSERT lw_entity_keys INTO TABLE lit_entity_keys.


CLEAR: lw_entity_data, lit_entity_data.
REFRESH:lit_entity_data.
lw_entity_data-entity = 'BP_HEADER'.
lw_entity_data-entity_data = lo_t_entity.
INSERT lw_entity_data INTO TABLE lit_entity_data.





TRY .

CALL METHOD lo_mdg_conv->enqueue_entity
  EXPORTING
    it_entity_keys = lit_entity_keys
    iv_lock_mode   = 'E'.
  CATCH cx_usmd_conv_som_gov_api INTO lo_conv_error.
  CATCH cx_usmd_gov_api INTO lo_error.
ENDTRY.

CALL METHOD lo_mdg_conv->get_entity_key_fields(
  EXPORTING
    iv_entity_name = 'BP_CENTRL'
    iv_struct      = lo_mdg_conv->gc_struct_key
  RECEIVING
    rt_key_fields  = lit_keyfields ).

CALL METHOD lo_mdg_conv->get_entity_structure(
  EXPORTING
    iv_entity_name = 'BP_CENTRL'
    iv_struct_type = lo_mdg_conv->gc_struct_key_attr
    iv_edition     = abap_false
  IMPORTING
    er_structure   = lo_s_entity
    er_table       = lo_t_entity ).

ASSIGN lo_s_entity->* TO <lw_0g_cctr>.
ASSIGN lo_t_entity->* TO <lit_0g_cctr>.

ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = 'A1402'.
ASSIGN COMPONENT 'name_org1' OF STRUCTURE <lw_0g_cctr> TO <lw_value>.
<lw_value> = 'test'.
INSERT <lw_0g_cctr> INTO TABLE <lit_0g_cctr>.
CLEAR: lw_entity_keys, lit_entity_keys.
lw_entity_keys-entity = 'BP_CENTRL'.
lw_entity_keys-tabl   = lo_t_entity.
INSERT lw_entity_keys INTO TABLE lit_entity_keys.
*CALL METHOD LO_MDG_CONV->ENQUEUE_ENTITY
*  EXPORTING
*    IT_ENTITY_KEYS = LIT_ENTITY_KEYS
*    IV_LOCK_MODE   = 'E'.

CLEAR: lw_entity_data, lit_entity_data.
REFRESH:lit_entity_data.
lw_entity_data-entity = 'BP_CENTRL'.
lw_entity_data-entity_data = lo_t_entity.
INSERT lw_entity_data INTO TABLE lit_entity_data.




TRY .
    CALL METHOD lo_mdg_conv->write_entity_data
      EXPORTING
        it_entity_data = lit_entity_data.
  CATCH cx_usmd_conv_som_gov_api INTO lo_conv_error.
  CATCH cx_usmd_gov_api INTO lo_error.
ENDTRY.





DATA: lw_crequest TYPE usmd_s_crequest.
CLEAR : lw_crequest.
lw_crequest-usmd_crequest = lv_crequest.
lw_crequest-usmd_creq_type = 'BP1P1'.
lw_crequest-usmd_creq_text = 'DEMO'."TEXT-006.
lw_crequest-usmd_created_by = sy-uname.
lw_crequest-usmd_created_at = sy-datum.

CALL METHOD lo_mdg_conv->write_crequest
  EXPORTING
    is_crequest_data = lw_crequest.
lo_mdg_conv->confirm_entity_data( ).

CALL METHOD lo_mdg_conv->if_usmd_conv_som_gov_cr~set_action
  EXPORTING
    iv_crequest_action = cl_usmd_crequest_action=>gc_draft_action-submit.
lo_mdg_conv->check( ).
    IF lo_mdg_conv->mv_successfully_checked EQ abap_true.
      BREAK-POINT.
      ENDIF.
lo_mdg_conv->save( ).

COMMIT WORK AND WAIT.
lo_mdg_conv->dequeue_entity_all( ).
lo_mdg_conv->dequeue_crequest( ).
lo_mdg_conv->refresh_buffers( ).



*
*
*FREE LO_MDG_CONV.
*
*CLEAR: LW_CREQUEST .
*clear :LO_MDG_CONV   ,
*      LO_GOV_API    ,
*      LO_S_ENTITY   ,
*      LO_T_ENTITY,
*       LV_TEMP_KEY  ,
*       LIT_KEYFIELDS ,
*       LIT_ENTITY_KEYS ,
*       LW_ENTITY_KEYS ,
*       LIT_ENTITY_DATA,
*       LW_ENTITY_DATA.
*
*CLEAR: <LW_0G_CCTR>  ,
*               <LIT_0G_CCTR> ,
*               <LW_VALUE>   .
*
*
*
*
*CALL METHOD CL_USMD_EDITION_API=>GET_INSTANCE
*  RECEIVING
*    RO_EDITION_API = LO_GOV_EDITION_API.
*
*
*CALL METHOD LO_GOV_EDITION_API->ENQUEUE_EDITION
*  EXPORTING
*    IV_EDITION = CS_EDI.
*
*LO_MDG_CONV = CL_USMD_CONV_SOM_GOV_API=>GET_INSTANCE(
* IV_MODEL_NAME = '0G' ).
*
*LO_MDG_CONV->SET_ENVIRONMENT(
*EXPORTING
* IV_CREQUEST_TYPE   = 'ZCCH1P1'
* IV_CREQUEST_ID = LV_CREQUEST
* IV_CREATE_CREQUEST = ABAP_FALSE
* IV_EDITION         = CS_EDI ).
*
*
*LO_MDG_CONV->CREATE_ENTITY_TMP_KEY(
*EXPORTING
*  IV_LOCK   = ABAP_FALSE
*  IV_ENTITY = 'CCTRG'
*IMPORTING
*  ES_KEY    = LV_TEMP_KEY ).
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_KEY_FIELDS(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT      = LO_MDG_CONV->GC_STRUCT_KEY
*  RECEIVING
*    RT_KEY_FIELDS  = LIT_KEYFIELDS ).
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_STRUCTURE(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT_TYPE = LO_MDG_CONV->GC_STRUCT_KEY_ATTR
*    IV_EDITION     = ABAP_FALSE
*  IMPORTING
*    ER_STRUCTURE   = LO_S_ENTITY
*    ER_TABLE       = LO_T_ENTITY ).
*
*ASSIGN LO_S_ENTITY->* TO <LW_0G_CCTR>.
*ASSIGN LO_T_ENTITY->* TO <LIT_0G_CCTR>.
*
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'ZMDG1'.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'UA01'.
*
*INSERT <LW_0G_CCTR> INTO TABLE <LIT_0G_CCTR>.
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'ZMDG1-1'.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'UA01'.
*
*INSERT <LW_0G_CCTR> INTO TABLE <LIT_0G_CCTR>.
*CLEAR: LW_ENTITY_KEYS, LIT_ENTITY_KEYS.
*LW_ENTITY_KEYS-ENTITY = 'CCTRG'.
*LW_ENTITY_KEYS-TABL   = LO_T_ENTITY.
*INSERT LW_ENTITY_KEYS INTO TABLE LIT_ENTITY_KEYS.
*CALL METHOD LO_MDG_CONV->ENQUEUE_ENTITY
*  EXPORTING
*    IT_ENTITY_KEYS = LIT_ENTITY_KEYS
*    IV_LOCK_MODE   = 'E'.
*
*CLEAR: LW_ENTITY_DATA, LIT_ENTITY_DATA.
*REFRESH:LIT_ENTITY_DATA.
*LW_ENTITY_DATA-ENTITY = 'CCTRG'.
*LW_ENTITY_DATA-ENTITY_DATA = LO_T_ENTITY.
*INSERT LW_ENTITY_DATA INTO TABLE LIT_ENTITY_DATA.
*DATA(V) = LO_MDG_CONV->MV_CREQUEST_ID .
*
*CALL METHOD LO_MDG_CONV->ENQUEUE_CREQUEST
*  EXPORTING
*    IV_LOCK_MODE = 'E'.
*CALL METHOD LO_MDG_CONV->WRITE_ENTITY_DATA
*  EXPORTING
*    IT_ENTITY_DATA = LIT_ENTITY_DATA.
*
*CLEAR : LW_CREQUEST.
*LW_CREQUEST-USMD_CREQUEST = LV_CREQUEST.
*LW_CREQUEST-USMD_CREQ_TYPE = 'ZCCH1P1'.
*LW_CREQUEST-USMD_EDITION   = CS_EDI.
*LW_CREQUEST-USMD_CREQ_TEXT = 'DEMO'."TEXT-006.
*LW_CREQUEST-USMD_CREQ_STATUS = '00'.
*LW_CREQUEST-USMD_CREATED_BY = SY-UNAME.
*LW_CREQUEST-USMD_CREATED_AT = SY-DATUM.
*
*CALL METHOD LO_MDG_CONV->WRITE_CREQUEST
*  EXPORTING
*    IS_CREQUEST_DATA = LW_CREQUEST.
*
*
*CALL METHOD LO_MDG_CONV->IF_USMD_CONV_SOM_GOV_CR~SET_ACTION
*  EXPORTING
*    IV_CREQUEST_ACTION = CL_USMD_CREQUEST_ACTION=>GC_DRAFT_ACTION-SUBMIT.
*LO_MDG_CONV->SAVE( ).
*
*COMMIT WORK AND WAIT.
*LO_MDG_CONV->DEQUEUE_ENTITY_ALL( ).
*LO_MDG_CONV->DEQUEUE_CREQUEST( ).
*LO_MDG_CONV->REFRESH_BUFFERS( ).
*LO_GOV_EDITION_API->DEQUEUE_EDITION(   EXPORTING
*    IV_EDITION = CS_EDI ).
**CLEARING THE INSATANCE
*     FREE LO_MDG_CONV.
*
*
*clear :LO_MDG_CONV   ,
*      LO_GOV_API    ,
*      LO_S_ENTITY   ,
*      LO_T_ENTITY,
*       LV_TEMP_KEY  ,
*       LIT_KEYFIELDS ,
*       LIT_ENTITY_KEYS ,
*       LW_ENTITY_KEYS ,
*       LIT_ENTITY_DATA,
*       LW_ENTITY_DATA.
*
*CLEAR: <LW_0G_CCTR>  ,
*               <LIT_0G_CCTR> ,
*               <LW_VALUE>   .
*
*
*CALL METHOD CL_USMD_EDITION_API=>GET_INSTANCE
*  RECEIVING
*    RO_EDITION_API = LO_GOV_EDITION_API.
*
*CALL METHOD LO_GOV_EDITION_API->ENQUEUE_EDITION
*  EXPORTING
*    IV_EDITION = CS_EDI.
*
*LO_MDG_CONV = CL_USMD_CONV_SOM_GOV_API=>GET_INSTANCE(
* IV_MODEL_NAME = '0G' ).
*
*LO_MDG_CONV->SET_ENVIRONMENT(
*EXPORTING
* IV_CREQUEST_TYPE   = 'ZCCH1P1'
* IV_CREQUEST_ID = LV_CREQUEST
* IV_CREATE_CREQUEST = ABAP_FALSE
* IV_EDITION         = CS_EDI ).
*
*
*LO_MDG_CONV->CREATE_ENTITY_TMP_KEY(
*EXPORTING
*  IV_LOCK   = ABAP_FALSE
*  IV_ENTITY = 'CCTRG'
*IMPORTING
*  ES_KEY    = LV_TEMP_KEY ).
*
*
**
*DATA : LV_KEY       TYPE USMD_S_ASSIGNMENT,
*       LV_HIER_KEY  TYPE USMD_S_HRY_NAME_RKEY,
*       LV_PAREN_KEY TYPE USMD_S_HRY_PNODE_RKEY,
*       LV_NODE_KEY  TYPE USMD_S_HRY_NODE_RKEY.
**
*
*
*TYPES: BEGIN OF WWH,
*  COAREA TYPE C LENGTH 4 ,
*         CCTRH TYPE C LENGTH 15,
*       END OF WWH.
*FIELD-SYMBOLS: <STRP> TYPE ANY.
*FIELD-SYMBOLS: <DATAP> TYPE ANY.
*CREATE DATA LV_PAREN_KEY-KEY TYPE WWH.
*
*ASSIGN LV_PAREN_KEY-KEY->* TO <STRP>.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <strp> TO <datap>.
*<datap> = 'UA01'.
*ASSIGN COMPONENT 'CCTRH' OF STRUCTURE <STRP> TO <DATAP>.
*<DATAP> = 'ZMDG'.
*
*LV_PAREN_KEY-ENTITY = 'CCTRH'.
*LV_KEY-PARENT = LV_PAREN_KEY.
*
*********************************************************
*
**
*TYPES: BEGIN OF WWG,
*         COAREA TYPE C LENGTH 4,
*         CCTRG  TYPE C LENGTH 15,
*       END OF WWG.
*FIELD-SYMBOLS: <STR> TYPE ANY.
*FIELD-SYMBOLS: <DATA> TYPE ANY.
*CREATE DATA LV_NODE_KEY-KEY TYPE WWG.
*
*ASSIGN LV_NODE_KEY-KEY->* TO <STR>.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <STR> TO <DATA>.
*<DATA> = 'UA01'.
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <STR> TO <DATA>.
*<DATA> = 'ZMDG1'.
*LV_NODE_KEY-ENTITY = 'CCTRG'.
*LV_KEY-NODE = LV_NODE_KEY.
*
*DATA : LV_DATA TYPE USMD_S_ASSIGNMENT_KEY.
*MOVE-CORRESPONDING LV_KEY TO LV_DATA.
*CALL METHOD LO_MDG_CONV->ENQUEUE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY = 'CCTRG'
*    IS_ASSIGNMENT_KEY = LV_DATA
*    IV_LOCK_MODE      = 'E' ).
*
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_KEY_FIELDS(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT      = LO_MDG_CONV->GC_STRUCT_KEY
*  RECEIVING
*    RT_KEY_FIELDS  = LIT_KEYFIELDS ).
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_STRUCTURE(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT_TYPE = LO_MDG_CONV->GC_STRUCT_KEY_ATTR
*    IV_EDITION     = ABAP_FALSE
*  IMPORTING
*    ER_STRUCTURE   = LO_S_ENTITY
*    ER_TABLE       = LO_T_ENTITY ).
*
*ASSIGN LO_S_ENTITY->* TO <LW_0G_CCTR>.
*ASSIGN LO_T_ENTITY->* TO <LIT_0G_CCTR>.
*
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
****          <lw_value> = <lw_file>-kostl.
*<LW_VALUE> = 'ZMDG1'.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'UA01'.
*
*INSERT <LW_0G_CCTR> INTO TABLE <LIT_0G_CCTR>.
*
*CLEAR: LW_ENTITY_KEYS, LIT_ENTITY_KEYS.
*LW_ENTITY_KEYS-ENTITY = 'CCTRG'.
*LW_ENTITY_KEYS-TABL   = LO_T_ENTITY.
*INSERT LW_ENTITY_KEYS INTO TABLE LIT_ENTITY_KEYS.
*
*
*CALL METHOD LO_MDG_CONV->ENQUEUE_ENTITY
*  EXPORTING
*    IT_ENTITY_KEYS = LIT_ENTITY_KEYS
*    IV_LOCK_MODE   = 'E'.
*
**
*CALL METHOD lo_mdg_conv->enqueue_crequest
*            EXPORTING
*              iv_lock_mode   = 'E'.
*
*
*CALL METHOD LO_MDG_CONV->ENQUEUE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY = 'CCTRG'
*    IS_ASSIGNMENT_KEY = LV_DATA
*    IV_LOCK_MODE      = 'E' ).
*
*
*
*CALL METHOD LO_MDG_CONV->WRITE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY  = 'CCTRG'
*    IS_ASSIGNMENT_DATA = LV_KEY ).
*
*
*
*CLEAR : LW_CREQUEST.
*LW_CREQUEST-USMD_CREQUEST = LV_CREQUEST.
*LW_CREQUEST-USMD_CREQ_TYPE = 'ZCCH1P1'.
*LW_CREQUEST-USMD_EDITION   = CS_EDI.
*LW_CREQUEST-USMD_CREQ_TEXT = 'DEMO'."TEXT-006.
*LW_CREQUEST-USMD_CREQ_STATUS = '00'.
*LW_CREQUEST-USMD_CREATED_BY = SY-UNAME.
*LW_CREQUEST-USMD_CREATED_AT = SY-DATUM.
*
*
*CALL METHOD LO_MDG_CONV->WRITE_CREQUEST
*  EXPORTING
*    IS_CREQUEST_DATA = LW_CREQUEST.
*
*
*CALL METHOD LO_MDG_CONV->IF_USMD_CONV_SOM_GOV_CR~SET_ACTION
*  EXPORTING
*    IV_CREQUEST_ACTION = CL_USMD_CREQUEST_ACTION=>GC_DRAFT_ACTION-SUBMIT.
*LO_MDG_CONV->SAVE( ).
*
*COMMIT WORK AND WAIT.
*LO_MDG_CONV->DEQUEUE_ENTITY_ALL( ).
*LO_MDG_CONV->DEQUEUE_CREQUEST( ).
*LO_MDG_CONV->REFRESH_BUFFERS( ).
*LO_GOV_EDITION_API->DEQUEUE_EDITION(   EXPORTING
*    IV_EDITION = CS_EDI ).
**CALL METHOD LO_MDG_CONV->deQUEUE_ASSIGNMENT(
**  EXPORTING
**    IV_LEADING_ENTITY = 'CCTRG'
**    IS_ASSIGNMENT_KEY = LV_DATA
**).
*
** Clearing the insatance
*FREE LO_MDG_CONV.
*
*
*
*clear :LO_MDG_CONV   ,
*      LO_GOV_API    ,
*      LO_S_ENTITY   ,
*      LO_T_ENTITY,
*       LV_TEMP_KEY  ,
*       LIT_KEYFIELDS ,
*       LIT_ENTITY_KEYS ,
*       LW_ENTITY_KEYS ,
*       LIT_ENTITY_DATA,
*       LW_ENTITY_DATA.
*
*CLEAR: <LW_0G_CCTR>  ,
*               <LIT_0G_CCTR> ,
*               <LW_VALUE>   .
*
*CALL METHOD CL_USMD_EDITION_API=>GET_INSTANCE
*  RECEIVING
*    RO_EDITION_API = LO_GOV_EDITION_API.
*
*
*CALL METHOD LO_GOV_EDITION_API->ENQUEUE_EDITION
*  EXPORTING
*    IV_EDITION = CS_EDI.
*
*LO_MDG_CONV = CL_USMD_CONV_SOM_GOV_API=>GET_INSTANCE(
* IV_MODEL_NAME = '0G' ).
*
*LO_MDG_CONV->SET_ENVIRONMENT(
*EXPORTING
* IV_CREQUEST_TYPE   = 'ZCCH1P1'
* IV_CREQUEST_ID = LV_CREQUEST
* IV_CREATE_CREQUEST = ABAP_FALSE
* IV_EDITION         = CS_EDI ).
*
*
*LO_MDG_CONV->CREATE_ENTITY_TMP_KEY(
*EXPORTING
*  IV_LOCK   = ABAP_FALSE
*  IV_ENTITY = 'CCTRG'
*IMPORTING
*  ES_KEY    = LV_TEMP_KEY ).
*
*
**
*CLEAR : LV_KEY ,
*       LV_HIER_KEY,
*       LV_PAREN_KEY ,
*       LV_NODE_KEY.
**
*TYPES: BEGIN OF WWP,
*         COAREA TYPE C LENGTH 4,
*         CCTRG  TYPE C LENGTH 15,
*       END OF WWP.
*CLEAR : <STRP> ,
* <DATAP> .
*CREATE DATA LV_PAREN_KEY-KEY TYPE WWP.
*
*ASSIGN LV_PAREN_KEY-KEY->* TO <STRP>.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <STRP> TO <DATAP>.
*<DATAP> = 'UA01'.
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <STRP> TO <DATAP>.
*<DATAP> = 'ZMDG1'.
*
*LV_PAREN_KEY-ENTITY = 'CCTRG'.
*LV_KEY-PARENT = LV_PAREN_KEY.
*
*********************************************************
*
**
*TYPES: BEGIN OF WW,
*         COAREA TYPE C LENGTH 4,
*         CCTRG  TYPE C LENGTH 15,
*       END OF WW.
*clear : <STR> ,
*<DATA> .
*CREATE DATA LV_NODE_KEY-KEY TYPE WW.
*
*ASSIGN LV_NODE_KEY-KEY->* TO <STR>.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <STR> TO <DATA>.
*<DATA> = 'UA01'.
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <STR> TO <DATA>.
*<DATA> = 'ZMDG1-1'.
*LV_NODE_KEY-ENTITY = 'CCTRG'.
*LV_KEY-NODE = LV_NODE_KEY.
*
*CLEAR LV_DATA .
*MOVE-CORRESPONDING LV_KEY TO LV_DATA.
*CALL METHOD LO_MDG_CONV->ENQUEUE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY = 'CCTRG'
*    IS_ASSIGNMENT_KEY = LV_DATA
*    IV_LOCK_MODE      = 'E' ).
*
*
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_KEY_FIELDS(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT      = LO_MDG_CONV->GC_STRUCT_KEY
*  RECEIVING
*    RT_KEY_FIELDS  = LIT_KEYFIELDS ).
*
*CALL METHOD LO_MDG_CONV->GET_ENTITY_STRUCTURE(
*  EXPORTING
*    IV_ENTITY_NAME = 'CCTRG'
*    IV_STRUCT_TYPE = LO_MDG_CONV->GC_STRUCT_KEY_ATTR
*    IV_EDITION     = ABAP_FALSE
*  IMPORTING
*    ER_STRUCTURE   = LO_S_ENTITY
*    ER_TABLE       = LO_T_ENTITY ).
*
*ASSIGN LO_S_ENTITY->* TO <LW_0G_CCTR>.
*ASSIGN LO_T_ENTITY->* TO <LIT_0G_CCTR>.
*
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'ZMDG1'.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'UA01'.
*
*INSERT <LW_0G_CCTR> INTO TABLE <LIT_0G_CCTR>.
*ASSIGN COMPONENT 'CCTRG' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'ZMDG1-1'.
*ASSIGN COMPONENT 'COAREA' OF STRUCTURE <LW_0G_CCTR> TO <LW_VALUE>.
*<LW_VALUE> = 'UA01'.
*INSERT <LW_0G_CCTR> INTO TABLE <LIT_0G_CCTR>.
*
*
*
*CLEAR: LW_ENTITY_KEYS, LIT_ENTITY_KEYS.
*LW_ENTITY_KEYS-ENTITY = 'CCTRG'.
*LW_ENTITY_KEYS-TABL   = LO_T_ENTITY.
*INSERT LW_ENTITY_KEYS INTO TABLE LIT_ENTITY_KEYS.
*
*
*CALL METHOD LO_MDG_CONV->ENQUEUE_ENTITY
*  EXPORTING
*    IT_ENTITY_KEYS = LIT_ENTITY_KEYS
*    IV_LOCK_MODE   = 'E'.
*
**
*CALL METHOD LO_MDG_CONV->ENQUEUE_CREQUEST
*  EXPORTING
*    IV_LOCK_MODE = 'E'.
*
*
*CALL METHOD LO_MDG_CONV->ENQUEUE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY = 'CCTRG'
*    IS_ASSIGNMENT_KEY = LV_DATA
*    IV_LOCK_MODE      = 'E' ).
*
*
*CALL METHOD LO_MDG_CONV->WRITE_ASSIGNMENT(
*  EXPORTING
*    IV_LEADING_ENTITY  = 'CCTRG'
*    IS_ASSIGNMENT_DATA = LV_KEY ).
*
*
*
*CLEAR : LW_CREQUEST.
*LW_CREQUEST-USMD_CREQUEST = LV_CREQUEST.
*LW_CREQUEST-USMD_CREQ_TYPE = 'ZCCH1P1'.
*LW_CREQUEST-USMD_EDITION   = CS_EDI.
*LW_CREQUEST-USMD_CREQ_TEXT = 'DEMO'."TEXT-006.
*LW_CREQUEST-USMD_CREQ_STATUS = '00'.
*LW_CREQUEST-USMD_CREATED_BY = SY-UNAME.
*LW_CREQUEST-USMD_CREATED_AT = SY-DATUM.
*
*
*CALL METHOD LO_MDG_CONV->WRITE_CREQUEST
*  EXPORTING
*    IS_CREQUEST_DATA = LW_CREQUEST.
*
*
*CALL METHOD LO_MDG_CONV->IF_USMD_CONV_SOM_GOV_CR~SET_ACTION
*  EXPORTING
*    IV_CREQUEST_ACTION = CL_USMD_CREQUEST_ACTION=>GC_DRAFT_ACTION-SUBMIT.
*LO_MDG_CONV->SAVE( ).
*
*COMMIT WORK AND WAIT.
*LO_MDG_CONV->DEQUEUE_ENTITY_ALL( ).
*LO_MDG_CONV->DEQUEUE_CREQUEST( ).
*LO_MDG_CONV->REFRESH_BUFFERS( ).
** Clearing the insatance
*FREE LO_MDG_CONV.

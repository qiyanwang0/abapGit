*&---------------------------------------------------------------------*
*& Report Z_GOV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_gov1.
*DATA :
*      lr_crequest                 TYPE REF TO if_usmd_crequest_api,
*      lr_gov_api                  TYPE REF TO if_usmd_gov_api,
*      t_crequest_obj_list         TYPE   usmd_gov_api_t_ent_struc,
*      lr_adpostal_detail_key_str  TYPE REF TO data,
*      lr_adpostal_detail_key_tab  TYPE REF TO data,
*      lr_adpostal_detail_data_str TYPE REF TO data,
*      lr_adpostal_detail_data_tab TYPE REF TO data,
*      w_crequest_obj_list         TYPE   usmd_gov_api_s_ent_struc,
*      ls_crequest                 TYPE usmd_s_crequest.
*
*    FIELD-SYMBOLS :
*      <fs_t_adpostal_detail_key>  TYPE ANY TABLE,
*      <fs_w_adpostal_detail_key>  TYPE any,
*      <fs_t_adpostal_detail_data> TYPE INDEX TABLE,
*      <fs_w_adpostal_detail_data> TYPE any.
*
*DATA : lv_crequest     TYPE usmd_crequest,
*       lv_temp_key     TYPE /mdgbp/_s_bp_ky_bp_header,
*       lit_keyfields   TYPE usmd_t_fieldname ##NEEDED,
*       lit_entity_keys TYPE usmd_gov_api_ts_ent_tabl,
*       lw_entity_keys  TYPE usmd_gov_api_s_ent_tabl,
*       lit_entity_data TYPE usmd_gov_api_ts_ent_data,
*       lw_entity_data  TYPE usmd_gov_api_s_ent_data.
*    "Get API Instance:
**    CALL METHOD cl_usmd_crequest_api=>get_instance
**      EXPORTING
**        iv_crequest          = 'BP'
**      IMPORTING
**        re_inst_crequest_api = lr_crequest.
**
**    "CR Request Details:
**    CALL METHOD lr_crequest->read_crequest
**      IMPORTING
**        es_crequest = ls_crequest.
**
**    ex_crequest = ls_crequest.
*
*    "Get Instance of Model:
*    lr_gov_api = cl_usmd_gov_api=>get_instance( iv_model_name = 'BP' ).
**DATA : lv_crequest     TYPE usmd_crequest.
*lv_crequest  = lr_gov_api->create_crequest( iv_crequest_type = 'BP1P1'
*iv_description = 'test' ).
*
*    "Check with Entity Type:
*
*
*    "Get KeyStr and KeyTab:
*    lr_gov_api->create_data_reference( EXPORTING iv_entity_name = 'BP_HEADER'
*                                                 iv_struct      = lr_gov_api->gc_struct_key
*                                       IMPORTING er_structure   = lr_adpostal_detail_key_str
*                                                 er_table       = lr_adpostal_detail_key_tab ).
*    "Get AdPostal data:
*    lr_gov_api->create_data_reference( EXPORTING iv_entity_name = 'BP_HEADER'
*                                       iv_struct      = lr_gov_api->gc_struct_key_attr
*                             IMPORTING er_structure   = lr_adpostal_detail_data_str
*                                       er_table       = lr_adpostal_detail_data_tab ).
*
**    ASSIGN lr_adpostal_detail_key_str->*  TO <fs_w_adpostal_detail_key>.
**    ASSIGN lr_adpostal_detail_key_tab->*  TO <fs_t_adpostal_detail_key>.
**    ASSIGN w_crequest_obj_list-r_struc->* TO <fs_w_adpostal_detail_key>.
**    INSERT <fs_w_adpostal_detail_key> INTO TABLE <fs_t_adpostal_detail_key>.
*    ASSIGN lr_adpostal_detail_data_str->*  TO <fs_w_adpostal_detail_data>.
*    ASSIGN lr_adpostal_detail_data_tab->* TO <fs_t_adpostal_detail_data>.
*FIELD-SYMBOLS: <lw_0g_cctr>  TYPE any,
*               <lit_0g_cctr> TYPE ANY TABLE,
*               <lw_value>    TYPE any.
*ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <fs_w_adpostal_detail_data> TO <lw_value>.
*<lw_value> = 'A1259'.
*ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <fs_w_adpostal_detail_data> TO <lw_value>.
*<lw_value> = cl_system_uuid=>create_uuid_x16_static( ).
*
*
*INSERT <fs_w_adpostal_detail_data>  INTO TABLE <fs_t_adpostal_detail_data>.
*CLEAR: lw_entity_keys, lit_entity_keys.
*lw_entity_keys-entity = 'BP_HEADER'.
*lw_entity_keys-tabl   = lr_adpostal_detail_data_tab.
*INSERT lw_entity_keys INTO TABLE lit_entity_keys.
*
*CLEAR: lw_entity_data, lit_entity_data.
*REFRESH:lit_entity_data.
*lw_entity_data-entity = 'BP_HEADER'.
*lw_entity_data-entity_data = lr_adpostal_detail_data_tab.
*INSERT lw_entity_data INTO TABLE lit_entity_data.
*
*CALL METHOD   lr_gov_api->enqueue_entity
*  EXPORTING
*   IT_DATA = lit_entity_keys
*    iv_lock_mode   = 'E'.


DATA:
  lo_gov_api       TYPE REF TO if_usmd_gov_api,
  lv_crequest_id   TYPE usmd_crequest, "Change Request ID
  lr_carr_key_str  TYPE REF TO data, "Entity Carrier - key structure
  lr_carr_key_tab  TYPE REF TO data, "Entity Carrier - key table
  lr_carr_data_str TYPE REF TO data, "Entity Carrier - data structure
  lr_carr_data_tab TYPE REF TO data, "Entity Carrier - data table
  ls_entity        TYPE usmd_gov_api_s_ent_tabl,
  lt_entity        TYPE usmd_gov_api_ts_ent_tabl,
  lt_messages      TYPE usmd_t_message.
FIELD-SYMBOLS:
  <ls_carr_key>  TYPE any,
  <lt_carr_key>  TYPE ANY TABLE,
  <ls_carr_data> TYPE any,
  <lt_carr_data> TYPE ANY TABLE,
  <value>        TYPE any.
DATA lo_conv_error TYPE REF TO cx_usmd_conv_som_gov_api.
DATA lo_error TYPE REF TO cx_usmd_gov_api.
"1st: Create an instance of the governance API
TRY.
    lo_gov_api = cl_usmd_gov_api=>get_instance( iv_model_name = 'BP' ).
  CATCH cx_usmd_gov_api.
    EXIT.
ENDTRY.
"2nd: Create all the data references needed to maintain the carrier entity
"Create a data reference of the key structure / table of entity CARR (Carrier)
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_HEADER'
iv_struct = lo_gov_api->gc_struct_key
IMPORTING er_structure = lr_carr_key_str
er_table = lr_carr_key_tab ).
"Create a data reference of the key and attribute structure / table of
"entity CARR (Carrier)
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_HEADER'
iv_struct = lo_gov_api->gc_struct_key_attr
IMPORTING er_structure = lr_carr_data_str
er_table = lr_carr_data_tab ).
"Assign the created data references for carrier key and carrier data
"to field symbols
ASSIGN lr_carr_key_str->* TO <ls_carr_key>.
ASSIGN lr_carr_key_tab->* TO <lt_carr_key>.
ASSIGN lr_carr_data_str->* TO <ls_carr_data>.
ASSIGN lr_carr_data_tab->* TO <lt_carr_data>.
"3rd: Fill the key and data structure with values to create a new carrier
"The entity CARR only has key field CARR. The new carrier ID should be 'YZ'
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_carr_key> TO <value>.
IF sy-subrc = 0.
  <value> = 'A1428'.
  insert <ls_carr_key> into table <lt_carr_key>.
ELSE.
  EXIT.
  "Tough luck – unfortunately, this field name is not part of the key structure
ENDIF.
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_carr_key> TO <value>.
IF sy-subrc = 0.
  <value> = 'A1429'.
  insert <ls_carr_key> into table <lt_carr_key>.
ELSE.
  EXIT.
  "Tough luck – unfortunately, this field name is not part of the key structure
ENDIF.
*ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <ls_carr_key> TO <value>.
*IF sy-subrc = 0.
*  <value> = cl_system_uuid=>create_uuid_x16_static( ).
*
*ELSE.
*  EXIT.
*  "Tough luck – unfortunately, this field name is not part of the key structure
*ENDIF.

"4th: Create a new change request using change request type and a
"description (required)
TRY.
    lv_crequest_id = lo_gov_api->create_crequest(
    iv_crequest_type = 'BP1P1'
*    iv_description = 'Create new Carrier YZ'
    ).
  CATCH cx_usmd_gov_api.
    "Something went wrong while creating the change request (e.g. model blocked
    "or change request type unknown).
    EXIT.
ENDTRY.
"5th: Before making changes to an object, the object needs to be enqueued
"even if this is a creation scenario
TRY.
    lo_gov_api->enqueue_entity( EXPORTING iv_crequest_id = lv_crequest_id
    iv_entity_name = 'BP_HEADER'
    it_data = <lt_carr_key> ).
  CATCH cx_usmd_gov_api_entity_lock cx_usmd_gov_api.
    EXIT.
    "Tough luck –
    "something went wrong while enqueueing the entity (it could be a
    "technical reason, or maybe the carrier is already interlocked?!
ENDTRY.
"6th: Provide some entity attributes (complete data structure)

ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = 'A1428'.
ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = cl_system_uuid=>create_uuid_x16_static( ).
ASSIGN COMPONENT 'bu_type' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = '2'.
ASSIGN COMPONENT 'txtlg' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = '㊑'.
*ASSIGN COMPONENT 'CURRCODE' OF STRUCTURE <ls_carr_data> TO <value>.
*<value> = 'USD'.
*ASSIGN COMPONENT 'URL' OF STRUCTURE <ls_carr_data> TO <value>.
*<value> = 'http://www.fantasyflight.com'.
INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.

ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = 'A1429'.
ASSIGN COMPONENT 'BP_GUID' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = cl_system_uuid=>create_uuid_x16_static( ).
ASSIGN COMPONENT 'bu_type' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = '2'.
ASSIGN COMPONENT 'txtlg' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = '㊑'.
*ASSIGN COMPONENT 'CURRCODE' OF STRUCTURE <ls_carr_data> TO <value>.
*<value> = 'USD'.
*ASSIGN COMPONENT 'URL' OF STRUCTURE <ls_carr_data> TO <value>.
*<value> = 'http://www.fantasyflight.com'.
INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.

"7th: Write the entity data to the change request
TRY.
    lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_crequest_id
    iv_entity_name = 'BP_HEADER'
    it_data = <lt_carr_data> ).
  CATCH cx_usmd_gov_api_entity_write.
    EXIT.
    "Tough luck - might be that you have no authorization, or the entity is
    "not enqueued or cannot be added to the object list of the change
    "request
ENDTRY.






*lo_gov_api->create_data_reference(
*EXPORTING iv_entity_name = 'BP_CENTRL'
*iv_struct = lo_gov_api->gc_struct_key
*IMPORTING er_structure = lr_carr_key_str
*er_table = lr_carr_key_tab ).
"Create a data reference of the key and attribute structure / table of
"entity CARR (Carrier)
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_CENTRL'
iv_struct = lo_gov_api->gc_struct_key_attr
IMPORTING er_structure = lr_carr_data_str
er_table = lr_carr_data_tab ).
"Assign the created data references for carrier key and carrier data
"to field symbols
*ASSIGN lr_carr_key_str->* TO <ls_carr_key>.
*ASSIGN lr_carr_key_tab->* TO <lt_carr_key>.
ASSIGN lr_carr_data_str->* TO <ls_carr_data>.
ASSIGN lr_carr_data_tab->* TO <lt_carr_data>.
"3rd: Fill the key and data structure with values to create a new carrier
"The entity CARR only has key field CARR. The new carrier ID should be 'YZ'

ASSIGN COMPONENT 'bp_header' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'A1428'.
ASSIGN COMPONENT 'name_org1' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'test'.


INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.

ASSIGN COMPONENT 'bp_header' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'A1429'.
ASSIGN COMPONENT 'name_org1' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'test'.


INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.
"7th: Write the entity data to the change request
TRY.
    lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_crequest_id
    iv_entity_name = 'BP_CENTRL'
    it_data = <lt_carr_data> ).
  CATCH cx_usmd_gov_api_entity_write.
    EXIT.
    "Tough luck - might be that you have no authorization, or the entity is
    "not enqueued or cannot be added to the object list of the change
    "request
ENDTRY.


lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_PORG'
iv_struct = lo_gov_api->gc_struct_key_attr
IMPORTING er_structure = lr_carr_data_str
er_table = lr_carr_data_tab ).
"Assign the created data references for carrier key and carrier data
"to field symbols
*ASSIGN lr_carr_key_str->* TO <ls_carr_key>.
*ASSIGN lr_carr_key_tab->* TO <lt_carr_key>.
ASSIGN lr_carr_data_str->* TO <ls_carr_data>.
ASSIGN lr_carr_data_tab->* TO <lt_carr_data>.
"3rd: Fill the key and data structure with values to create a new carrier
"The entity CARR only has key field CARR. The new carrier ID should be 'YZ'

ASSIGN COMPONENT 'bp_header' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'A1429'.
ASSIGN COMPONENT 'assgnm_id'  OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  '1'.
ASSIGN COMPONENT 'prch_org' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  '1010'.
ASSIGN COMPONENT 'waers' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = 'JPY'.



INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.
ASSIGN COMPONENT 'bp_header' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  'A1429'.
ASSIGN COMPONENT 'assgnm_id'  OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  '1'.
ASSIGN COMPONENT 'prch_org' OF STRUCTURE <ls_carr_data> TO <value>.
<value> =  '1010'.
ASSIGN COMPONENT 'waers' OF STRUCTURE <ls_carr_data> TO <value>.
<value> = 'JPY'.



INSERT <ls_carr_data> INTO TABLE <lt_carr_data>.

TRY.
    lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_crequest_id
    iv_entity_name = 'BP_PORG'
    it_data = <lt_carr_data> ).
  CATCH cx_usmd_gov_api_entity_write.
    EXIT.
    "Tough luck - might be that you have no authorization, or the entity is
    "not enqueued or cannot be added to the object list of the change
    "request
ENDTRY.
"8th: optionally, the entity data is read again... just to make sure everything
"went right.




*TRY.
*    lo_gov_api->read_entity( EXPORTING iv_crequest_id = lv_crequest_id
*    iv_entity_name = 'BP_HEADER'
*    it_key = <lt_carr_key>
*    IMPORTING et_data = <lt_carr_data> ).
*
*    catch cx_usmd_gov_api_core_error cx_usmd_gov_api.
*    EXIT.
*    "Adequate Exception handling
*ENDTRY.
"9th: The complete change request should be checked before it is saved
TRY.
    lo_gov_api->check_crequest_data( iv_crequest_id = lv_crequest_id ).
    "Collect the entities to be checked
    ls_entity-entity = 'BP_HEADER'.
    ls_entity-tabl = lr_carr_key_tab.
    INSERT ls_entity INTO TABLE lt_entity.
    "check the entity
    lo_gov_api->check_complete_data(
    EXPORTING iv_crequest_id = lv_crequest_id
    it_key = lt_entity ).
  CATCH cx_usmd_conv_som_gov_api INTO lo_conv_error.
  CATCH cx_usmd_gov_api INTO lo_error.
    "Possibility to handle the erroneous data or go on.
ENDTRY.
"10th: Save the change request (and the entity data of course)
TRY.
    lo_gov_api->save( ).
    "Save is done in draft mode by default so it is possible to
    "save the change request even if change request data or
    "entity data is not consistent.
  CATCH cx_usmd_gov_api_core_error.
    EXIT.
    "Adequate Exception handling
ENDTRY.
"11th: At the end, it is necessary to clean the house
TRY.
    lo_gov_api->dequeue_entity( EXPORTING iv_crequest_id = lv_crequest_id
    iv_entity_name = 'BP_HEADER'
    it_data = <lt_carr_key> ).
    lo_gov_api->dequeue_crequest(
    EXPORTING iv_crequest_id = lv_crequest_id ).
  CATCH cx_usmd_gov_api.
    "Not a tragedy - maybe the workflow could not be processed properly after
    "it was started
ENDTRY.
COMMIT WORK AND WAIT.
"12th: If everything is fine, the workflow can be started for
"the change request (this is like a 'submit')
TRY.
    lo_gov_api->start_workflow( iv_crequest_id = lv_crequest_id ).
  CATCH cx_usmd_gov_api_core_error.
    "Adequate Exception handling
ENDTRY.
"Interested in the errors occured?
lt_messages = lo_gov_api->get_messages( ).

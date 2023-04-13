*&---------------------------------------------------------------------*
*& Report Z_GOV_API
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_GOV_API1.
DATA:
lo_gov_api TYPE REF TO if_usmd_gov_api,
lv_crequest_id TYPE usmd_crequest, "Change Request ID
lr_pfli_key_str TYPE REF TO data, "Entity Flight Connection - key structure
lr_pfli_key_tab TYPE REF TO data, "Entity Flight Connection - key table
lr_pfli_data_str TYPE REF TO data, "Entity Flight Connection - data structure
lr_pfli_data_tab TYPE REF TO data, "Entity Flight Connection - data table
lr_flight_key_str TYPE REF TO data, "Entity Flight - key structu
lr_flight_key_tab TYPE REF TO data, "Entity Flight - key table
lr_flight_data_str TYPE REF TO data, "Entity Flight - data structure
lr_flight_data_tab TYPE REF TO data, "Entity Flight - data table
ls_entity TYPE usmd_gov_api_s_ent_tabl,
lt_entity TYPE usmd_gov_api_ts_ent_tabl,
lt_messages TYPE usmd_t_message.
FIELD-SYMBOLS:
<ls_pfli_key> TYPE any,
<lt_pfli_key> TYPE INDEX TABLE,
<ls_pfli_data> TYPE any,
<lt_pfli_data> TYPE INDEX TABLE,
<ls_flight_key> TYPE any,
<lt_flight_key> TYPE INDEX TABLE,
<ls_flight_data> TYPE any,
<lt_flight_data> TYPE INDEX TABLE,
<value> TYPE any.
"1: Create an instance of the governance API
TRY.
lo_gov_api = cl_usmd_gov_api=>get_instance( iv_model_name = 'BP' ).
CATCH cx_usmd_gov_api.
EXIT.
ENDTRY.
TRY.
lv_crequest_id = lo_gov_api->create_crequest(
iv_crequest_type = 'BP1P1'
iv_description = 'test' ).
CATCH cx_usmd_gov_api.

EXIT.
ENDTRY.
"2: Create the data references needed to maintain the flight connection entity
"Create a data reference of the key structure/table of entity PFLI
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_HEADER'
iv_struct = lo_gov_api->gc_struct_key
IMPORTING er_structure = lr_pfli_key_str
er_table = lr_pfli_key_tab ).
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_HEADER'
iv_struct = lo_gov_api->gc_struct_key_attr
IMPORTING er_structure = lr_pfli_data_str
er_table = lr_pfli_data_tab ).
"Assign the created data references for the flight connection key to the field symbols
ASSIGN lr_pfli_key_str->* TO <ls_pfli_key>.
ASSIGN lr_pfli_key_tab->* TO <lt_pfli_key>.
ASSIGN lr_pfli_data_str->* TO <ls_pfli_data>.
ASSIGN lr_pfli_data_tab->* TO <lt_pfli_data>.
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_pfli_key> TO <value>.
<value> = 'A1413'.
INSERT <ls_pfli_key> INTO TABLE <lt_pfli_key>.
TRY.
lo_gov_api->enqueue_entity( EXPORTING iv_crequest_id = lv_crequest_id
iv_entity_name = 'BP_HEADER'
it_data = <lt_pfli_key> ).
CATCH cx_usmd_gov_api_entity_lock cx_usmd_gov_api.
EXIT.
ENDTRY.
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_pfli_data>  TO <value>.
<value> = 'A1413'.
ASSIGN COMPONENT 'bp_guid' OF STRUCTURE <ls_pfli_data> TO <value>.
<value> = cl_system_uuid=>create_uuid_x16_static( ).
ASSIGN COMPONENT 'bu_type'  OF STRUCTURE <ls_pfli_data>  TO <value>.
<value> = '2'.
ASSIGN COMPONENT 'txtlg'  OF STRUCTURE <ls_pfli_data>  TO <value>.
<value> = '\est'.
INSERT <ls_pfli_data> INTO TABLE <lt_pfli_data>.



TRY.
lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_crequest_id
iv_entity_name = 'BP_HEADER'
it_data = <lt_pfli_data> ).
CATCH cx_usmd_gov_api_entity_write.
EXIT. "Do better next time!
ENDTRY.
*
ls_entity-entity = 'BP_HEADER'.
ls_entity-tabl = lr_pfli_key_tab.
INSERT ls_entity INTO TABLE lt_entity.


*
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_CENTRL'
iv_struct = lo_gov_api->gc_struct_key
IMPORTING er_structure = lr_pfli_key_str
er_table = lr_pfli_key_tab ).
lo_gov_api->create_data_reference(
EXPORTING iv_entity_name = 'BP_CENTRL'
iv_struct = lo_gov_api->gc_struct_key_attr
IMPORTING er_structure = lr_pfli_data_str
er_table = lr_pfli_data_tab ).
"Assign the created data references for the flight connection key to the field symbols
ASSIGN lr_pfli_key_str->* TO <ls_pfli_key>.
ASSIGN lr_pfli_key_tab->* TO <lt_pfli_key>.
ASSIGN lr_pfli_data_str->* TO <ls_pfli_data>.
ASSIGN lr_pfli_data_tab->* TO <lt_pfli_data>.
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_pfli_key> TO <value>.
<value> = 'A1413'.
INSERT <ls_pfli_key> INTO TABLE <lt_pfli_key>.
ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE <ls_pfli_data> TO <value>.
<value> = 'A1413'.
ASSIGN COMPONENT 'name_org1' OF STRUCTURE <ls_pfli_data> TO <value>.
<value> = 'test'.
INSERT <ls_pfli_data> INTO TABLE <lt_pfli_data>.
*
*
*"5: Read the flight connection data in order to do some changes
*
TRY.
lo_gov_api->write_entity( EXPORTING iv_crequest_id = lv_crequest_id
iv_entity_name = 'BP_CENTRL'
it_data = <lt_pfli_data> ).
CATCH cx_usmd_gov_api_entity_write.
EXIT. "Do better next time!
ENDTRY.

"10: The complete change request should be checked before it is saved
TRY.
lo_gov_api->check_crequest_data( iv_crequest_id = lv_crequest_id ).
"Collect the entities to be checked

"Check the entity
lo_gov_api->check_complete_data(
EXPORTING iv_crequest_id = lv_crequest_id
it_key = lt_entity ).
CATCH cx_usmd_gov_api_core_error cx_usmd_gov_api.
"Handle the erroneous data or go on.
ENDTRY.

"11: Save the change request (and the entity data, of course)
TRY.
lo_gov_api->save( ).
"Save is done in draft mode by default so it is possible to

CATCH cx_usmd_gov_api_core_error.
  lt_messages = lo_gov_api->get_messages( ).
EXIT.
"Adequate exception handling
ENDTRY.
"12: At the end, it is necessary to clean the house
TRY.
lo_gov_api->dequeue_entity( EXPORTING iv_crequest_id = lv_crequest_id
iv_entity_name = 'BP_HEADER'
it_data = <lt_pfli_key> ).
lo_gov_api->dequeue_crequest( EXPORTING iv_crequest_id = lv_crequest_id ).
CATCH cx_usmd_gov_api.
"Adequate exception handling
ENDTRY.
*COMMIT WORK AND WAIT.
TRY.
lo_gov_api->start_workflow( iv_crequest_id = lv_crequest_id ).
**
CATCH cx_usmd_gov_api_core_error.
"Adequate exception handling
ENDTRY.



DATA: lv_ET_ENTITY_DATA_ACTIVATED TYPE USMD_TS_ENTITY_DATA_ALL,
lv_ET_MESSAGE TYPE USMD_T_MESSAGE,
lv_other TYPE c.

   DATA :
      lr_crequest                 TYPE REF TO if_usmd_crequest_api.

   CALL METHOD cl_usmd_crequest_api=>get_instance
      EXPORTING
        iv_crequest          = lv_crequest_id
      IMPORTING
        re_inst_crequest_api = lr_crequest.
lr_crequest->enqueue_crequest(

  ).
lr_crequest->ACTIVATE_CREQUEST(
IMPORTING
ET_ENTITY_DATA_ACTIVATED = lv_ET_ENTITY_DATA_ACTIVATED
ET_MESSAGE = lv_ET_MESSAGE ).
"Interested in the messages occurred?

COMMIT WORK.
BREAK-POINT.

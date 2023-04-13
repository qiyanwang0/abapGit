class ZCL_USMD_CR_GUIBB_ATTACHMENT3 definition
  public
  inheriting from CL_USMD_CR_GUIBB_LIST
  create public .

public section.

  constants CV_EVENT_ID_LINK_ADD type FPM_EVENT_ID value 'CR_ATT_LINK_ADD' ##NO_TEXT.
  constants CV_EVENT_ID_FILE_ADD type FPM_EVENT_ID value 'CR_ATT_FILE_ADD' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_EDIT type FPM_EVENT_ID value 'CR_ATT_EDIT' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_LINK_CREATE type FPM_EVENT_ID value 'CR_ATT_LINK_CREATE' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_FILE_CREATE type FPM_EVENT_ID value 'CR_ATT_FILE_CREATE' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_FILE_EDIT type FPM_EVENT_ID value 'CR_ATT_FILE_EDIT' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_LINK_EDIT type FPM_EVENT_ID value 'CR_ATT_LINK_EDIT' ##NO_TEXT.
  constants CV_EVENT_ID_SHOW_ATT type FPM_EVENT_ID value 'CR_ATT_SHOW' ##NO_TEXT.
  constants CV_EVENT_ID_ATT_DEL type FPM_EVENT_ID value 'CR_ATT_DELETE' ##NO_TEXT.

  methods CONSTRUCTOR .

  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_LIST~PROCESS_EVENT
    redefinition .
protected section.

  data MV_OCA_DELETE_ENABLED type ABAP_BOOL value 'X' ##NO_TEXT.
  data MV_OCA_DELETE_ENABLED_REF type NAME_KOMP value 'OCA_DELETE_ENABLED' ##NO_TEXT.
  data MV_OCA_DISPLAY_VISIBLE_REF type NAME_KOMP value 'OCA_DISPLAY_VISIBLE' ##NO_TEXT.
  data MV_OCA_EDIT_ENABLED type ABAP_BOOL value 'X' ##NO_TEXT.
  data MV_OCA_EDIT_ENABLED_REF type NAME_KOMP value 'OCA_EDIT_ENABLED' ##NO_TEXT.
  data MO_CONV_SOM_GOV_API type ref to IF_USMD_CONV_SOM_GOV_API .

  methods GET_ENTITY_KEY
    exporting
      !ES_KEY type ANY .
  methods CONVERT_UTC_TIMESTAMP
    importing
      !IV_TIMESTAMP type TIMESTAMP
    exporting
      !EV_TIMESTAMP type TIMESTAMP .

  methods ADD_STANDARD_ROW_ACTIONS
    redefinition .
  methods CHECK_ACTION_USAGE_SINGLE
    redefinition .
  methods CREATE_STRUCT_RTTI
    redefinition .
  methods GET_ACTIONS
    redefinition .
  methods GET_ENTITY_DATA
    redefinition .
  methods HANDLE_EDIT_ENTRANCE
    redefinition .
  methods IS_ROW_ACTION_ENABLED
    redefinition .
  methods PROCESS_LIST_CELL_ACTION
    redefinition .
private section.
*"* private components of class ZCL_USMD_CR_GUIBB_ATTACHMENT3
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_USMD_CR_GUIBB_ATTACHMENT3 IMPLEMENTATION.


METHOD ADD_STANDARD_ROW_ACTIONS.

  DATA:
    ls_row_action_ref LIKE LINE OF mt_row_action_ref.

  FIELD-SYMBOLS:
    <ls_row_action> LIKE LINE OF me->mt_row_action.


  super->add_standard_row_actions( ).

  APPEND INITIAL LINE TO me->mt_row_action ASSIGNING <ls_row_action>.
  <ls_row_action>-id = cv_event_id_att_edit.
  <ls_row_action>-image_source = '~Icon/Edit'.
  <ls_row_action>-is_implicit_edit = abap_true.
  "<ls_row_action>-visible_ref = me->mv_oca_display_visible_ref.
  ls_row_action_ref-id = <ls_row_action>-id.
  INSERT ls_row_action_ref INTO TABLE me->mt_row_action_ref.

  CALL METHOD me->create_row_action_refs
    .
ENDMETHOD.


METHOD CHECK_ACTION_USAGE_SINGLE.

  CASE cs_action_usage-id.
    WHEN cv_event_id_att_del.
      IF me->is_mode_read_only( ) = abap_true OR
         me->mo_collection IS NOT BOUND.
        cs_action_usage-enabled = abap_false.
        "me->mo_collection->size( ) = 0.
      ELSEIF me->is_mode_read_only( ) = abap_false AND
             me->mo_collection->size( ) = 0.
        cs_action_usage-enabled = abap_false.
      ELSE.
        cs_action_usage-enabled = abap_true.
      ENDIF.
    WHEN OTHERS.
      IF me->is_mode_read_only( ) = abap_true.
        cs_action_usage-enabled = abap_false.
      ELSE.
        cs_action_usage-enabled = abap_true.
      ENDIF.
  ENDCASE.
  ms_change-action_usage = abap_true.
ENDMETHOD.


METHOD CONSTRUCTOR.
  super->constructor( ).

  TRY.
      CALL METHOD cl_usmd_conv_som_gov_api=>get_instance
        RECEIVING
          ro_so_gov_api = me->mo_conv_som_gov_api.
    CATCH cx_usmd_conv_som_gov_api.
    CATCH cx_usmd_app_context_cons_error.
    CATCH cx_usmd_gov_api.
      "Nothing to do here: Instantiation of convenience API will be tried again later on
  ENDTRY.
ENDMETHOD.


METHOD CONVERT_UTC_TIMESTAMP.
  DATA: lv_date TYPE systdatlo,
        lv_time TYPE systtimlo,
        lv_zone TYPE tzonref-tzone.

  CONVERT TIME STAMP iv_timestamp TIME ZONE sy-zonlo INTO DATE lv_date TIME lv_time.
  CONVERT DATE lv_date TIME lv_time INTO TIME STAMP ev_timestamp TIME ZONE lv_zone.

ENDMETHOD.


method CREATE_STRUCT_RTTI.
  DATA:
    lt_component TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS:
    <ls_component> LIKE LINE OF lt_component.

  super->create_struct_rtti( ).

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  "<ls_component>-name = cv_comp_bol_data.
  <ls_component>-type = me->mo_struct_rtti.
  <ls_component>-as_include = abap_true.

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  <ls_component>-name = 'CREATEDBY__TEXT'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  <ls_component>-name = 'FILE_ICON'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  <ls_component>-name = 'FILE_SIZE_DESCR'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  me->mo_struct_rtti = cl_abap_structdescr=>create( lt_component ).
endmethod.


method GET_ACTIONS.



  FIELD-SYMBOLS:
    <ls_actiondef> LIKE LINE OF mt_actiondef.


*----- inherit
  super->get_actions( ).

*----- Add link
  APPEND INITIAL LINE TO mt_actiondef ASSIGNING <ls_actiondef>.
  <ls_actiondef>-id = cv_event_id_link_add.
  <ls_actiondef>-text = text-001.
  <ls_actiondef>-enabled = abap_true.
  <ls_actiondef>-visible = cl_wd_uielement=>e_visible-visible.

*----- Add file
  APPEND INITIAL LINE TO mt_actiondef ASSIGNING <ls_actiondef>.
  <ls_actiondef>-id = cv_event_id_file_add.
  <ls_actiondef>-text = text-002.
  <ls_actiondef>-enabled = abap_true.
  <ls_actiondef>-visible = cl_wd_uielement=>e_visible-visible.

*-----
  APPEND INITIAL LINE TO mt_actiondef ASSIGNING <ls_actiondef>.
  <ls_actiondef>-id = cv_event_id_show_att.
  <ls_actiondef>-text = text-003.
  <ls_actiondef>-enabled = abap_true.
  <ls_actiondef>-visible = cl_wd_uielement=>e_visible-visible.

*------ Delete Attachments
  APPEND INITIAL LINE TO mt_actiondef ASSIGNING <ls_actiondef>.
  <ls_actiondef>-id = cv_event_id_att_del.
  "<ls_actiondef>-text = text-002.
  <ls_actiondef>-imagesrc = '~Icon/Delete'.
  <ls_actiondef>-enabled = abap_true.
  <ls_actiondef>-visible = cl_wd_uielement=>e_visible-visible.

endmethod.


METHOD GET_ENTITY_DATA.
  FIELD-SYMBOLS:
    <lv_data_dest>      TYPE any,
    <lv_usr_create>     TYPE any,
    <lv_data_file_type> TYPE any,
    <lv_data_file_icon> TYPE any,
    <lv_data_file_size> TYPE any.

  DATA lv_file_size TYPE int4.

  super->get_entity_data(
    EXPORTING io_access = io_access
    CHANGING cs_data = cs_data
  ).

  CHECK io_access IS BOUND.

  " Fill Field CREATEDBY__TEXT
  ASSIGN COMPONENT 'CREATEDBY__TEXT' OF STRUCTURE cs_data TO <lv_data_dest>.
  ASSIGN COMPONENT 'CREATED_BY'      OF STRUCTURE cs_data TO <lv_usr_create>.
  <lv_data_dest> = <lv_usr_create>.
  IF <lv_usr_create> IS NOT INITIAL.
    CALL METHOD cl_usmd_cr_ui_service=>get_user_full_name
      EXPORTING
        iv_username  = <lv_usr_create>
      RECEIVING
        rv_name_text = <lv_data_dest>.
    .

    IF <lv_data_dest> IS INITIAL.
      <lv_data_dest> = <lv_usr_create>.
    ENDIF.
  ENDIF.

  ASSIGN COMPONENT 'FILE_TYPE' OF STRUCTURE cs_data TO <lv_data_file_type>.
  ASSIGN COMPONENT 'FILE_ICON' OF STRUCTURE cs_data TO <lv_data_file_icon>.
  IF <lv_data_file_type> IS ASSIGNED AND <lv_data_file_icon> IS ASSIGNED.
    IF <lv_data_file_type> IS INITIAL AND <lv_data_file_icon> IS INITIAL.
      <lv_data_file_type> = 'text/html'.
      " Get icon for file type
      CALL METHOD cl_usmd_cr_ui_service=>get_file_icon
        EXPORTING
          iv_mime_type  = <lv_data_file_type>
        RECEIVING
          rv_icon_alias = <lv_data_file_icon>.
    ELSEIF <lv_data_file_type> IS NOT INITIAL AND <lv_data_file_icon> IS INITIAL.
      " Get icon for file type
      CALL METHOD cl_usmd_cr_ui_service=>get_file_icon
        EXPORTING
          iv_mime_type  = <lv_data_file_type>
        RECEIVING
          rv_icon_alias = <lv_data_file_icon>.
    ENDIF.
  ENDIF.

  ASSIGN COMPONENT 'FILE_SIZE' OF STRUCTURE cs_data TO <lv_data_file_size>.
  ASSIGN COMPONENT 'FILE_SIZE_DESCR' OF STRUCTURE cs_data TO <lv_data_dest>.
  IF <lv_data_file_size> IS ASSIGNED AND <lv_data_dest> IS ASSIGNED.
    IF <lv_data_file_size> IS NOT INITIAL.
      lv_file_size = <lv_data_file_size>.
      CALL METHOD cl_usmd_cr_ui_service=>get_file_size_description
        EXPORTING
          iv_file_size_bytes       = lv_file_size
        RECEIVING
          rv_file_size_description = <lv_data_dest>.

    ELSE.
      CLEAR <lv_data_dest>.
    ENDIF.
  ENDIF.

  ASSIGN COMPONENT 'TIMESTAMP' OF STRUCTURE cs_data TO <lv_data_dest>.
  IF sy-subrc = 0.
    CALL METHOD me->convert_utc_timestamp
      EXPORTING
        iv_timestamp = <lv_data_dest>
      IMPORTING
        ev_timestamp = <lv_data_dest>.
  ENDIF.

  FIELD-SYMBOLS:  <lv_USMD_TITLE> TYPE any.

  ASSIGN COMPONENT 'TITLE' OF STRUCTURE cs_data TO <lv_USMD_TITLE>.

* Remove the extension from the file name. There might be more than one dot in the name.
  REPLACE REGEX '\.\w+((\s+)|($))' IN <lv_usmd_title> WITH space .
ENDMETHOD.


METHOD GET_ENTITY_KEY.
  CHECK me->mo_entity IS BOUND.
  cl_crm_genil_container_tools=>get_key_from_object_id(
    EXPORTING
      iv_object_name = me->mo_entity->get_name( )
      iv_object_id = me->mo_entity->get_key( )
    IMPORTING
      es_key = es_key
  ).
ENDMETHOD.


method HANDLE_EDIT_ENTRANCE.

  CASE io_event->mv_event_id.
    WHEN if_fpm_constants=>gc_event-local_edit.
      CHECK iv_raised_by_own_ui = abap_true.
      " ToDo: Lock Change Request (if not already done)

  ENDCASE.
endmethod.


METHOD IF_FPM_GUIBB_LIST~GET_DATA.
  CALL METHOD super->if_fpm_guibb_list~get_data
    EXPORTING
      iv_eventid                = iv_eventid
      it_selected_fields        = it_selected_fields
      iv_raised_by_own_ui       = iv_raised_by_own_ui
      iv_visible_rows           = iv_visible_rows
      iv_edit_mode              = iv_edit_mode
    IMPORTING
      et_messages               = et_messages
      ev_data_changed           = ev_data_changed
      ev_field_usage_changed    = ev_field_usage_changed
      ev_action_usage_changed   = ev_action_usage_changed
      ev_selected_lines_changed = ev_selected_lines_changed
      ev_dnd_attr_changed       = ev_dnd_attr_changed
      eo_itab_change_log        = eo_itab_change_log
    CHANGING
      ct_data                   = ct_data
      ct_field_usage            = ct_field_usage
      ct_action_usage           = ct_action_usage
      ct_selected_lines         = ct_selected_lines
      cv_lead_index             = cv_lead_index
      cv_first_visible_row      = cv_first_visible_row
      cs_additional_info        = cs_additional_info
      ct_dnd_attributes         = ct_dnd_attributes.

  " Set CR UIBB to edit mode in the following case:
  " - New Crequest (Initial CRequest Status)
  " - Workflow is stable
  " - CR UIBB is in read only mode
  " - Crequest ID has been set to the convenience API
  IF "me->mv_workflow_is_stable = abap_true AND
     me->is_mode_read_only( ) = abap_true AND
     me->mo_conv_som_gov_api->mv_crequest_create = abap_true AND
    me->mo_conv_som_gov_api->mv_crequest_id IS NOT INITIAL AND
    NOT ( iv_eventid->mv_event_id = if_fpm_constants=>gc_event-local_edit AND iv_raised_by_own_ui = abap_true ).
    me->raise_local_event_by_id( if_fpm_constants=>gc_event-local_edit ).
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~GET_DEFINITION.
  DATA ls_row_action_ref LIKE LINE OF me->mt_row_action_ref.

  FIELD-SYMBOLS <fs_row_action> LIKE LINE OF et_row_actions.

  CALL METHOD super->if_fpm_guibb_list~get_definition
    IMPORTING
      eo_field_catalog         = eo_field_catalog
      et_field_description     = et_field_description
      et_action_definition     = et_action_definition
      et_special_groups        = et_special_groups
      es_message               = es_message
      ev_additional_error_info = ev_additional_error_info
      et_dnd_definition        = et_dnd_definition
      et_row_actions           = et_row_actions.

  IF me->mt_row_action_ref IS NOT INITIAL.
    LOOP AT me->mt_row_action_ref INTO ls_row_action_ref.
      READ TABLE et_row_actions WITH KEY id = ls_row_action_ref-id ASSIGNING <fs_row_action>.
      IF <fs_row_action> IS ASSIGNED.
        <fs_row_action>-enabled_ref = ls_row_action_ref-enabled_ref.
        <fs_row_action>-visible_ref = ls_row_action_ref-visibility_ref.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~PROCESS_EVENT.
  DATA: lv_row_index TYPE sytabix,
        lo_entity    LIKE me->mo_entity,
        lv_att_type  TYPE c,
        lv_success   TYPE abap_bool,
        lv_lead_index LIKE iv_lead_index.

  " Dat declaration for show the selected attachment
  DATA: lo_fpm TYPE REF TO if_fpm,
        ls_url_launcher TYPE fpm_s_launch_url,
        ls_navigator_parameters TYPE apb_lpd_s_portal_parameters,
        lo_navigator TYPE REF TO if_fpm_navigate_to,
        lv_error_occured TYPE abap_bool,
        lt_messages TYPE fpm_t_t100_messages,
        ls_attachment TYPE bss_cril_attachment_attributes,
        ls_message  LIKE LINE OF et_messages,
        ls_selected_lines LIKE LINE OF it_selected_lines,
        lv_delete_denied TYPE abap_bool,
        lv_uname TYPE sy-uname.

  CASE io_event->mv_event_id.
    WHEN cv_event_id_link_add.

      " Creation of new attachment of type link
      CALL METHOD me->create
        EXPORTING
          iv_event_id    = io_event->mv_event_id
*         is_create_ctrl =
        RECEIVING
          ro_entity      = lo_entity.
      IF lo_entity IS BOUND.
        ev_result = if_fpm_constants=>gc_event_result-ok.
        me->raise_local_event_by_id( cv_event_id_att_link_create ).
      ELSE.
        ev_result = if_fpm_constants=>gc_event_result-failed.
      ENDIF.
      et_messages = get_messages( ) .
      RETURN.
    WHEN cv_event_id_file_add.
      " Creation of new attachment of type link
      CALL METHOD me->create
        EXPORTING
          iv_event_id    = io_event->mv_event_id
*         is_create_ctrl =
        RECEIVING
          ro_entity      = lo_entity.
      IF lo_entity IS BOUND.
        ev_result = if_fpm_constants=>gc_event_result-ok.
        me->raise_local_event_by_id( cv_event_id_att_file_create ).
      ELSE.
        ev_result = if_fpm_constants=>gc_event_result-failed.
      ENDIF.
      et_messages = get_messages( ) .
      RETURN.
    WHEN cv_event_id_att_edit.
      " Editing of already existing attachment
*----- get row index
      CALL METHOD io_event->mo_event_data->get_value
        EXPORTING
          iv_key   = if_fpm_guibb_list=>gc_event_par_row
        IMPORTING
          ev_value = lv_row_index.

*----- bound for row action
      IF lv_row_index > 0.

*----- put selection temporarily on row to set lead selection outport
        IF mo_collection->find( iv_index = lv_row_index ) IS BOUND.
          ev_result = if_fpm_constants=>gc_event_result-ok.
        ELSE.
          ev_result = if_fpm_constants=>gc_event_result-failed.
        ENDIF.
        IF ev_result = if_fpm_constants=>gc_event_result-ok.
          CALL METHOD me->set_selection
            EXPORTING
              iv_lead_index = lv_row_index
*             it_selection  =
            .
        ENDIF.
        " Determine if it is a link or a file
        " Get the corresponding bol entity for the selected link
        CALL METHOD mo_collection->find
          EXPORTING
            iv_index           = lv_row_index
*           iv_entity          =
*           iv_object_instance =
          RECEIVING
            rv_result          = lo_entity.

        IF lo_entity IS BOUND.
          " Get relevant data for showing the attachment
          CALL METHOD me->mo_entity->if_bol_bo_property_access~get_properties
            IMPORTING
              es_attributes = ls_attachment.

          " Determine if it is a link or a file
          IF ls_attachment-link IS NOT INITIAL.
            lv_att_type = 'L'.
          ELSE.
            lv_att_type = 'F'.
          ENDIF.

          "When file
          IF lv_att_type = 'F'.
            me->raise_local_event_by_id( cv_event_id_att_file_edit ).
            "when link
          ELSEIF lv_att_type = 'L'.
            me->raise_local_event_by_id( cv_event_id_att_link_edit ).
          ENDIF.
          RETURN.
        ENDIF.
      ENDIF.
    WHEN cv_event_id_show_att.
      " Nothing to do here
    WHEN cv_event_id_att_del.
      " Set the current selected lines to the bol collection
      IF it_selected_lines IS NOT INITIAL.
        " Check if all selected attachments are allowed to be deleted
        lv_delete_denied = abap_false.
        LOOP AT it_selected_lines INTO ls_selected_lines.
          lv_row_index = ls_selected_lines-tabix.
          " Get the corresponding bol entity
          CALL METHOD mo_collection->find
            EXPORTING
              iv_index           = lv_row_index
*             iv_entity          =
*             iv_object_instance =
            RECEIVING
              rv_result          = lo_entity.

          IF lo_entity IS BOUND.
            " Get relevant data for showing the attachment
            CALL METHOD lo_entity->if_bol_bo_property_access~get_properties
              IMPORTING
                es_attributes = ls_attachment.
            " Check if current user is allowed to delete the attachment
            lv_uname = sy-uname.
            IF ls_attachment-created_by NE lv_uname.
              lv_delete_denied = abap_true.
              IF 1 = 2.
                MESSAGE e175(usmd2) WITH ls_attachment-title ls_attachment-created_by.
              ELSE.
                CLEAR ls_message.
                ls_message-msgid    = 'USMD2'.
                ls_message-msgno    = '175'.
                ls_message-severity = 'E'.
                ls_message-parameter_1 = ls_attachment-title.
                ls_message-parameter_2 = ls_attachment-created_by.
                APPEND ls_message TO et_messages.
                lv_lead_index = iv_lead_index.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_delete_denied = abap_true. " Deletion was denied
          IF 1 = 2.
            MESSAGE e301(usmd_gov_api).
          ELSE.
            CLEAR ls_message.
            ls_message-msgid    = 'USMD_GOV_API'.
            ls_message-msgno    = '301'.
            ls_message-severity = 'E'.
            APPEND ls_message TO et_messages.
          ENDIF.
          CLEAR lv_lead_index.
          CALL METHOD me->set_selection
            EXPORTING
              iv_lead_index = lv_lead_index
              it_selection  = it_selected_lines.
          ev_result = if_fpm_constants=>gc_event_result-failed.
        ELSE. " Deletion is allowed:
          " All attachments that are selected are to be deleted
          lv_lead_index = iv_lead_index.
          CLEAR lv_lead_index.
          CALL METHOD me->set_selection
            EXPORTING
              iv_lead_index = lv_lead_index
              it_selection  = it_selected_lines.
          CALL METHOD me->delete
            RECEIVING
              rv_success = lv_success.
          IF lv_success = abap_true.
            ev_result = if_fpm_constants=>gc_event_result-ok.
          ELSE.
            ev_result = if_fpm_constants=>gc_event_result-failed.
          ENDIF.
        ENDIF.
      ELSE.
        " Error message: No lines selected
        IF 1 = 2.
          MESSAGE e117(usmd2).
        ELSE.
          CLEAR ls_message.
          ls_message-msgid    = 'USMD2'.
          ls_message-msgno    = '157'.
          ls_message-severity = 'E'.
          APPEND ls_message TO et_messages.
          ev_result = if_fpm_constants=>gc_event_result-failed.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      CALL METHOD super->if_fpm_guibb_list~process_event
        EXPORTING
          io_event            = io_event
          iv_raised_by_own_ui = iv_raised_by_own_ui
          iv_lead_index       = iv_lead_index
          iv_event_index      = iv_event_index
          it_selected_lines   = it_selected_lines
        IMPORTING
          ev_result           = ev_result
          et_messages         = et_messages.
  ENDCASE.
ENDMETHOD.


METHOD IS_ROW_ACTION_ENABLED.

  DATA: ls_attributes TYPE bss_cril_attachment_attributes,
        lv_uname TYPE sy-uname.
  lv_uname = sy-uname.
  CASE iv_event_id.
    WHEN cv_event_id_att_edit OR cv_event_id_row_delete.
      CHECK io_entity IS BOUND.
      CALL METHOD io_entity->if_bol_bo_property_access~get_properties
        IMPORTING
          es_attributes = ls_attributes.
      " Deletion/Editing is not allowed when edit mode or object was created by another user
      IF ( ls_attributes-created_by NE lv_uname AND
           ls_attributes-created_by IS NOT INITIAL ) OR
           me->is_mode_read_only( ) = abap_true.
        rv_enabled = abap_false.
      ELSE.
        rv_enabled = abap_true.
      ENDIF.
    WHEN OTHERS.
      CALL METHOD super->is_row_action_enabled
        EXPORTING
          iv_event_id = iv_event_id
          io_entity   = io_entity
        RECEIVING
          rv_enabled  = rv_enabled.
  ENDCASE.
ENDMETHOD.


METHOD PROCESS_LIST_CELL_ACTION.
  DATA: lo_entity     TYPE REF TO cl_crm_bol_entity,
        ls_attachment TYPE bss_cril_attachment_attributes,
        lv_att_type   TYPE c,
        lv_title      TYPE string,
        lv_mime_type  TYPE string.

  " Dat declaration for show the selected attachment
  DATA: lo_fpm TYPE REF TO if_fpm,
        ls_url_launcher TYPE fpm_s_launch_url,
        ls_navigator_parameters TYPE apb_lpd_s_portal_parameters,
        lo_navigator TYPE REF TO if_fpm_navigate_to,
        lt_messages TYPE fpm_t_t100_messages,
        lv_error_occured TYPE abap_bool.

  CASE io_event->mv_event_id.
    WHEN 'FPM_GUIBB_LIST_CELL_ACTION'.
      " Determine the data from the current line (IV_ROW_INDEX)
      IF iv_fieldname = 'TITLE'.
        " Get the corresponding bol entity for the selected link
        CALL METHOD mo_collection->find
          EXPORTING
            iv_index           = iv_row_index
*           iv_entity          =
*           iv_object_instance =
          RECEIVING
            rv_result          = lo_entity.

        IF lo_entity IS BOUND.
          " Get relevant data for showing the attachment
          CALL METHOD lo_entity->if_bol_bo_property_access~get_properties
            IMPORTING
              es_attributes = ls_attachment.

          " Determine if it is a link or a file
          IF ls_attachment-link IS NOT INITIAL.
            lv_att_type = 'L'.
          ELSE.
            lv_att_type = 'F'.
          ENDIF.

          " When Link
          IF lv_att_type = 'L'.
            lo_fpm = cl_fpm_factory=>get_instance( ).
            lo_navigator = lo_fpm->get_navigate_to( ).
            ls_url_launcher-url = ls_attachment-link.
            ls_url_launcher-header_text = ls_attachment-title.
            ls_navigator_parameters-navigation_mode = 'EXTERNAL'. "opens new window for navigation target
            ls_navigator_parameters-history_mode = '0'. "can occur multiple times in history
            lo_navigator->launch_url(
               EXPORTING
                 is_url_fields = ls_url_launcher
                 is_additional_parameters = ls_navigator_parameters
               IMPORTING
                 et_messages = lt_messages
                 ev_error = lv_error_occured ).
            IF lv_error_occured = abap_true.
              rv_result = if_fpm_constants=>gc_event_result-failed.
            ELSE.
              rv_result = if_fpm_constants=>gc_event_result-ok.
            ENDIF.
            " When File
          ELSEIF lv_att_type = 'F'. "When File
            IF ls_attachment-content IS NOT INITIAL.
              lv_title     = ls_attachment-title.
              lv_mime_type = ls_attachment-file_type.
              IF lv_mime_type = cl_usmd_crequest_util=>gc_mime_type_outlook_msg.
                CONCATENATE lv_title '.msg' INTO lv_title.
              ENDIF.
              cl_wd_runtime_services=>attach_file_to_response(
                      i_filename      = lv_title
                      i_content       = ls_attachment-content
                      i_mime_type     = lv_mime_type
                      i_in_new_window = abap_true
                      i_inplace       = abap_false ).

            ENDIF.
          ENDIF.
        ELSE.
          rv_result = if_fpm_constants=>gc_event_result-failed.
        ENDIF.

      ELSEIF iv_fieldname = 'CREATEDBY__TEXT'.
        DATA: lo_event TYPE REF TO cl_fpm_event.

        " Get the corresponding bol entity for the selected link
        CALL METHOD mo_collection->find
          EXPORTING
            iv_index           = iv_row_index
*           iv_entity          =
*           iv_object_instance =
          RECEIVING
            rv_result          = lo_entity.

        IF lo_entity IS BOUND.
          " Get relevant data for showing user profile
          CALL METHOD lo_entity->if_bol_bo_property_access~get_properties
            IMPORTING
              es_attributes = ls_attachment.

          " Only if user is set
          IF ls_attachment-created_by IS NOT INITIAL.
            lo_event = me->raise_local_event_by_id( cl_usmd_cr_user_popup=>cv_event_id_show_user_profile ).
          ELSE.
            rv_result = if_fpm_constants=>gc_event_result-failed.
            RETURN.
          ENDIF.

          " If we have the reference to the event, attach the user data
          IF lo_event IS BOUND.
            CALL METHOD lo_event->mo_event_data->set_value
              EXPORTING
                iv_key   = cl_usmd_cr_user_popup=>cv_user
                iv_value = ls_attachment-created_by.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMETHOD.
ENDCLASS.

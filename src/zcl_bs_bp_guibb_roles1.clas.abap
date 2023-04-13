class ZCL_BS_BP_GUIBB_ROLES1 definition
  public
  inheriting from CL_BS_BP_GUIBB_LIST
  create public .

public section.

  methods GET_VALUE_SET_PARTNERROLE
    importing
      !IO_ACCESS type ref to IF_BOL_BO_PROPERTY_ACCESS
      !IV_ATTR_NAME type NAME_KOMP
    exporting
      !ET_VALUE_SET type WDR_CONTEXT_ATTR_VALUE_LIST .

  methods IF_FPM_GUIBB_LIST_PAGING~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_LIST~FLUSH
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DEFINITION
    redefinition .
protected section.

  types:
    BEGIN OF ty_partnerrole_text,
      partnerrole TYPE bu_partnerrole,
      text TYPE bu_partnerroletitl,
    END OF ty_partnerrole_text .
  types:
    ty_partnerrole_text_t TYPE STANDARD TABLE OF ty_partnerrole_text .
  types:
    ty_roles TYPE STANDARD TABLE OF bus_tb003_t .

  methods FILTER_EMPLOYEE_ROLES
    changing
      !CT_ROLES type TY_ROLES .
  methods OVS_OUTPUT_PARTNERROLE
    importing
      !IV_FIELD_NAME type NAME_KOMP
      !IR_QUERY_PARAMETER type ref to DATA
      !IO_ACCESS type ref to IF_BOL_BO_PROPERTY_ACCESS optional
    exporting
      !ER_OUTPUT type ref to DATA
      !EV_TABLE_HEADER type STRING
      !ET_COLUMN_TEXTS type WDR_NAME_VALUE_LIST .

  methods CHECK_ACTION_USAGE
    redefinition .
  methods CREATE_ENTITY
    redefinition .
  methods CREATE_STRUCT_RTTI
    redefinition .
  methods GET_ATTR_TEXT
    redefinition .
  methods GET_ATTR_VALUE_SET
    redefinition .
  methods GET_DEFAULT_DISPLAY_TYPE
    redefinition .
  methods GET_ENTITY_DATA
    redefinition .
  methods GET_FIELD_UI_PROP
    redefinition .
  methods OVS_HANDLE_PHASE_2
    redefinition .
  methods OVS_HANDLE_PHASE_3
    redefinition .
private section.
*"* private components of class ZCL_BS_BP_GUIBB_ROLES1
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BS_BP_GUIBB_ROLES1 IMPLEMENTATION.


METHOD CHECK_ACTION_USAGE.
  DATA:
    lo_connector TYPE REF TO cl_bs_bp_connector_bol_rel,
    lo_entity TYPE REF TO cl_crm_bol_entity,
    lt_value_set TYPE wdr_context_attr_value_list.

  FIELD-SYMBOLS:
    <ls_action_usage> TYPE fpmgb_s_actionusage.

  CALL METHOD super->check_action_usage
    CHANGING
      ct_action_usage = ct_action_usage.

  READ TABLE ct_action_usage ASSIGNING <ls_action_usage>
    WITH KEY id = '_CREA_'.
  IF sy-subrc = 0.
    CHECK me->mo_connector IS BOUND.
    lo_connector ?= me->mo_connector.
    lo_entity = lo_connector->get_parent( ).
    CHECK lo_entity IS BOUND.
    me->get_attr_value_set(
      EXPORTING
        io_access    = lo_entity
        iv_attr_name = 'PARTNERROLE'
      IMPORTING
        et_value_set = lt_value_set
    ).
    IF lines( lt_value_set ) = 0.
      "If the value help is empty, the entry must not be created
      <ls_action_usage>-enabled = abap_false.
      ms_change-action_usage = abap_true.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD CREATE_ENTITY.
  CALL METHOD super->create_entity
    EXPORTING
      is_initial_data   = is_initial_data
      is_create_control = is_create_control
    RECEIVING
      ro_entity         = ro_entity.

ENDMETHOD.


METHOD CREATE_STRUCT_RTTI.
  DATA:
    lt_component TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS:
    <ls_component> LIKE LINE OF lt_component.



  me->mv_unchangeable_keys = 'PARTNERROLE'.
  super->create_struct_rtti( ).

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  <ls_component>-name = cv_comp_bol_attr && '_WITH_OCA'.
  <ls_component>-type = mo_struct_rtti.
  <ls_component>-as_include = abap_true.

  APPEND INITIAL LINE TO lt_component ASSIGNING <ls_component>.
  <ls_component>-name = 'PARTNERROLE__TEXT'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  mo_struct_rtti = cl_abap_structdescr=>create( lt_component ).

ENDMETHOD.


  METHOD FILTER_EMPLOYEE_ROLES.
*! Filter employee related roles from the given table.
*  - BUP003 Employee
*  - BBP005 Service Provider
*  - BBP010 Freelance
    DELETE ct_roles WHERE rolecategory = 'BUP003'
                       OR rolecategory = 'BBP005'
                       OR rolecategory = 'BBP010'.
    DELETE ct_roles WHERE role = 'BUP003'
                       OR role = 'BBP005'
                       OR role = 'BBP010'.
  ENDMETHOD.


METHOD GET_ATTR_TEXT.
  DATA:
    lv_role TYPE bu_partnerrole,
    ls_role_details TYPE bus_tb003_t.

  super->get_attr_text(
    EXPORTING
      io_access    = io_access
      iv_attr_name = iv_attr_name
    RECEIVING
      rv_text = rv_text
  ).

  IF iv_attr_name = 'PARTNERROLE'.
    io_access->get_property_as_value(
      EXPORTING iv_attr_name = iv_attr_name
      IMPORTING ev_result    = lv_role
    ).
    IF  lv_role IS NOT INITIAL
    AND lv_role = rv_text.
      CALL FUNCTION 'BUP_ROLES_GET_SINGLE'
        EXPORTING
          i_role    = lv_role
        IMPORTING
          e_tb003_t = ls_role_details
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc = 0.
        rv_text = ls_role_details-rltitl.
      ELSEIF sy-subrc = 1.
        CLEAR rv_text.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD GET_ATTR_VALUE_SET.
  CASE iv_attr_name.
    WHEN 'PARTNERROLE'.
      me->get_value_set_partnerrole(
        EXPORTING
          io_access    = io_access
          iv_attr_name = iv_attr_name
        IMPORTING
          et_value_set = et_value_set
      ).

    WHEN OTHERS.
      super->get_attr_value_set(
        EXPORTING
          io_access    = io_access
          iv_attr_name = iv_attr_name
        IMPORTING
          et_value_set = et_value_set
      ).
  ENDCASE.
ENDMETHOD.


METHOD GET_DEFAULT_DISPLAY_TYPE.
  CASE iv_name.
*    WHEN 'PARTNERROLE'.
*      rv_display_type = if_fpm_guibb_constants=>gc_display_type-drop_down_list_box.
    WHEN OTHERS.
      rv_display_type = super->get_default_display_type( iv_name ).
  ENDCASE.
ENDMETHOD.


METHOD GET_ENTITY_DATA.

  FIELD-SYMBOLS:
    <lv_description> TYPE string.

  super->get_entity_data(
    EXPORTING io_access = io_access
    CHANGING cs_data   = cs_data
  ).

  ASSIGN COMPONENT 'PARTNERROLE__TEXT' OF STRUCTURE cs_data TO <lv_description>.
  IF sy-subrc = 0.
    me->get_attr_text(
      EXPORTING
        io_access    = io_access
        iv_attr_name = 'PARTNERROLE'
      RECEIVING
        rv_text = <lv_description>
    ).
  ENDIF.

ENDMETHOD.


method GET_FIELD_UI_PROP.
  rs_property = super->get_field_ui_prop(
     io_entity         = io_entity
     iv_attr_name      = iv_attr_name
     iv_change_allowed = iv_change_allowed
     is_prop           = is_prop
  ).
  CASE iv_attr_name.
    WHEN 'PARTNERROLE'.
      IF io_entity IS BOUND.
        rs_property-read_only = io_entity->is_send_active( ).
      ENDIF.
  ENDCASE.
endmethod.


METHOD GET_VALUE_SET_PARTNERROLE.
*! Build the F4 for the roles.
  DATA:
    lo_bp_connector TYPE REF TO cl_bs_bp_connector_bol_rel,
    lo_root TYPE REF TO cl_crm_bol_entity,
    lo_entity LIKE me->mo_entity,
    lv_condition TYPE string,
    ls_role TYPE bus_tb003_t,
    lt_roles TYPE ty_roles,
    lo_iterator TYPE REF TO if_bol_entity_col_iterator.

  FIELD-SYMBOLS:
    <ls_value_set> LIKE LINE OF et_value_set.

  CLEAR et_value_set.
  IF me->mo_connector IS NOT BOUND.
    RETURN.
  ENDIF.

  "get all BP-roles that are defined in customizing
  CALL FUNCTION 'BUP_ROLES_GET_ALL'
    EXPORTING
      i_xtext   = abap_true
    TABLES
      t_tb003_t = lt_roles.

  "initial filters
  DELETE lt_roles WHERE xsuppress = abap_true.
  me->filter_employee_roles( CHANGING ct_roles = lt_roles ).

  "sort
  ls_role-posnr = '999'.
  MODIFY lt_roles FROM ls_role TRANSPORTING posnr WHERE posnr IS INITIAL.
  SORT lt_roles BY posnr rltitl.

  "depending on the category of the current business partner filter out
  "those BP-roles that are invalid for that particular category or that
  "are suppressed by customizing
  TRY.
      lo_bp_connector ?= me->mo_connector.
      lo_root = lo_bp_connector->get_parent( ).
    CATCH cx_sy_move_cast_error.
      RETURN.
  ENDTRY.
  IF lo_root IS BOUND AND lo_root->alive( ) = abap_true.
    CASE lo_root->get_property_as_string( 'CATEGORY' ).
      WHEN '1'. lv_condition = |xpers = 'X' AND|.  " Person
      WHEN '2'. lv_condition = |xorg = 'X' AND|.   " Organization
      WHEN '3'. lv_condition = |xgroup = 'X' AND|. " Group
    ENDCASE.
  ENDIF.
  lv_condition = lv_condition && | xsuppress = abap_false|
                              && | AND rolecategory IS NOT INITIAL|.

  "now fill the selection-table that defines which roles are to be shown
  "in the dropdown-list of BP-roles in the UI
  IF me->mo_collection IS BOUND.
    lo_iterator = me->mo_collection->get_iterator( ).
  ENDIF.
  LOOP AT lt_roles INTO ls_role WHERE (lv_condition).
    FREE: lo_entity.
    IF lo_iterator IS BOUND.
      lo_entity ?= lo_iterator->find_by_property(
        iv_attr_name = 'PARTNERROLE'
        iv_value     = ls_role-role ).
      IF lo_entity IS BOUND.
        CONTINUE.
      ENDIF.
    ENDIF.
    APPEND INITIAL LINE TO et_value_set ASSIGNING <ls_value_set>.
    <ls_value_set>-value = ls_role-role.
    IF ls_role-rltxt IS NOT INITIAL.
      <ls_value_set>-text = ls_role-rltitl.
    ELSE.
      <ls_value_set>-text = ls_role-role.
    ENDIF.
  ENDLOOP.

  "filter according to authorization
  cl_mdg_bs_fnd_bp_services=>filter_role_list_by_auth(
    EXPORTING iv_actvt = '01'
    CHANGING ct_role = et_value_set ).
ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST_PAGING~GET_DATA.
*! Get the data to be displayed.
  FIELD-SYMBOLS:
    <ls_field_usage> LIKE LINE OF ct_field_usage.

  super->if_fpm_guibb_list_paging~get_data(
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
    CHANGING
      ct_field_usage            = ct_field_usage
      ct_action_usage           = ct_action_usage
      ct_selected_lines         = ct_selected_lines
      cv_lead_index             = cv_lead_index
      ct_dnd_attributes         = ct_dnd_attributes ).

  READ TABLE ct_field_usage ASSIGNING <ls_field_usage>
    WITH KEY name = 'PARTNERROLE'.
  IF sy-subrc = 0.
    me->get_attr_value_set(
      EXPORTING
        iv_attr_name = <ls_field_usage>-name
      IMPORTING
        et_value_set = <ls_field_usage>-fixed_values ).
      <ls_field_usage>-fixed_values_changed = abap_true.
      ev_field_usage_changed = abap_true.
  ENDIF.
ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~FLUSH.
*! Flush changes.
*
* If the value has been chosen from the OVS search help,
* then the FPM refresh will be triggered to support
* behavior like a drop-down box. In this case the value
* will be updated twice. If there are several records
* being created, then it might happen, that the changes
* will be sent for the wrong line. Therefore it should
* be ignored.
  DATA lt_change_log TYPE fpmgb_t_changelog.
  DATA ls_change_log LIKE LINE OF lt_change_log.

  lt_change_log = it_change_log.
  DELETE lt_change_log WHERE name = 'PARTNERROLE__TEXT'.
  IF lines( it_change_log ) EQ 1.
    READ TABLE it_change_log INTO ls_change_log
      WITH KEY name = 'PARTNERROLE'.
    IF sy-subrc EQ 0 AND ls_change_log-line_index NE iv_new_lead_sel.
      RETURN.
    ENDIF.
  ENDIF.

  super->if_fpm_guibb_list~flush(
    it_change_log   = it_change_log
    it_data         = it_data
    iv_old_lead_sel = iv_old_lead_sel
    iv_new_lead_sel = iv_new_lead_sel
  ).

ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~GET_DATA.
*! Get the data to be displayed.
  FIELD-SYMBOLS:
    <ls_field_usage> LIKE LINE OF ct_field_usage.

  super->if_fpm_guibb_list~get_data(
    EXPORTING
      iv_eventid                = iv_eventid
      it_selected_fields        = it_selected_fields
      iv_raised_by_own_ui       = iv_raised_by_own_ui
      iv_visible_rows           = iv_visible_rows
      iv_edit_mode              = iv_edit_mode
      io_extended_ctrl          = io_extended_ctrl
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
      ct_dnd_attributes         = ct_dnd_attributes ).

  READ TABLE ct_field_usage ASSIGNING <ls_field_usage>
    WITH KEY name = 'PARTNERROLE'.
  IF sy-subrc = 0.
    me->get_attr_value_set(
      EXPORTING
        iv_attr_name = <ls_field_usage>-name
      IMPORTING
        et_value_set = <ls_field_usage>-fixed_values ).
      <ls_field_usage>-fixed_values_changed = abap_true.
      ev_field_usage_changed = abap_true.
  ENDIF.
ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~GET_DEFINITION.

  FIELD-SYMBOLS:
    <ls_field_description> LIKE LINE OF et_field_description.

  super->if_fpm_guibb_list~get_definition(
    IMPORTING
      eo_field_catalog         = eo_field_catalog
      et_field_description     = et_field_description
      et_action_definition     = et_action_definition
      et_special_groups        = et_special_groups
      es_message               = es_message
      ev_additional_error_info = ev_additional_error_info
      et_dnd_definition        = et_dnd_definition
      et_row_actions           = et_row_actions
      es_options = es_options
  ).

  READ TABLE et_field_description ASSIGNING <ls_field_description>
    WITH KEY name = 'PARTNERROLE'.
  IF sy-subrc = 0.
    "Implement the BP Role as edit field with Value Help
    "to control the possible entries for each new entry
    <ls_field_description>-ovs_name = me->class_name.

  ENDIF.

  es_options-allow_create_rows_by_list_uibb = abap_true.
ENDMETHOD.


METHOD OVS_HANDLE_PHASE_2.

  CASE iv_field_name.
    WHEN 'PARTNERROLE'.
      me->ovs_output_partnerrole(
        EXPORTING
          iv_field_name      = iv_field_name
          ir_query_parameter = ir_query_parameter
          io_access = io_access
        IMPORTING
          er_output          = er_output
          ev_table_header    = ev_table_header
          et_column_texts    = et_column_texts
      ).

    WHEN OTHERS.
      super->ovs_handle_phase_2(
        EXPORTING
          iv_field_name      = iv_field_name
          ir_query_parameter = ir_query_parameter
          io_access = io_access
        IMPORTING
          er_output          = er_output
          ev_table_header    = ev_table_header
          et_column_texts    = et_column_texts
      ).
  ENDCASE.

ENDMETHOD.


METHOD OVS_HANDLE_PHASE_3.
  CALL METHOD super->ovs_handle_phase_3
    EXPORTING
      iv_field_name  = iv_field_name
      ir_selection   = ir_selection
    IMPORTING
      et_field_value = et_field_value
      eo_fpm_event   = eo_fpm_event.

  CASE iv_field_name.
    WHEN 'PARTNERROLE'.
      "Trigger flush as it works for drop-down box
      eo_fpm_event = cl_fpm_event=>create_by_id( iv_event_id = if_fpm_constants=>gc_event-refresh ).
  ENDCASE.

ENDMETHOD.


METHOD OVS_OUTPUT_PARTNERROLE.

  DATA:
    lt_value_set TYPE wdr_context_attr_value_list,
    ls_value_set TYPE wdr_context_attr_value,
    ls_partnerrole_text TYPE ty_partnerrole_text.

  FIELD-SYMBOLS:
   <lt_partnerrole_texts> TYPE ty_partnerrole_text_t.

  "ensure that this coding only runs for the proper field
  CHECK iv_field_name = 'PARTNERROLE'.

  CREATE DATA er_output TYPE ty_partnerrole_text_t.
  ASSIGN er_output->* TO <lt_partnerrole_texts>.

  CLEAR <lt_partnerrole_texts>.

  me->get_attr_value_set(
    EXPORTING
      io_access    = io_access
      iv_attr_name = iv_field_name
    IMPORTING
      et_value_set = lt_value_set
  ).

  LOOP AT lt_value_set INTO ls_value_set.
    ls_partnerrole_text-partnerrole = ls_value_set-value.
    ls_partnerrole_text-text = ls_value_set-text.
    APPEND ls_partnerrole_text TO <lt_partnerrole_texts>.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.

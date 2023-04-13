class ZCL_BS_BP_GUIBB_ID_NUMBERS1 definition
  public
  inheriting from CL_BS_BP_GUIBB_LIST
  create public .

public section.

  types:
    BEGIN OF element_ovs_countries,
              country TYPE bu_idcountry,
            END OF element_ovs_countries .
  types:
    elements_ovs_countries TYPE STANDARD TABLE OF element_ovs_countries .

  methods IF_FPM_GUIBB_LIST~FLUSH
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_LIST~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_OVS~HANDLE_PHASE_0
    redefinition .
protected section.

  data MS_OVS_STATIC_ATTRIBUTES type BSS_BPIL_IDENTIFICATIONNUM .
  data MT_OVS_COUNTRIES type ELEMENTS_OVS_COUNTRIES .
  class-data GT_ID_TYPE_PROPTY type BUP_TS_IDEN_TB039 .
  data CV_COUNT type CHAR6 .

  methods GET_VALUE_SET_ID_TYPE
    importing
      !IO_ACCESS type ref to IF_BOL_BO_PROPERTY_ACCESS optional
      !IV_OBJECT_NAME type CRMT_EXT_OBJ_NAME optional
      !IV_ATTR_NAME type NAME_KOMP
    exporting
      !ET_VALUE_SET type WDR_CONTEXT_ATTR_VALUE_LIST .
  methods OVS_OUTPUT_REGION
    importing
      !IV_FIELD_NAME type NAME_KOMP
      !IR_QUERY_PARAMETER type ref to DATA
    exporting
      !ER_OUTPUT type ref to DATA
      !EV_TABLE_HEADER type STRING
      !ET_COLUMN_TEXTS type WDR_NAME_VALUE_LIST .

  methods CREATE_STRUCT_RTTI
    redefinition .
  methods GET_ATTR_VALUE_SET
    redefinition .
  methods GET_DEFAULT_DISPLAY_TYPE
    redefinition .
  methods GET_ENTITY_DATA
    redefinition .
  methods GET_FIELD_UI_PROP
    redefinition .
  methods IS_ROW_ACTION_ENABLED
    redefinition .
  methods OVS_HANDLE_PHASE_2
    redefinition .
private section.
*"* private components of class ZCL_BS_BP_GUIBB_ID_NUMBERS1
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BS_BP_GUIBB_ID_NUMBERS1 IMPLEMENTATION.


METHOD CREATE_STRUCT_RTTI.
  DATA:
    lt_components TYPE cl_abap_structdescr=>component_table.

  FIELD-SYMBOLS:
    <ls_component> LIKE LINE OF lt_components.



  me->mv_unchangeable_keys = 'IDENTIFICATIONTYPE,IDENTIFICATIONNUMBER'.
  super->create_struct_rtti( ).

  lt_components = me->mo_struct_rtti->get_components( ).

*** Create the text fields for input fields dynamically
  APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
  <ls_component>-name = 'COUNTRY__TEXT'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
  <ls_component>-name = 'REGION__TEXT'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  APPEND INITIAL LINE TO lt_components ASSIGNING <ls_component>.
  <ls_component>-name = 'IDENTIFICATIONTYPE__TEXT'.
  <ls_component>-type = cl_abap_elemdescr=>get_string( ).

  me->mo_struct_rtti = cl_abap_structdescr=>create( lt_components ).
ENDMETHOD.


METHOD GET_ATTR_VALUE_SET.
  "The description fields are only the text fields without the value tables
  CHECK iv_attr_name NP '*__TEXT'.

  super->get_attr_value_set(
    EXPORTING
      io_access      = io_access
      iv_object_name = iv_object_name
      iv_attr_name   = iv_attr_name
    IMPORTING
      et_value_set   = et_value_set
  ).

  CASE iv_attr_name.
    WHEN 'IDENTIFICATIONTYPE'.
      me->get_value_set_id_type(
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
    WHEN OTHERS.
      rv_display_type = super->get_default_display_type( iv_name ).
  ENDCASE.
ENDMETHOD.


METHOD GET_ENTITY_DATA.
  FIELD-SYMBOLS:
    <lv_data> TYPE any.



  CALL METHOD super->get_entity_data
    EXPORTING
      io_access = io_access
    CHANGING
      cs_data   = cs_data.


  ASSIGN COMPONENT 'IDENTIFICATIONTYPE' OF STRUCTURE cs_data TO <lv_data>.
  IF sy-subrc = 0 .
    IF <lv_data> = '' .
      IF cv_count < 1.
        Cv_count = cv_count + 1.

      ENDIF.
      io_access->set_property(
      EXPORTING iv_attr_name = 'IDENTIFICATIONTYPE'
        iv_value = cv_count  ).
    ENDIF.
  ENDIF.

* Read the descriptions for input fields with key values
  ASSIGN COMPONENT 'IDENTIFICATIONTYPE__TEXT' OF STRUCTURE cs_data TO <lv_data>.
  IF sy-subrc = 0.
    <lv_data> = io_access->get_property_text( 'IDENTIFICATIONTYPE' ).
  ENDIF.

ENDMETHOD.


METHOD GET_FIELD_UI_PROP.

  DATA: lv_id_type   TYPE bu_id_type,
        lv_id_number TYPE bu_id_number,
        lt_tb039     TYPE TABLE OF tb039,
        ls_ID_TYPE_PROPTY LIKE LINE OF lt_tb039.


  rs_property = super->get_field_ui_prop(
     io_entity         = io_entity
     iv_attr_name      = iv_attr_name
     iv_change_allowed = iv_change_allowed
     is_prop           = is_prop
  ).
*get properties for Identification category
  IF GT_ID_TYPE_PROPTY[] IS INITIAL.
   SELECT * FROM tb039 INTO TABLE GT_ID_TYPE_PROPTY.
  ENDIF.

* Set these key-fields to read-only after roundtrip
  CASE iv_attr_name.
    WHEN 'IDENTIFICATIONTYPE'.
      IF io_entity IS BOUND.
        io_entity->get_property_as_value(
        EXPORTING iv_attr_name = 'IDENTIFICATIONTYPE'
        IMPORTING ev_result    = lv_id_type
      ).
        IF lv_id_type IS NOT INITIAL AND
           io_entity->is_send_active( ) = abap_true.
          rs_property-read_only = abap_true.
        ENDIF.
      ENDIF.

    WHEN 'IDENTIFICATIONNUMBER'.
      IF io_entity IS BOUND.
        io_entity->get_property_as_value(
        EXPORTING iv_attr_name = 'IDENTIFICATIONNUMBER'
        IMPORTING ev_result    = lv_id_number
      ).
        IF lv_id_number IS NOT INITIAL AND
          io_entity->is_send_active( ) = abap_true.
          rs_property-read_only = abap_true.
        ENDIF.
      ENDIF.
  ENDCASE.

* Read Customization for Identification category for editable/readonly
     IF io_entity IS BOUND.
        io_entity->get_property_as_value(
        EXPORTING iv_attr_name = 'IDENTIFICATIONTYPE'
        IMPORTING ev_result    = lv_id_type
      ).
        READ TABLE GT_ID_TYPE_PROPTY INTO ls_ID_TYPE_PROPTY WITH KEY
                                    CATEGORY = lv_id_type.
        IF lv_id_type IS NOT INITIAL AND
           ls_ID_TYPE_PROPTY-ID_DISPLAY_ONLY = abap_true.
          rs_property-read_only = abap_true.
        ENDIF.
    ENDIF.

ENDMETHOD.


METHOD GET_VALUE_SET_ID_TYPE.
  DATA:
    lo_connector TYPE REF TO cl_bs_bp_connector_bol_rel,
    lo_parent TYPE REF TO cl_crm_bol_entity,
    lv_bp_category TYPE bu_type,
    lt_valid_idtypes TYPE STANDARD TABLE OF tb039a,
    lt_display_idtypes TYPE STANDARD TABLE OF tb039,
    ls_value_set LIKE LINE OF et_value_set.

  IF me->mo_connector IS BOUND.
    lo_connector ?= me->mo_connector.
    lo_parent = lo_connector->get_parent( ).
   IF lo_parent IS BOUND.
    lo_parent->get_property_as_value(
      EXPORTING iv_attr_name = 'CATEGORY'
      IMPORTING ev_result    = lv_bp_category
    ).
   ENDIF.
  ENDIF.

  CASE lv_bp_category.
    WHEN '1'. "person
      SELECT * FROM tb039a INTO TABLE lt_valid_idtypes
        WHERE xperson = abap_true AND category <> space.
    WHEN '2'. "organization
      SELECT * FROM tb039a INTO TABLE lt_valid_idtypes
        WHERE xorganisation = abap_true AND category <> space.
    WHEN '3'. "group
      SELECT * FROM tb039a INTO TABLE lt_valid_idtypes
        WHERE xgroup = abap_true AND category <> space.
  ENDCASE.

  SELECT * FROM tb039 INTO TABLE lt_display_idtypes
    WHERE id_display_only = abap_true.

  LOOP AT et_value_set INTO ls_value_set.
    IF NOT line_exists( lt_valid_idtypes[ type = ls_value_set-value ] )
       OR line_exists( lt_display_idtypes[ category = ls_value_set-value ] ).
        DELETE et_value_set.
    ENDIF.
  ENDLOOP.
  SORT et_value_set BY text.

ENDMETHOD.


  METHOD IF_FPM_GUIBB_LIST~FLUSH.


FIELD-SYMBOLS:
    <ls_carr_key> TYPE any,
    <value> TYPE any.

      super->if_fpm_guibb_list~flush(
        it_change_log   = it_change_log
        it_data         = it_data
        iv_old_lead_sel = iv_old_lead_sel
        iv_new_lead_sel = iv_new_lead_sel
      ).
      IF it_change_log IS NOT INITIAL.
        me->mv_refresh_data = abap_true.
      ENDIF.

*ASSIGN it_data->*  to <ls_carr_key>.
*
*LOOP AT <ls_carr_key> ASSIGNING FIELD-SYMBOL(<b>).
*  ASSIGN COMPONENT 'identificationtype' OF STRUCTURE <b> TO <value>.
*  IF <value> is INITIAL.
*<value>  = 'TM0002'.
*ENDIF.
*
*ENDLOOP.






  ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~GET_DATA.


DATA l_table    TYPE REF TO data.
FIELD-SYMBOLS:   <fs_t_data> TYPE STANDARD TABLE,
                 <fs_s_data> TYPE any,
                 <fs_i_data> TYPE any.

CREATE DATA l_table LIKE   ct_data.
ASSIGN l_table->* TO <fs_t_data>.
<fs_t_data> = ct_data.

LOOP AT <fs_t_data> ASSIGNING <fs_s_data>.
ASSIGN COMPONENT 'IDENTIFICATIONTYPE' OF STRUCTURE <fs_s_data> TO  <fs_i_data>.
IF  <fs_i_data> >= cv_count.
  cv_count  =  <fs_i_data> + 1.
ENDIF.
ENDLOOP.

*LOOP AT <fs_t_data> ASSIGNING <fs_b_data>.
*ASSIGN COMPONENT 'IDENTIFICATIONTYPE' OF STRUCTURE <fs_b_data> TO   <fs_i_data>.
*IF  <fs_i_data> = ''.
*ASSIGN COMPONENT 'FPM_KEY_BY_BOL_ENTITY' OF STRUCTURE <fs_b_data> TO   <fs_o_ref>.
*lo_bol_ref = <fs_o_ref>.
*
*
*CALL METHOD lo_bol_ref->if_bol_bo_property_access~set_property
* EXPORTING
*   iv_attr_name = 'IDENTIFICATIONTYPE'
*   iv_value     = ms_max + 1.
*ENDIF.
*ENDLOOP.


  FIELD-SYMBOLS:
    <ls_field_usage> LIKE LINE OF ct_field_usage.

  super->if_fpm_guibb_list~get_data(
    EXPORTING
      iv_eventid                = iv_eventid
      it_selected_fields        = it_selected_fields
      iv_raised_by_own_ui       = iv_raised_by_own_ui
      iv_visible_rows           = iv_visible_rows
      iv_edit_mode = iv_edit_mode
      io_extended_ctrl = io_extended_ctrl
    IMPORTING
      et_messages               = et_messages
      ev_data_changed           = ev_data_changed
      ev_field_usage_changed    = ev_field_usage_changed
      ev_action_usage_changed   = ev_action_usage_changed
      ev_selected_lines_changed = ev_selected_lines_changed
      ev_dnd_attr_changed       = ev_dnd_attr_changed
      eo_itab_change_log = eo_itab_change_log
    CHANGING
      ct_data                   = ct_data
      ct_field_usage            = ct_field_usage
      ct_action_usage           = ct_action_usage
      ct_selected_lines         = ct_selected_lines
      cv_lead_index             = cv_lead_index
      cv_first_visible_row      = cv_first_visible_row
      cs_additional_info        = cs_additional_info
      ct_dnd_attributes         = ct_dnd_attributes
  ).


DATA lr TYPE REF TO data.

FIELD-SYMBOLS <fs> TYPE ANY.
CREATE DATA lr LIKE ct_data.

ASSIGN lr->* TO <fs>.
FIELD-SYMBOLS <fs_another> TYPE ANY .
LOOP AT ct_data ASSIGNING <fs> .
ASSIGN COMPONENT 'IDENTIFICATIONTYPE' OF STRUCTURE <fs> TO <fs_another> .
IF <fs_another> is INITIAL  .

<fs_another> = cv_count .
ENDIF.
ENDLOOP.







  READ TABLE ct_field_usage ASSIGNING <ls_field_usage>
             WITH KEY name = 'IDENTIFICATIONTYPE'.
  IF sy-subrc = 0.
    IF <ls_field_usage>-fixed_values IS INITIAL. "we have to read
      me->get_attr_value_set(
        EXPORTING
          iv_object_name = me->ms_object_key-object_name
          iv_attr_name = <ls_field_usage>-name
        IMPORTING
          et_value_set = <ls_field_usage>-fixed_values
      ).
      <ls_field_usage>-fixed_values_changed = abap_true.
      ev_field_usage_changed = abap_true.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_LIST~GET_DEFINITION.
  DATA:
    lo_connector   TYPE REF TO cl_bs_bp_connector_bol_rel,
    lo_parent      TYPE REF TO cl_crm_bol_entity,
    lv_bp_category TYPE bu_type.

  FIELD-SYMBOLS:
    <ls_field_description> LIKE LINE OF et_field_description.



  CALL METHOD super->if_fpm_guibb_list~get_definition
    IMPORTING
      eo_field_catalog         = eo_field_catalog
      et_field_description     = et_field_description
      et_action_definition     = et_action_definition
      et_special_groups        = et_special_groups
      es_message               = es_message
      ev_additional_error_info = ev_additional_error_info
      et_dnd_definition        = et_dnd_definition
      et_row_actions           = et_row_actions
      es_options               = es_options.

* Add search help (OVS) to UI field
  READ TABLE et_field_description ASSIGNING <ls_field_description>
             WITH KEY name = 'IDENTIFICATIONTYPE'.
  IF sy-subrc = 0.
*    "depending on the category of the current business partner filter out
*    "those IDTYPES that are invalid for that particular category or that
*    "are suppressed by customizing
*    IF me->mo_connector IS BOUND.
*      lo_connector ?= me->mo_connector.
*      lo_parent = lo_connector->get_parent( ).
*      lo_parent->get_property_as_value(
*        EXPORTING iv_attr_name = 'CATEGORY'
*        IMPORTING ev_result    = lv_bp_category
*      ).
*    ENDIF.
*
*    CASE lv_bp_category.
*      WHEN '1'. "person
*        <ls_field_description>-ddic_shlp_name = 'BUPA_IDTYPE_PRS'.
*      WHEN '2'. "organization
*        <ls_field_description>-ddic_shlp_name = 'BUPA_IDTYPE_ORG'.
*      WHEN '3'. "group
*        <ls_field_description>-ddic_shlp_name = 'BUPA_IDTYPE_GRP'.
*    ENDCASE.
*
*    IF <ls_field_description>-ddic_shlp_name IS INITIAL.
*      <ls_field_description>-ddic_shlp_name = 'BUPA_IDTYPE_ORG'.
*    ENDIF.

  ENDIF.

  READ TABLE et_field_description ASSIGNING <ls_field_description>
             WITH KEY name = 'REGION'.
  IF sy-subrc = 0.
    <ls_field_description>-ovs_name = me->class_name.
    "The fixed values are not needed in the case of OVS
    CLEAR <ls_field_description>-fixed_values[].
  ENDIF.

  READ TABLE et_field_description ASSIGNING <ls_field_description>
             WITH KEY name = 'COUNTRY'.
  IF sy-subrc = 0.
    <ls_field_description>-ddic_shlp_name = 'BS_SEARCH_COUNTRY'.
  ENDIF.
ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_0.
*  "determine the data that has already been maintained by the user without
*  "triggering a server round-trip (no FLUSH)
  io_ovs_callback->context_element->get_static_attributes( IMPORTING static_attributes = me->ms_ovs_static_attributes  ).
  super->if_fpm_guibb_ovs~handle_phase_0(
    iv_field_name   = iv_field_name
    io_ovs_callback = io_ovs_callback
  ).
ENDMETHOD.


method IS_ROW_ACTION_ENABLED.

  DATA: lv_id_type        TYPE bu_id_type,
        lt_tb039          TYPE TABLE OF tb039,
        ls_id_type_propty LIKE LINE OF lt_tb039.

  "inherit first
  rv_enabled = super->is_row_action_enabled( iv_event_id = iv_event_id
                                             io_entity   = io_entity ).

  IF rv_enabled = abap_false.
    RETURN.
  ENDIF.

  CASE iv_event_id.
    WHEN cv_event_id_row_delete.
      IF io_entity IS BOUND AND GT_ID_TYPE_PROPTY[] IS NOT INITIAL.
        io_entity->get_property_as_value(
          EXPORTING iv_attr_name = 'IDENTIFICATIONTYPE'
          IMPORTING ev_result    = lv_id_type ).
        READ TABLE GT_ID_TYPE_PROPTY INTO ls_id_type_propty WITH KEY CATEGORY = lv_id_type.
        IF sy-subrc = 0 AND ls_id_type_propty-id_display_only = abap_true.
          " Display only -- disable delete icon
          rv_enabled = abap_false.
        ENDIF.
      ENDIF.
  ENDCASE.

endmethod.


METHOD OVS_HANDLE_PHASE_2.
  CASE iv_field_name.
    WHEN 'REGION'.
      me->ovs_output_region(
        EXPORTING
          iv_field_name      = iv_field_name
          ir_query_parameter = ir_query_parameter
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


METHOD OVS_OUTPUT_REGION.
  TYPES:
    BEGIN OF ty_region_text,
      region TYPE t005u-bland, "name like in target stucture!
      text TYPE t005u-bezei,
    END OF ty_region_text,
    ty_region_texts TYPE STANDARD TABLE OF ty_region_text.

  FIELD-SYMBOLS:
    <lt_region_texts> TYPE ty_region_texts,
    <ls_region_text> LIKE LINE OF <lt_region_texts>.



  "ensure that this coding only runs for the proper field
  CHECK iv_field_name = 'REGION'.
  "define the type of the list to be returned, laterly filled with the
  "descriptions of the regions that are available for the given country
  CREATE DATA er_output TYPE ty_region_texts.
  ASSIGN er_output->* TO <lt_region_texts>.
  "now get all regions that are defined for the given country - and if no
  "country was entered yet the list of available regions is empty
  IF me->ms_ovs_static_attributes-country IS NOT INITIAL.
    "write the key value in both fields, the key field AND the text field...
    SELECT bland bland FROM t005s APPENDING TABLE <lt_region_texts>
           WHERE land1 = me->ms_ovs_static_attributes-country.
    "...because if no text ist maintained in the logon language at least the
    "key is available as description and indicating to the user that the text
    "is missing
    LOOP AT <lt_region_texts> ASSIGNING <ls_region_text>.
      SELECT SINGLE bezei FROM t005u INTO <ls_region_text>-text
        WHERE land1 = me->ms_ovs_static_attributes-country
        AND bland = <ls_region_text>-region
        AND spras = sy-langu.
    ENDLOOP.
    "clean up the returning table a bit
    SORT <lt_region_texts>.
  ENDIF.
ENDMETHOD.
ENDCLASS.

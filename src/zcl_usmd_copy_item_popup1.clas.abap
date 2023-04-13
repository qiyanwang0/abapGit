class ZCL_USMD_COPY_ITEM_POPUP1 definition
  public
  final
  create public .

public section.
  type-pools USMD0 .

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_FORM .
  interfaces IF_FPM_GUIBB_DYNAMIC_CONFIG .
  interfaces IF_FPM_GUIBB_OVS .

  data MV_CLASS_NAME type ABAP_ABSTYPENAME .
  constants GV_EVENT_COPY_SR_ITEM type FPM_EVENT_ID value 'COPY_SR_ITEM' ##NO_TEXT.

  methods CONSTRUCTOR .
  class-methods GET_DDIC_INFO
    importing
      !IV_DATA_ELEM type ROLLNAME
    returning
      value(RS_DDIC_INFO) type DFIES .
  class-methods IS_POPUP_NEEDED
    importing
      !IV_MODEL type USMD_MODEL
      !IV_ENTITY type USMD_ENTITY
    returning
      value(RV_IS_NEEDED) type ABAP_BOOL .
protected section.

  types:
    BEGIN OF ty_s_field,
      fieldname     TYPE usmd_fieldname,
      label         TYPE string,
      tooltip       TYPE string,
      rollname      TYPE rollname,
      shlpname      TYPE usmd_search_hlp,
      ovs_name      TYPE seoclsname,
      mandatory     TYPE abap_bool,
      prefill_value TYPE abap_bool,  "prefill field from source object key
      int_key       TYPE abap_bool,  "internal key assignment
      super_key     TYPE abap_bool,  "field is superordinate key
      END OF ty_s_field .
  types:
    ty_t_field TYPE STANDARD TABLE OF ty_s_field WITH KEY fieldname .
  types:
    BEGIN OF ty_s_ovs_out_edition,
          usmd_edition    TYPE usmd_edition,
          txtmi           TYPE usmd_txtmi,
          usmd_vdate_from TYPE usmd_edition_vdate_from,
          usmd_vper_from  TYPE usmd_edition_vper_from,
        END OF ty_s_ovs_out_edition .
  types:
    ty_t_ovs_out_edition TYPE STANDARD TABLE OF ty_s_ovs_out_edition
      WITH KEY usmd_edition .
  types:
    BEGIN OF ty_s_ovs_out_edition_date,
          usmd_edition    TYPE usmd_edition,
          txtmi           TYPE usmd_txtmi,
          usmd_vdate_from TYPE usmd_edition_vdate_from,
          END OF ty_s_ovs_out_edition_date .
  types:
    ty_t_ovs_out_edition_date TYPE STANDARD TABLE OF ty_s_ovs_out_edition_date
      WITH KEY usmd_edition .
  types:
    BEGIN OF ty_s_ovs_out_edition_period,
          usmd_edition    TYPE usmd_edition,
          txtmi           TYPE usmd_txtmi,
          usmd_vper_from  TYPE usmd_edition_vper_from,
          END OF ty_s_ovs_out_edition_period .
  types:
    ty_t_ovs_out_edition_period TYPE STANDARD TABLE OF ty_s_ovs_out_edition_period
      WITH KEY usmd_edition .
  types:
    BEGIN OF ty_s_super_entity,
      usmd_entity   TYPE usmd_entity,
      key_field_tab TYPE usmd_t_fieldname,
    END OF ty_s_super_entity .
  types:
    ty_t_super_entity TYPE STANDARD TABLE OF ty_s_super_entity
      WITH KEY usmd_entity .

  data MO_MODEL type ref to IF_USMD_MODEL .
  data MV_MODEL type USMD_MODEL .
  data MV_MAIN_ENTITY type USMD_ENTITY .
  data MV_ENTITY_TEXT type STRING .
  data MR_DATA type ref to DATA .
  data MR_DATA_OVS type ref to DATA .
  data MO_DATA_RTTI type ref to CL_ABAP_STRUCTDESCR .
  data MT_KEY_FIELD type TY_T_FIELD .
  data MR_DATA_FROM type ref to DATA .
  data MO_EDITION_API type ref to IF_USMD_EDITION_API .
  data MV_EDITION_TIMEBASE type USMD_EDITION_TIMEBASE .
  data MV_EDITION_TYPE type USMD_EDITION_TYPE .
  data MV_EDITION_PERIV type PERIV .
  data MV_EDITION_MAX_PERIOD type ANZBP .
  data MV_EDITION_BASED type ABAP_BOOL .
  data MV_COPY_ACROSS_EDT_ENABLED type ABAP_BOOL .
  data MT_SUPER_ENTITY_WITH_EDT type TY_T_SUPER_ENTITY .
  data MV_FEBYSV_BUFFER_FILLED type ABAP_BOOL .

  methods PROCESS_GENERIC_OVS
    importing
      !IV_FIELD_NAME type NAME_KOMP
      !IO_OVS_CALLBACK type ref to IF_WD_OVS .
  methods CHECK_EXISTENCE
    importing
      !IS_DATA type DATA
      !IV_CONSIDER_EDITION type ABAP_BOOL default ABAP_FALSE
      !IV_CHECK_LOCKING type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_FFIX type USMD_TS_VALUE
      !ET_MESSAGE type FPMGB_T_MESSAGES .
  methods PROCESS_EDITION_OVS
    importing
      !IV_FIELD_NAME type NAME_KOMP
      !IO_OVS_CALLBACK type ref to IF_WD_OVS
      !IV_WD_CONTEXT_ATTR_NAME type STRING optional
    exporting
      !EO_FPM_EVENT type ref to CL_FPM_EVENT .
  methods GET_VALID_EDITIONS
    importing
      !IS_KEY_TO type DATA
    exporting
      !ET_EDITION type USMD_T_EDITIONS
      !ET_EDITION_INFO type INDEX TABLE
      !ET_MESSAGE type FPMGB_T_MESSAGES .
  methods GET_PREV_PERIOD
    importing
      !IV_PERIOD type USMD_EDITION_VPER
    returning
      value(RV_PREV_PERIOD) type USMD_EDITION_VPER .
  methods GET_NEXT_PERIOD
    importing
      !IV_PERIOD type USMD_EDITION_VPER
    returning
      value(RV_NEXT_PERIOD) type USMD_EDITION_VPER .
  methods GET_PREV_DATE
    importing
      !IV_DATE type USMD_EDITION_VDATE
    returning
      value(RV_PREV_DATE) type USMD_EDITION_VDATE .
  methods GET_NEXT_DATE
    importing
      !IV_DATE type USMD_EDITION_VDATE
    returning
      value(RV_NEXT_DATE) type USMD_EDITION_VDATE .
  methods ADD_FILTER_CRITERION
    importing
      !IS_FILTER_VALUE type ANY
      !IV_FIELDNAME type STRING
    changing
      !CV_FILTER_TXT type STRING .
  methods FILTER_EDITIONS_BY_SUPER_VAL
    importing
      !IS_KEY_TO type DATA
    changing
      !CT_EDITION type USMD_T_EDITIONS .
private section.
ENDCLASS.



CLASS ZCL_USMD_COPY_ITEM_POPUP1 IMPLEMENTATION.


METHOD PROCESS_GENERIC_OVS.
*--------------------------------------------------------------------*
* This method processes all phases of the generic OVS for the
* input field whose name is passed in IV_FIELD_NAME.
*--------------------------------------------------------------------*

  DATA:
    lo_model         TYPE REF TO if_usmd_model,
    lo_service       TYPE REF TO if_usmd_ui_services,
    lo_context_dummy TYPE REF TO if_wd_context_node ##needed,
    lt_message       TYPE usmd_t_message,
    lt_ffix          TYPE usmd_ts_value,
    lt_sel           TYPE usmd_ts_sel,
    lt_sel_attr      TYPE usmd_ts_sel,
    ls_ffix          LIKE LINE OF lt_ffix.

  FIELD-SYMBOLS:
    <ls_data>      TYPE data,
    <ls_key_field> LIKE LINE OF me->mt_key_field,
    <lv_value>     TYPE data.

  "Get instance of data model
  cl_usmd_model=>get_instance(
    EXPORTING
      i_usmd_model = me->mv_model
    IMPORTING
      eo_instance  = lo_model
      et_message   = lt_message ).
  IF lo_model IS INITIAL OR lt_message IS NOT INITIAL.
    "Unknown data model
    RETURN.
  ENDIF.

  "Check for values of superordinate input fields that could be used
  "to restrict the value set (like 'Country' = 'DE' for 'City')
  ASSIGN me->mr_data_ovs->* TO <ls_data>.
  LOOP AT me->mt_key_field ASSIGNING <ls_key_field>
    WHERE fieldname <> iv_field_name.
    ASSIGN COMPONENT <ls_key_field>-fieldname
      OF STRUCTURE <ls_data> TO <lv_value>.
    CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
    READ TABLE lo_model->dt_fdep
      WITH TABLE KEY masterfield COMPONENTS
        masterfield = <ls_key_field>-fieldname
        fieldname   = iv_field_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ls_ffix-fieldname = <ls_key_field>-fieldname.
      ls_ffix-value     = <lv_value>.
      INSERT ls_ffix INTO TABLE lt_ffix.
    ENDIF.
  ENDLOOP.

  "Call OVS functionality of MDG UI Services
  lo_service = cl_usmd_ui_services=>get_instance( ).
  lo_service->process_ovs_callback(
    EXPORTING
      i_fieldname     = iv_field_name
      io_model        = lo_model
      it_ffix         = lt_ffix
      io_context      = lo_context_dummy
      io_ovs_callback = io_ovs_callback
      it_sel          = lt_sel
      it_sel_attr     = lt_sel_attr ).

ENDMETHOD.


METHOD PROCESS_EDITION_OVS.
*--------------------------------------------------------------------*
* This method processes all phases of the Edition OVS.
*--------------------------------------------------------------------*

  DATA:
    lr_input          TYPE REF TO data,
    lr_output         TYPE REF TO data,
    lv_edition        TYPE usmd_edition,
    lv_input_strname  TYPE string,
    lv_output_tabname TYPE string,
    lv_filter         TYPE string,
    lt_message        TYPE fpmgb_t_messages,
    lt_ovs_message     TYPE io_ovs_callback->ty_t_messages,
    ls_ovs_message     TYPE io_ovs_callback->ty_s_message.

  FIELD-SYMBOLS:
    <lt_output>    TYPE INDEX TABLE,
    <ls_input>     TYPE data,
    <ls_selection> TYPE data,
    <ls_parameter> TYPE data,
    <ls_data_ovs>  TYPE data,
    <lv_edition>   TYPE usmd_edition,
    <ls_message>      LIKE LINE OF lt_message.

  CASE io_ovs_callback->phase_indicator.
    WHEN if_wd_ovs=>co_phase_0.

    WHEN if_wd_ovs=>co_phase_1.
      "Choose input structure based on time base
      CASE me->mv_edition_timebase.
        WHEN if_usmd_edition_constants=>gc_timebase_date.
          lv_input_strname = 'TY_S_OVS_OUT_EDITION_DATE'.
        WHEN if_usmd_edition_constants=>gc_timebase_period.
          lv_input_strname = 'TY_S_OVS_OUT_EDITION_PERIOD'.
        WHEN OTHERS.
          lv_input_strname = 'TY_S_OVS_OUT_EDITION'.
      ENDCASE.
      CREATE DATA lr_input TYPE (lv_input_strname).
      ASSIGN lr_input->* TO <ls_input>.

      "If current value of 'Edition' input field contains '*'
      "pass this value as selection
      io_ovs_callback->context_element->get_attribute(
        EXPORTING
          name  = io_ovs_callback->context_attribute
        IMPORTING
          value = lv_edition ).
      IF lv_edition CA '*'.
        ASSIGN COMPONENT if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition
          OF STRUCTURE <ls_input> TO <lv_edition>.
        IF sy-subrc = 0.
          <lv_edition> = lv_edition.
        ENDIF.
      ENDIF.

      "Set OVS input structure
      io_ovs_callback->set_input_structure(
        EXPORTING
          input                      = <ls_input>
          display_values_immediately = abap_true ).

    WHEN if_wd_ovs=>co_phase_2.
      "Choose output table based on time base
      CASE me->mv_edition_timebase.
        WHEN if_usmd_edition_constants=>gc_timebase_date.
          lv_output_tabname = 'TY_T_OVS_OUT_EDITION_DATE'.
        WHEN if_usmd_edition_constants=>gc_timebase_period.
          lv_output_tabname = 'TY_T_OVS_OUT_EDITION_PERIOD'.
        WHEN OTHERS.
          lv_output_tabname = 'TY_T_OVS_OUT_EDITION'.
      ENDCASE.
      CREATE DATA lr_output TYPE (lv_output_tabname).
      ASSIGN lr_output->* TO <lt_output>.

      "Determine all editions valid for the current use case
      ASSIGN me->mr_data_ovs->* TO <ls_data_ovs>.
      me->get_valid_editions(
        EXPORTING
          is_key_to       = <ls_data_ovs>
        IMPORTING
          et_edition_info = <lt_output>
          et_message = lt_message ).

      LOOP AT lt_message ASSIGNING <ls_message>.
        MOVE-CORRESPONDING <ls_message> TO ls_ovs_message-t100_message.
        ls_ovs_message-t100_message-msgty = <ls_message>-severity.
        ls_ovs_message-t100_message-p1 = <ls_message>-parameter_1.
        ls_ovs_message-t100_message-p2 = <ls_message>-parameter_2.
        ls_ovs_message-t100_message-p3 = <ls_message>-parameter_3.
        ls_ovs_message-t100_message-p4 = <ls_message>-parameter_4.
        APPEND ls_ovs_message TO lt_ovs_message.
      ENDLOOP.

      io_ovs_callback->set_messages( lt_ovs_message ).

      "Filter out editions not matching the selection
      IF <lt_output> IS NOT INITIAL AND
         io_ovs_callback->query_parameters IS BOUND.
        "Create filter conditions
        ASSIGN io_ovs_callback->query_parameters->* TO <ls_parameter>.
        me->add_filter_criterion(
          EXPORTING
            is_filter_value = <ls_parameter>
            iv_fieldname    = if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition
          CHANGING
            cv_filter_txt   = lv_filter ).
        me->add_filter_criterion(
          EXPORTING
            is_filter_value = <ls_parameter>
            iv_fieldname    = if_usmd_generic_bolui_const=>gc_fieldname_text_middle
          CHANGING
            cv_filter_txt   = lv_filter ).
        me->add_filter_criterion(
          EXPORTING
            is_filter_value = <ls_parameter>
            iv_fieldname    = if_usmd_generic_bolui_const=>gc_fieldname_usmd_valid_from
          CHANGING
            cv_filter_txt   = lv_filter ).
        me->add_filter_criterion(
          EXPORTING
            is_filter_value = <ls_parameter>
            iv_fieldname    = if_usmd_generic_bolui_const=>gc_fieldname_usmd_period_from
          CHANGING
            cv_filter_txt   = lv_filter ).
        IF lv_filter IS NOT INITIAL.
          "Apply filter conditions
          DELETE <lt_output> WHERE (lv_filter).
        ENDIF.
      ENDIF.

      "Set OVS output table
      io_ovs_callback->set_output_table(
        EXPORTING
          output = <lt_output> ).

    WHEN if_wd_ovs=>co_phase_3.
      "Get selected edition
      IF io_ovs_callback->selection IS NOT BOUND.
        RETURN.
      ENDIF.
      ASSIGN io_ovs_callback->selection->* TO <ls_selection>.
      ASSIGN COMPONENT if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition
        OF STRUCTURE <ls_selection> TO <lv_edition>.
      IF sy-subrc <> 0 OR <lv_edition> IS INITIAL.
        RETURN.
      ENDIF.

      "Write edition to OVS context
      io_ovs_callback->context_element->set_attribute(
        EXPORTING
          name  = io_ovs_callback->context_attribute
          value = <lv_edition> ).
      io_ovs_callback->context_element->set_changed_by_client( ).

  ENDCASE.

ENDMETHOD.


METHOD IS_POPUP_NEEDED.
*--------------------------------------------------------------------*
* This method determines whether it is necessary to display the
* 'Copy' popup during the copy process of the specified data model
* and entity type.
*--------------------------------------------------------------------*

  DATA:
    lo_model   TYPE REF TO if_usmd_model,
    lt_message TYPE usmd_t_message.

  FIELD-SYMBOLS:
    <ls_entity_prop> TYPE LINE OF usmd_ts_entity_prop.

  rv_is_needed = abap_false.

  IF iv_model IS INITIAL OR iv_entity IS INITIAL.
    "Necessary information missing
    RETURN.
  ENDIF.

  "Get instance of data model
  cl_usmd_model=>get_instance(
    EXPORTING
      i_usmd_model = iv_model
    IMPORTING
      eo_instance  = lo_model
      et_message   = lt_message ).
  IF lt_message IS NOT INITIAL.
    "Unknown data model
    RETURN.
  ENDIF.

  "The popup is only needed if no internal key assignment is defined
  "for the entity type, there are "superordinate" entity types, or
  "the entity type is edition-based
  READ TABLE lo_model->dt_entity_prop ASSIGNING <ls_entity_prop>
    WITH TABLE KEY usmd_entity = iv_entity.
  IF sy-subrc = 0.
    IF <ls_entity_prop>-key_handling <> usmd0_cs_key_handling-internal.
      rv_is_needed = abap_true.
    ELSEIF <ls_entity_prop>-validity_concept = usmd0_cs_validity_entity_type-edition AND
           cl_usmd_switch_check=>mdg_application_framework_5( ) = abap_true.
      rv_is_needed = abap_true.
    ELSE.
      LOOP AT lo_model->dt_fdep TRANSPORTING NO FIELDS
        WHERE fieldname = <ls_entity_prop>-r_fprop->fieldname AND
              masterfield <> <ls_entity_prop>-r_fprop->fieldname.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        rv_is_needed = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB~INITIALIZE.
*--------------------------------------------------------------------*
* This method determines the key fields of the current entity type
* (taken from the application context) and decides how they should
* be displayed on the screen.
*
* The method is called once when the corresponding UIBB is created
* (immediately after constructor).
*--------------------------------------------------------------------*

  DATA:
    lo_app_context  TYPE REF TO if_usmd_app_context,
    lt_message      TYPE usmd_t_message,
    lt_component    TYPE cl_abap_structdescr=>component_table,
    ls_component    LIKE LINE OF lt_component,
    ls_key_field    LIKE LINE OF me->mt_key_field,
    ls_edition_type TYPE usmd_s_edition_type,
    ls_ddic_info    TYPE dfies,
    lv_param_value  TYPE usmd_value.

  FIELD-SYMBOLS:
    <ls_entity_prop>  TYPE LINE OF usmd_ts_entity_prop,
    <ls_entity_prop2> TYPE LINE OF usmd_ts_entity_prop,
    <ls_fdep>         TYPE LINE OF usmd_ts_fdep,
    <ls_fprop>        TYPE LINE OF usmd_ts_fprop.

  "Read current model/entity from application context
  lo_app_context = cl_usmd_app_context=>get_context( ).
  IF lo_app_context IS BOUND.
    me->mv_model = lo_app_context->mv_usmd_model.
    lo_app_context->get_parameter(
      EXPORTING
        iv_parameter_name   = 'MAIN_ENTITY'
      IMPORTING
        ev_parameter_value  = lv_param_value ).
    me->mv_main_entity = lv_param_value.
  ENDIF.

  IF me->mv_model IS INITIAL OR me->mv_main_entity IS INITIAL.
    "Necessary information missing
    RETURN.
  ENDIF.

  "Get instance of data model
  cl_usmd_model=>get_instance(
    EXPORTING
      i_usmd_model = me->mv_model
    IMPORTING
      eo_instance  = me->mo_model
      et_message   = lt_message ).
  IF lt_message IS NOT INITIAL.
    "Unknown data model
    RETURN.
  ENDIF.

  "Get key fields of entity and collect key structure information
  READ TABLE me->mo_model->dt_entity_prop ASSIGNING <ls_entity_prop>
    WITH TABLE KEY usmd_entity = me->mv_main_entity.
  IF sy-subrc <> 0.
    "Unknown entity type
    RETURN.
  ENDIF.
  IF <ls_entity_prop>-validity_concept = usmd0_cs_validity_entity_type-edition.
    me->mv_edition_based = abap_true.
  ENDIF.
  LOOP AT me->mo_model->dt_fdep ASSIGNING <ls_fdep>
    WHERE fieldname = <ls_entity_prop>-r_fprop->fieldname.
    READ TABLE me->mo_model->dt_fprop ASSIGNING <ls_fprop>
      WITH TABLE KEY fieldname = <ls_fdep>-masterfield.
    CHECK sy-subrc = 0.
    CLEAR ls_key_field.
    ls_key_field-fieldname = <ls_fprop>-fieldname.
    "Set name of assigned DDIC element
    IF <ls_fprop>-rollname IS NOT INITIAL.
      ls_key_field-rollname = <ls_fprop>-rollname.
    ELSE.
      ls_key_field-rollname = 'STRING'.
    ENDIF.
    "Set explicit label (if maintained in data model)
    "(prefer medium text due to restricted space in dialog box)
    IF <ls_fprop>-scrtext_m IS NOT INITIAL.
      IF strlen( <ls_fprop>-scrtext_l ) > strlen( <ls_fprop>-scrtext_m ) AND
         strlen( <ls_fprop>-scrtext_l ) <= 20.
        "Sometimes, the 'L' label is also quite short but still more
        "readable than the 'M' label
        ls_key_field-label = <ls_fprop>-scrtext_l.
      ELSE.
        ls_key_field-label = <ls_fprop>-scrtext_m.
      ENDIF.
    ELSEIF <ls_fprop>-scrtext_l IS NOT INITIAL.
      ls_key_field-label = <ls_fprop>-scrtext_l.
    ELSEIF <ls_fprop>-reptext IS NOT INITIAL.
      ls_key_field-label = <ls_fprop>-reptext.
    ELSEIF <ls_fprop>-rollname IS INITIAL.
      "Fallback: Use technical field name
      ls_key_field-label = <ls_fprop>-fieldname.
    ENDIF.
    "Set explicit tooltip
    "(prefer the longest text since it carries most information)
    ls_key_field-tooltip = <ls_fprop>-reptext.
    IF strlen( <ls_fprop>-scrtext_l ) > strlen( ls_key_field-tooltip ).
      ls_key_field-tooltip = <ls_fprop>-scrtext_l.
    ENDIF.
    IF strlen( <ls_fprop>-scrtext_m ) > strlen( ls_key_field-tooltip ).
      ls_key_field-tooltip = <ls_fprop>-scrtext_m.
    ENDIF.
    IF ls_key_field-fieldname <> <ls_entity_prop>-r_fprop->fieldname.
      "For key fields inherited from "superordinate" entities the key
      "value is prefilled from the key of the source object
      ls_key_field-prefill_value = abap_true.
      ls_key_field-mandatory = abap_true.
      ls_key_field-super_key = abap_true.
    ELSE.
      IF me->mv_edition_based = abap_true AND
         me->mv_copy_across_edt_enabled = abap_true.
        "In 'Copy Across Editions' scenario, the key value is prefilled
        "from the key of the source object
        ls_key_field-prefill_value = abap_true.
      ENDIF.
      READ TABLE me->mo_model->dt_entity_prop ASSIGNING <ls_entity_prop2>
        WITH TABLE KEY usmd_entity = ls_key_field-fieldname. "#EC WARNOK
      IF sy-subrc = 0.
        "Key handling:
        "  + When the user has to provide the key, a mandatory input field
        "    is displayed
        "  + When the system creates the key, an information text is displayed
        "  + Otherwise, an optional input field is displayed
        CASE <ls_entity_prop2>-key_handling.
          WHEN usmd0_cs_key_handling-external OR usmd0_cs_key_handling-changeable.
            ls_key_field-mandatory = abap_true.
          WHEN usmd0_cs_key_handling-internal.
            ls_key_field-int_key = abap_true.
        ENDCASE.
      ENDIF.
    ENDIF.
    IF ls_key_field-int_key = abap_false.
      "Set search help information
      CLEAR ls_ddic_info.
      IF <ls_fprop>-shlpname IS NOT INITIAL.
        "DDIC search help from data model
        ls_key_field-shlpname = <ls_fprop>-shlpname.
      ELSE.
        "Get DDIC information of data element (to check for DDIC search help)
        ls_ddic_info = get_ddic_info( iv_data_elem = ls_key_field-rollname ).
      ENDIF.
      IF ls_key_field-shlpname IS INITIAL AND
         ls_ddic_info-mac = abap_false    AND
         <ls_fprop>-has_valhelp IS NOT INITIAL.
        "Use generic OVS search help
        ls_key_field-ovs_name = me->mv_class_name.
      ENDIF.
    ENDIF.
    INSERT ls_key_field INTO TABLE me->mt_key_field.
    CLEAR ls_component.
    ls_component-name = ls_key_field-fieldname.
    IF ls_key_field-int_key = abap_false.
      ls_component-type ?= cl_abap_datadescr=>describe_by_name( ls_key_field-rollname ).
    ELSE.
      ls_component-type = cl_abap_elemdescr=>get_string( ).
    ENDIF.
    INSERT ls_component INTO TABLE lt_component.
  ENDLOOP.

  "For edition-based entities also consider the edition
  IF me->mv_edition_based = abap_true.
    READ TABLE me->mt_key_field
      WITH KEY fieldname = usmd0_cs_fld-edition
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CLEAR ls_key_field.
      ls_key_field-fieldname     = usmd0_cs_fld-edition.
      ls_key_field-rollname      = 'USMD_EDITION'.
      ls_key_field-mandatory     = abap_true.
      ls_key_field-prefill_value = abap_true.
      ls_key_field-ovs_name      = me->mv_class_name.
      INSERT ls_key_field INTO TABLE me->mt_key_field.
      CLEAR ls_component.
      ls_component-name = ls_key_field-fieldname.
      ls_component-type ?= cl_abap_datadescr=>describe_by_name( ls_key_field-rollname ).
      INSERT ls_component INTO TABLE lt_component.
    ENDIF.
  ENDIF.

  "Create key structure RTTI and data object
  IF lt_component IS NOT INITIAL.
    me->mo_data_rtti = cl_abap_structdescr=>create( lt_component ).
    CREATE DATA me->mr_data TYPE HANDLE me->mo_data_rtti.
    CREATE DATA me->mr_data_ovs TYPE HANDLE me->mo_data_rtti.
    CREATE DATA me->mr_data_from TYPE HANDLE me->mo_data_rtti.
  ENDIF.

  "Get entity text (for dialog box title)
  ASSIGN <ls_entity_prop>-r_fprop->* TO <ls_fprop>.
  IF sy-subrc = 0.
    IF <ls_fprop>-scrtext_l IS NOT INITIAL.
      me->mv_entity_text = <ls_fprop>-scrtext_l.
    ELSEIF <ls_fprop>-scrtext_m IS NOT INITIAL.
      me->mv_entity_text = <ls_fprop>-scrtext_m.
    ELSEIF <ls_fprop>-reptext IS NOT INITIAL.
      me->mv_entity_text = <ls_fprop>-reptext.
    ENDIF.
  ENDIF.

  "Get edition type properties
  IF me->mv_edition_based = abap_true.
    ls_edition_type = me->mo_edition_api->get_edition_type_by_entity(
                        iv_model  = me->mv_model
                        iv_entity = me->mv_main_entity ).
    me->mv_edition_type     = ls_edition_type-usmd_edtn_type.
    me->mv_edition_timebase = ls_edition_type-usmd_timebase.
    IF me->mv_edition_timebase = if_usmd_edition_constants=>gc_timebase_period.
      me->mv_edition_periv = ls_edition_type-usmd_periv.
      cl_usmd_edition_helper=>get_maxperiods(
        EXPORTING
          iv_periv      = me->mv_edition_periv
        IMPORTING
          ev_max_number = me->mv_edition_max_period  ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB~GET_PARAMETER_LIST ##needed.

ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_3.

  CLEAR eo_fpm_event.

  IF iv_field_name = usmd0_cs_fld-edition.
    "Special handling of edition since only some of the existing
    "editions are allowed (dependent on entered object key)
    process_edition_ovs(
      EXPORTING
        iv_field_name           = iv_field_name
        io_ovs_callback         = io_ovs_callback
        iv_wd_context_attr_name = iv_wd_context_attr_name
      IMPORTING
        eo_fpm_event            = eo_fpm_event ).
  ELSE.
    process_generic_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_2.

  IF iv_field_name = usmd0_cs_fld-edition.
    "Special handling of edition since only some of the existing
    "editions are allowed (dependent on entered object key)
    process_edition_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ELSE.
    process_generic_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_1.

  IF iv_field_name = usmd0_cs_fld-edition.
    "Special handling of edition since only some of the existing
    "editions are allowed (dependent on entered object key)
    process_edition_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ELSE.
    process_generic_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_0.

  FIELD-SYMBOLS:
    <ls_data_ovs> TYPE data.

  "Store current UI values of object key for use in OVS
  "(using values from MR_DATA is not sufficient since opening
  " an OVS does not trigger FLUSH so that MR_DATA still contains
  " the [possibly outdated] values from the last roundtrip)
  IF me->mr_data_ovs IS BOUND.
    ASSIGN me->mr_data_ovs->* TO <ls_data_ovs>.
    io_ovs_callback->context_element->get_static_attributes(
      IMPORTING
        static_attributes = <ls_data_ovs> ).
  ENDIF.

  IF iv_field_name = usmd0_cs_fld-edition.
    "Special handling of edition since only some of the existing
    "editions are allowed (dependent on entered object key)
    process_edition_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ELSE.
    process_generic_ovs(
      EXPORTING
        iv_field_name   = iv_field_name
        io_ovs_callback = io_ovs_callback ).
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~PROCESS_EVENT.
*--------------------------------------------------------------------*
* This method processes the events relevant for the corresponding
* UIBB. Especially, it handles the FPM_CLOSE_DIALOG event that is
* raised when the user presses the 'OK' or 'Cancel' button of the
* dialog box.
*--------------------------------------------------------------------*

  DATA:
    lt_ffix             TYPE usmd_ts_value,
    lt_edition          TYPE usmd_t_editions,
    ls_message          LIKE LINE OF et_messages,
    lv_param_name       TYPE string,
    lv_param_value      TYPE string,
    lv_consider_edition TYPE abap_bool,
    lv_super_dn_exist   TYPE abap_bool.

  FIELD-SYMBOLS:
    <ls_data>    TYPE data,
    <ls_ffix>    LIKE LINE OF lt_ffix,
    <lv_edition> TYPE usmd_edition.

  CLEAR: ev_result, et_messages.

  CASE io_event->mv_event_id.
    WHEN cl_fpm_event=>gc_event_close_dialog_box.
      "Dialog box is closed (either with OK or Cancel)
      io_event->mo_event_data->get_value(
        EXPORTING
          iv_key = if_fpm_constants=>gc_dialog_box-dialog_buton_key
        IMPORTING
          ev_value = lv_param_value ).
      IF lv_param_value = if_fpm_constants=>gc_dialog_action_id-ok.
        "User pressed 'OK' button
        ASSIGN me->mr_data->* TO <ls_data>.

        "For edition-based entity types, check edition
        IF me->mv_copy_across_edt_enabled = abap_true.
          ASSIGN COMPONENT usmd0_cs_fld-edition OF STRUCTURE <ls_data>
            TO <lv_edition>.
          IF sy-subrc = 0 AND <lv_edition> IS NOT INITIAL.
            me->get_valid_editions(
              EXPORTING
                is_key_to  = <ls_data>
              IMPORTING
                et_edition = lt_edition
                et_message = et_messages ).
            IF et_messages IS INITIAL.
              READ TABLE lt_edition
                WITH KEY usmd_edition = <lv_edition>
                TRANSPORTING NO FIELDS.
              IF sy-subrc <> 0.
                "Edition entered by the user is not valid
                "-> Do not close dialog box but display message
                IF 0 = 1.
                  MESSAGE e006(usmdcrui).
                ELSE.
                  ls_message-msgid    = 'USMDCRUI'.
                  ls_message-msgno    = '006'.
                  ls_message-severity = if_fpm_message_manager=>gc_severity_error.
                  ls_message-ref_name = usmd0_cs_fld-edition.
                  INSERT ls_message INTO TABLE et_messages.
                ENDIF.
              ENDIF.
            ENDIF.
            IF et_messages IS NOT INITIAL.
              "Error occurred --> Do not close dialog box
              ev_result = if_fpm_constants=>gc_event_result-failed.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.

        "Check existence of entered object key
        IF me->mv_copy_across_edt_enabled = abap_true.
          "If possible check for object instance, not just object
          lv_consider_edition = abap_true.
        ENDIF.
        me->check_existence(
          EXPORTING
            is_data             = <ls_data>
            iv_consider_edition = lv_consider_edition
          IMPORTING
            et_ffix             = lt_ffix
            et_message          = et_messages ).
        IF et_messages IS NOT INITIAL.
          "Error messages exist
          "-> Do not close dialog box but display messages
          ev_result = if_fpm_constants=>gc_event_result-failed.
          RETURN.
        ENDIF.

        "Pass object key as event payload
        LOOP AT lt_ffix ASSIGNING <ls_ffix>.
          lv_param_name  = <ls_ffix>-fieldname.
          lv_param_value = <ls_ffix>-value.
          io_event->mo_event_data->set_value(
            EXPORTING
              iv_key   = lv_param_name
              iv_value = lv_param_value ).
        ENDLOOP.
      ENDIF.
  ENDCASE.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~GET_DEFINITION.
*--------------------------------------------------------------------*
* This method defines the fields available in the corresponding UIBB.
* All key fields of the current entity type are considered.
*
* The method is called once when the corresponding UIBB is created
* (after the INITIALIZE method).
*--------------------------------------------------------------------*

  DATA:
    lt_component   TYPE cl_abap_structdescr=>component_table,
    ls_component   LIKE LINE OF lt_component,
    ls_field_descr LIKE LINE OF et_field_description.

  FIELD-SYMBOLS:
    <ls_key_field> LIKE LINE OF me->mt_key_field.

  CLEAR: es_message, eo_field_catalog, et_field_description,
         et_action_definition, et_special_groups,
         ev_additional_error_info, et_dnd_definition.

  IF me->mo_data_rtti IS NOT BOUND.
    "For some reason (e.g., unknown entity) the initialization failed.
    "We create a dummy field here to prevent a dump.
    ls_component-name = 'DUMMY'.
    ls_component-type = cl_abap_elemdescr=>get_string( ).
    INSERT ls_component INTO TABLE lt_component.
    me->mo_data_rtti = cl_abap_structdescr=>create( lt_component ).
    CREATE DATA me->mr_data TYPE HANDLE me->mo_data_rtti.
    CREATE DATA me->mr_data_from TYPE HANDLE me->mo_data_rtti.
    eo_field_catalog = me->mo_data_rtti.
    ls_field_descr-name = ls_component-name.
    INSERT ls_field_descr INTO TABLE et_field_description.
    RETURN.
  ENDIF.

  "Set field catalog
  eo_field_catalog = me->mo_data_rtti.

  "Set field description
  LOOP AT me->mt_key_field ASSIGNING <ls_key_field>.
    CLEAR ls_field_descr.
    ls_field_descr-name = <ls_key_field>-fieldname.
    IF <ls_key_field>-label IS NOT INITIAL.
      ls_field_descr-label_text = <ls_key_field>-label.
    ELSE.
      ls_field_descr-label_by_ddic = abap_true.
    ENDIF.
    ls_field_descr-length_by_ddic = abap_true.
    IF <ls_key_field>-shlpname IS NOT INITIAL.
      ls_field_descr-ddic_shlp_name = <ls_key_field>-shlpname.
    ELSEIF <ls_key_field>-ovs_name IS NOT INITIAL.
      ls_field_descr-ovs_name = <ls_key_field>-ovs_name.
    ENDIF.
    ls_field_descr-mandatory = <ls_key_field>-mandatory.
    IF <ls_key_field>-fieldname = 'ACCOUNT' .
     ls_field_descr-read_only = abap_true.
    ENDIF.
    IF <ls_key_field>-fieldname = 'COA' .
     ls_field_descr-read_only = abap_false.
    ENDIF.
    INSERT ls_field_descr INTO TABLE et_field_description.
  ENDLOOP.

FIELD-SYMBOLS:
    <fs_field> LIKE LINE OF et_field_description.

READ TABLE et_field_description ASSIGNING <fs_field> WITH KEY name = 'COA' .
<fs_field>-read_only = abap_true.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~GET_DEFAULT_CONFIG.
*--------------------------------------------------------------------*
* This method creates the configuration used for the corresponding
* UIBB. This is only possible at runtime since only when the entity
* type is known that specifies the available fields.
*
* The configuration consists of an input field for each key field
* of the entity type (in an 8/1 layout).
*
* The method is called once when the corresponding UIBB is created
* (after the GET_DEFINITION method).
*--------------------------------------------------------------------*

  CONSTANTS: lc_id_prefix TYPE string VALUE `IF_`,
             lc_grp_name  TYPE string VALUE `GRP1`.

  DATA:
    ls_group      TYPE if_fpm_guibb_form_cfg_read_gl2=>ty_s_form_group,
    ls_element    TYPE if_fpm_guibb_form_cfg_read_gl2=>ty_s_standard_element,
    ls_group_elem TYPE if_fpm_guibb_form_cfg_read_gl2=>ty_s_form_group_element,
    lv_num_txt    TYPE string.

  FIELD-SYMBOLS:
    <ls_key_field> LIKE LINE OF me->mt_key_field.

  "For effort reasons, we only implement the feeder class for use
  "with the new Form GUIBB (FPM_FORM_UIBB_GL2)
  ASSERT io_layout_config_gl2 IS BOUND.

  TRY.
      "Set general form attributes
      io_layout_config_gl2->set_form_attributes(
        EXPORTING
          iv_layout_type     = if_fpm_guibb_constants=>gc_form_layout_type_gl2-layout_8_1
          iv_check_mandatory = abap_true ).

      "Add an input field for each key field of the entity
      LOOP AT me->mt_key_field ASSIGNING <ls_key_field>.
        IF <ls_key_field>-fieldname = usmd0_cs_fld-edition AND
           me->mv_copy_across_edt_enabled = abap_false.
          "Don't display 'Edition' input field
          CONTINUE.
        ENDIF.
        CLEAR ls_element.
        lv_num_txt = sy-tabix.
        ls_element-id = lc_id_prefix && lv_num_txt.
        CONDENSE ls_element-id NO-GAPS.
        ls_element-name = <ls_key_field>-fieldname.
        IF <ls_key_field>-int_key = abap_false.
          ls_element-type = if_fpm_guibb_constants=>gc_display_type-input_field.
        ELSE.
          ls_element-type = if_fpm_guibb_constants=>gc_display_type-text_view.
        ENDIF.
        ls_element-position-start_row          = sy-tabix.
        ls_element-position-end_row            = ls_element-position-start_row.
        ls_element-position-start_column       = 'D'.
        ls_element-position-end_column         = 'H'.
        ls_element-position-label_start_row    = ls_element-position-start_row.
        ls_element-position-label_end_row      = ls_element-position-start_row.
        ls_element-position-label_start_column = 'A'.
        ls_element-position-label_end_column   = 'C'.
        ls_element-label_visibility = cl_wd_uielement=>e_visible-visible.
        IF <ls_key_field>-label IS NOT INITIAL.
          ls_element-label_text = <ls_key_field>-label.
        ENDIF.
        IF <ls_key_field>-tooltip IS NOT INITIAL.
          ls_element-tooltip = <ls_key_field>-tooltip.
        ENDIF.
        "For some reason, the rendering of the dialog box dumps
        "when the next line (alignment setting) is removed
        ls_element-individual_properties-alignment = cl_wd_abstract_input_field=>e_alignment-auto.
        io_layout_config_gl2->add_standard_element( ls_element ).
        CLEAR ls_group_elem.
        ls_group_elem-id                = ls_element-id.
        ls_group_elem-form_element_id   = ls_element-id.
        ls_group_elem-form_element_type = if_fpm_guibb_constants=>gc_form_element_type-standard_element.
        INSERT ls_group_elem INTO TABLE ls_group-form_group_elements.
      ENDLOOP.

      "Add input fields to group
      IF ls_group-form_group_elements IS NOT INITIAL.
        ls_group-id = lc_grp_name.
        io_layout_config_gl2->add_form_group( ls_group ).
      ENDIF.
    CATCH cx_fpm_configuration.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~GET_DATA.
*--------------------------------------------------------------------*
* This method handles the synchronization between the internal data
* (MR_DATA) of the feeder class and the external data (CS_DATA) of
* the corresponding UIBB.
*
* When used in a dialog box and the dialog box is just opened, the
* key fields of "superordinate" entity types are prefilled with the
* values of the source object passed as event parameters.
*--------------------------------------------------------------------*

  DATA:
    ls_dialog_prop TYPE fpm_s_dialog_box_properties,
    lv_first_time  TYPE abap_bool,
    lv_title       TYPE string,
    lv_ev_data_key TYPE string.

  FIELD-SYMBOLS:
    <ls_data_int>    TYPE data,
    <ls_data_from>   TYPE data,
    <ls_key_field>   LIKE LINE OF me->mt_key_field,
    <ls_field_usage> LIKE LINE OF ct_field_usage,
    <lv_data>        TYPE data,
    <lv_data_from>   TYPE data.

  CLEAR: et_messages,
         ev_data_changed,
         ev_field_usage_changed,
         ev_action_usage_changed.

  "Check whether the surrounding dialog box is just being opened
  lv_first_time = abap_false.
  IF io_event IS BOUND AND
     io_event->mv_event_id = cl_fpm_event=>gc_event_open_dialog_box.
    lv_first_time = abap_true.
  ENDIF.

  IF lv_first_time = abap_true.
    "The surrounding dialog box is just being opened

    "Set the dialog box title ('Copy: <entity text>')
    CONCATENATE text-001 me->mv_entity_text INTO lv_title SEPARATED BY space.
    io_event->mo_event_data->get_value(
      EXPORTING
        iv_key   = if_fpm_constants=>gc_dialog_box-dialog_box_properties
      IMPORTING
        ev_value = ls_dialog_prop ).
    ls_dialog_prop-title = lv_title.
    io_event->mo_event_data->set_value(
      EXPORTING
        iv_key   = if_fpm_constants=>gc_dialog_box-dialog_box_properties
        iv_value = ls_dialog_prop ).

    "Fill key of source ("from") object
    ASSIGN me->mr_data_from->* TO <ls_data_from>.
    LOOP AT me->mt_key_field ASSIGNING <ls_key_field>.
      ASSIGN COMPONENT <ls_key_field>-fieldname
        OF STRUCTURE <ls_data_from> TO <lv_data_from>.
      CHECK sy-subrc = 0.
      lv_ev_data_key = <ls_key_field>-fieldname.
      io_event->mo_event_data->get_value(
        EXPORTING
          iv_key   = lv_ev_data_key
        IMPORTING
          ev_value = <lv_data_from> ).
    ENDLOOP.

    "Prefill input fields from source key
    CLEAR cs_data.
    LOOP AT me->mt_key_field ASSIGNING <ls_key_field>
      WHERE prefill_value = abap_true OR
            int_key = abap_true.
      ASSIGN COMPONENT <ls_key_field>-fieldname
        OF STRUCTURE cs_data TO <lv_data>.
      CHECK sy-subrc = 0.
      IF <ls_key_field>-prefill_value = abap_true.
        ASSIGN COMPONENT <ls_key_field>-fieldname
          OF STRUCTURE <ls_data_from> TO <lv_data_from>.
        CHECK sy-subrc = 0.
        <lv_data> = <lv_data_from>.
      ELSE.
        <lv_data> = text-002.
      ENDIF.
    ENDLOOP.
    ev_data_changed = abap_true.

    "Set the focus to the first "input-ready" input field
    "(otherwise, the focus would be on the dialog box title)
    IF io_extended_ctrl IS BOUND.
      LOOP AT ct_field_usage ASSIGNING <ls_field_usage>
        WHERE read_only  = abap_false AND
              enabled    = abap_true  AND
              visibility = cl_wd_uielement=>e_visible-visible.
        io_extended_ctrl->request_focus_on_field( <ls_field_usage>-name ).
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "Update internal data from UI
  FIELD-SYMBOLS <field> TYPE any .
  ASSIGN me->mr_data->* TO <ls_data_int>.

  DATA:
NUMBER TYPE I.

CALL FUNCTION 'NUMBER_GET_NEXT'
EXPORTING
nr_range_nr = '01'
object = 'ZTEST'
IMPORTING
NUMBER = NUMBER
EXCEPTIONS
INTERVAL_NOT_FOUND = 1
NUMBER_RANGE_NOT_INTERN = 2
OBJECT_NOT_FOUND = 3
QUANTITY_IS_0 = 4
QUANTITY_IS_NOT_1 = 5
INTERVAL_OVERFLOW = 6
BUFFER_OVERFLOW = 7
OTHERS = 8.

IF sy-subrc <> 0.
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
FIELD-SYMBOLS <cs_data1> TYPE ANY .
ASSIGN cs_data to <cs_data1>.
 ASSIGN COMPONENT 'ACCOUNT'
    OF STRUCTURE <cs_data1> TO <field>.
<field> = NUMBER.
  MOVE-CORRESPONDING cs_data TO <ls_data_int>.



EV_DATA_CHANGED = 'X'.
ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~FLUSH.
*--------------------------------------------------------------------*
* This method updates the internal data (MR_DATA) by the values
* entered by the user in the corresponding UIBB.
*--------------------------------------------------------------------*

  FIELD-SYMBOLS:
    <ls_data_int> TYPE data,
    <ls_data_ui>  TYPE data.

  "Update internal data from UI
  IF it_change_log IS NOT INITIAL.
    ASSIGN is_data->* TO <ls_data_ui>.
    ASSIGN me->mr_data->* TO <ls_data_int>.
    MOVE-CORRESPONDING <ls_data_ui> TO <ls_data_int>.
  ENDIF.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~CHECK_CONFIG ##needed.

ENDMETHOD.


METHOD IF_FPM_GUIBB_DYNAMIC_CONFIG~HAS_DYNAMIC_CONFIGURATION.
*--------------------------------------------------------------------*
* This method states that no explicit configuration will be considered
* for the corresponding UIBB but that the configuration will be
* created dynamically at runtime. This is necessary since at design
* time the input fields of the UIBB are not known.
*--------------------------------------------------------------------*

  rv_has_dynamic_configuration = abap_true.

ENDMETHOD.


METHOD GET_VALID_EDITIONS.
*--------------------------------------------------------------------*
* This method determines all valid editions in which the object
* specified in IS_KEY_TO can be created as a copy of the source
* object (specified in MT_DATA).
*
* The following scenarios can be distinguished:
*   (1) The copied object is completely new, that is, does not exist
*       yet in any edition.
*   (2) Source object = copied object, that is, a new instance of
*       the source object is created.
*   (3) Source object <> copied object, that is, a new instance of
*       an other (existing) object is created.
*
* The returned edition tables are sorted by validity start.
*--------------------------------------------------------------------*

  DATA:
    lr_key_from        TYPE REF TO data,
    lr_key_to          TYPE REF TO data,
    lr_edition_res     TYPE REF TO data,
    lt_key_to          TYPE usmd_t_value,
    lt_message         TYPE fpmgb_t_messages,
    lt_edition         LIKE et_edition,
    lt_edition_inst    TYPE usmd_t_edition_with_obs,
    ls_key_to          LIKE LINE OF lt_key_to,
    ls_edition_from    TYPE usmd_s_edition,
    ls_message         LIKE LINE OF et_message,
    lv_edition_from    TYPE usmd_edition,
    lv_prev_vper_from  TYPE usmd_edition_vper,
    lv_prev_vper_to    TYPE usmd_edition_vper,
    lv_next_vper_from  TYPE usmd_edition_vper,
    lv_next_vper_to    TYPE usmd_edition_vper,
    lv_prev_vdate_from TYPE usmd_edition_vdate,
    lv_prev_vdate_to   TYPE usmd_edition_vdate,
    lv_next_vdate_from TYPE usmd_edition_vdate,
    lv_next_vdate_to   TYPE usmd_edition_vdate,
    lv_mandt_missing   TYPE abap_bool,
    lv_opt_missing     TYPE abap_bool,
    lv_int_key         TYPE abap_bool,
    lv_exists          TYPE abap_bool,
    lv_index           TYPE i.

  FIELD-SYMBOLS:
    <ls_key_field>    LIKE LINE OF me->mt_key_field,
    <ls_message>      LIKE LINE OF lt_message,
    <ls_edition>      LIKE LINE OF lt_edition,
    <ls_edition_inst> LIKE LINE OF lt_edition_inst,
    <ls_edition_prev> LIKE LINE OF lt_edition_inst,
    <ls_edition_next> LIKE LINE OF lt_edition_inst,
    <ls_edition_res>  TYPE data,
    <ls_key_from>     TYPE data,
    <ls_key_from_tmp> TYPE data,
    <ls_key_to>       TYPE data,
    <lv_value>        TYPE data,
    <lv_edition>      TYPE usmd_edition.

  CLEAR: et_edition, et_edition_info, et_message.

  "Check completeness of target key
  LOOP AT me->mt_key_field ASSIGNING <ls_key_field>
    WHERE fieldname <> if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition.
    ASSIGN COMPONENT <ls_key_field>-fieldname OF STRUCTURE is_key_to TO <lv_value>.
    IF sy-subrc = 0.
      IF <lv_value> IS INITIAL.
        "Key field is empty
        IF <ls_key_field>-mandatory = abap_true.
          lv_mandt_missing = abap_true.
        ELSEIF <ls_key_field>-int_key = abap_true.
          lv_int_key = abap_true.
        ELSE.
          lv_opt_missing = abap_true.
        ENDIF.
      ELSE.
        "Key field is filled
        ls_key_to-fieldname = <ls_key_field>-fieldname.
        ls_key_to-value     = <lv_value>.
        INSERT ls_key_to INTO TABLE lt_key_to.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_mandt_missing = abap_true.
    "Mandatory key field is empty -> no sufficient information
    RETURN.
  ENDIF.

  "Get all existing editions of current edition type
  TRY.
      lt_edition = me->mo_edition_api->get_editions_by_type( me->mv_edition_type ).
    CATCH cx_usmd_edition ##no_handler.
  ENDTRY.

  "Remove all editions that are not 'In Process'
  DELETE lt_edition WHERE usmd_edtn_status <> if_usmd_edition_constants=>gc_edition_status-inprocess.

  IF lt_edition IS INITIAL.
    "No open edition exists -> no chance to copy an edition-based object
    IF 0 = 1.
      MESSAGE e012(usmdcrui).
    ELSE.
      ls_message-msgid    = 'USMDCRUI'.
      ls_message-msgno    = '012'.
      ls_message-severity = if_fpm_message_manager=>gc_severity_error.
      ls_message-ref_name = usmd0_cs_fld-edition.
      INSERT ls_message INTO TABLE et_message.
    ENDIF.
    RETURN.
  ENDIF.

  "Sort editions by validity start
  SORT lt_edition BY usmd_vdate_from usmd_vper_from.

  "Create auxiliary variables
  CREATE DATA lr_key_from TYPE HANDLE me->mo_data_rtti.
  ASSIGN lr_key_from->* TO <ls_key_from>.
  CREATE DATA lr_key_to TYPE HANDLE me->mo_data_rtti.
  ASSIGN lr_key_to->* TO <ls_key_to>.

  "Fill auxiliary variables with source/target keys
  ASSIGN me->mr_data_from->* TO <ls_key_from_tmp>.
  <ls_key_from> = <ls_key_from_tmp>.
  ASSIGN COMPONENT if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition
    OF STRUCTURE <ls_key_from> TO <lv_edition>.
  IF sy-subrc = 0.
    lv_edition_from = <lv_edition>.
    CLEAR <lv_edition>.
  ENDIF.
  <ls_key_to> = is_key_to.
  ASSIGN COMPONENT if_usmd_generic_bolui_const=>gc_fieldname_usmd_edition
    OF STRUCTURE <ls_key_to> TO <lv_edition>.
  IF sy-subrc = 0.
    CLEAR <lv_edition>.
  ENDIF.

  "Check whether target key describes existing object and whether
  "an instance of the object is currently locked in CR
  IF lv_opt_missing = abap_false AND lv_int_key = abap_false.
    me->check_existence(
      EXPORTING
        is_data          = <ls_key_to>
        iv_check_locking = abap_true
      IMPORTING
        et_message       = lt_message ).
    LOOP AT lt_message ASSIGNING <ls_message>.
      IF <ls_message>-msgid = 'USMDCRUI' AND
         <ls_message>-msgno = '007'.
        "This message indicates that the object already exists and no
        "instance of the object is currently locked
        lv_exists = abap_true.
        EXIT.
      ELSEIF <ls_message>-msgid = 'USMDCRUI' AND
             <ls_message>-msgno = '011'.
        "This message indicates that the object already exists but an
        "instance of the object is currently locked
        "--> No new instance of the object can be created
        INSERT <ls_message> INTO TABLE et_message.
        RETURN.
      ELSEIF <ls_message>-msgid = 'USMD1A' AND
             <ls_message>-msgno = '001'.
        "This message indicates that the superordinate object does not exist
        "-> No new instance of the object can be created
        INSERT <ls_message> INTO TABLE et_message.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF <ls_key_from> = <ls_key_to>.
      "Scenario (2): Creation of a new instance of the existing source object
      "-> those open editions of the correct edition type are valid that are
      "   adjacent to the source edition but contain no instance of the object
      "   yet (neither directly nor via propagation)

      "Determine all editions that contain instances of the object
      lt_edition_inst = me->mo_edition_api->if_usmd_edition_entity_api~get_editions_of_entity_inc_obs(
                          iv_model  = me->mv_model
                          iv_entity = me->mv_main_entity
                          it_key    = lt_key_to ).
      SORT lt_edition_inst BY usmd_vdate_from usmd_vper_from usmd_edtn_number ASCENDING.

      "Remove editions that contain deleted instances
      "(since we are allowed to create new instances there)
      DELETE lt_edition_inst WHERE entity_obsolete = abap_true.

      "Get properties of source edition
      TRY.
          ls_edition_from = me->mo_edition_api->get_edition( iv_edition = lv_edition_from ).
        CATCH cx_usmd_edition.
          RETURN.
      ENDTRY.

      IF me->mv_edition_timebase = if_usmd_edition_constants=>gc_timebase_period.
        "Determine adjacent "gaps" (time periods where no object instance exists)
        READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst>
          WITH KEY usmd_vper_from = ls_edition_from-usmd_vper_from
          BINARY SEARCH.
        lv_index = sy-tabix.
        IF sy-subrc <> 0.
          "The reason why no edition with the validity start of the source edition
          "is found in the edition table is that the "real" source edition is already
          "closed and was therefore replaced by the earliest open edition when calling
          "the dialog box.
          "-> Look for the "real" source edition
          IF lv_index > 1.
            lv_index = lv_index - 1.
            READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst> INDEX lv_index.
          ELSE.
            RETURN.
          ENDIF.
        ENDIF.
        "Determine previous gap
        IF lv_index = 1.
          "No edition before current edition
          lv_prev_vper_from = if_usmd_edition_constants=>gc_minimum_period.
          lv_prev_vper_to   = me->get_prev_period( <ls_edition_inst>-usmd_vper_from ).
        ELSE.
          READ TABLE lt_edition_inst INDEX lv_index - 1 ASSIGNING <ls_edition_prev>.
          lv_prev_vper_from = me->get_next_period( <ls_edition_prev>-usmd_vper_to ).
          lv_prev_vper_to   = me->get_prev_period( <ls_edition_inst>-usmd_vper_from ).
        ENDIF.
        "Determine next gap
        IF lv_index = lines( lt_edition_inst ).
          "No edition after current edition
          lv_next_vper_from = me->get_next_period( <ls_edition_inst>-usmd_vper_to ).
          lv_next_vper_to   = if_usmd_edition_constants=>gc_infinit_period.
        ELSE.
          READ TABLE lt_edition_inst INDEX lv_index + 1 ASSIGNING <ls_edition_next>.
          lv_next_vper_from = me->get_next_period( <ls_edition_inst>-usmd_vper_to ).
          lv_next_vper_to   = me->get_prev_period( <ls_edition_next>-usmd_vper_from ).
        ENDIF.

        "Remove all editions that do not start in one of the gaps
        DELETE lt_edition
          WHERE usmd_vper_from NOT BETWEEN lv_prev_vper_from AND lv_prev_vper_to AND
                usmd_vper_from NOT BETWEEN lv_next_vper_from AND lv_next_vper_to.
      ELSE.
        "Determine adjacent "gaps" (time periods where no object instance exists)
        READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst>
          WITH KEY usmd_vdate_from = ls_edition_from-usmd_vdate_from
          BINARY SEARCH.
        lv_index = sy-tabix.
        IF sy-subrc <> 0.
          "The reason why no edition with the validity start of the source edition
          "is found in the edition table is that the "real" source edition is already
          "closed and was therefore replaced by the earliest open edition when calling
          "the dialog box.
          "-> Look for the "real" source edition
          IF lv_index > 1.
            lv_index = lv_index - 1.
            READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst> INDEX lv_index.
          ELSE.
            RETURN.
          ENDIF.
        ENDIF.
        "Determine previous gap
        IF lv_index = 1.
          "No edition before current edition
          lv_prev_vdate_from = if_usmd_edition_constants=>gc_minimum_date.
          lv_prev_vdate_to   = me->get_prev_date( <ls_edition_inst>-usmd_vdate_from ).
        ELSE.
          READ TABLE lt_edition_inst INDEX lv_index - 1 ASSIGNING <ls_edition_prev>.
          lv_prev_vdate_from = me->get_next_date( <ls_edition_prev>-usmd_vdate_to ).
          lv_prev_vdate_to   = me->get_prev_date( <ls_edition_inst>-usmd_vdate_from ).
        ENDIF.
        "Determine next gap
        IF lv_index = lines( lt_edition_inst ).
          "No edition after current edition
          lv_next_vdate_from = me->get_next_date( <ls_edition_inst>-usmd_vdate_to ).
          lv_next_vdate_to   = if_usmd_edition_constants=>gc_infinit_date.
        ELSE.
          READ TABLE lt_edition_inst INDEX lv_index + 1 ASSIGNING <ls_edition_next>.
          lv_next_vdate_from = me->get_next_date( <ls_edition_inst>-usmd_vdate_to ).
          lv_next_vdate_to   = me->get_prev_date( <ls_edition_next>-usmd_vdate_from ).
        ENDIF.

        "Remove all editions that do not start in one of the gaps
        DELETE lt_edition
          WHERE usmd_vdate_from NOT BETWEEN lv_prev_vdate_from AND lv_prev_vdate_to AND
                usmd_vdate_from NOT BETWEEN lv_next_vdate_from AND lv_next_vdate_to.
      ENDIF.

    ELSEIF lv_exists = abap_false.
      "Scenario (1): Creation of a completely new object
      "-> any open edition of the correct edition type is valid

      "Valid editions are already in LT_EDITION

    ELSE.
      "Scenario (3): Creation of a new instance of an existing object (<> source object)
      "-> those open editions of the correct edition type are valid where no
      "   instance of the object exists yet (neither directly nor via propagation)

      "Determine all editions that contain instances of the object
      lt_edition_inst = me->mo_edition_api->if_usmd_edition_entity_api~get_editions_of_entity_inc_obs(
                          iv_model  = me->mv_model
                          iv_entity = me->mv_main_entity
                          it_key    = lt_key_to ).
      SORT lt_edition_inst BY usmd_vdate_from usmd_vper_from usmd_edtn_number ASCENDING.

      "Remove editions that contain deleted instances
      "(since we are allowed to create new instances there)
      DELETE lt_edition_inst WHERE entity_obsolete = abap_true.

      "In table of all open editions, filter out those editions where an instance of the
      "object already exists (directly or via propagation)
      LOOP AT lt_edition ASSIGNING <ls_edition>.
        IF me->mv_edition_timebase = if_usmd_edition_constants=>gc_timebase_period.
          LOOP AT lt_edition_inst ASSIGNING <ls_edition_inst>
            WHERE usmd_vper_from <= <ls_edition>-usmd_vper_from AND
                  usmd_vper_to   >= <ls_edition>-usmd_vper_from.
            "Edition covers period where an instance already exists -> filter out edition
            DELETE lt_edition.
            EXIT.
          ENDLOOP.
        ELSE.
          LOOP AT lt_edition_inst ASSIGNING <ls_edition_inst>
            WHERE usmd_vdate_from <= <ls_edition>-usmd_vdate_from AND
                  usmd_vdate_to   >= <ls_edition>-usmd_vdate_from.
            "Edition covers period where an instance already exists -> filter out edition
            DELETE lt_edition.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

  "If the copied object has an edition-based superordinate object, remove all editions from
  "the result in whose validity period the superordinate object does not exist.
  "(Since the copied object is a "dependent" object that cannot live alone.)
  me->filter_editions_by_super_val(
    EXPORTING
      is_key_to  = is_key_to
    CHANGING
      ct_edition = lt_edition ).

  "Fill result tables
  et_edition = lt_edition.
  IF et_edition_info IS SUPPLIED.
    CREATE DATA lr_edition_res LIKE LINE OF et_edition_info.
    ASSIGN lr_edition_res->* TO <ls_edition_res>.
    LOOP AT et_edition ASSIGNING <ls_edition>.
      MOVE-CORRESPONDING <ls_edition> TO <ls_edition_res>.
      INSERT <ls_edition_res> INTO TABLE et_edition_info.
    ENDLOOP.
  ENDIF.

  "Check whether at least one valid edition was found
  IF et_edition IS INITIAL.
    IF 0 = 1.
      MESSAGE e012(usmdcrui).
    ELSE.
      ls_message-msgid    = 'USMDCRUI'.
      ls_message-msgno    = '012'.
      ls_message-severity = if_fpm_message_manager=>gc_severity_error.
      ls_message-ref_name = usmd0_cs_fld-edition.
      INSERT ls_message INTO TABLE et_message.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD GET_PREV_PERIOD.
*--------------------------------------------------------------------*
* This method calculates the period immediately before IV_PERIOD.
*
* The number of periods per fiscal year is taken from
* MV_EDITION_MAX_PERIOD.
*--------------------------------------------------------------------*

  DATA:
   lv_fyear(4)   TYPE n,
   lv_period(3)  TYPE n,
   lv_fyear_num  TYPE i,
   lv_period_num TYPE i.

  CLEAR rv_prev_period.

  IF iv_period IS INITIAL.
    RETURN.
  ENDIF.

  "Split period into fiscal year and period number
  lv_fyear  = iv_period(4).
  lv_period = iv_period+4(3).
  lv_fyear_num  = lv_fyear.
  lv_period_num = lv_period.

  "Decrease period number
  IF lv_period_num <= 1.
    "Switch to previous fiscal year
    IF lv_fyear_num > 1.
      lv_fyear_num  = lv_fyear_num - 1.
      lv_period_num = me->mv_edition_max_period.
    ENDIF.
  ELSE.
    lv_period_num = lv_period_num - 1.
  ENDIF.

  "Assemble period
  lv_fyear  = lv_fyear_num.
  lv_period = lv_period_num.
  rv_prev_period(4)   = lv_fyear.
  rv_prev_period+4(3) = lv_period.

ENDMETHOD.


METHOD GET_PREV_DATE.
*--------------------------------------------------------------------*
* This method calculates the date immediately before IV_DATE.
*--------------------------------------------------------------------*

  rv_prev_date = iv_date.

  IF iv_date IS INITIAL OR iv_date = '00010101'.
    RETURN.
  ENDIF.

  "Decrease date
  rv_prev_date = iv_date - 1.

ENDMETHOD.


METHOD GET_NEXT_PERIOD.
*--------------------------------------------------------------------*
* This method calculates the period immediately after IV_PERIOD.
*
* The number of periods per fiscal year is taken from
* MV_EDITION_MAX_PERIOD.
*--------------------------------------------------------------------*

  DATA:
   lv_fyear(4)   TYPE n,
   lv_period(3)  TYPE n,
   lv_fyear_num  TYPE i,
   lv_period_num TYPE i.

  CLEAR rv_next_period.

  IF iv_period IS INITIAL.
    RETURN.
  ENDIF.

  "Split period into fiscal year and period number
  lv_fyear  = iv_period(4).
  lv_period = iv_period+4(3).
  lv_fyear_num  = lv_fyear.
  lv_period_num = lv_period.

  "Increase period number
  IF lv_period_num >= me->mv_edition_max_period.
    "Switch to next fiscal year
    IF lv_fyear_num < 9999.
      lv_fyear_num  = lv_fyear_num + 1.
      lv_period_num = 1.
    ENDIF.
  ELSE.
    lv_period_num = lv_period_num + 1.
  ENDIF.

  "Assemble period
  lv_fyear  = lv_fyear_num.
  lv_period = lv_period_num.
  rv_next_period(4)   = lv_fyear.
  rv_next_period+4(3) = lv_period.

ENDMETHOD.


METHOD GET_NEXT_DATE.
*--------------------------------------------------------------------*
* This method calculates the date immediately after IV_DATE.
*--------------------------------------------------------------------*

  rv_next_date = iv_date.

  IF iv_date IS INITIAL OR iv_date = '99991231'.
    RETURN.
  ENDIF.

  "Increase date
  rv_next_date = iv_date + 1.

ENDMETHOD.


METHOD GET_DDIC_INFO.
*--------------------------------------------------------------------*
* This method determines the DDIC information (data type, conversion
* exit, etc.) of the given data element.
*--------------------------------------------------------------------*

  DATA:
    lo_typedescr TYPE REF TO cl_abap_typedescr,
    lo_elemdescr TYPE REF TO cl_abap_elemdescr.

  CLEAR rs_ddic_info.

  IF iv_data_elem IS INITIAL.
    "No data element name provided
    RETURN.
  ENDIF.

  "Get type description of data element
  cl_abap_typedescr=>describe_by_name(
    EXPORTING
      p_name         = iv_data_elem
    RECEIVING
      p_descr_ref    = lo_typedescr
    EXCEPTIONS
      type_not_found = 1
      OTHERS         = 2 ).
  IF sy-subrc <> 0 OR
     lo_typedescr->kind <> cl_abap_typedescr=>kind_elem.
    "Unknown DDIC data element
    RETURN.
  ENDIF.

  "Read DDIC information
  lo_elemdescr ?= lo_typedescr.
  lo_elemdescr->get_ddic_field(
*    EXPORTING
*      p_langu      = iv_language
    RECEIVING
      p_flddescr   = rs_ddic_info
    EXCEPTIONS
      not_found    = 1
      no_ddic_type = 2
      OTHERS       = 3 ).
  IF sy-subrc <> 0.
    "Unknown DDIC data element
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD FILTER_EDITIONS_BY_SUPER_VAL.
*--------------------------------------------------------------------*
* This method removes all editions from table CT_EDITION that refer
* to validity periods where the superordinate object of the new
* object (described by key in IS_KEY_TO) does not exist.
*
* Example: In Financials, an 'Account in Company Codes' (ACCCCDET)
*          object ("B segment") can only be created if the
*          superordinate 'Account' (ACCOUNT) object ("A segment")
*          already exists in the validity period of the target
*          edition.
*--------------------------------------------------------------------*

  DATA:
    lt_field        TYPE usmd_ts_field,
    lt_super_key    TYPE usmd_t_value,
    lt_edition_inst TYPE usmd_t_edition_with_obs,
    ls_super_w_edt  LIKE LINE OF me->mt_super_entity_with_edt,
    ls_super_key    LIKE LINE OF lt_super_key,
    lv_fieldname    TYPE usmd_fieldname.

  FIELD-SYMBOLS:
    <ls_field>        LIKE LINE OF lt_field,
    <ls_entity_prop>  TYPE LINE OF usmd_ts_entity_prop,
    <ls_fdep>         TYPE LINE OF usmd_ts_fdep,
    <ls_super_w_edt>  LIKE LINE OF me->mt_super_entity_with_edt,
    <ls_edition>      LIKE LINE OF ct_edition,
    <ls_edition_inst> LIKE LINE OF lt_edition_inst,
    <lv_value>        TYPE data.

  IF me->mv_edition_based = abap_false OR ct_edition IS INITIAL.
    RETURN.
  ENDIF.

  "Determine all direct superordinate entity types of the new object
  "that are edition-based.
  IF me->mv_febysv_buffer_filled = abap_false.
    me->mv_febysv_buffer_filled = abap_true.
    "Get all direct superordinate entity types
    lv_fieldname = me->mv_main_entity.
    me->mo_model->get_direct_fdep(
      EXPORTING
        i_fieldname = lv_fieldname
      IMPORTING
        et_field    = lt_field ).
    LOOP AT lt_field ASSIGNING <ls_field>.
      "Remove entity types that are not edition-based
      READ TABLE me->mo_model->dt_entity_prop ASSIGNING <ls_entity_prop>
        WITH TABLE KEY usmd_entity = <ls_field>-fieldname.  "#EC WARNOK
      CHECK sy-subrc = 0 AND
            <ls_entity_prop>-validity_concept = usmd0_cs_validity_entity_type-edition.
      "Collect key fields of entity types and fill buffer
      CLEAR ls_super_w_edt.
      ls_super_w_edt-usmd_entity = <ls_field>-fieldname.
      LOOP AT me->mo_model->dt_fdep ASSIGNING <ls_fdep>
        WHERE fieldname = <ls_field>-fieldname.
        INSERT <ls_fdep>-masterfield INTO TABLE ls_super_w_edt-key_field_tab.
      ENDLOOP.
      CHECK ls_super_w_edt-key_field_tab IS NOT INITIAL.
      INSERT ls_super_w_edt INTO TABLE me->mt_super_entity_with_edt.
    ENDLOOP.
  ENDIF.

  "Remove all editions where at least one of the direct superordinate objects
  "of the new object does not exist.
  LOOP AT me->mt_super_entity_with_edt ASSIGNING <ls_super_w_edt>.
    "Fill key of superordinate object
    CLEAR lt_super_key.
    LOOP AT <ls_super_w_edt>-key_field_tab INTO lv_fieldname.
      ASSIGN COMPONENT lv_fieldname OF STRUCTURE is_key_to TO <lv_value>.
      CHECK sy-subrc = 0.
      ls_super_key-fieldname = lv_fieldname.
      ls_super_key-value     = <lv_value>.
      INSERT ls_super_key INTO TABLE lt_super_key.
    ENDLOOP.
    CHECK lt_super_key IS NOT INITIAL.
    "Determine all editions that contain instances of the superordinate object
    lt_edition_inst = me->mo_edition_api->if_usmd_edition_entity_api~get_editions_of_entity_inc_obs(
                        iv_model  = me->mv_model
                        iv_entity = <ls_super_w_edt>-usmd_entity
                        it_key    = lt_super_key ).
    "From all editions starting at the same day we discard all except the currently valid one
    CASE me->mv_edition_timebase.
      WHEN if_usmd_edition_constants=>gc_timebase_date.
        SORT lt_edition_inst BY usmd_vdate_from ASCENDING usmd_edtn_number ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_edition_inst COMPARING usmd_vdate_from.
      WHEN if_usmd_edition_constants=>gc_timebase_period.
        SORT lt_edition_inst  BY usmd_vper_from ASCENDING usmd_edtn_number ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_edition_inst COMPARING usmd_vper_from.
    ENDCASE.
    "Remove editions that contain deleted instances
    DELETE lt_edition_inst WHERE entity_obsolete = abap_true.
    IF lt_edition_inst IS INITIAL.
      "No instances of the superordinate object exist
      "--> Dependent object cannot be created
      CLEAR ct_edition.
      RETURN.
    ENDIF.
    LOOP AT ct_edition ASSIGNING <ls_edition>.
      UNASSIGN <ls_edition_inst>.
      "Assumption: Edition types of copied object and superordinate object are both either
      "            period-based or date-based.
      IF me->mv_edition_timebase = if_usmd_edition_constants=>gc_timebase_period.
        "Determine the validity period of the superordinate object against which
        "the current edition is to be checked.
        READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst>
          WITH KEY usmd_vper_from = <ls_edition>-usmd_vper_from
          BINARY SEARCH.
        IF sy-subrc <> 0 AND sy-tabix > 1.
          READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst> INDEX sy-tabix - 1.
        ENDIF.
        CHECK <ls_edition_inst> IS NOT ASSIGNED                              OR
              <ls_edition>-usmd_vper_from < <ls_edition_inst>-usmd_vper_from OR
              <ls_edition>-usmd_vper_from > <ls_edition_inst>-usmd_vper_to.
      ELSE.
        "Determine the validity period of the superordinate object against which
        "the current edition is to be checked.
        READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst>
          WITH KEY usmd_vdate_from = <ls_edition>-usmd_vdate_from
          BINARY SEARCH.
        IF sy-subrc <> 0 AND sy-tabix > 1.
          READ TABLE lt_edition_inst ASSIGNING <ls_edition_inst> INDEX sy-tabix - 1.
        ENDIF.
        CHECK <ls_edition_inst> IS NOT ASSIGNED                                OR
              <ls_edition>-usmd_vdate_from < <ls_edition_inst>-usmd_vdate_from OR
              <ls_edition>-usmd_vdate_from > <ls_edition_inst>-usmd_vdate_to.
      ENDIF.
      "Superordinate object does not exist in validity period of currently checked edition
      "--> Remove edition from the result
      DELETE ct_edition.
    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD CONSTRUCTOR.

  me->mv_class_name = substring_after( sub = '\CLASS=' val = cl_abap_classdescr=>get_class_name( me ) ).

  me->mo_edition_api = cl_usmd_edition_factory=>get_edition_api( ).

  "In MDG 7.0 Feature Pack, 'Copy Across Editions' functionality was introduced.
  IF cl_usmd_switch_check=>mdg_application_framework_5( ) = abap_true.
    me->mv_copy_across_edt_enabled = abap_true.
  ENDIF.

ENDMETHOD.


METHOD CHECK_EXISTENCE.
*--------------------------------------------------------------------*
* This method checks the existence of the object key entered by the
* user. The parts of the key referring to "superordinate" entity
* types have to identify existing objects. The complete key does not
* have to identify an existing object since it should be created.
* If IV_CONSIDER_EDITION is set, the method checks whether the
* concrete opject instance exists, not just the object.
*--------------------------------------------------------------------*

  DATA:
    lt_mdg_mess TYPE usmd_t_message,
    lt_obj_list TYPE STANDARD TABLE OF usmd_s_crequest_entity,
    lt_crequest TYPE usmd_ts_crequest,
    lt_sel      TYPE usmd_ts_sel,
    ls_sel      LIKE LINE OF lt_sel,
    ls_ffix     LIKE LINE OF et_ffix,
    ls_ffix_edt LIKE LINE OF et_ffix,
    ls_message  LIKE LINE OF et_message,
    lv_crequest TYPE usmd_crequest,
    lv_exists   TYPE usmd_flg.

  FIELD-SYMBOLS:
    <ls_key_field> LIKE LINE OF me->mt_key_field,
    <ls_obj_list>  LIKE LINE OF lt_obj_list,
    <ls_crequest>  LIKE LINE OF lt_crequest,
    <ls_mdg_mess>  LIKE LINE OF lt_mdg_mess,
    <ls_ffix>      LIKE LINE OF et_ffix,
    <lv_value>     TYPE data.

  CLEAR: et_ffix, et_message.

  LOOP AT me->mt_key_field ASSIGNING <ls_key_field>
    WHERE int_key = abap_false.
    CLEAR ls_ffix.

    "Get value entered for current key field
    "(FPM ensures that the value is non-initial for each mandatory key field)
    ls_ffix-fieldname = <ls_key_field>-fieldname.
    ASSIGN COMPONENT <ls_key_field>-fieldname
      OF STRUCTURE is_data TO <lv_value>.
    ls_ffix-value = <lv_value>.

    CHECK ls_ffix-value IS NOT INITIAL.

    IF <ls_key_field>-fieldname = usmd0_cs_fld-edition.
      INSERT ls_ffix INTO TABLE et_ffix.
      CONTINUE.
    ENDIF.

    "Convert key value to internal format
    me->mo_model->convert_char_input(
      EXPORTING
        i_fieldname = ls_ffix-fieldname
        i_value_ext = ls_ffix-value
      IMPORTING
        e_value_int = ls_ffix-value
        et_message  = lt_mdg_mess ).
    IF lt_mdg_mess IS INITIAL.
      INSERT ls_ffix INTO TABLE et_ffix.
    ELSE.
      LOOP AT lt_mdg_mess ASSIGNING <ls_mdg_mess>.
        CLEAR ls_message.
        ls_message-msgid       = <ls_mdg_mess>-msgid.
        ls_message-msgno       = <ls_mdg_mess>-msgno.
        ls_message-severity    = if_fpm_message_manager=>gc_severity_error.
        ls_message-parameter_1 = <ls_mdg_mess>-msgv1.
        ls_message-parameter_2 = <ls_mdg_mess>-msgv2.
        ls_message-parameter_3 = <ls_mdg_mess>-msgv3.
        ls_message-parameter_4 = <ls_mdg_mess>-msgv4.
        ls_message-ref_name    = ls_ffix-fieldname.
        INSERT ls_message INTO TABLE et_message.
      ENDLOOP.
      RETURN.
    ENDIF.

    "Check existence of key value
    "  + For "superordinate" key fields, the key value has to exist
    "    (in active version, inactive version is not sufficient)
    "  + For other key fields, the key value must not exist
    "    (neither in active nor inactive version or CR object list)

    IF iv_consider_edition = abap_true AND
       <ls_key_field>-super_key = abap_false.
      "Check for the object instance instead for the object
      "-> Add edition
      ASSIGN COMPONENT usmd0_cs_fld-edition OF STRUCTURE is_data TO <lv_value>.
      IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
        ls_ffix_edt-fieldname = usmd0_cs_fld-edition.
        ls_ffix_edt-value = <lv_value>.
        INSERT ls_ffix_edt INTO TABLE et_ffix.
      ENDIF.
    ENDIF.

    "(1) Check for active version
    me->mo_model->check_single_char_existence(
      EXPORTING
        i_fieldname = ls_ffix-fieldname
        i_value     = ls_ffix-value
        it_ffix     = et_ffix
        i_readmode  = if_usmd_db_adapter=>gc_readmode_no_inact
      IMPORTING
        ef_exist    = lv_exists
        et_message  = lt_mdg_mess ).
    IF <ls_key_field>-super_key = abap_false AND
       ( lv_exists = abap_false              OR
         iv_check_locking = abap_true ).
      "(2) Check for inactive version or object list entry
      "(2.1) Read all change requests containing the object
      ls_sel-sign      = 'I'.
      ls_sel-option    = 'EQ'.
      ls_sel-fieldname = usmd0_cs_fld-fieldname_objlist.
      ls_sel-low       = ls_ffix-fieldname.
      INSERT ls_sel INTO TABLE lt_sel.
      LOOP AT et_ffix ASSIGNING <ls_ffix>.
        ls_sel-fieldname = <ls_ffix>-fieldname.
        ls_sel-low       = <ls_ffix>-value.
        INSERT ls_sel INTO TABLE lt_sel.
      ENDLOOP.
      me->mo_model->read_char_value(
        EXPORTING
          i_fieldname = usmd0_cs_fld-crequest
          it_sel      = lt_sel
          if_no_flush = abap_true
        IMPORTING
          et_data     = lt_obj_list ).
      IF NOT lt_obj_list IS INITIAL.
        "(2.2) Determine change requests that are not finally rejected or approved
        CLEAR lt_sel.
        SORT lt_obj_list BY usmd_crequest.
        DELETE ADJACENT DUPLICATES FROM lt_obj_list COMPARING usmd_crequest.
        ls_sel-fieldname = usmd0_cs_fld-crequest.
        LOOP AT lt_obj_list ASSIGNING <ls_obj_list>.
          ls_sel-low = <ls_obj_list>-usmd_crequest.
          INSERT ls_sel INTO TABLE lt_sel.
        ENDLOOP.
        IF iv_consider_edition = abap_true AND
           ls_ffix_edt-value IS NOT INITIAL.
          "Add the edition to the selection to get only CRs of this edition
          ls_sel-fieldname = usmd0_cs_fld-edition.
          ls_sel-low       = ls_ffix_edt-value.
          INSERT ls_sel INTO TABLE lt_sel.
        ENDIF.
        ls_sel-sign = 'E'.
        ls_sel-fieldname = usmd0_cs_fld-crequest_status.
        ls_sel-low = usmd0_cs_crequest_status-finally_rejected.
        INSERT ls_sel INTO TABLE lt_sel.
        ls_sel-low = usmd0_cs_crequest_status-finally_approved.
        INSERT ls_sel INTO TABLE lt_sel.
        me->mo_model->read_char_value(
          EXPORTING
            i_fieldname = usmd0_cs_fld-crequest
            it_sel      = lt_sel
            if_no_flush = abap_true
          IMPORTING
            et_data     = lt_crequest ).
        READ TABLE lt_crequest ASSIGNING <ls_crequest> INDEX 1.
        IF sy-subrc = 0.
          "There is at least one change request that contains the object
          "and is not finally rejected -> the object already "exists"
          lv_exists = abap_true.
          lv_crequest = <ls_crequest>-usmd_crequest.
        ENDIF.
      ENDIF.
    ENDIF.
    IF <ls_key_field>-super_key = abap_true AND
       lv_exists = abap_false.
      "Superordinate key field with unknown key value
      LOOP AT lt_mdg_mess ASSIGNING <ls_mdg_mess>.
        CLEAR ls_message.
        ls_message-msgid       = <ls_mdg_mess>-msgid.
        ls_message-msgno       = <ls_mdg_mess>-msgno.
        ls_message-severity    = if_fpm_message_manager=>gc_severity_error.
        ls_message-parameter_1 = <ls_mdg_mess>-msgv1.
        ls_message-parameter_2 = <ls_mdg_mess>-msgv2.
        ls_message-parameter_3 = <ls_mdg_mess>-msgv3.
        ls_message-parameter_4 = <ls_mdg_mess>-msgv4.
        ls_message-ref_name    = ls_ffix-fieldname.
        INSERT ls_message INTO TABLE et_message.
      ENDLOOP.
      RETURN.
    ELSEIF <ls_key_field>-super_key = abap_false AND
           lv_exists = abap_true.
      "Other key field with existing key value
      IF 0 = 1.
        MESSAGE e007(usmdcrui) WITH '_' '_'.
        MESSAGE e008(usmdcrui) WITH '_' '_' '_'.
        MESSAGE e011(usmdcrui) WITH '_' '_' '_'.
      ELSE.
        ls_message-msgid       = 'USMDCRUI'.
        IF lv_crequest IS INITIAL.
          ls_message-msgno     = '007'.
        ELSEIF iv_check_locking = abap_false.
          ls_message-msgno     = '008'.
        ELSE.
          ls_message-msgno     = '011'.
        ENDIF.
        ls_message-severity    = if_fpm_message_manager=>gc_severity_error.
        ls_message-parameter_1 = me->mv_entity_text.
        me->mo_model->chaval2msgvar(
          EXPORTING
            i_fieldname = ls_ffix-fieldname
            i_value     = ls_ffix-value
            it_ffix     = et_ffix
          IMPORTING
            e_chaval    = ls_message-parameter_2 ).
        IF lv_crequest IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lv_crequest
            IMPORTING
              output = ls_message-parameter_3.
        ENDIF.
        ls_message-ref_name = ls_ffix-fieldname.
        INSERT ls_message INTO TABLE et_message.
      ENDIF.
      RETURN.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD ADD_FILTER_CRITERION.
*--------------------------------------------------------------------*
* This method adds a filter criterion to filter text CV_FILTER_TXT
* that can be used in dynamic WHERE conditions.
* The criterion is of form <operand> NE/NP <value> where <operand>
* is the field name from IV_FIELDNAME and <value> is taken from
* structure IS_FILTER_VALUE. Multiple criteria are linked via ORs.
*--------------------------------------------------------------------*

  DATA:
    lv_fieldname type string,
    lv_value_txt TYPE string.

  FIELD-SYMBOLS:
    <lv_value> TYPE data.

  "The following check is done for security reasons.
  TRY.
      lv_fieldname = cl_abap_dyn_prg=>check_column_name( val = iv_fieldname ).
    CATCH cx_abap_invalid_name.
      RETURN.
  ENDTRY.

  "Get filter value from structure
  ASSIGN COMPONENT lv_fieldname OF STRUCTURE is_filter_value TO <lv_value>.
  IF sy-subrc <> 0 OR <lv_value> IS INITIAL.
    RETURN.
  ENDIF.

  "Add quotes to the value securely
  lv_value_txt = <lv_value>.
  lv_value_txt = cl_abap_dyn_prg=>quote( lv_value_txt ).

  "Create filter criterion
  IF cv_filter_txt IS NOT INITIAL.
    CONCATENATE cv_filter_txt 'OR' INTO cv_filter_txt SEPARATED BY ' '.
  ENDIF.
  IF lv_value_txt CA '*?'.
    "Pattern (with wildcards)
    CONCATENATE cv_filter_txt lv_fieldname 'NP' lv_value_txt
      INTO cv_filter_txt SEPARATED BY ' '.
  ELSE.
    "Plain value
    CONCATENATE cv_filter_txt lv_fieldname 'NE' lv_value_txt
      INTO cv_filter_txt SEPARATED BY ' '.
  ENDIF.

ENDMETHOD.
ENDCLASS.

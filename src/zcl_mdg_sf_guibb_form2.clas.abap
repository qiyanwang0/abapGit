class ZCL_MDG_SF_GUIBB_FORM2 definition
  public
  inheriting from CL_MDG_BS_GUIBB_FORM
  final
  create public .

public section.

  methods IF_FPM_GUIBB_FORM~GET_DATA
    redefinition .
  methods IF_FPM_GUIBB_FORM~GET_DEFINITION
    redefinition .
  methods IF_FPM_GUIBB_OVS~HANDLE_PHASE_1
    redefinition .
  methods IF_FPM_GUIBB_OVS~HANDLE_PHASE_2
    redefinition .
  methods IF_FPM_GUIBB_OVS~HANDLE_PHASE_3
    redefinition .
protected section.

  data MV_ORIG_PAGE_TITLE type FPM_CONTENT_AREA_TITLE .

  methods GET_ATTR_VALUE_SET
    redefinition .
  methods OVS_HANDLE_PHASE_2
    redefinition .
  methods OVS_HANDLE_PHASE_3
    redefinition .
private section.

  data M_COUNTRY type LAND1 .
  data C type CHAR1 .

  methods OVS_OUTPUT_CITIES_FROM
    exporting
      !ER_OUTPUT type ref to DATA .
  methods OVS_OUTPUT_CITIES_TO
    exporting
      !ER_OUTPUT type ref to DATA .
ENDCLASS.



CLASS ZCL_MDG_SF_GUIBB_FORM2 IMPLEMENTATION.


  method GET_ATTR_VALUE_SET.

    if IV_ATTR_NAME = 'AIRPFROM'  or
       IV_ATTR_NAME = 'AIRPTO'    or
       IV_ATTR_NAME = 'COUNTRYFR' or
       IV_ATTR_NAME = 'COUNTRYTO' .

      CALL METHOD SUPER->GET_ATTR_VALUE_SET
        EXPORTING
          IO_ACCESS      = IO_ACCESS
          IV_OBJECT_NAME = IV_OBJECT_NAME
          IV_ATTR_NAME   = IV_ATTR_NAME
        IMPORTING
          ET_VALUE_SET   = ET_VALUE_SET.

    endif.

* All other fields don't need the value set. The unit of measure (DISTID) is checked via
* a conversion exit (debug in CL_WDR_CONVERSION_UTILS=>CONVERT_INTERNAL).
  endmethod.


METHOD IF_FPM_GUIBB_FORM~GET_DATA.

  DATA:
    lv_entity_name          TYPE usmd_entity,
    lv_model                TYPE usmd_model,
    lr_entity_key_structure TYPE REF TO data,
    lr_entity_key_table     TYPE REF TO data,
    lo_conv_api             TYPE REF TO if_usmd_conv_som_gov_api,
    lo_strucdescr           TYPE REF TO cl_abap_structdescr,
    lt_key_component        TYPE cl_abap_structdescr=>component_table,
    ls_key_component        TYPE cl_abap_structdescr=>component,
    lv_entity_formatted     TYPE string,
    lo_fpm                  TYPE REF TO if_fpm,
    lo_cnr_ovp              TYPE REF TO if_fpm_cnr_ovp,
    ls_content_area         TYPE if_fpm_ovp=>ty_s_content_area.

  FIELD-SYMBOLS:
    <ls_entity_key>         TYPE any,
    <lt_entity_key>         TYPE INDEX TABLE,
    <lv_field>              TYPE any,
    <lv_value>              TYPE any,
    <fs_field_usage>        type FPMGB_S_FIELDUSAGE .

  CALL METHOD super->if_fpm_guibb_form~get_data
    EXPORTING
      io_event                = io_event
      iv_raised_by_own_ui     = iv_raised_by_own_ui
      it_selected_fields      = it_selected_fields
      iv_edit_mode            = iv_edit_mode
      io_extended_ctrl        = io_extended_ctrl
    IMPORTING
      et_messages             = et_messages
      ev_data_changed         = ev_data_changed
      ev_field_usage_changed  = ev_field_usage_changed
      ev_action_usage_changed = ev_action_usage_changed
    CHANGING
      cs_data                 = cs_data
      ct_field_usage          = ct_field_usage
      ct_action_usage         = ct_action_usage.

  lo_fpm = cl_fpm_factory=>get_instance( ).

  cl_usmd_generic_bolui_assist=>get_application_parameter(
    IMPORTING
      ev_entity = lv_entity_name
      ev_model  = lv_model ).

  lo_conv_api = cl_usmd_conv_som_gov_api=>get_instance( iv_model_name = lv_model ).

  lo_conv_api->get_entity_structure(
    EXPORTING
      iv_entity_name = lv_entity_name
      iv_struct_type = if_usmd_conv_som_gov_entity=>gc_struct_key
    IMPORTING
      er_structure   = lr_entity_key_structure
      er_table       = lr_entity_key_table ).

  ASSIGN lr_entity_key_structure->* TO <ls_entity_key>.
  ASSIGN lr_entity_key_table->* TO <lt_entity_key>.

  lo_strucdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_entity_key_structure ).
  lt_key_component = lo_strucdescr->get_components( ).

  LOOP AT lt_key_component INTO ls_key_component.
    ASSIGN COMPONENT ls_key_component-name OF STRUCTURE cs_data TO <lv_field>.
    ASSIGN COMPONENT ls_key_component-name OF STRUCTURE <ls_entity_key> TO <lv_value>.
    <lv_value> = <lv_field>.
    CONCATENATE lv_entity_formatted <lv_value> INTO lv_entity_formatted SEPARATED BY space.
  ENDLOOP.

  lo_cnr_ovp ?= lo_fpm->get_service( cl_fpm_service_manager=>gc_key_cnr_ovp ).

  TRY.
      ls_content_area = lo_cnr_ovp->get_current_content_area( ).
      IF me->mv_orig_page_title IS INITIAL.
        "Remember original page title defined in OVP configuration
        me->mv_orig_page_title = ls_content_area-title.
      ENDIF.
      "Create new page title from original page title and entity key
      CONCATENATE me->mv_orig_page_title lv_entity_formatted INTO lv_entity_formatted SEPARATED BY space.

      IF ls_content_area-title <> lv_entity_formatted.

        lo_cnr_ovp->change_content_area_restricted(
          EXPORTING
            iv_content_area_id      = ls_content_area-id
            iv_title                = lv_entity_formatted ).

      ENDIF.
    CATCH cx_fpm_floorplan.
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD IF_FPM_GUIBB_FORM~GET_DEFINITION.

  FIELD-SYMBOLS:
   <ls_field_description> LIKE LINE OF et_field_description.

  super->if_fpm_guibb_form~get_definition(
    IMPORTING
      es_message               = es_message
      eo_field_catalog         = eo_field_catalog
      et_field_description     = et_field_description
      et_action_definition     = et_action_definition
      et_special_groups        = et_special_groups
      ev_additional_error_info = ev_additional_error_info
      et_dnd_definition        = et_dnd_definition ).

* Make OVS available for the city
  READ TABLE et_field_description ASSIGNING
    <ls_field_description> WITH KEY name = 'CITYTO'.
  IF sy-subrc = 0.
    <ls_field_description>-ovs_name = me->mv_my_classname.
  ENDIF.

  READ TABLE et_field_description ASSIGNING
    <ls_field_description> WITH KEY name = 'CITYFROM'.
  IF sy-subrc = 0.
    <ls_field_description>-ovs_name = me->mv_my_classname.
  ENDIF.

  READ TABLE et_field_description ASSIGNING
    <ls_field_description> WITH KEY name = 'PERIOD'.
  IF sy-subrc = 0.
    <ls_field_description>-ovs_name = me->mv_my_classname.
  ENDIF.

  LOOP AT et_field_description ASSIGNING <ls_field_description> WHERE
     name = 'DISTANCE' OR
     name = 'DISTID'   OR
     name = 'FLTIME'   OR
     name = 'FLTYPE'   OR
     name = 'PERIOD' .
    <ls_field_description>-is_nullable = abap_true.
  ENDLOOP.

ENDMETHOD.


METHOD IF_FPM_GUIBB_OVS~HANDLE_PHASE_1.

  DATA lf_fieldname TYPE string.

  CLEAR me->m_country.
clear me->C.
  CASE iv_field_name.
    WHEN 'CITYTO' .
      lf_fieldname = 'COUNTRYTO'.
    WHEN 'CITYFROM' .
      lf_fieldname = 'COUNTRYFR'.
      when 'PERIOD'.
        lf_fieldname = 'period'.
  ENDCASE.

  IF iv_field_name = 'CITYTO' OR iv_field_name = 'CITYFROM'.
    io_ovs_callback->context_element->get_attribute(
      EXPORTING
        name  = lf_fieldname
      IMPORTING
        value = me->m_country ).
  ENDIF.

  IF iv_field_name = 'PERIOD'.
    io_ovs_callback->context_element->get_attribute(
      EXPORTING
        name  = lf_fieldname
      IMPORTING
        value = me->m_country ).
  ENDIF.
ENDMETHOD.


  method IF_FPM_GUIBB_OVS~HANDLE_PHASE_2.
*CALL METHOD SUPER->IF_FPM_GUIBB_OVS~HANDLE_PHASE_2
*  EXPORTING
*    IV_FIELD_NAME   =
*    IO_OVS_CALLBACK =
**    iv_index        =
*    .


           TYPES : BEGIN OF lty_teds2,
            matnr TYPE c LENGTH 1,
             matnr1  TYPE c LENGTH 1,
          END OF lty_teds2.
     DATA : lit_teds2 TYPE STANDARD TABLE OF lty_teds2.

        SELECT matnr matnr1 FROM ztestwang INTO TABLE lit_teds2 UP TO 2 ROWS.



       io_ovs_callback->set_output_table(
    EXPORTING
      output       = lit_teds2
*      table_header = table_header
*      column_texts = column_texts    ” Table of Name Value Pairs

).
  endmethod.


  method IF_FPM_GUIBB_OVS~HANDLE_PHASE_3.
*CALL METHOD SUPER->IF_FPM_GUIBB_OVS~HANDLE_PHASE_3
*  EXPORTING
*    IV_FIELD_NAME           =
*    IO_OVS_CALLBACK         =
**    iv_wd_context_attr_name =
**  IMPORTING
**    eo_fpm_event            =
*    .

            TYPES : BEGIN OF lty_teds2,
            matnr TYPE c LENGTH 1,
            matnr1 TYPE c LENGTH 1,
          END OF lty_teds2.
     FIELD-SYMBOLS: <ls_selection>    TYPE lty_teds2.


             ASSIGN io_ovs_callback->selection->* TO <ls_selection>.

         io_ovs_callback->context_element->set_attribute(
      EXPORTING
        value = <ls_selection>-matnr
        name  = iv_wd_context_attr_name ).
  endmethod.


METHOD OVS_HANDLE_PHASE_2.

  CASE iv_field_name.
    WHEN 'CITYTO' .
      me->ovs_output_cities_to( IMPORTING er_output  = er_output ).
    WHEN 'CITYFROM'.
      me->ovs_output_cities_from( IMPORTING er_output  = er_output ).
  ENDCASE.

ENDMETHOD.


METHOD OVS_HANDLE_PHASE_3.

  super->ovs_handle_phase_3(
    EXPORTING
      iv_field_name  = iv_field_name
      ir_selection   = ir_selection
    IMPORTING
      et_field_value = et_field_value
      eo_fpm_event   = eo_fpm_event ).

ENDMETHOD.


METHOD OVS_OUTPUT_CITIES_FROM.

  DATA:
    lv_message           TYPE string,                       "#EC NEEDED
    lo_message_container TYPE REF TO cl_crm_genil_global_mess_cont.

  TYPES:
   BEGIN OF lty_s_cities,
     cityfrom TYPE s_from_cit, "field name has to be identical to attribute in context. Ohterwise phase_3 has to be redefined
    END  OF lty_s_cities,

   lty_t_cities TYPE SORTED TABLE OF lty_s_cities WITH UNIQUE KEY cityfrom.

  FIELD-SYMBOLS: <lt_cities> TYPE lty_t_cities .

  CREATE DATA er_output TYPE lty_t_cities .

  IF m_country IS INITIAL.
    MESSAGE w020(mdg_sf) INTO lv_message.
    lo_message_container = cl_crm_bol_core=>get_instance( )->get_global_message_cont( ).
    ASSERT lo_message_container IS BOUND.
    lo_message_container->add_message(
      iv_msg_type       = sy-msgty
      iv_msg_id         = sy-msgid
      iv_msg_number     = sy-msgno
      iv_show_only_once = abap_true ).
    RETURN.
  ENDIF.

  ASSIGN er_output->* TO <lt_cities> .
  SELECT city FROM sgeocity INTO TABLE <lt_cities>
    WHERE country = m_country .

ENDMETHOD.


METHOD OVS_OUTPUT_CITIES_TO.

  DATA:
    lv_message           TYPE string,                       "#EC NEEDED
    lo_message_container TYPE REF TO cl_crm_genil_global_mess_cont.

  TYPES:
   BEGIN OF lty_s_cities,
     cityto TYPE s_to_city, "field name has to be identical to attribute in context. Ohterwise phase_3 has to be redefined
    END  OF lty_s_cities,

   lty_t_cities TYPE SORTED TABLE OF lty_s_cities WITH UNIQUE KEY cityto.

  FIELD-SYMBOLS: <lt_cities> TYPE lty_t_cities .

  CREATE DATA er_output TYPE lty_t_cities .

  IF m_country IS INITIAL.
    MESSAGE w020(mdg_sf) INTO lv_message.
    lo_message_container = cl_crm_bol_core=>get_instance( )->get_global_message_cont( ).
    ASSERT lo_message_container IS BOUND.
    lo_message_container->add_message(
      iv_msg_type       = sy-msgty
      iv_msg_id         = sy-msgid
      iv_msg_number     = sy-msgno
      iv_show_only_once = abap_true ).
    RETURN.
  ENDIF.

  ASSIGN er_output->* TO <lt_cities> .
  SELECT city FROM sgeocity INTO TABLE <lt_cities>
    WHERE country = m_country .

ENDMETHOD.
ENDCLASS.

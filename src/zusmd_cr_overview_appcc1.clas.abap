class ZUSMD_CR_OVERVIEW_APPCC1 definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_TABBED_CONF_EXIT .

  methods CONSTRUCTOR .
protected section.

  data MO_MDG_API type ref to IF_USMD_CONV_SOM_GOV_API .
  data MO_FPM type ref to IF_FPM .
private section.
ENDCLASS.



CLASS ZUSMD_CR_OVERVIEW_APPCC1 IMPLEMENTATION.


method CONSTRUCTOR.

  me->mo_fpm = cl_fpm_factory=>get_instance( ).
  me->mo_mdg_api = cl_usmd_conv_som_gov_api=>get_instance( ).

endmethod.


METHOD IF_FPM_TABBED_CONF_EXIT~OVERRIDE_CONFIG_TABBED.

  DATA:
    tab_id       TYPE string,
    lv_cr_action TYPE usmd_crequest_action.

  "View or Hide Tab for Targetsystems based on Conv_API variable
  "(Variable is filled there via SET_ENVIRONMENT based on Custom. USMD110)
  IF mo_mdg_api IS BOUND.
    io_tabbed->set_tab_visible(
      EXPORTING
        iv_tab_id  = 'SYSTEMS'
        iv_visible = mo_mdg_api->mv_target_systems_enabled ).
  ENDIF.

  DATA ftype TYPE usmd_crequest_type.
  ftype = mo_mdg_api->mv_crequest_type.
  IF ftype = 'CCT1P1'.
    TRY.
        io_tabbed->set_tab_visible( iv_tab_id = 'ATTACHMENTS'  iv_visible = '' ).
      CATCH cx_fpm_floorplan.
    ENDTRY.
  ENDIF.
  "Process events that are change request actions
  IF cl_usmd5_cust_access_service=>is_cr_action( |{ io_tabbed->mo_event->mv_event_id }| ) = abap_true.
    "For any kind of Reject-Action without note, jump to Tab Notes
    lv_cr_action = io_tabbed->mo_event->mv_event_id.

    IF  cl_usmd5_cust_access_service=>is_note_required( lv_cr_action ) = abap_true
    AND me->mo_mdg_api->mv_note_created IS INITIAL.

      TRY.
          io_tabbed->set_selected_tab( iv_tab_id = 'NOTES' ).  " ID of new active tab
        CATCH cx_fpm_floorplan.                         "#EC NO_HANDLER
      ENDTRY.

      me->mo_fpm->raise_event_by_id( iv_event_id = cl_usmd_cr_guibb_notes=>cv_event_id_switch_note_tab ).
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.

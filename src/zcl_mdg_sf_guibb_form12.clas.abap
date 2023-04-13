class ZCL_MDG_SF_GUIBB_FORM12 definition
  public
  final
  create public .

public section.

  interfaces IF_FPM_GUIBB .
  interfaces IF_FPM_GUIBB_SEARCH .
  interfaces IF_FPM_GUIBB_OVS .
  interfaces IF_FPM_GUIBB_FORM .
protected section.
private section.

 methods IF_FPM_GUIBB .
  methods IF_PFM_GUIBB_SEARCH .
  methods IF_FPM_GUIBB_OVS .
ENDCLASS.



CLASS ZCL_MDG_SF_GUIBB_FORM12 IMPLEMENTATION.


  method IF_FPM_GUIBB.
  endmethod.


  method IF_FPM_GUIBB_OVS.
  endmethod.


  method IF_FPM_GUIBB_OVS~HANDLE_PHASE_2.

           TYPES : BEGIN OF lty_teds2,
            matnr TYPE matnr,
            maktx TYPE maktx,
          END OF lty_teds2.
     DATA : lit_teds2 TYPE STANDARD TABLE OF lty_teds2.

        SELECT matnr maktx FROM makt INTO TABLE lit_teds2 UP TO 10 ROWS.



       io_ovs_callback->set_output_table(
    EXPORTING
      output       = lit_teds2
*      table_header = table_header
*      column_texts = column_texts    ” Table of Name Value Pairs

).
  endmethod.


  method IF_FPM_GUIBB_OVS~HANDLE_PHASE_3.

        TYPES : BEGIN OF lty_teds2,
            matnr TYPE c LENGTH 1 ,
            matnr1  TYPE c LENGTH 2 ,
          END OF lty_teds2.
     FIELD-SYMBOLS: <ls_selection>    TYPE lty_teds2.


             ASSIGN io_ovs_callback->selection->* TO <ls_selection>.

         io_ovs_callback->context_element->set_attribute(
      EXPORTING
        value = <ls_selection>-matnr
        name  = iv_wd_context_attr_name ).
  endmethod.


  method IF_FPM_GUIBB_SEARCH~GET_DEFINITION.

    APPEND INITIAL LINE TO et_field_description_attr
    ASSIGNING FIELD-SYMBOL(<fs_field_description_sttr>).
    <fs_field_description_sttr>-name = 'COUNTRYTO__TXT2'.
    <fs_field_description_sttr>-ovs_name = 'ZCL_MDG_SF_GUIBB_FORM1'.
  endmethod.


  method IF_PFM_GUIBB_SEARCH.
  endmethod.
ENDCLASS.

*&---------------------------------------------------------------------*
*&  Include           Z4_MONSTER_AM_TOP
*&---------------------------------------------------------------------*
REPORT z4_monster_atrocity_monitor."REPORT statement here to avoid bogus syntax errors
*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
TYPES: g_typ_am_cds_view TYPE z4c_monster_atrocity_monitor.

TYPES: BEGIN OF g_typ_alv_output_data,
         check   TYPE char01.
         INCLUDE TYPE g_typ_am_cds_view.
         "Need this to make individual cells/columns editable
         TYPES: celltab TYPE lvc_t_styl,
       END OF g_typ_alv_output_data.

TYPES: g_tt_output_data TYPE STANDARD TABLE OF g_typ_alv_output_data WITH EMPTY KEY.

*--------------------------------------------------------------------*
* Global Variables
*--------------------------------------------------------------------*
* Global Variables for Push Channels
* Not 100% sure they need to be global as yet, but they are in the
* standard SAP example programs
*--------------------------------------------------------------------*
DATA: gd_appl_id          TYPE amc_application_id VALUE 'ZAMC_FOR_MONSTERS' ##NEEDED,
      gd_ch_id            TYPE amc_channel_id     VALUE '/monsters' ##NEEDED,
      gd_chext_id         TYPE amc_channel_extension_id ##NEEDED,
      go_consumer         TYPE REF TO if_amc_message_consumer ##NEEDED,
      gf_message_received TYPE abap_bool ##NEEDED,
      gv_message          TYPE string ##NEEDED,
      gt_message_list     TYPE TABLE OF string ##NEEDED,
      gt_pcp_list         TYPE TABLE OF REF TO if_ac_message_type_pcp ##NEEDED,
      go_pcp              TYPE REF TO if_ac_message_type_pcp ##NEEDED,
      gt_pcp_fields       TYPE pcp_fields ##NEEDED.

*--------------------------------------------------------------------*
* Local Classes
*--------------------------------------------------------------------*
INCLUDE z4_monster_am_cd01.

*--------------------------------------------------------------------*
* Global Variables that Refer to Local Classes
*--------------------------------------------------------------------*
DATA: go_selections TYPE REF TO lcl_selections ##NEEDED,
      go_receiver   TYPE REF TO lcl_amc_receiver ##NEEDED.

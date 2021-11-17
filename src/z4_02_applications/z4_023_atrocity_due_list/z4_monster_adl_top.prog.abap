*&---------------------------------------------------------------------*
*&  Include           Z_MONSTER_ADL_TOP
*&---------------------------------------------------------------------*
REPORT z4_monster_atrocity_due_list.
*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
TYPES: g_typ_adl_cds TYPE z4c_monster_atrocity_due_list."CDS Consumption View

TYPES: BEGIN OF g_typ_alv_output_data,
         check   TYPE char01.
         INCLUDE TYPE g_typ_adl_cds.
         "Need this to make individual cells/columns editable
         TYPES: celltab TYPE lvc_t_styl,
       END OF g_typ_alv_output_data.

TYPES: g_tt_output_data TYPE STANDARD TABLE OF g_typ_alv_output_data WITH EMPTY KEY ##NEEDED.

*--------------------------------------------------------------------*
* Constants
*--------------------------------------------------------------------*
* Listing 11.13: Strength Column Constant
*--------------------------------------------------------------------*
CONSTANTS:
  monster_column_constant  TYPE zexcel_cell_column_alpha VALUE 'A',
  strength_column_constant TYPE zexcel_cell_column_alpha VALUE 'E',
  age_column_constant      TYPE zexcel_cell_column_alpha VALUE 'F'.

*--------------------------------------------------------------------*
* Local Classes
*--------------------------------------------------------------------*
INCLUDE z4_monster_adl_cd01.

DATA: go_selections TYPE REF TO lcl_selections ##NEEDED,
      gs_selections TYPE z4c_monster_atrocity_monitor ##NEEDED."For Selection SCreen

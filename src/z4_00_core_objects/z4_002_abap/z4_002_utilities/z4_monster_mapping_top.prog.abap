*&---------------------------------------------------------------------*
*& Include          Z_MONSTER_MAPPING_TOP
*&---------------------------------------------------------------------*
REPORT z4_monster_listings_mapping.
**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF g_typ_alv_output_data,
         chapter     TYPE sy-tabix,
         listing     TYPE sy-tabix,
         name        TYPE string,
         object_type TYPE trobjtype, "e.g. class/program
         object_name TYPE string, "e.g. class name
         subobject   TYPE string, "e.g. method name
         devclass    TYPE tadir-devclass,"Package
       END OF g_typ_alv_output_data.

TYPES: g_tt_output_data TYPE STANDARD TABLE OF g_typ_alv_output_data WITH EMPTY KEY.

*--------------------------------------------------------------------*
* Local Class Definitions
*--------------------------------------------------------------------*
INCLUDE z4_monster_map_cd01.

DATA: go_selections TYPE REF TO lcl_selections ##NEEDED.

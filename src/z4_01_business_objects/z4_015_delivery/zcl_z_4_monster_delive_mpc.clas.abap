class ZCL_Z_4_MONSTER_DELIVE_MPC definition
  public
  inheriting from /IWBEP/CL_MGW_PUSH_ABS_MODEL
  create public .

public section.

  interfaces IF_SADL_GW_MODEL_EXPOSURE_DATA .

  types:
   TS_I_DRAFTADMINISTRATIVEDATATY type SDRAFT_ADMIN_CDS .
  types:
   TT_I_DRAFTADMINISTRATIVEDATATY type standard table of TS_I_DRAFTADMINISTRATIVEDATATY .
  types:
    begin of TS_Z4CDS_MONSTER_DELIVERIESTYP.
      include type Z4V_MONS_DELS.
  types:
      ACTIVEUUID type Z4T_DRAFT_DELS-ACTIVEUUID,
      DRAFTENTITYCREATIONDATETIME type Z4T_DRAFT_DELS-DRAFTENTITYCREATIONDATETIME,
      DRAFTENTITYLASTCHANGEDATETIME type Z4T_DRAFT_DELS-DRAFTENTITYLASTCHANGEDATETIME,
      HASACTIVEENTITY type SDRAFT_HAS_ACTIVE,
      HASDRAFTENTITY type SDRAFT_HAS_DRAFT,
      ISACTIVEENTITY type SDRAFT_IS_ACTIVE,
      A_ACTIVATION type SADL_GW_DYNAMIC_ACTN_PROPERTY,
      A_EDIT type SADL_GW_DYNAMIC_ACTN_PROPERTY,
      A_PREPARATION type SADL_GW_DYNAMIC_ACTN_PROPERTY,
      A_VALIDATION type SADL_GW_DYNAMIC_ACTN_PROPERTY,
    end of TS_Z4CDS_MONSTER_DELIVERIESTYP .
  types:
   TT_Z4CDS_MONSTER_DELIVERIESTYP type standard table of TS_Z4CDS_MONSTER_DELIVERIESTYP .

  constants GC_I_DRAFTADMINISTRATIVEDATATY type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'I_DraftAdministrativeDataType' ##NO_TEXT.
  constants GC_Z4CDS_MONSTER_DELIVERIESTYP type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME value 'Z4CDS_MONSTER_DELIVERIESType' ##NO_TEXT.

  methods DEFINE
    redefinition .
  methods GET_LAST_MODIFIED
    redefinition .
protected section.
private section.

  methods DEFINE_RDS_4
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GET_LAST_MODIFIED_RDS_4
    returning
      value(RV_LAST_MODIFIED_RDS) type TIMESTAMP .
ENDCLASS.



CLASS ZCL_Z_4_MONSTER_DELIVE_MPC IMPLEMENTATION.


  method DEFINE.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*

model->set_schema_namespace( 'Z_4_MONSTER_DELIVERY_CDS_PULL_SRV' ).

define_rds_4( ).
get_last_modified_rds_4( ).
  endmethod.


  method DEFINE_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
    TRY.
        if_sadl_gw_model_exposure_data~get_model_exposure( )->expose( model )->expose_vocabulary( vocab_anno_model ).
      CATCH cx_sadl_exposure_error INTO DATA(lx_sadl_exposure_error).
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_med_exception
          EXPORTING
            previous = lx_sadl_exposure_error.
    ENDTRY.
  endmethod.


  method GET_LAST_MODIFIED.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS         &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL  &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                   &*
*&                                                                     &*
*&---------------------------------------------------------------------*


  CONSTANTS: lc_gen_date_time TYPE timestamp VALUE '20210630082719'.                  "#EC NOTEXT
 DATA: lv_rds_last_modified TYPE timestamp .
  rv_last_modified = super->get_last_modified( ).
  IF rv_last_modified LT lc_gen_date_time.
    rv_last_modified = lc_gen_date_time.
  ENDIF.
 lv_rds_last_modified =  GET_LAST_MODIFIED_RDS_4( ).
 IF rv_last_modified LT lv_rds_last_modified.
 rv_last_modified  = lv_rds_last_modified .
 ENDIF .
  endmethod.


  method GET_LAST_MODIFIED_RDS_4.
*&---------------------------------------------------------------------*
*&           Generated code for the MODEL PROVIDER BASE CLASS          &*
*&                                                                     &*
*&  !!!NEVER MODIFY THIS CLASS. IN CASE YOU WANT TO CHANGE THE MODEL   &*
*&        DO THIS IN THE MODEL PROVIDER SUBCLASS!!!                    &*
*&                                                                     &*
*&---------------------------------------------------------------------*
*   This code is generated for Reference Data Source
*   4
*&---------------------------------------------------------------------*
    CONSTANTS: co_gen_date_time TYPE timestamp VALUE '20210630082720'.
    TRY.
        rv_last_modified_rds = CAST cl_sadl_gw_model_exposure( if_sadl_gw_model_exposure_data~get_model_exposure( ) )->get_last_modified( ).
      CATCH cx_root.
        rv_last_modified_rds = co_gen_date_time.
    ENDTRY.
    IF rv_last_modified_rds < co_gen_date_time.
      rv_last_modified_rds = co_gen_date_time.
    ENDIF.
  endmethod.


  method IF_SADL_GW_MODEL_EXPOSURE_DATA~GET_MODEL_EXPOSURE.
    CONSTANTS: co_gen_timestamp TYPE timestamp VALUE '20210630082720'.
    DATA(lv_sadl_xml) =
               |<?xml version="1.0" encoding="utf-16"?>|  &
               |<sadl:definition xmlns:sadl="http://sap.com/sap.nw.f.sadl" syntaxVersion="" >|  &
               | <sadl:dataSource type="CDS" name="Z4CDS_MONSTER_DELIVERIES" binding="Z4CDS_MONSTER_DELIVERIES" />|  &
               |<sadl:resultSet>|  &
               |<sadl:structure name="Z4CDS_MONSTER_DELIVERIES" dataSource="Z4CDS_MONSTER_DELIVERIES" maxEditMode="RO" exposure="TRUE" >|  &
               | <sadl:query name="SADL_QUERY">|  &
               | </sadl:query>|  &
               |</sadl:structure>|  &
               |</sadl:resultSet>|  &
               |</sadl:definition>| .

   ro_model_exposure = cl_sadl_gw_model_exposure=>get_exposure_xml( iv_uuid      = CONV #( 'Z_4_MONSTER_DELIVERY_CDS_PULL' )
                                                                    iv_timestamp = co_gen_timestamp
                                                                    iv_sadl_xml  = lv_sadl_xml ).
  endmethod.
ENDCLASS.

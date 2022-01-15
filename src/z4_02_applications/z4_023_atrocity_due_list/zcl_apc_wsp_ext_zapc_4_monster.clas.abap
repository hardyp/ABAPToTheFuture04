class ZCL_APC_WSP_EXT_ZAPC_4_MONSTER definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
protected section.
private section.

  data MO_MONSTER_MODEL type ref to ZCL_4_MONSTER_MODEL ##NEEDED.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZAPC_4_MONSTER IMPLEMENTATION.


  METHOD if_apc_wsp_extension~on_message.
*--------------------------------------------------------------------*
* Listing 14.08: -  Coding ON_MESSAGE Method
*--------------------------------------------------------------------*
    TRY.
        "(1) Decode incoming message
        DATA(incoming_text_message) = i_message->get_text( ).

        DATA: order_number TYPE ztmonster_adl-order_number,
              monster_name TYPE ztmonster_am-monster_name.

        SPLIT incoming_text_message AT ';'
        INTO order_number monster_name.

        "(2) Get existing order details
        SELECT *
          FROM z4t_order_items UP TO 1 ROWS
          INTO @DATA(monster_due_list_record)
          WHERE order_number = @order_number
          ORDER BY PRIMARY KEY.
        ENDSELECT.

        IF sy-subrc NE 0.
          RETURN.
        ENDIF.

        "(3) Prepare new "delivery" details
        DATA: monitor_list_record TYPE z4t_deliveries.

        monitor_list_record = CORRESPONDING #( monster_due_list_record ).

        DATA(lo_uuid_generator) = cl_uuid_factory=>create_system_uuid( ).

        TRY.
            monitor_list_record-db_key =  lo_uuid_generator->create_uuid_x16( ).
          CATCH cx_uuid_error INTO DATA(uuid_error).
            MESSAGE uuid_error TYPE 'I'.
            RETURN.
        ENDTRY.

        monitor_list_record-monster_name    = monster_name.
        monitor_list_record-current_status  = 'A'."Atrocity ready to be committed
        monitor_list_record-delivery_number = sy-datum+2(2) &&
                                              sy-datum+4(2) &&
                                              sy-datum+6(2) &&
                                              sy-uzeit(2)   &&
                                              sy-uzeit+2(2).

        "(4) Update delivery and order tables
        "Create the delivery
        MODIFY z4t_deliveries FROM monitor_list_record.

        IF sy-subrc <> 0.
          ROLLBACK WORK."#EC CI_ROLLBACK
          RETURN.
        ENDIF.

        "Now update the order
        UPDATE z4t_order_items
        "C = Foul deed has been requested
        SET   foul_deed_status = 'C'
        WHERE order_number     = order_number.

        IF sy-subrc <> 0.
          ROLLBACK WORK."#EC CI_ROLLBACK
          RETURN.
        ELSE.
          "(5) Send confirmation message back to web app via APC
          DATA(message) = i_message_manager->create_message( ).
          message->set_text( |{ 'Horrible, Horrible, Deed has Been Scheduled'(004) }| ).
          i_message_manager->send( message ).
          COMMIT WORK.
        ENDIF.

        "(6) Send message to SAP GUI application via AMC
        "Determine message type of the AMC channel
        TRY.
            DATA(amc_dt_manager) =
            cl_amc_dt_manager=>create(
            i_application_id = 'ZAMC_4_MONSTERS'
            i_channel_id     = '/monsters' ).
            DATA(amc_message_type) = amc_dt_manager->get_message_type( ).
          CATCH cx_amc_dt_error INTO DATA(amc_dt_error).
            MESSAGE amc_dt_error TYPE 'E'.
        ENDTRY.

        IF amc_message_type NE 'PCP'.
          RETURN.
        ENDIF.

        "Fill payload
        TRY.
            "Create bottle to send
            "Set extension ID to be the castle number
            DATA(message_bottle) = CAST if_amc_message_producer_pcp(
            cl_amc_channel_manager=>create_message_producer(
            i_application_id       = 'ZAMC_4_MONSTERS'
            i_channel_id           = '/monsters'
            i_channel_extension_id = CONV #( monitor_list_record-castle_number ) ) ).
            "Create message for bottle
            DATA(pcp_message) = cl_ac_message_type_pcp=>create( ).
            pcp_message->set_text( |{ 'A New Atrocity needs to be Committed'(003) }| ).
            pcp_message->set_field(
            i_name  = |{ 'Delivery Number'(002) }|
            i_value = CONV #( monitor_list_record-delivery_number ) ).
            "Put message in bottle and throw bottle into the sea
            message_bottle->send( pcp_message ).
          CATCH cx_ac_message_type_pcp_error INTO DATA(pcp_error).
            MESSAGE pcp_error TYPE 'E'.
          CATCH cx_amc_error INTO DATA(amc_error).
            MESSAGE amc_error TYPE 'E'.
        ENDTRY.

      CATCH cx_apc_error INTO DATA(apc_error).
        MESSAGE apc_error TYPE 'E'.
    ENDTRY.
*----------------------------------------------------------------------------------*
* The Green Monster reads "Object Oriented Design with ABAP" by James E. McDonough
*----------------------------------------------------------------------------------*
  ENDMETHOD."On Message


  METHOD if_apc_wsp_extension~on_start.
*--------------------------------------------------------------------*
* Listing 14.07: - Coding ON_START Method
*--------------------------------------------------------------------*
    mo_monster_model = NEW #( ).

    TRY.
        DATA(message) = i_message_manager->create_message( ).
        message->set_text( |{ 'Monster Object has been Created!'(001) }| ).
        i_message_manager->send( message ).

      CATCH cx_apc_error INTO DATA(apc_error).
        MESSAGE apc_error TYPE 'E'.
    ENDTRY.

*--------------------------------------------------------------------*
* Listing 14.09: - Binding ABAP Push Channel to ABAP Messaging Channel
*--------------------------------------------------------------------*
    TRY.
        DATA(binding) = i_context->get_binding_manager( ).
        binding->bind_amc_message_consumer(
        i_application_id = 'ZAMC_4_MONSTERS'
        i_channel_id     = '/monsters' ).

      CATCH cx_apc_error INTO apc_error.
        MESSAGE apc_error TYPE 'E'.
    ENDTRY.

* The 1 Foot Tall Monster was one castle away from the Monster that reads "SAP Service and Support"
  ENDMETHOD."On Start
ENDCLASS.

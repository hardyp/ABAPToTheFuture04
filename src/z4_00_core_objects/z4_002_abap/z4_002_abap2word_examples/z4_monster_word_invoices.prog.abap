*&---------------------------------------------------------------------*
*& Report Z4_MONSTER_WORD_INVOICES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_monster_word_invoices.
*----------------------------------------------------------------------*
* Listing 11.35: - Generated TYPE Definitions for ABAP2DOCX
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF g_typ_invoice_items,
    monster_type TYPE string,
    quantity     TYPE string,
    unit_price   TYPE string,
    net_price    TYPE string,
  END OF g_typ_invoice_items,

  g_tt_invoice_items TYPE STANDARD TABLE OF g_typ_invoice_items WITH EMPTY KEY,

  BEGIN OF g_typ_data,
    invoice_number        TYPE string,
    invoice_date          TYPE string,
    customer_name         TYPE string,
    street_address        TYPE string,
    town                  TYPE string,
    purchase_order_number TYPE string,
    invoice_items         TYPE g_tt_invoice_items,
  END OF g_typ_data.

START-OF-SELECTION.
  PERFORM main.

FORM main.
*--------------------------------------------------------------------*
* Listing 11.36: Program to Generate and Email Dynamic Word Invoice
*--------------------------------------------------------------------*
  "Fill up data structure
  DATA(ls_templ_data)   = VALUE g_typ_data(
  invoice_number        = '9876543210'
  invoice_date          = |{ sy-datum DATE = ENVIRONMENT }|
  customer_name         = 'Lord Snooty'(001)
  street_address        = 'Snooty Towers'(002)
  town                  = 'Poortown'(003)
  purchase_order_number = 'CRUSHTHEPOOR'
  invoice_items         = VALUE #(
  ( monster_type = 'Clowns'(004)
    quantity     = '1'
    unit_price   = '100.00'
    net_price    = '100.00' )
  ( monster_type = 'I.S.Dead'(005)
    quantity     = '5'
    unit_price   = '200.00'
    net_price    = '1000.00' )
  ( monster_type = 'M.Salesmen'(006)
    quantity     = '1'
    unit_price   = '2000.00'
    net_price    = '2000.00' )
   )  "End of Invoice Item Table
   ). "End of Structure

  "Merge data structure values with stored template
  DATA(document_as_xstring) = zcl_docx3=>get_document(
   iv_w3objid    = 'Z4_MONSTER_INVOICE'
   iv_data       = ls_templ_data
   iv_protect    = abap_true
   iv_no_save    = abap_true   ).

  "Email out the result
  PERFORM send_document_as_attachment USING document_as_xstring.

ENDFORM.

FORM send_document_as_attachment USING pud_document_as_xstring TYPE xstring.
* Local Variables
  DATA: email_sender             TYPE REF TO cl_bcs,
        email_object             TYPE REF TO cl_document_bcs,
        email_recipient          TYPE REF TO if_recipient_bcs,
        email_body_table         TYPE bcsy_text,
        did_we_send_the_email    TYPE os_boolean,
        email_xml_object_size    TYPE i,
        attachment_size          TYPE so_obj_len,
        contents_as_binary_table TYPE solix_tab,
        email_address            TYPE ad_smtpadr,
        subject                  TYPE so_obj_des,
        attachment_subject       TYPE sood-objdes.

  subject            = 'Monster Invoice'(007).
  attachment_subject = 'Monsters'(008).

  email_body_table = VALUE #(
  ( |{ 'Wibbly Wobbly Woo'(009) }| ) ).

  DATA(email_addresses) = VALUE bcsy_smtpa( ( 'baron.frankenstien@monstersrus.com.tl' ) ) ##NO_TEXT.

  "Convert to Binary
  contents_as_binary_table =
  cl_bcs_convert=>xstring_to_solix( pud_document_as_xstring  ).
  email_xml_object_size = xstrlen( pud_document_as_xstring ).

  TRY.
      "Create persistent send request
      email_sender = cl_bcs=>create_persistent( ).

      "Create and set document with attachment
      "Create document object from internal table with text
      email_object = cl_document_bcs=>create_document(
      i_type    = 'RAW'
      i_text    = email_body_table
      i_subject = subject ).

      "Add the attachment to document object
      attachment_size = email_xml_object_size.

      DATA: attachment_header_table       TYPE soli_tab,
            attachment_header_description TYPE soli.
      CONCATENATE '&SO_FILENAME=' attachment_subject '.docx'
      INTO attachment_header_description ##NO_TEXT.
      INSERT attachment_header_description INTO TABLE attachment_header_table.

      email_object->add_attachment(
      i_attachment_type    = 'DOC'
      i_attachment_subject = attachment_subject
      i_attachment_size    = attachment_size
      i_att_content_hex    = contents_as_binary_table
      i_attachment_header  = attachment_header_table ) ##NO_TEXT.

      "Add document object to send request
      email_sender->set_document( email_object ).

      "Aadd recipient (e-mail address)
      LOOP AT email_addresses ASSIGNING FIELD-SYMBOL(<email_address>).
        "Create recipient object
        email_recipient =
        cl_cam_address_bcs=>create_internet_address( <email_address> ).

        "Add recipient object to send request
        email_sender->add_recipient( email_recipient ).

      ENDLOOP."email addresses

      "Send document
      did_we_send_the_email = email_sender->send( abap_true ).

      COMMIT WORK.

      IF did_we_send_the_email EQ abap_false.
        MESSAGE i500(sbcoms) WITH email_address."Document not sent to &1
      ELSE.
        MESSAGE s022(so)."Document sent
        "Kick off the send job so the email goes out immediately
        WAIT UP TO 2 SECONDS.     "ensure the mail has been queued
        SUBMIT rsconn01
           WITH mode   = '*'      "process everything you find.
           WITH output = ' '
           AND RETURN."#EC CI_SUBMIT
      ENDIF.

    "Exception Handling
    CATCH cx_bcs INTO DATA(email_sending_exception) ##NEEDED ##NO_HANDLER."#EC EMPTY_CATCH
* If something goes wrong you probably want to send a simple
* failure message to the user and a more detailed one to the
* application log and then break down and cry and finally
* decide that programming is not for you and thus run away
* to the Circus (Carny for you Americans)
  ENDTRY.

ENDFORM.

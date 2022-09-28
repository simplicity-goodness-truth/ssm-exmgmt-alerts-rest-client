class ZCL_SERVESS_ALERTS_INTEGRATION definition
  public
  final
  create public .

public section.

  interfaces IF_ALERT_REACTION .
  interfaces IF_BADI_INTERFACE .

  types:
    begin of st_my_alert_data,
        alert_name          type string,
        managed_object_name type string,
        managed_object_type type string,
        managed_object_id   type string,
        category            type ac_category,
        severity            type ac_severity,
        timestamp           type string,
        rating              type string,
        status              type string,
        alert_id            type string,
        visited             type string,
        xml                 type xstring,
        metricpath          type string,
        reasonforclosure    type string,
        alert_tname         type ac_name,
        desc                type string,
        cust_desc           type string,
        technical_scenario  type ac_technical_scenario,
      end of st_my_alert_data .
  types:
    begin of st_my_sub_object_data,
        name             type string,
        event_id         type string,
        obj_type         type string,
        alert_id         type string,  "contains the alert id from the root
        parent_id        type string, "contains immediate parent
        rating           type string,
        text             type string,
        visited          type string,
        value            type string,
        timestamp        type string,
        metricpath       type string,
        reasonforclosure type string,

      end of st_my_sub_object_data .
  types:
    tt_my_alert_data type standard table of st_my_alert_data with key alert_id .
  types:
    tt_my_sub_objects type standard table of st_my_sub_object_data .
  types:
*  types:
*    begin of ty_json_req,
*        alert_group_state   type char6,
*        managed_object_name type string,
*        managed_object_type type string,
*        managed_object_id   type string,
*        it_solution         type char3,
*        category            type ac_category,
*        severity            type ac_severity,
*        timestamp           type string,
*        rating              type string,
*        status              type string,
*        alert_id            type string,
*        short_description   type string,
*        alert_tname         type ac_name,
*        desc                type string,
*      end of ty_json_req .
    begin of ty_json_req,

        aa_sourcesubsystem type char100,
        aa_sourceeventid   type char50,
        aa_severity        type char20,
        aa_errorcode       type string,
        title              type string,
        description        type string,
        aa_citype          type char100,
        ciname             type char100,
        aa_ci_fqdn         type string,
      end of ty_json_req .

  class-data GT_EVENTS type TT_MY_SUB_OBJECTS .

  class-methods GET_RULEID_FROM_MAP
    importing
      !IP_SUBSYSTEM type CPIS_LOG_TEXT
      !IP_TITLE type CHAR258
    returning
      value(EP_RULEID) type SMOCC_TXT .
  class-methods GET_ALRT_REST_INT_PARAM_VALUE
    importing
      !IP_PARAM_NAME type CHAR20
    returning
      value(EP_VALUE) type CHAR258 .
protected section.
private section.

  class-methods REMOVE_SPECIAL_SYMBOLS
    changing
      !CS_STRING type STRING .
  class-methods OBJECT_HAS_EXMGMT_LINK
    importing
      !IP_ALERT_GUID type AC_GUID optional
      !IP_METRIC_PATH type STRING optional
    returning
      value(EP_RESULT) type ABAP_BOOL .
  class-methods GET_SEVERITY_TEXT
    importing
      !IP_SEVERITY type AC_SEVERITY
    returning
      value(EP_RESULT) type CHAR100 .
  class-methods GET_PI_INTERFACE_TITLE
    importing
      !IP_METRIC_PATH type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_PI_INTERFACE_SUBSYSTEM
    importing
      !IP_METRIC_PATH type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_PI_INTERFACE_COMPONENTS
    importing
      !IP_METRIC_PATH type STRING
      !IP_COMPONENT_NAME type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_METRIC_TECH_CTX
    importing
      !IP_CONTEXT_ID type AC_GUID
      !IP_EVENT_TYPE_ID type AC_GUID
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_EXMGMT_PROCESS_ID
    importing
      !IP_ALERT_GUID type AC_GUID optional
      !IP_METRIC_PATH type STRING optional
      !IP_HCIDS_MANAGED_OBJECT_NAME type E2EEM_IMPL_SOURCE optional
      !IP_HCIDS_INTERFACE_NAME type E2EEM_SUBCATEGORY optional
    returning
      value(EP_RESULT) type AC_GUID .
  class-methods GET_CPI_SOURCE_SYSTEM
    importing
      !IP_IFLOW_NAME type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_CPI_IFLOW_NAME
    importing
      !IP_METRIC_PATH type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_CPI_DS_PROJECT
    importing
      !IP_METRIC_PATH type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_CPIDS_TASK_NAME
    importing
      !IP_METRIC_PATH type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_CPIDS_SOURCE_SYSTEM
    importing
      !IP_TASK_NAME type STRING
    returning
      value(EP_RESULT) type STRING .
  class-methods GET_CITYPE
    returning
      value(EP_RESULT) type CHAR100 .
  class-methods COLLECT_EXMGMT_CONTENTS
    importing
      !IP_PROCESS_ID type AC_GUID
    returning
      value(EP_RESULT) type STRING .
  methods COLLECT_DATA_FOR_PAYLOAD
    importing
      !IP_ALERT_GROUP_STATE type CHAR6
      !IT_ALERT type E2EA_T_ALERT_CONSM_OBJECT
    returning
      value(ET_DATA_FOR_PAYLOAD) type TT_MY_ALERT_DATA .
  class-methods ADJUST_PAYLOAD
    changing
      !CS_PAYLOAD type STRING .
  class-methods ADD_FORMATTED_TIMESTAMP_LINE
    importing
      !IP_PARAMETER type STRING
      !IP_TIMESTAMP type TIMESTAMP
    changing
      !CP_TEXT type STRING .
  class-methods ADD_FORMATTED_PARAM_VALUE_LINE
    importing
      !IP_PARAMETER type STRING
      !IP_VALUE type STRING
    changing
      !CP_TEXT type STRING .
  class-methods COMPILE_PAYLOAD
    importing
      !IT_EXTRACTED_ALERTS type TT_MY_ALERT_DATA
    exporting
      !EP_PAYLOAD type STRING
      !EP_DESCRIPTION type STRING .
  class-methods GET_TSYS_URL
    importing
      !IP_ALERT_TNAME type AC_NAME
      !IP_EXT_SID type E2EEM_IMPL_SOURCE
    returning
      value(EP_RESULT) type STRING .
  class-methods UPDATE_RECORD_IN_INTGR_LOG_TAB
    importing
      value(IT_EXTRACTED_ALERTS) type TT_MY_ALERT_DATA optional
      value(IP_PAYLOAD) type STRING
      value(IP_ACTION) type CHAR1
      !IP_DESCRIPTION type STRING .
  methods EXTRACT_SUB_OBJECTS
    importing
      !IPO_OBJECT type ref to IF_ALERT_CONSM_OBJECT
      !IPV_ALERT_ID type STRING
      !IPV_PARENT_ID type STRING .
  class-methods ADD_RECORD_TO_APP_LOG
    importing
      !IP_MSGTY type SYMSGTY
      !IP_LOG_RECORD_TEXT type STRING .
  class-methods SEND_PAYLOAD_AS_POST_REQUEST
    importing
      !IP_DESTINATION type RFCDEST
      !IP_PARAMLINE type CHAR1024 optional
      !IP_JSON_REQUEST type STRING
    exporting
      !EP_JSON_RESPONSE type STRING .
ENDCLASS.



CLASS ZCL_SERVESS_ALERTS_INTEGRATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>ADD_FORMATTED_PARAM_VALUE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PARAMETER                   TYPE        STRING
* | [--->] IP_VALUE                       TYPE        STRING
* | [<-->] CP_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_formatted_param_value_line.

    data: lv_comment_line_left  type string,
          lv_comment_line_right type string,
          c_newline value cl_abap_char_utilities=>newline.

    lv_comment_line_left = ip_parameter.

    lv_comment_line_left = cl_alert_consm_utility=>get_formatted_label( lv_comment_line_left ).

    concatenate lv_comment_line_left ip_value into lv_comment_line_right.

    concatenate cp_text c_newline lv_comment_line_right into cp_text.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>ADD_FORMATTED_TIMESTAMP_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PARAMETER                   TYPE        STRING
* | [--->] IP_TIMESTAMP                   TYPE        TIMESTAMP
* | [<-->] CP_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_formatted_timestamp_line.

    data: lv_comment_line_left  type string,
          lv_comment_line_right type string,
          c_newline             value cl_abap_char_utilities=>newline,
          lv_date               type d,
          lv_time               type t,
          lv_dst                type c length 1,
          lv_timestamp          type timestamp,
          lv_date_char          type  char12,
          lv_time_char          type  char10.

    lv_comment_line_left = ip_parameter.
    lv_comment_line_left = cl_alert_consm_utility=>get_formatted_label( lv_comment_line_left ).

    lv_timestamp = ip_timestamp.

    convert time stamp lv_timestamp time zone sy-zonlo into date lv_date time lv_time daylight saving time lv_dst.

    write  lv_date to  lv_date_char using edit mask  '__.__.____'.

    write  lv_time to  lv_time_char using edit mask  '__:__:__'.

    concatenate lv_date_char lv_time_char sy-zonlo into lv_comment_line_right separated by space.

    concatenate   lv_comment_line_left lv_comment_line_right into lv_comment_line_right.

    concatenate cp_text c_newline lv_comment_line_right into cp_text.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>ADD_RECORD_TO_APP_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_MSGTY                       TYPE        SYMSGTY
* | [--->] IP_LOG_RECORD_TEXT             TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method add_record_to_app_log.


    " Data declaration for Application Log operations

    data: ls_log        type bal_s_log,
          ev_log_handle type balloghndl,
          ls_msg        type bal_s_msg,
          lt_log_handle type bal_t_logh,
          lt_log_num    type bal_t_lgnm,
          lv_subrc      type sysubrc.
    data:
      begin of ls_string,
        part1 type symsgv,
        part2 type symsgv,
        part3 type symsgv,
        part4 type symsgv,
      end of ls_string.

    concatenate sy-uzeit ip_log_record_text into ls_string separated by space.

    " Preparing the Application Log

    clear ev_log_handle.

    ls_log-object    = get_alrt_rest_int_param_value('APP_LOG_OBJECT').
    ls_log-subobject = get_alrt_rest_int_param_value('APP_LOG_SUBOBJECT').
    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.
    ls_log-aluser    = sy-uname.

    if ( ls_log-object is initial ) or ( ls_log-subobject is initial ).
      return.
    endif.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log                 = ls_log
      importing
        e_log_handle            = ev_log_handle
      exceptions
        log_header_inconsistent = 1
        others                  = 2.
    if sy-subrc <> 0.
      lv_subrc = 1.
      return.
    endif.

    ls_msg-msgv1     = ls_string-part1.
    ls_msg-msgv2     = ls_string-part2.
    ls_msg-msgv3     = ls_string-part3.
    ls_msg-msgv4     = ls_string-part4.

    ls_msg-msgty = ip_msgty.
    ls_msg-msgid = 'BL'.
    ls_msg-msgno = '001'.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle  = ev_log_handle
        i_s_msg       = ls_msg
      exceptions
        log_not_found = 0
        others        = 1.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    endif.

    " Finalizing the Application Log records

    insert ev_log_handle into lt_log_handle index 1.

    call function 'BAL_DB_SAVE'
      exporting
        i_client         = sy-mandt
        i_save_all       = ' '
        i_t_log_handle   = lt_log_handle
      importing
        e_new_lognumbers = lt_log_num
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
      lv_subrc = sy-subrc.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>ADJUST_PAYLOAD
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_PAYLOAD                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method adjust_payload.

    " Special ServESS transformations

    types: begin of ty_json_field_replacement,
             source type string,
             target type string,
           end of ty_json_field_replacement.


    data: lt_json_field_replacement type table of ty_json_field_replacement,
          ls_json_field_replacement type ty_json_field_replacement.

    ls_json_field_replacement-source = 'aa_sourcesubsystem'.
    ls_json_field_replacement-target = 'AA_SourceSubSystem'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'AA_SourceEventID'.
    ls_json_field_replacement-target = 'AA_SourceEventID'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'AA_Severity'.
    ls_json_field_replacement-target = 'AA_Severity'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'aa_errorcode'.
    ls_json_field_replacement-target = 'AA_ErrorCode'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'title'.
    ls_json_field_replacement-target = 'Title'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'description'.
    ls_json_field_replacement-target = 'Description'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'aa_citype'.
    ls_json_field_replacement-target = 'AA_CIType'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'ciname'.
    ls_json_field_replacement-target = 'CIName'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    ls_json_field_replacement-source = 'aa_ci_fqdn'.
    ls_json_field_replacement-target = 'AA_CI_FQDN'.
    append ls_json_field_replacement to lt_json_field_replacement.
    clear ls_json_field_replacement.

    loop at lt_json_field_replacement assigning field-symbol(<ls_json_field_replacement>).

      replace all occurrences of <ls_json_field_replacement>-source in cs_payload with <ls_json_field_replacement>-target ignoring case.

    endloop. " loop at lt_json_field_replacement assigning field-symbol(<ls_json_field_replacement>)



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SERVESS_ALERTS_INTEGRATION->COLLECT_DATA_FOR_PAYLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_ALERT_GROUP_STATE           TYPE        CHAR6
* | [--->] IT_ALERT                       TYPE        E2EA_T_ALERT_CONSM_OBJECT
* | [<-()] ET_DATA_FOR_PAYLOAD            TYPE        TT_MY_ALERT_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method collect_data_for_payload.
    data: lv_object_type           type char10,
          lt_sub_objects           type e2ea_t_alert_consm_object,
          lo_sub_object            type ref to if_alert_consm_object,
          ls_extracted_alert       type st_my_alert_data,
          lt_extracted_alerts      type tt_my_alert_data,
          lv_field_value           type string,

          lv_servess_closed_status type char3,
          lv_open_inc_count        type i.

    field-symbols <lfs_alert> like line of it_alert.

    loop at it_alert assigning <lfs_alert>.

      " Getting a GUID for an Alert

      lv_object_type = <lfs_alert>->get_object_type( ).

      " Focusing on Alert (A) type

      if lv_object_type = 'A'.

        ls_extracted_alert-alert_name = <lfs_alert>->get_name( ).
        ls_extracted_alert-alert_tname = <lfs_alert>->get_technical_name( ).
        ls_extracted_alert-managed_object_name = <lfs_alert>->get_managed_object_name( ).
        ls_extracted_alert-managed_object_type = <lfs_alert>->get_managed_object_type( ).
        ls_extracted_alert-category = <lfs_alert>->get_category( ).
        ls_extracted_alert-severity = <lfs_alert>->get_severity( ).
        ls_extracted_alert-alert_id = <lfs_alert>->get_id( ).
        ls_extracted_alert-status = <lfs_alert>->get_status( ).
        ls_extracted_alert-timestamp = <lfs_alert>->get_timestamp( ).
        ls_extracted_alert-desc = <lfs_alert>->get_description( ).
        ls_extracted_alert-cust_desc = <lfs_alert>->get_custom_description( ).
        ls_extracted_alert-managed_object_id = <lfs_alert>->get_managed_object_id( ).
        ls_extracted_alert-technical_scenario = <lfs_alert>->get_technical_scenario( ).
        ls_extracted_alert-visited = 'TRUE'.

        lv_field_value = <lfs_alert>->get_rating( ).
        ls_extracted_alert-rating = cl_alert_consm_utility=>get_domain_value_text(
        i_domain_name = cl_alert_consm_constants=>ac_domname_rating
        i_value = lv_field_value ).


        " Getting all metrics corresponding to the alert

        if <lfs_alert>->has_sub_objects( ) = abap_true.

          lt_sub_objects = <lfs_alert>->get_sub_objects( ).

          loop at lt_sub_objects into lo_sub_object.

            extract_sub_objects( ipo_object = lo_sub_object

            ipv_parent_id = <lfs_alert>->get_id( )

            ipv_alert_id = ls_extracted_alert-alert_id ).

          endloop. " LOOP AT lt_sub_objects INTO lo_sub_object

        endif. " IF <lfs_alert>->has_sub_objects( ) = abap_true

        append ls_extracted_alert to lt_extracted_alerts.
        clear ls_extracted_alert.

      endif. " IF lv_object_type = 'A'

    endloop. " LOOP AT it_alert ASSIGNING <lfs_alert>

    et_data_for_payload = lt_extracted_alerts.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>COLLECT_EXMGMT_CONTENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PROCESS_ID                  TYPE        AC_GUID
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method collect_exmgmt_contents.

    types: begin of ty_variables,
             cname  type char255,
             cvalue type char255,
           end of ty_variables.

    types: begin of ty_dump_details,
             parameter type char1024,
           end of ty_dump_details.

    data: lv_process_instance_id   type ac_guid,
          lo_e2eem_store_instance  type ref to cl_e2eem_store_instance,
          lt_call_stack            type  e2eem_call_stk_tt,
          lv_dump_call_stack       type string,
          c_newline                value cl_abap_char_utilities=>newline,
          lv_line_number           type string,
          lv_call_stack_depth      type string,
          lv_call_stack_line       type string,
          lt_dump_context          type e2eem_inst_ctx_tt,
          lv_dump_context          type string,
          lt_dump_gen_context      type e2eem_gen_ctx_tt,
          lv_dump_variables        type xstring,
          lv_dump_variables_parsed type string,
          lt_xml                   type table of smum_xmltb initial size 2,
          lt_return                type table of bapiret2 initial size 2,
          lt_variables             type standard table of ty_variables,
          ls_variables             like line of lt_variables,
          lo_e2eem_detail_me       type ref to cl_e2eem_detail_me,
          lv_dump_detail           type string,
          lv_dump_detail_string    type string,
          lt_dump_details          type standard table of ty_dump_details,
          lo_cl_e2eem_wd_helper    type ref to cl_e2eem_wd_helper,
          lv_exmgmt_url            type string,
          lv_exception_log         type string.


    field-symbols: <ls_call_stack>       like line of lt_call_stack,
                   <ls_dump_context>     like line of lt_dump_context,
                   <ls_dump_gen_context> like line of lt_dump_gen_context,
                   <ls_variable>         like line of lt_variables,
                   <ls_dump_details>     like line of lt_dump_details,
                   <ls_xml>              like line of lt_xml.

    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Getting details from Exception Management
    " ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lv_process_instance_id = ip_process_id.

    try.

        " Getting active calls and events from Exception Management

        create object lo_e2eem_store_instance
          exporting
            iv_instance_id = lv_process_instance_id.

        lt_call_stack = lo_e2eem_store_instance->if_e2eem_store_api_instance~get_call_stack( ).

        if lt_call_stack is not initial.

          concatenate 'Active Calls/Events' c_newline lv_dump_call_stack into lv_dump_call_stack.

          loop at lt_call_stack assigning <ls_call_stack>.

            lv_line_number = <ls_call_stack>-line_no.
            lv_call_stack_depth = <ls_call_stack>-call_stack_depth.
            lv_call_stack_line = <ls_call_stack>-call_stack_line.

            concatenate lv_dump_call_stack  'STEP ID:' <ls_call_stack>-step_id '|LINE NO:' lv_line_number  'STACK DEPTH:' lv_call_stack_depth  '|STACK LINE' lv_call_stack_line c_newline
              into lv_dump_call_stack.

          endloop. "LOOP AT lt_call_stack ASSIGNING <ls_call_stack>.


        endif. " if lt_call_stack is not initial

        " Getting context data from Exception Management

        lt_dump_context = lo_e2eem_store_instance->if_e2eem_store_api_instance~get_context(
              exporting
                iv_ctx_name = ''
                iv_ctx_type = '' ).


        if lt_dump_context is not initial.

          concatenate 'Collection Context' c_newline into lv_dump_context.

          " Skipping empty values

          loop at lt_dump_context assigning <ls_dump_context>.

            if <ls_dump_context>-ctx_attr_value is not initial.

              concatenate lv_dump_context   <ls_dump_context>-ctx_attr_name  '=' <ls_dump_context>-ctx_attr_value c_newline into lv_dump_context.

            endif.

          endloop. "LOOP AT lt_dump_context ASSIGNING <ls_dump_context>.

        endif. " if lt_dump_context is not initial.

        " Getting variables data from Exception Management

        lt_dump_gen_context = lo_e2eem_store_instance->if_e2eem_store_api_instance~get_general_context( exporting
          iv_ctx_name = ''
          iv_ctx_type = '' ).

        if lt_dump_gen_context is not initial.

          loop at lt_dump_gen_context assigning <ls_dump_gen_context>.

            case <ls_dump_gen_context>-ctx_attr_name.

                " Processing exception variables

              when 'VARIABLES'.

                lv_dump_variables = <ls_dump_gen_context>-ctx_attr_value.

                concatenate lv_dump_variables_parsed 'Variables' c_newline into lv_dump_variables_parsed.

                " Conversion of xml with variables

                call function 'SMUM_XML_PARSE'
                  exporting
                    xml_input = lv_dump_variables
                  tables
                    xml_table = lt_xml
                    return    = lt_return.

                loop at lt_xml assigning <ls_xml>.

                  " Variables in Exception Management are recorded on hierarchy number 7 in xml format
                  " Adding records from Exception Management xml into a local table

                  if ( <ls_xml>-hier = 7 ).

                    if ( <ls_xml>-cname = 'NAME' ).
                      ls_variables-cname = <ls_xml>-cvalue.
                    endif.

                    if ( <ls_xml>-cname = 'VALUE' ).
                      ls_variables-cvalue = <ls_xml>-cvalue.
                      append ls_variables to lt_variables.

                    endif. " if ( <ls_xml>-hier = 7 )

                  endif.

                endloop. "LOOP AT lt_xml ASSIGNING <ls_xml>.

                " Adding table of variables into one string

                loop at lt_variables assigning <ls_variable>.

                  concatenate  lv_dump_variables_parsed <ls_variable>-cname '=' <ls_variable>-cvalue c_newline into lv_dump_variables_parsed separated by space.

                endloop. " loop at lt_variables assigning <ls_variable>

                " Processing exception Error Log (needed for CPI-DS) exceptions

              when 'Error Log'.

                " Converting xstring to string to get error log

                cl_bcs_convert=>xstring_to_string(
                  exporting
                    iv_xstr   = <ls_dump_gen_context>-ctx_attr_value
                    iv_cp     =  1100                " SAP character set identification
                  receiving
                    rv_string = lv_exception_log ).


                if sy-subrc = 0.

                  concatenate 'Error Log' c_newline lv_exception_log c_newline  into lv_exception_log.

                endif.

            endcase. " case <ls_dump_gen_context>-ctx_attr_name

          endloop. "  loop at lt_dump_gen_context assigning <ls_dump_gen_context>

        endif. " if lt_dump_gen_context is not initial

        " Getting exception description from Exception Management

        create object lo_e2eem_detail_me.

        lo_e2eem_detail_me->get_incident_content_by_id(
        exporting
          iv_instance_id = lv_process_instance_id
         importing
            ev_msg = lv_dump_detail ).

        split lv_dump_detail at c_newline into table lt_dump_details .

        loop at lt_dump_details assigning <ls_dump_details>.

          " Skip first line as it just contains text header

          if sy-tabix ne 1.

            concatenate lv_dump_detail_string <ls_dump_details>-parameter c_newline into lv_dump_detail_string.

          endif. " if sy-tabix ne 1

        endloop.

        create object lo_cl_e2eem_wd_helper.

        " Getting a direct URL to Exception Management tool

        lv_exmgmt_url = lo_cl_e2eem_wd_helper->get_instance_detail_url( exporting iv_instance_id = lv_process_instance_id ).

        concatenate 'Exception Management application URL: ' lv_exmgmt_url c_newline into lv_exmgmt_url separated by space.

      catch cx_e2eem_store_factory .

    endtry.

    " Combining all collected data from Exception Management into one variable

    concatenate lv_dump_detail_string c_newline lv_dump_context c_newline lv_dump_call_stack c_newline
      lv_dump_variables_parsed c_newline lv_exception_log c_newline lv_exmgmt_url c_newline
       into ep_result.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>COMPILE_PAYLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_EXTRACTED_ALERTS            TYPE        TT_MY_ALERT_DATA
* | [<---] EP_PAYLOAD                     TYPE        STRING
* | [<---] EP_DESCRIPTION                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method compile_payload.

    data:
      lv_description            type string,
      lv_alert_guid             type ac_guid,
      c_newline                 value cl_abap_char_utilities=>newline,
      lv_ticket_name_prefix     type char256,
      lv_short_description      type string,
      lv_timestamp              type timestamp,
      lv_status_text            type string,
      lv_status                 type string,
      lv_category_text          type string,
      lv_category               type string,
      lv_alert_exmgmt_content   type string,
      lv_metric_exmgmt_content  type string,
      lv_process_id             type ac_guid,
      lv_metric_counter         type int2,
      lv_metric_counter_char    type char2,
      lv_managed_object_name    type e2eem_impl_source,
      lv_hcids_interface_name   type  e2eem_subcategory,
      lv_context_id             type ac_guid,
      lv_event_type_id          type ac_guid,
      lv_route_it_solution      type int4,
      "   lv_subsystem              type char20,
      lv_subsystem              type cpis_log_text,
      lv_iflow_name             type string,
      lv_task_name              type string,
      lv_explicit_title         type string,
      lv_explicit_errorcode     type string,
      lv_map_error_code_to_rule type char1,
      lv_title                  type char258.

    " JSON related data

    data:
      ls_json_req_obj    type ty_json_req,
      lv_json_req_str    type string,
      lr_json_serializer type ref to zcl_json_serializer.

    field-symbols:
      <gt_events>           like line of gt_events,
      <ls_extracted_alerts> like line of it_extracted_alerts.

    loop at it_extracted_alerts assigning <ls_extracted_alerts>.

      " -------------------------------------------------------------------------
      "       Preparing alerts and metrics data
      " -------------------------------------------------------------------------



      " ----------------- Description -----------------------

      " Adding alert details

      concatenate 'Alert Data' c_newline into lv_description.

      if lv_short_description is not initial.

        add_formatted_param_value_line(
           exporting
             ip_parameter = 'Short description'
             ip_value = lv_short_description
          changing
          cp_text = lv_description ).

      endif. " if lv_short_description is not initial

      add_formatted_param_value_line(
        exporting
          ip_parameter = 'Name'
          ip_value = <ls_extracted_alerts>-alert_name
        changing
         cp_text = lv_description ).


      " Setting specific error codes for CPI-DS and PI systems


      if ( <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_HCIDS_EXCEPTION_ALERT' )  .

        lv_explicit_errorcode = <ls_extracted_alerts>-alert_name.

      endif.

      " Setting specific error code for PI alerts

      if ( <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_IFCHANNEL_PI_EXCEPTION_ALERT' ).

        lv_explicit_errorcode = <ls_extracted_alerts>-alert_name.

      endif.



      lv_timestamp = <ls_extracted_alerts>-timestamp.

      add_formatted_timestamp_line(
              exporting
                ip_parameter = 'Timestamp'
                ip_timestamp = lv_timestamp
              changing
               cp_text = lv_description ).


      add_formatted_param_value_line(
         exporting
           ip_parameter = 'Managed Object'
           ip_value = <ls_extracted_alerts>-managed_object_name
         changing
           cp_text = lv_description ).

      add_formatted_param_value_line(
         exporting
           ip_parameter = 'Managed Object Type'
           ip_value = <ls_extracted_alerts>-managed_object_type
         changing
           cp_text = lv_description ).

      add_formatted_param_value_line(
         exporting
           ip_parameter = 'Rating'
           ip_value = <ls_extracted_alerts>-rating
         changing
           cp_text = lv_description ).

      lv_category = <ls_extracted_alerts>-category.
      lv_category_text = cl_alert_consm_utility=>get_domain_value_text(
      i_domain_name = cl_alert_consm_constants=>ac_domname_category
      i_value =  lv_category ).

      if lv_category_text is not initial.

        add_formatted_param_value_line(
           exporting
             ip_parameter = 'Category'
             ip_value = lv_category_text
           changing
            cp_text = lv_description ).

      endif. "if lv_category_text is not initial


      lv_status = <ls_extracted_alerts>-status.
      lv_status_text = cl_alert_consm_utility=>get_domain_value_text(
      i_domain_name = cl_alert_consm_constants=>ac_domname_status
      i_value =  lv_status ).

      if lv_status_text is not initial.

        add_formatted_param_value_line(
            exporting
              ip_parameter = 'Status'
              ip_value = lv_status_text
            changing
              cp_text = lv_description ).

      endif. " if lv_status_text is not initial

      " Recommendations text

      concatenate c_newline c_newline lv_description c_newline <ls_extracted_alerts>-desc into lv_description.

      " --------------------------------------------------------------------------------------------
      "       If an ALERT is related to Exception Management, we collect it's data in addition
      " --------------------------------------------------------------------------------------------

      lv_alert_guid = <ls_extracted_alerts>-alert_id.

      if object_has_exmgmt_link( ip_alert_guid = lv_alert_guid ) eq abap_true.

        lv_process_id = get_exmgmt_process_id( ip_alert_guid = lv_alert_guid ).

        if lv_process_id is not initial.

          lv_alert_exmgmt_content = collect_exmgmt_contents( lv_process_id ).

          if lv_alert_exmgmt_content is not initial.

            concatenate lv_description c_newline lv_alert_exmgmt_content into lv_description.

          endif. " if lv_exmgmt_content is not initial

        endif. " if lv_process_id is not initial.

      endif. " if alert_has_exmgmt_content( ip_alert_guid = lv_alert_guid ) eq abap_true




      " -------------------------------------------------------------------------
      "       Metrics data processing
      " -------------------------------------------------------------------------

      loop at gt_events assigning <gt_events>.

        if ( <gt_events>-obj_type = 'M' ) and ( <gt_events>-rating <> 'Green' ).

          lv_metric_counter = lv_metric_counter + 1.
          lv_metric_counter_char = lv_metric_counter.

          concatenate lv_description c_newline into lv_description.
          concatenate lv_description 'Metric' into lv_description.
          concatenate lv_description  lv_metric_counter_char 'Data' into lv_description separated by space.
          concatenate lv_description c_newline into lv_description.

          if ( <gt_events>-name ne '' ).

            add_formatted_param_value_line(
               exporting
                 ip_parameter = 'Name'
                 ip_value = <gt_events>-name
               changing
                cp_text = lv_description ).

          endif. " if ( <gt_events>-name ne '' )


          " Getting managed object name

          if ( <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_HCI_S_EXCEPTION_ALERT' ) or
           ( <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_HCIDS_EXCEPTION_ALERT' )  .


            " For CPI and CPI-DS we pickup Technical Context as managed object name,
            " as interface name itself doesn't contain a name of integration platform

            lv_context_id = <ls_extracted_alerts>-managed_object_id.
            lv_event_type_id = <gt_events>-event_id.

            translate  lv_context_id to upper case.
            translate  lv_event_type_id to upper case.

            lv_managed_object_name = get_metric_tech_ctx(
                                             exporting
                                               ip_context_id    = lv_context_id
                                               ip_event_type_id = lv_event_type_id ).

          else.

            lv_managed_object_name = <ls_extracted_alerts>-managed_object_name.

          endif.


          if ( <gt_events>-metricpath ne '' ).

            add_formatted_param_value_line(
             exporting
               ip_parameter = 'Path'
               ip_value = <gt_events>-metricpath
             changing
              cp_text = lv_description ).


            " Check if METRIC has Exception Management content

            if object_has_exmgmt_link( ip_metric_path = <gt_events>-metricpath ) eq abap_true.

              lv_process_id = get_exmgmt_process_id( ip_metric_path = <gt_events>-metricpath ).

            else.

              " Additional extra unique case for CPI-DS exceptions
              " For CPI-DS we have to search exception data manually, as PROCESS ID link
              " is not included into standard alert for CPI-DS

              if <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_HCIDS_EXCEPTION_ALERT'.

                if lv_managed_object_name is not initial.

                  lv_hcids_interface_name = get_cpi_ds_project( <gt_events>-metricpath ).

                  if lv_hcids_interface_name is not initial.

                    lv_process_id = get_exmgmt_process_id( exporting
                        ip_hcids_managed_object_name = lv_managed_object_name
                        ip_hcids_interface_name = lv_hcids_interface_name ).

                  endif. " if lv_hcids_interface_name is not initial

                endif. " if lv_hcids_managed_object_name is not initial

              endif. " if <ls_extracted_alerts>-alert_name eq 'ICMON_HIGH_NO_OF_HCI_S_EXCEPTION_ALERT'

            endif. " if object_has_exmgmt_link( ip_metric_path = <gt_events>-metricpath ) eq abap_true

            if lv_process_id is not initial.

              lv_metric_exmgmt_content = collect_exmgmt_contents( lv_process_id ).

            endif. " if lv_process_id is not initial.


          endif. " if ( <gt_events>-metricpath ne '' )

          if ( <gt_events>-rating ne '' ).

            add_formatted_param_value_line(
             exporting
               ip_parameter = 'Rating'
               ip_value = <gt_events>-rating
             changing
              cp_text = lv_description ).

          endif. " if ( <gt_events>-rating ne '' )

          if ( <gt_events>-value ne '' ).

            add_formatted_param_value_line(
             exporting
               ip_parameter = 'Value'
               ip_value = <gt_events>-value
             changing
              cp_text = lv_description ).

          endif. " if ( <gt_events>-value ne '' )

          if ( <gt_events>-text ne '' ).

            add_formatted_param_value_line(
             exporting
               ip_parameter = 'Text'
               ip_value = <gt_events>-text
             changing
              cp_text = lv_description ).

            " Setting specific error code for CPI and CPI-DS alerts

            if ( <ls_extracted_alerts>-alert_tname eq 'ICMON_HIGH_NO_OF_HCI_S_EXCEPTION_ALERT' ).


              lv_explicit_errorcode = <gt_events>-text.

            endif.



          endif. " if ( <gt_events>-text ne '' )

          " Adding metric related Exception data

          if  lv_metric_exmgmt_content is not initial.

            concatenate lv_description c_newline c_newline into lv_description.
            concatenate lv_description 'Metric' into lv_description.
            concatenate lv_description  lv_metric_counter_char 'Exception Data' into lv_description separated by space.
            concatenate lv_description c_newline c_newline lv_metric_exmgmt_content c_newline c_newline into lv_description.
            clear lv_metric_exmgmt_content.

          endif. " if  lv_metric_exmgmt_content is not initial

          concatenate lv_description c_newline into lv_description.


          " Preparing subsystem and title

          case <ls_extracted_alerts>-technical_scenario.

            when 'IC_MON'.

              case <ls_extracted_alerts>-alert_tname.

                  " CPI exception

                when 'ICMON_HIGH_NO_OF_HCI_S_EXCEPTION_ALERT'.

                  lv_iflow_name = get_cpi_iflow_name( <gt_events>-metricpath ).

                  if lv_iflow_name is not initial.

                    lv_subsystem = get_cpi_source_system( lv_iflow_name ).
                    lv_explicit_title = lv_iflow_name.

                  endif. " if lv_iflow_name is not initial

                  " CPI-DS exception

                when 'ICMON_HIGH_NO_OF_HCIDS_EXCEPTION_ALERT'.

                  lv_task_name = get_cpids_task_name( <gt_events>-metricpath ).

                  if lv_task_name is not initial.

                    lv_subsystem = get_cpids_source_system( lv_task_name ).
                    lv_explicit_title = lv_task_name.

                  endif. " if lv_iflow_name is not initial

                  " PI exception

                when 'ICMON_HIGH_NO_OF_IFCHANNEL_PI_EXCEPTION_ALERT'.

                  lv_subsystem = get_pi_interface_subsystem( <gt_events>-metricpath ).
                  lv_explicit_title = get_pi_interface_title( <gt_events>-metricpath ).

                when others.

                  lv_subsystem = <gt_events>-name.

              endcase.  " case <ls_extracted_alerts>-alert_tname

            when others.

              lv_subsystem = lv_managed_object_name.

          endcase. "  case <ls_extracted_alerts>-technical_scenario

        endif. " IF ( <gt_events>-obj_type = 'M' )

      endloop. " LOOP AT gt_events ASSIGNING <gt_events>

      " ----------------- Short description -----------------------

      concatenate lv_managed_object_name ':' into lv_short_description.
      concatenate lv_short_description <ls_extracted_alerts>-alert_name into lv_short_description separated by space.

      " -------------------------------------------------------------------------
      "       Preparing JSON object
      " -------------------------------------------------------------------------

      ls_json_req_obj-aa_sourcesubsystem  = lv_subsystem.
      ls_json_req_obj-aa_sourceeventid = <ls_extracted_alerts>-alert_id.
      ls_json_req_obj-aa_severity  = get_severity_text( <ls_extracted_alerts>-severity ).


      if lv_explicit_errorcode is not initial.

        ls_json_req_obj-title     = lv_explicit_errorcode.
      else.
        ls_json_req_obj-title     = lv_short_description .

      endif.


      lv_map_error_code_to_rule = get_alrt_rest_int_param_value('MAP_ERR_CODE_TO_RULE').

      if lv_map_error_code_to_rule ne 'X'.

        if lv_explicit_title is not initial.

          ls_json_req_obj-aa_errorcode  = lv_explicit_title.

        else.

          ls_json_req_obj-aa_errorcode  = <ls_extracted_alerts>-alert_tname.

        endif. " if lv_explicit_errorcode is not initial

      else.

        lv_title = ls_json_req_obj-title.

        ls_json_req_obj-aa_errorcode = get_ruleid_from_map(
                   exporting
                     ip_subsystem = lv_subsystem
                     ip_title     = lv_title ).

      endif.


      " Checking if prefix should be added

      lv_ticket_name_prefix = get_alrt_rest_int_param_value('TICKET_NAME_PREFIX').

      if ( lv_ticket_name_prefix ne '' ) .

        concatenate lv_ticket_name_prefix ls_json_req_obj-title into ls_json_req_obj-title separated by space.

      endif. " IF ( lv_snow_inc_prefix NE '' )

      " Replacing special symbols in description

      remove_special_symbols(
        changing
           cs_string = lv_description ).


      ls_json_req_obj-description = lv_description.

      " Executing serialization

      create object lr_json_serializer
        exporting
          data = ls_json_req_obj.

      lr_json_serializer->serialize( ).
      lv_json_req_str = lr_json_serializer->get_data( ).

      " -------------------------------------------------------------------------
      "       Assinging output parameters
      " -------------------------------------------------------------------------

      ep_payload = lv_json_req_str.
      ep_description = lv_description.

    endloop. " loop at it_extracted_alerts assigning <ls_extracted_alerts>

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SERVESS_ALERTS_INTEGRATION->EXTRACT_SUB_OBJECTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IPO_OBJECT                     TYPE REF TO IF_ALERT_CONSM_OBJECT
* | [--->] IPV_ALERT_ID                   TYPE        STRING
* | [--->] IPV_PARENT_ID                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method extract_sub_objects.

    data: ls_extracted_sub_object type class_app_metric=>st_my_sub_object_data,
          ls_sub_object_temp      type  class_app_metric=>st_my_sub_object_data,
          lt_sub_objects          type e2ea_t_alert_consm_object,
          lo_sub_object           type ref to if_alert_consm_object.

    "Code for cycle detection in event hierarchy

    read table gt_events into ls_sub_object_temp with key event_id = ipo_object->get_id( ).

    if sy-subrc = 0.
      if ls_sub_object_temp-visited = 'TRUE'.
        " we have a cycle in the event hierarchy
        " handle it according to your convenience.
        "This is an error or exception case.
      endif.
    endif. " if sy-subrc = 0

    ls_extracted_sub_object-alert_id = ipv_alert_id.
    ls_extracted_sub_object-event_id = ipo_object->get_id( ).
    ls_extracted_sub_object-obj_type = ipo_object->get_object_type( ).
    ls_extracted_sub_object-parent_id = ipv_parent_id.
    ls_extracted_sub_object-visited = 'TRUE'.
    ls_extracted_sub_object-value = ipo_object->get_value( ).
    ls_extracted_sub_object-timestamp = ipo_object->get_timestamp( ).
    ls_extracted_sub_object-metricpath = ipo_object->get_metric_path( ).
    ls_extracted_sub_object-reasonforclosure = ipo_object->get_reason_for_closure( ).
    ls_extracted_sub_object-name = ipo_object->get_name( ).

    ls_extracted_sub_object-rating = cl_alert_consm_utility=>get_domain_value_text(
      i_domain_name = cl_alert_consm_constants=>ac_domname_rating
      i_value = ipo_object->get_rating( )
      ).

    ls_extracted_sub_object-text = ipo_object->get_text_value( ).

    append ls_extracted_sub_object to gt_events.

    if ipo_object->get_object_type( ) = cl_alert_consm_constants=>ac_metric_consm_object.
      return.
    elseif ipo_object->get_object_type( ) = cl_alert_consm_constants=>ac_event_consm_object
      or ipo_object->get_object_type( ) = cl_alert_consm_constants=>ac_metricgrp_consm_object.

      if ipo_object->has_sub_objects( ) = abap_true.

        lt_sub_objects = ipo_object->get_sub_objects( ).

        loop at lt_sub_objects into lo_sub_object.
          extract_sub_objects( ipo_object = lo_sub_object
                               ipv_parent_id = ls_extracted_sub_object-event_id
                               ipv_alert_id = ipv_alert_id ).
        endloop. " loop at lt_sub_objects into lo_sub_object

      endif. " if ipo_object->has_sub_objects( ) = abap_true

    endif. " if ipo_object->get_object_type( ) = cl_alert_consm_constants=>ac_metric_consm_object

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_ALRT_REST_INT_PARAM_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PARAM_NAME                  TYPE        CHAR20
* | [<-()] EP_VALUE                       TYPE        CHAR258
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_alrt_rest_int_param_value.

    select single value from zalrtrestintpara
      into ep_value
        where param eq ip_param_name.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CITYPE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EP_RESULT                      TYPE        CHAR100
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_CITYPE.

    ep_result = 'TechnicalService'.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CPIDS_SOURCE_SYSTEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_TASK_NAME                   TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_cpids_source_system.

    ep_result = ip_task_name.

    find all occurrences of regex '_*_(.*)_' in ip_task_name
         match offset data(lv_moff)
         match length data(lv_mlen).

    if ( lv_moff is not initial ) and ( lv_mlen is not initial ).

      " Excluding _ symbols

      lv_moff = lv_moff + 1.
      lv_mlen = lv_mlen - 2.

      ep_result = substring( val = ip_task_name off = lv_moff len = lv_mlen ).

    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CPIDS_TASK_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_CPIDS_TASK_NAME.

    data lv_iflow_name type string.

    lv_iflow_name = substring_after( val = ip_metric_path sub = 'TASK=' ).

    search lv_iflow_name for '|'.

    if sy-subrc = 0.

      ep_result = substring_before( val = lv_iflow_name sub = '|' ).

    else.

      ep_result = lv_iflow_name.

    endif. " if sy-subrc = 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CPI_DS_PROJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_cpi_ds_project.

    data lv_project type string.

    lv_project = substring_after( val = ip_metric_path sub = 'PROJECT=' ).

    search lv_project for '|'.

    if sy-subrc = 0.

      ep_result = substring_before( val = lv_project sub = '|' ).

    else.

      ep_result = lv_project.

    endif. " if sy-subrc = 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CPI_IFLOW_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_CPI_IFLOW_NAME.

    data lv_iflow_name type string.

    lv_iflow_name = substring_after( val = ip_metric_path sub = 'IFLOW=' ).

    search lv_iflow_name for '|'.

    if sy-subrc = 0.

      ep_result = substring_before( val = lv_iflow_name sub = '|' ).

    else.

      ep_result = lv_iflow_name.

    endif. " if sy-subrc = 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_CPI_SOURCE_SYSTEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_IFLOW_NAME                  TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_cpi_source_system.

*    search ip_iflow_name for '_'.
*
*    if sy-subrc = 0.
*
*      ep_result = substring_before( val = ip_iflow_name sub = '_' ).
*
*    else.
*
*      ep_result = ip_iflow_name.
*
*    endif. " if sy-subrc = 0
*

    " New logic as per September 8th 2022: CPI source system = IFLOW

    ep_result = ip_iflow_name.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_EXMGMT_PROCESS_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_ALERT_GUID                  TYPE        AC_GUID(optional)
* | [--->] IP_METRIC_PATH                 TYPE        STRING(optional)
* | [--->] IP_HCIDS_MANAGED_OBJECT_NAME   TYPE        E2EEM_IMPL_SOURCE(optional)
* | [--->] IP_HCIDS_INTERFACE_NAME        TYPE        E2EEM_SUBCATEGORY(optional)
* | [<-()] EP_RESULT                      TYPE        AC_GUID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_exmgmt_process_id.

    constants process_id_length type int2 value 32.

    data:
          lv_process_id_char type char32.

    if ip_alert_guid is supplied.

      select single exception_id
         into ep_result from e2ea_alertgroup
         where algroup_id = ip_alert_guid.

    endif. " if ip_alert_guid is supplied

    if ip_metric_path is supplied.

      lv_process_id_char = substring_after( val = ip_metric_path sub = 'PROCESS_INSTANCE=' ).

      if strlen( lv_process_id_char ) ge process_id_length.

        ep_result = substring( val = lv_process_id_char off = 0 len = process_id_length ).

      endif. " if strlen( lv_process_id_char ) GE process_id_length

    endif. " if ip_metric_path is supplied

    if ip_hcids_managed_object_name is supplied.

      select instance_id from e2eem_core into ep_result
        up to 1 rows
        where imp_source = 'HCIDS' and
        tech_ctxt = ip_hcids_managed_object_name and
        subcategory = ip_hcids_interface_name
        order by save_timestamp descending.

      endselect.


    endif. " if ip_hcids_managed_object_name is supplied


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_METRIC_TECH_CTX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_CONTEXT_ID                  TYPE        AC_GUID
* | [--->] IP_EVENT_TYPE_ID               TYPE        AC_GUID
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_metric_tech_ctx.

    select single value_low into ep_result from accollseloptdir
      where context_id = ip_context_id and
      event_type_id = ip_event_type_id and
      ac_variant eq 'A' and
      parameter_id eq 'TECH_CTX'.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_PI_INTERFACE_COMPONENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [--->] IP_COMPONENT_NAME              TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_pi_interface_components.

    data:
      lv_component_name   type string,
      lv_string_to_search type string.

    concatenate ip_component_name '=' into lv_string_to_search.

    lv_component_name = substring_after( val = ip_metric_path sub = lv_string_to_search ).

    search lv_component_name for '|'.

    if sy-subrc = 0.

      ep_result = substring_before( val = lv_component_name sub = '|' ).

    else.

      ep_result = lv_component_name.

    endif. " if sy-subrc = 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_PI_INTERFACE_SUBSYSTEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_pi_interface_subsystem.


    ep_result = get_pi_interface_components(
      exporting
        ip_metric_path    = ip_metric_path
        ip_component_name = 'RCV_COMPONENT' ).

    if strlen( ep_result )  eq 0.

      ep_result = get_pi_interface_components(
        exporting
          ip_metric_path    = ip_metric_path
          ip_component_name = 'SND_COMPONENT' ).
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_PI_INTERFACE_TITLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_METRIC_PATH                 TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_PI_INTERFACE_TITLE.


    ep_result = get_pi_interface_components(
      exporting
        ip_metric_path    = ip_metric_path
        ip_component_name = 'RCV_INTERFACE' ).

    if strlen( ep_result )  eq 0.

      ep_result = get_pi_interface_components(
        exporting
          ip_metric_path    = ip_metric_path
          ip_component_name = 'SND_INTERFACE' ).
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_RULEID_FROM_MAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_SUBSYSTEM                   TYPE        CPIS_LOG_TEXT
* | [--->] IP_TITLE                       TYPE        CHAR258
* | [<-()] EP_RULEID                      TYPE        SMOCC_TXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_ruleid_from_map.

    types:
      begin of ty_rules_map,
        subsystem type cpis_log_text,
        title     type char258,
        ruleid    type smocc_txt,

      end of ty_rules_map.

    data: lv_subsystem       type cpis_log_text,
          lv_title           type char258,
          lv_found_subsystem type abap_bool,
          lv_found_title     type abap_bool,
          lt_rules_map       type table of ty_rules_map.

    clear lt_rules_map.

    select subsystem title ruleid from zservessrulesmap into corresponding fields of table lt_rules_map.

    loop at lt_rules_map assigning field-symbol(<ls_rules_map>).

      clear:
        lv_subsystem,
        lv_title,
        lv_found_subsystem,
        lv_found_title.

      lv_subsystem = <ls_rules_map>-subsystem.
      lv_title = <ls_rules_map>-title.

      if ip_subsystem ne '*'.

        if ( ip_subsystem cp lv_subsystem ).

          lv_found_subsystem = abap_true.
        endif.

      else.

        if ( lv_subsystem eq ip_subsystem ).

          lv_found_subsystem = abap_true.
        endif.

      endif. " if ip_subsystem ne '*'


      if ip_title ne '*'.

        if ( ip_title cp lv_title ).

          lv_found_title = abap_true.

        endif.

      else.

        if ( lv_title eq ip_title ).

          lv_found_title = abap_true.

        endif.

      endif. "  if ip_title ne '*'

      if ( lv_found_title eq abap_true ) and ( lv_found_subsystem eq abap_true ).

        ep_ruleid = <ls_rules_map>-ruleid.

      endif. " if ( lv_found_title eq abap_true ) and ( lv_found_subsystem eq abap_true )

    endloop. " loop at lt_rules_map assigning field-symbol(<ls_rules_map>)

    " If we haven't found the rule, then doing a recursion with *

    if ep_ruleid is initial.

      if ip_title ne '*'.

        ep_ruleid = get_ruleid_from_map(
          exporting
            ip_subsystem = ip_subsystem
            ip_title     = '*' ).
      else.

        ep_ruleid = get_ruleid_from_map(
          exporting
            ip_subsystem = '*'
            ip_title     = '*' ).

      endif. " if ip_title ne '*'

    endif. " if ep_ruleid is initial

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_SEVERITY_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_SEVERITY                    TYPE        AC_SEVERITY
* | [<-()] EP_RESULT                      TYPE        CHAR100
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_severity_text.

    " List of available values

    " Critical
    " Error
    " Warning
    " Informational
    " Verbose

    if ( ip_severity ge 9 ).

      ep_result = 'Critical'.

    elseif ( ( ip_severity ge 7 ) and ( ip_severity le 8 ) ).

      ep_result = 'Error'.

    elseif ( ( ip_severity ge 5 ) and ( ip_severity le 6 ) ).

      ep_result = 'Warning'.

    else.

     ep_result = 'Informational'.

    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>GET_TSYS_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_ALERT_TNAME                 TYPE        AC_NAME
* | [--->] IP_EXT_SID                     TYPE        E2EEM_IMPL_SOURCE
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_TSYS_URL.

    data :
      lo_lmdb_data_finder type ref to zcl_lmdb_data_finder,
      lv_system_type      type lmdb_system_type.

    create object lo_lmdb_data_finder.


    case ip_alert_tname.

      when 'ICMON_HIGH_NO_OF_HCI_S_EXCEPTION_ALERT' or 'ICMON_HIGH_NO_OF_HCIDS_EXCEPTION_ALERT'.

        lv_system_type = 'EXT_SRV'.

      when others.

        lv_system_type = 'ABAP'.

    endcase.

    call method lo_lmdb_data_finder->get_tsys_url
      exporting
        ip_extsid      = ip_ext_sid
        ip_system_type = lv_system_type
      receiving
        ep_result      = ep_result.



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SERVESS_ALERTS_INTEGRATION->IF_ALERT_REACTION~IS_AUTO_REACTION
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_FLAG                        TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method if_alert_reaction~is_auto_reaction.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SERVESS_ALERTS_INTEGRATION->IF_ALERT_REACTION~REACT_TO_ALERTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IPT_ALERTS                     TYPE        E2EA_T_ALERT_CONSM_OBJECT
* | [--->] IP_XML                         TYPE        AC_XSTRING
* | [--->] IP_FILTER_VAL                  TYPE        AC_REACTION_ID(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method if_alert_reaction~react_to_alerts.

    data: lt_data_for_payload type tt_my_alert_data,
          lv_post_payload     type string,
          lv_post_response    type string,
          lv_description      type string,
          lv_rfc_name         type rfcdest,
          lv_log_record_text  type string.

    clear gt_events.

    " Collecting alert data from BADI input parameters

    lt_data_for_payload = me->collect_data_for_payload( it_alert = ipt_alerts
                               ip_alert_group_state = 'Opened' ).

    " Preparing JSON payload for REST API POST

    me->compile_payload(
        exporting
          it_extracted_alerts = lt_data_for_payload
        importing
          ep_description = lv_description
          ep_payload = lv_post_payload ).


    " Special ServESS JSON transformation for fields cases

    me->adjust_payload(
      changing
        cs_payload = lv_post_payload ).

    " Getting RFC name

    lv_rfc_name = zcl_servess_alerts_integration=>get_alrt_rest_int_param_value( 'SERVESS_RFC_DEST' ).

    if lv_rfc_name is initial.

      lv_log_record_text =  'ERROR: RFC destination not set, stopping'.

      add_record_to_app_log(
               ip_msgty  = 'E'
               ip_log_record_text = lv_log_record_text ).

      return.

    endif. " if lv_api_key is initial

    " Executing POST REST API request to ServESS

    send_payload_as_post_request(
      exporting
       ip_destination = lv_rfc_name
       ip_json_request = lv_post_payload
      importing
       ep_json_response = lv_post_response ).


    me->update_record_in_intgr_log_tab(
    exporting
      ip_description = lv_description
      ip_payload = lv_post_payload
      it_extracted_alerts = lt_data_for_payload
      ip_action = 'A').


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SERVESS_ALERTS_INTEGRATION->IF_ALERT_REACTION~REACT_TO_CLOSED_ALERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ALERT                       TYPE REF TO IF_ALERT_CONSM_OBJECT
* | [--->] IV_XML                         TYPE        AC_XSTRING
* | [--->] IV_FILTER_VAL                  TYPE        AC_REACTION_ID(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method if_alert_reaction~react_to_closed_alert.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>OBJECT_HAS_EXMGMT_LINK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_ALERT_GUID                  TYPE        AC_GUID(optional)
* | [--->] IP_METRIC_PATH                 TYPE        STRING(optional)
* | [<-()] EP_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method object_has_exmgmt_link.

    ep_result = abap_false.

    if ip_alert_guid is supplied.

      select count( * ) up to 1 rows from e2ea_alertgroup
        where algroup_id = ip_alert_guid.

      if sy-subrc = 0.

        " The alert is  linked with Exception Management, exiting a method

        ep_result = abap_true.

      endif. "IF ( lv_process_instance_id IS NOT INITIAL )

    endif. " if ip_alert_guid is supplied

    if ip_metric_path is supplied.

      find 'PROCESS_INSTANCE=' in ip_metric_path.

      if sy-subrc = 0.

        " The metric is linked with Exception Management, exiting a method

        ep_result = abap_true.

      endif. "IF ( lv_process_instance_id IS NOT INITIAL )

    endif. " if ip_metric_path is supplied


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>REMOVE_SPECIAL_SYMBOLS
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CS_STRING                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method remove_special_symbols.

    data c_newline value cl_abap_char_utilities=>newline.

    replace all occurrences of '<h2>' in cs_string with '' ignoring case.
    replace all occurrences of '</h2>' in cs_string with '' ignoring case.
    replace all occurrences of '<strong>' in cs_string with '' ignoring case.
    replace all occurrences of '</strong>' in cs_string with '' ignoring case.
    replace all occurrences of '<p>' in cs_string with '' ignoring case.
    replace all occurrences of '</p>' in cs_string with c_newline ignoring case.
    replace all occurrences of '<b>' in cs_string with '' ignoring case.
    replace all occurrences of '</b>' in cs_string with '' ignoring case.
    replace all occurrences of '<u>' in cs_string with '' ignoring case.
    replace all occurrences of '</u>' in cs_string with '' ignoring case.
    replace all occurrences of '<i>' in cs_string with '' ignoring case.
    replace all occurrences of '</i>' in cs_string with '' ignoring case.
    replace all occurrences of '<ul>' in cs_string with '' ignoring case.
    replace all occurrences of '</ul>' in cs_string with '' ignoring case.
    replace all occurrences of '<li>' in cs_string with '' ignoring case.
    replace all occurrences of '</li>' in cs_string with c_newline ignoring case.
    replace all occurrences of '<a href="' in cs_string with '' ignoring case.
    replace all occurrences of regex '">[A-Za-z0-9_\~\-+=&[:space:]]*</a>' in  cs_string with '' ignoring case.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>SEND_PAYLOAD_AS_POST_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_DESTINATION                 TYPE        RFCDEST
* | [--->] IP_PARAMLINE                   TYPE        CHAR1024(optional)
* | [--->] IP_JSON_REQUEST                TYPE        STRING
* | [<---] EP_JSON_RESPONSE               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method send_payload_as_post_request.
    data: lo_http_client     type ref to if_http_client,
          lo_rest_client     type ref to cl_rest_http_client,
          lv_body            type        string,
          lv_token           type        string,
          agreements         type        string,
          lo_response        type ref to     if_rest_entity,
          lo_request         type ref to if_rest_entity,
          lv_log_record_text type string,
          lv_api_key         type char32,
          lv_path_prefix     type string.


    concatenate 'Sending payload via REST API POST request:' ip_json_request  into lv_log_record_text separated by space.

    add_record_to_app_log(
             ip_msgty  = 'I'
             ip_log_record_text = lv_log_record_text ).

    clear lv_log_record_text.

    " Getting ServESS API key

    lv_api_key = zcl_servess_alerts_integration=>get_alrt_rest_int_param_value( 'SERVESS_API_KEY' ).

    if lv_api_key is initial.

      lv_log_record_text = 'ERROR: API key not set, stopping' .

      add_record_to_app_log(
               ip_msgty  = 'E'
               ip_log_record_text = lv_log_record_text ).

      return.

    endif. " if lv_api_key is initial

    concatenate 'Selected destination:' ip_destination   into lv_log_record_text separated by space.

    add_record_to_app_log(
             ip_msgty  = 'I'
             ip_log_record_text = lv_log_record_text ).


    cl_http_client=>create_by_destination(
     exporting
       destination              = ip_destination    " Logical destination (specified in function call)
     importing
       client                   = lo_http_client    " HTTP Client Abstraction
     exceptions
       argument_not_found       = 1
       destination_not_found    = 2
       destination_no_authority = 3
       plugin_not_active        = 4
       internal_error           = 5
       others                   = 6
    ).

    if ( sy-subrc <> 0 ).

      case sy-subrc.
        when '1'.
          lv_log_record_text = 'argument_not_found'.
        when '2'.
          lv_log_record_text = 'destination_not_found'.
        when '3'.
          lv_log_record_text = 'destination_no_authority'.
        when '4'.
          lv_log_record_text = 'plugin_not_active'.
        when '5'.
          lv_log_record_text = 'internal_error'.
        when others.
          lv_log_record_text ='not_known_exception'.

      endcase.

      concatenate 'HTTPS destination error:' lv_log_record_text  into lv_log_record_text separated by space.

      add_record_to_app_log(
               ip_msgty  = 'E'
               ip_log_record_text = lv_log_record_text ).

      ep_json_response = ''.

      exit.

    endif. " IF ( sy_subrc <> 0 )


* Create REST client instance

    create object lo_rest_client
      exporting
        io_http_client = lo_http_client.

* Set HTTP version

    lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).

    if lo_http_client is bound and lo_rest_client is bound.

      " Getting RFC destination path prefix

      lv_path_prefix = zcl_servess_alerts_integration=>get_alrt_rest_int_param_value( 'RFC_DEST_PATH_PREFIX' ).


      concatenate 'Selected path prefix:' lv_path_prefix   into lv_log_record_text separated by space.

      add_record_to_app_log(
               ip_msgty  = 'I'
               ip_log_record_text = lv_log_record_text ).

      " Set the URI

      cl_http_utility=>set_request_uri(
          exporting
            request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
            uri     = lv_path_prefix                     " URI String (in the Form of /path?query-string)
        ).


      " Set headers

      lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
      lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
      lo_request->set_string_data( ip_json_request ).


      concatenate 'rest_api_key=' lv_api_key into lv_token.

      call method lo_rest_client->if_rest_client~set_request_header
        exporting
          iv_name  = 'Authorization'
          iv_value = lv_token.



      " Executing HTTP POST

      try.

      lo_rest_client->if_rest_client~post( lo_request ).

    catch cx_rest_client_exception into data(lo_rest_client_error) .

      lv_log_record_text = lo_rest_client_error->get_text( ).

      add_record_to_app_log(
               ip_msgty  = 'E'
               ip_log_record_text = lv_log_record_text ).

      ep_json_response = ''.

      exit.

  endtry.

  " HTTP response

  lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

  " HTTP return status

  data(lv_http_status)   = lo_response->get_header_field( '~response_line' ).

  concatenate 'Received REST API POST response line:' lv_http_status  into lv_log_record_text separated by space.

  add_record_to_app_log(
           ip_msgty  = 'I'
           ip_log_record_text = lv_log_record_text ).


endif. " IF lo_http_client IS BOUND AND lo_rest_client IS BOUND

" Filling response

ep_json_response = lo_response->get_string_data( ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_SERVESS_ALERTS_INTEGRATION=>UPDATE_RECORD_IN_INTGR_LOG_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_EXTRACTED_ALERTS            TYPE        TT_MY_ALERT_DATA(optional)
* | [--->] IP_PAYLOAD                     TYPE        STRING
* | [--->] IP_ACTION                      TYPE        CHAR1
* | [--->] IP_DESCRIPTION                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method update_record_in_intgr_log_tab.

    data:
      wa_zalrtservesslog       type zalrtservesslog.

    field-symbols:
      <ls_extracted_alerts> like line of it_extracted_alerts.

    loop at it_extracted_alerts assigning <ls_extracted_alerts>.

      wa_zalrtservesslog-mai_object_name = <ls_extracted_alerts>-managed_object_name.
      wa_zalrtservesslog-mai_alert_name = <ls_extracted_alerts>-alert_name.
      wa_zalrtservesslog-mai_alert_tname  = <ls_extracted_alerts>-alert_tname.
      wa_zalrtservesslog-mai_status = <ls_extracted_alerts>-status.
      wa_zalrtservesslog-mai_timestamp = <ls_extracted_alerts>-timestamp.
      wa_zalrtservesslog-mai_alert_guid = <ls_extracted_alerts>-alert_id.
      wa_zalrtservesslog-description = ip_description.
      wa_zalrtservesslog-payload =  ip_payload.
      wa_zalrtservesslog-update_date = sy-datum.
      wa_zalrtservesslog-update_time = sy-uzeit.

    endloop. " loop at it_extracted_alerts assigning <ls_extracted_alerts>

    case ip_action.

      when 'A'.
        insert zalrtservesslog from wa_zalrtservesslog.

    endcase.


  endmethod.
ENDCLASS.

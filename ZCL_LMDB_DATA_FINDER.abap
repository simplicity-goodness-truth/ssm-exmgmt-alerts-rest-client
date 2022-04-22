class ZCL_LMDB_DATA_FINDER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods GET_TSYS_URL
    importing
      !IP_EXTSID type E2EEM_IMPL_SOURCE
      !IP_SYSTEM_TYPE type LMDB_SYSTEM_TYPE
    returning
      value(EP_RESULT) type STRING .
protected section.
private section.

  data ATTR_WBEM_CLIENT type ref to IF_WBEM_SAP_CLIENT .
  data ATTR_TECHNICAL_SYSTEM_API type ref to IF_LMDB_API_FOR_TECH_SYST .
ENDCLASS.



CLASS ZCL_LMDB_DATA_FINDER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LMDB_DATA_FINDER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method constructor.

    data: lx_error          type ref to cx_wbem_cim_err,
          et_message        type /plmb/t_spi_msg,
          lo_domain_context type ref to if_lmdb_cim_domain_context,
          lo_domain_manager type ref to if_lmdb_cim_domain_manager.


    lo_domain_manager = cl_lmdb_cim_factory=>get_domain_manager( ).


    " Get local WBEM client and technical system API

    clear et_message.

    try.

        lo_domain_context ?= lo_domain_manager->get_domain_context( cl_lmdb_cim_domain=>ldb ).

        attr_wbem_client = lo_domain_context->create_wbem_client( ).

        attr_technical_system_api = cl_lmdb_api_factory=>create_from_client( attr_wbem_client )->get_technical_system_api( ).

      catch cx_wbem_cim_err into lx_error.

        return.

    endtry.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LMDB_DATA_FINDER->GET_TSYS_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_EXTSID                      TYPE        E2EEM_IMPL_SOURCE
* | [--->] IP_SYSTEM_TYPE                 TYPE        LMDB_SYSTEM_TYPE
* | [<-()] EP_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method get_tsys_url.

    data:
      lv_applicationdomain       type string,
      lv_externalserverid        type string,
      lv_cim_i_applicationdomain type string,
      lv_cim_i_externalserverid  type string,
      lv_extsid                  type lmdb_esid,
      lt_technical_system        type ref to cl_lmdb_technical_system.



    if ip_system_type eq 'EXT_SRV'.

      lv_applicationdomain = substring_before( val = ip_extsid sub = '~' ).
      lv_externalserverid = substring_after( val = ip_extsid sub = '~' ).

      if ( lv_applicationdomain is not initial ) and ( lv_externalserverid is not initial ).

        data(lt_technical_systems) = attr_technical_system_api->get_all_technical_systems( ).

        loop at lt_technical_systems into data(ls_techical_system).

          data(lo_cim_instance) = ls_techical_system->get_cim_instance( ).

          data(lt_cim_instance_prop) = lo_cim_instance->property_list.

          clear:
            lv_cim_i_applicationdomain,
            lv_cim_i_externalserverid.

          read table lt_cim_instance_prop into data(ls_cim_instance_prop_domain) with key comparable_name = 'applicationdomain'.

          if ( sy-subrc = 0 ).

            lv_cim_i_applicationdomain = ls_cim_instance_prop_domain-value.

            translate lv_cim_i_applicationdomain to upper case.

            if ( lv_cim_i_applicationdomain eq lv_applicationdomain ).

              read table lt_cim_instance_prop into data(ls_cim_instance_prop_server) with key comparable_name = 'externalserverid'.

              if ( sy-subrc = 0 ).

                lv_cim_i_externalserverid = ls_cim_instance_prop_server-value.

                translate lv_cim_i_externalserverid to upper case.

                if ( lv_cim_i_externalserverid eq lv_externalserverid ).

                  read table lt_cim_instance_prop into data(ls_cim_instance_prop_rooturl) with key comparable_name = 'rooturl'.

                  if ( sy-subrc = 0 ).

                    ep_result = ls_cim_instance_prop_rooturl-value.

                  endif. " if ( sy-subrc = 0 )

                endif. "if ( lv_cim_i_externalserverid eq lv_externalserverid )

              endif. " if ls_cim_instance_prop_domain-value eq lv_applicationdomain

            endif. " if ( lv_cim_i_applicationdomain eq lv_applicationdomain )

          endif. " if ls_cim_instance_prop_domain-value eq lv_applicationdomain

        endloop.

      endif. " if ( lv_applicationdomain is not initial ) and ( lv_externalserverid is not initial )

    else.

      lv_extsid = ip_extsid.


      try.

          lt_technical_system = attr_technical_system_api->get_by_extsid(
                                extsid          = lv_extsid
                                system_type     = ip_system_type ).


        catch cx_lmdb_no_technical_system.

          if lt_technical_system is initial.

            lv_extsid = substring_before( val = ip_extsid sub = '~' ).

            try.

                lt_technical_system = attr_technical_system_api->get_by_extsid(
                              extsid          = lv_extsid
                              system_type     = ip_system_type ).

              catch cx_lmdb_no_technical_system.

                return.

            endtry.

            data(lo_tsys_cim_instance) = lt_technical_system->get_cim_instance( ).

            data(lt_tsys_cim_instance_prop) = lo_tsys_cim_instance->property_list.

            read table lt_tsys_cim_instance_prop into data(ls_tsys_cim_instance_prop) with key comparable_name = 'systemhome'.

            if ( sy-subrc = 0 ).

              ep_result = ls_tsys_cim_instance_prop-value.

            endif. " if ( sy-subrc = 0 )

          endif. " if lt_technical_system is initial

      endtry.

    endif. "  if ip_system_type eq 'EXT_SRV'


  endmethod.
ENDCLASS.
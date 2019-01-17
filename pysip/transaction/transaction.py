import pysip.message.method as sip_method


def new_server(sip_msg, options):
    serv_id = server_id(sip_msg)
    if sip_msg.method == sip_method.invite():
        module = ersip_trans_inv_server
    elif sip_msg.method == sip_method.ack():
        raise
    else:
        module = ersip_trans_server
    return module.new(transport(sip_msg), sip_msg, options)


def new_client(out_request, options):
    cl_id = client_id(out_request)
    if out_request.method == sip_method.invite():
        module = ersip_trans_inv_client
    elif out_request.method == sip_method.ack():
        raise
    else:
        module = ersip_trans_client
    NexthopURI = nexthop(out_request)
    Transport = make_by_uri(NexthopURI)
    TransportType = transport_type_by_transport(Transport)
    return module.new(TransportType, out_request, options)


def server_id(sip_msg):

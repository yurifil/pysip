from pysip.uri.transport import Transport


class Source(object):
    def __init__(self, peer=None, transport=None, source_id=None):
        self.peer = peer
        self.transport = transport
        self.source_id = source_id


def is_tls(source):
    """

    Args:
        source (:obj;Source)

    Returns:
        True if source has TLS transport attribute
        False otherwise
    """
    if Transport.is_tls(source.transport):
        return True
    return False

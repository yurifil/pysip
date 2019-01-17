from pysip.message.hnames import compact_form, print_form

IANAPageCopy = b"Accept-Contact	a	[RFC3841]\
          |Referred-By	b	[RFC3892]\
          |Content-Type	c	[RFC3261]\
          |Request-Disposition	d	[RFC3841]\
          |Content-Encoding	e	[RFC3261]\
          |From	f	[RFC3261]\
          |Call-ID	i	[RFC3261]\
          |Reject-Contact	j	[RFC3841]\
          |Supported	k	[RFC3261]\
          |Content-Length	l	[RFC3261]\
          |Contact	m	[RFC3261]\
          |Event	o	[RFC6665][RFC6446]\
          |Refer-To	r	[RFC3515]\
          |Subject	s	[RFC3261]\
          |To	t	[RFC3261]\
          |Allow-Events	u	[RFC6665]\
          |Via	v	[RFC3261][RFC7118]\
          |Session-Expires	x	[RFC4028]\
          |Identity	y	[RFC8224]"

PrintForms = [b"From",
              b"To",
              b"CSeq",
              b"Call-Id",
              b"Max-Forwards",
              b"Content-Type",
              b"Route",
              b"Record-Route",
              b"Allow",
              b"Supported",
              b"Unsupported",
              b"Require",
              b"Proxy-Require",
              b"p-custom-header"]

headers_f = list()
for h in IANAPageCopy.split(b'|'):
    splitted = h.split(b'\t')
    headers_f.append((splitted[0], splitted[1]))


def test_compact_form():
    for field_name, field_compact_form in headers_f:
        assert compact_form(field_name) == field_compact_form


def test_print_form():
    for pf in PrintForms:
        assert print_form(pf.lower()) == pf

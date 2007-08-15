## snippet splitlines
def splitlines(s):
    ret = []
    while True:
        head, sep, tail = s.partition('\r\n')
        if not (sep or tail):
            head, sep, tail = s.partition('\r')
        if not (sep or tail):
            head, sep, tail = s.partition('\n')
        if not (sep or tail):
            break
        ret.append(head)
        s = tail
    if s:
        ret.append(s)
    return ret
## /snippet splitlines

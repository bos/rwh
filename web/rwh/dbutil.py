import MySQLdb as mysql

def connect():
    try:
        import secrets
    except ImportError:
        print >> sys.stderr, 'Decrypt secrets.py.gpg or create a new copy!'
        sys.exit(1)

    if secrets.DATABASE_ENGINE != 'mysql':
        print >> sys.stderr, ('You are using a %s database' %
                              secrets.DATABASE_ENGINE)
        sys.exit(1)

    kwargs = {
        'charset': 'utf8',
        'use_unicode': True,
        }
    if secrets.DATABASE_USER:
        kwargs['user'] = secrets.DATABASE_USER
    if secrets.DATABASE_NAME:
        kwargs['db'] = secrets.DATABASE_NAME
    if secrets.DATABASE_PASSWORD:
        kwargs['passwd'] = secrets.DATABASE_PASSWORD
    if secrets.DATABASE_HOST.startswith('/'):
        kwargs['unix_socket'] = secrets.DATABASE_HOST
    elif secrets.DATABASE_HOST:
        kwargs['host'] = secrets.DATABASE_HOST
    if secrets.DATABASE_PORT:
        kwargs['port'] = int(secrets.DATABASE_PORT)
    return mysql.connect(**kwargs)

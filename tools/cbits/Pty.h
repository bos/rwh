#ifndef Pty_H
#define Pty_H

extern char *__pty_ptsname(int fd);

extern int __pty_grantpt(int fd);

extern int __pty_unlockpt(int fd);

extern int __pty_setctty(int fd);

#endif /* Pty_H */

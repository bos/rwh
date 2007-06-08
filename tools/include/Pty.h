#ifndef Pty_H
#define Pty_H

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef INLINE
# if defined(__GNUC__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

INLINE char *__pty_ptsname(int fd)
{
    extern char *ptsname(int);
    return ptsname(fd);
}

INLINE int __pty_grantpt(int fd)
{
    extern int grantpt(int);
    return grantpt(fd);
}

INLINE int __pty_unlockpt(int fd)
{
    extern int unlockpt(int);
    return unlockpt(fd);
}

INLINE int __pty_setctty(int fd)
{
#ifdef TIOCSCTTY
    if (ioctl(fd, TIOCSCTTY, NULL) == -1)
	return -1;
#else
    // If we don't have TIOCSCTTY, we should be able to replace the
    // current controlling terminal by detaching from it and opening
    // the named terminal again.  This open should cause the terminal
    // to become our controlling terminal.

    char *name = ttyname(fd);
    int nfd;
    if (name == NULL)
	return -1;

    if (fd != STDIN_FILENO)
	close(STDIN_FILENO);
    if (fd != STDOUT_FILENO)
	close(STDOUT_FILENO);
    if (fd != STDERR_FILENO)
	close(STDERR_FILENO);
    nfd = open(name, O_RDWR);
    if (nfd >= 0)
	close(nfd);
#endif
    return 0;
}

#endif /* Pty_H */

/*
 * This file is part of the ZRDX OSE project
 * (C) 2026, Viacheslav Komenda
 *
 * MSVCRT.DLL: time functions. time_t is a 32 bit count of
 * seconds since 1970 (the Microsoft runtime of this
 * generation). The DOS clock is the local time and there is no
 * time zone information, so localtime equals gmtime and
 * _timezone is 0.
 */
#include "crt.h"

typedef long time_t;

typedef struct Tm
{
    int tm_sec;
    int tm_min;
    int tm_hour;
    int tm_mday;
    int tm_mon;
    int tm_year;
    int tm_wday;
    int tm_yday;
    int tm_isdst;
} Tm;

typedef struct TimeB
{
    time_t time;
    unsigned short millitm;
    short timezone;
    short dstflag;
} TimeB;

typedef struct SysTime
{
    WORD year, month, wday, day, hour, minute, second, msec;
} SysTime;

extern void WINAPI GetLocalTime(SysTime *);
extern DWORD __cdecl strlen(const char *s);

long _timezone;
int _daylight;
char *_tzname[2] = {"", ""};

static Tm result;
static char ascbuf[28];
static DWORD start_tick;

static const char *const wdays[7] = {"Sunday", "Monday",
    "Tuesday",
                                     "Wednesday", "Thursday",
                                     "Friday", "Saturday"
                                    };
static const char *const months[12] = {"January", "February",
    "March",
                                       "April", "May", "June",
                                           "July",
                                       "August", "September",
                                       "October", "November",
                                       "December"
                                      };

void crt_clock_start(void)
{
    start_tick = GetTickCount();
}

static long fdiv(long a, long b)
{
    long q = a / b;

    return (a % b != 0 && ((a < 0) != (b < 0))) ? q - 1 : q;
}

static long days_from_civil(long y, long m, long d)
{
    long era, yoe, doy, doe;

    y -= m <= 2;
    era = fdiv(y, 400);
    yoe = y - era * 400;
    doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
}

static void civil_from_days(long z, long *y, int *m, int *d)
{
    long era, doe, yoe, doy, mp;

    z += 719468;
    era = fdiv(z, 146097);
    doe = z - era * 146097;
    yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    mp = (5 * doy + 2) / 153;
    *d = (int)(doy - (153 * mp + 2) / 5 + 1);
    *m = (int)(mp < 10 ? mp + 3 : mp - 9);
    *y = yoe + era * 400 + (*m <= 2);
}

static Tm *fill(time_t t, Tm * tm)
{
    long days = fdiv(t, 86400L), secs = t - days * 86400L, y;
    int m, d;

    civil_from_days(days, &y, &m, &d);
    tm->tm_year = (int)(y - 1900);
    tm->tm_mon = m - 1;
    tm->tm_mday = d;
    tm->tm_hour = (int)(secs / 3600);
    tm->tm_min = (int)(secs % 3600 / 60);
    tm->tm_sec = (int)(secs % 60);
    tm->tm_wday = (int)(((days % 7) + 11) % 7);
    tm->tm_yday = (int)(days - days_from_civil(y, 1, 1));
    tm->tm_isdst = 0;
    return tm;
}

Tm *__cdecl gmtime(const time_t * t)
{
    if (*t < 0)
        return NULL;
    return fill(*t, &result);
}

Tm *__cdecl localtime(const time_t * t)
{
    return gmtime(t);
}

time_t __cdecl mktime(Tm * tm)
{
    long mon = tm->tm_mon, y = tm->tm_year + 1900L, days, t;

    y += fdiv(mon, 12);
    mon -= fdiv(mon, 12) * 12;
    days = days_from_civil(y, mon + 1, 1) + tm->tm_mday - 1;
    t = days * 86400L + tm->tm_hour * 3600L + tm->tm_min * 60L +
        tm->tm_sec;
    if (t < 0 || y > 2038)
        return -1;
    fill(t, tm);
    return t;
}

time_t __cdecl time(time_t * out)
{
    SysTime st;
    time_t t;

    GetLocalTime(&st);
    t = days_from_civil(st.year, st.month, st.day) * 86400L +
        st.hour * 3600L + st.minute * 60L + st.second;
    if (out)
        *out = t;
    return t;
}

void __cdecl _ftime(TimeB * tb)
{
    SysTime st;

    GetLocalTime(&st);
    tb->time = time(NULL);
    tb->millitm = st.msec;
    tb->timezone = 0;
    tb->dstflag = 0;
}

long __cdecl clock(void)
{
    return (long)(GetTickCount() - start_tick);
}

double __cdecl difftime(time_t a, time_t b)
{
    return (double)(a - b);
}

void __cdecl _tzset(void)
{
}

static void two(char *p, int v)
{
    p[0] = (char)('0' + v / 10 % 10);
    p[1] = (char)('0' + v % 10);
}

char *__cdecl asctime(const Tm * tm)
{
    char *p = ascbuf;
    int y = tm->tm_year + 1900, i;

    for (i = 0; i < 3; i++)
        *p++ = wdays[tm->tm_wday % 7][i];
    *p++ = ' ';
    for (i = 0; i < 3; i++)
        *p++ = months[tm->tm_mon % 12][i];
    *p++ = ' ';
    *p++ = tm->tm_mday < 10 ? ' ' :
           (char)('0' + tm->tm_mday / 10);
    *p++ = (char)('0' + tm->tm_mday % 10);
    *p++ = ' ';
    two(p, tm->tm_hour);
    p[2] = ':';
    two(p + 3, tm->tm_min);
    p[5] = ':';
    two(p + 6, tm->tm_sec);
    p += 8;
    *p++ = ' ';
    two(p, y / 100);
    two(p + 2, y);
    p += 4;
    *p++ = '\n';
    *p = 0;
    return ascbuf;
}

char *__cdecl ctime(const time_t * t)
{
    Tm *tm = localtime(t);

    return tm ? asctime(tm) : NULL;
}

/* appends s to buf, 0 if it does not fit */
static int add(char *buf, DWORD max, DWORD *n, const char *s)
{
    DWORD len = strlen(s);

    if (*n + len >= max)
        return 0;
    crt_copy(buf + *n, s, len);
    *n += len;
    buf[*n] = 0;
    return 1;
}

/* appends the first three characters of s */
static int add3(char *buf, DWORD max, DWORD *n, const char *s)
{
    if (*n + 3 >= max)
        return 0;
    crt_copy(buf + *n, s, 3);
    *n += 3;
    buf[*n] = 0;
    return 1;
}

static int num(char *buf, DWORD max, DWORD *n, int v, int width)
{
    char t[12];
    int i;

    for (i = width - 1; i >= 0; i--)
    {
        t[i] = (char)('0' + v % 10);
        v /= 10;
    }
    t[width] = 0;
    return add(buf, max, n, t);
}

static int format(char *buf, DWORD max, DWORD *n, char f,
                  const Tm * tm)
{
    int h12 = tm->tm_hour % 12 ? tm->tm_hour % 12 : 12;

    switch (f)
    {
    case 'a':
        return add3(buf, max, n, wdays[tm->tm_wday % 7]);
    case 'A':
        return add(buf, max, n, wdays[tm->tm_wday % 7]);
    case 'b':
    case 'h':
        return add3(buf, max, n, months[tm->tm_mon % 12]);
    case 'B':
        return add(buf, max, n, months[tm->tm_mon % 12]);
    case 'c':
        return format(buf, max, n, 'x', tm) && add(buf, max, n,
            " ") &&
               format(buf, max, n, 'X', tm);
    case 'd':
        return num(buf, max, n, tm->tm_mday, 2);
    case 'H':
        return num(buf, max, n, tm->tm_hour, 2);
    case 'I':
        return num(buf, max, n, h12, 2);
    case 'j':
        return num(buf, max, n, tm->tm_yday + 1, 3);
    case 'm':
        return num(buf, max, n, tm->tm_mon + 1, 2);
    case 'M':
        return num(buf, max, n, tm->tm_min, 2);
    case 'p':
        return add(buf, max, n, tm->tm_hour < 12 ? "AM" : "PM");
    case 'S':
        return num(buf, max, n, tm->tm_sec, 2);
    case 'U':
        return num(buf, max, n,
                   (tm->tm_yday + 7 - tm->tm_wday) / 7, 2);
    case 'w':
        return num(buf, max, n, tm->tm_wday, 1);
    case 'W':
        return num(buf, max, n,
                   (tm->tm_yday + 7 - (tm->tm_wday + 6) % 7) / 7,
                       2);
    case 'x':
        return num(buf, max, n, tm->tm_mon + 1, 2) &&
               add(buf, max, n, "/") &&
               num(buf, max, n, tm->tm_mday, 2) &&
               add(buf, max, n, "/") &&
               num(buf, max, n, tm->tm_year, 2);
    case 'X':
        return num(buf, max, n, tm->tm_hour, 2) &&
               add(buf, max, n, ":") &&
               num(buf, max, n, tm->tm_min, 2) &&
               add(buf, max, n, ":") &&
               num(buf, max, n, tm->tm_sec, 2);
    case 'y':
        return num(buf, max, n, tm->tm_year, 2);
    case 'Y':
        return num(buf, max, n, tm->tm_year + 1900, 4);
    case 'Z':
        return 1;
    case '%':
        return add(buf, max, n, "%");
    }
    return 1;
}

DWORD __cdecl strftime(char *buf, DWORD max, const char *fmt,
                       const Tm * tm)
{
    DWORD n = 0;
    char one[2];

    if (max == 0)
        return 0;
    buf[0] = 0;
    for (; *fmt; fmt++)
    {
        if (*fmt != '%' || fmt[1] == 0)
        {
            one[0] = *fmt;
            one[1] = 0;
            if (!add(buf, max, &n, one))
                return 0;
            continue;
        }
        fmt++;
        if (!format(buf, max, &n, *fmt, tm))
            return 0;
    }
    return n;
}

char *__cdecl _strdate(char *buf)
{
    Tm tm;
    time_t t = time(NULL);

    fill(t, &tm);
    two(buf, tm.tm_mon + 1);
    buf[2] = '/';
    two(buf + 3, tm.tm_mday);
    buf[5] = '/';
    two(buf + 6, tm.tm_year);
    buf[8] = 0;
    return buf;
}

char *__cdecl _strtime(char *buf)
{
    Tm tm;
    time_t t = time(NULL);

    fill(t, &tm);
    two(buf, tm.tm_hour);
    buf[2] = ':';
    two(buf + 3, tm.tm_min);
    buf[5] = ':';
    two(buf + 6, tm.tm_sec);
    buf[8] = 0;
    return buf;
}

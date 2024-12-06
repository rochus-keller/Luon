/*
* Copyright 2021-2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon runtime library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// adopted from Oberon+ ObSdl.obx and ObFiles.c

#include <SDL2/SDL.h>
#include <assert.h>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

typedef unsigned char BOOL;
enum {
    BLACK = 0x000000,
    WHITE = 0xFFFFFF,
    QueueLen = 100
};

static SDL_Window* window = 0;
static SDL_Texture* texture = 0;
static SDL_Renderer* renderer = 0;
static uint8_t* buffer = 0;
static uint8_t pixelBuf[2000*2000];
static uint16_t queue[QueueLen];
static int head = 0, tail = 0, count = 0;
static int sleepTime = 0, x = 0, y = 0, lastUpdate = 0;
static int WIDTH, HEIGHT;
static void (*idler)() = 0;
static uint32_t lastEvent = 0;
static int shiftDown = 0;
static int ctrlDown = 0;
static int capsLockDown = 0;
static const uint32_t msPerFrame = 30; // 20ms according to BB

DllExport void PAL2_setIdle(void (*tick)() )
{
    idler = tick;
}

static void disposeWindow()
{
    if( texture != 0 )
        SDL_DestroyTexture(texture);
    if( renderer != 0)
        SDL_DestroyRenderer(renderer);
    if( window != 0)
        SDL_DestroyWindow(window);
    window = 0;
    renderer = 0;
    texture = 0;
    buffer = 0;
}

static void time_init();
DllExport int32_t PAL2_getTime();

DllExport int PAL2_init(uint8_t* b, int w, int h)
{
    SDL_version v;
    SDL_GetVersion(&v);
    SDL_Log("Loaded SDL version %d.%d.%d\n", v.major, v.minor, v.patch );
    disposeWindow();
    time_init();
    WIDTH = w;
    HEIGHT = h;
    window = SDL_CreateWindow("Luon PAL on SDL",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              WIDTH,
                              HEIGHT,
                              SDL_WINDOW_SHOWN);
    if( window == 0 )
    {
        SDL_Log("There was an issue creating the window. %s",SDL_GetError());
        return 0;
    }

    renderer = SDL_CreateRenderer(window,
                                  -1,
                                  SDL_RENDERER_ACCELERATED |
                                  SDL_RENDERER_PRESENTVSYNC);
    if( renderer == 0 )
    {
        SDL_Log("There was an issue creating the renderer. %s",SDL_GetError());
        disposeWindow();
        return 0;
    }

    texture = SDL_CreateTexture(renderer,
                                SDL_PIXELFORMAT_ARGB8888,
                                SDL_TEXTUREACCESS_STREAMING,
                                WIDTH,
                                HEIGHT);
    if( texture == 0)
    {
        SDL_Log("There was an issue creating the texture. %s",SDL_GetError());
        disposeWindow();
        return 0;
    }

    buffer = b;
    return 1;
}

DllExport int PAL2_deinit()
{
    disposeWindow();
    buffer = 0;
    return 0;
}

DllExport int PAL2_setCursorBitmap(uint8_t* b, int w, int h)
{
    // TODO SDL_SetCursor & SDL_CreateCursor
    return 0;
}

DllExport void PAL2_setCursorPos(int x, int y)
{
    SDL_WarpMouseInWindow(window, x, y);
}

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static void enqueue( uint16_t c)
{
    if(count == QueueLen)
    {
        printf("buffer overflow");
        return;
    }
    count++;
    queue[head] = c;
    head = (head + 1) % QueueLen;
}

static uint16_t dequeue()
{
    uint16_t res = 0;
    if(count == 0)
        return 0;
    count--;
    res = queue[tail];
    tail = (tail + 1) % QueueLen;
    return res;
}

static uint16_t compose( uint8_t t, uint16_t p )
{
    return t << 12 | p;
}

typedef enum EventType {
    DeltaTime = 0,
    XLocation = 1,
    YLocation = 2,
    BiStateOn = 3,
    BiStateOff = 4,
    AbsoluteTime = 5, // followed by 2 words
} EventType;

typedef enum MousButton { LeftButton = 130,
                          MidButton = 128, // BB error, mixed up 129 and 128, VIM fixed
                          RightButton = 129
                        } MousButton;

enum { MaxPos = 0xfff }; // 12 bits

static void notify()
{
    // TODO
}

static int postEvent(EventType t, uint16_t param, int withTime )
{
    assert( t >= XLocation && t <= BiStateOff );

    if( withTime )
    {
        uint32_t time = PAL2_getTime();
        uint32_t diff = time - lastEvent;
        lastEvent = time;

        if( diff <= MaxPos )
        {
            enqueue( compose( DeltaTime, diff ) );
            notify();
        }else
        {
            enqueue( compose( AbsoluteTime, 0 ) );
            notify();
            enqueue( ( time >> 16 ) & 0xffff);
            notify();
            enqueue( time & 0xffff );
            notify();
        }
    }
    enqueue( compose( t, param ) );
    notify();
    return 1;
}

static char toAltoUpper( char ch )
{
    switch( ch )
    {
    case '+':
        return '='; // means: the key is labeled with '=' for normal press and '+' for shift press
                    // if we want a '+' to appear we have to send shift-down '=' shift-up to the VM
    case '_':
        return '-';
    case '|':
        return '\\';
    case '{':
        return '[';
    case '}':
        return ']';
    case ':':
        return ';';
    case '"':
        return '\'';
    case '<':
        return ',';
    case '>':
        return '.';
    case '?':
        return '/';
    case '!':
        return '1';
    case '@':
        return '2';
    case '#':
        return '3';
    case '$':
        return '4';
    case '%':
        return '5';
    case '~':
        return '6';
    case '&':
        return '7';
    case '*':
        return '8';
    case '(':
        return '9';
    case ')':
        return '0';
    }
    if( ch >= 'A' && ch <= 'Z' )
        return tolower(ch);
    return 0;
}

static int isAltoLower(char ch )
{
    if( ( ch >= 'a' && ch <= 'z' ) || ( ch >= '0' && ch <= '9' ) )
        return 1;
    switch( ch )
    {
    case '-':
    case '=':
    case '\\':
    case '[':
    case ']':
    case ';':
    case '\'':
    case ',':
    case '.':
    case '/':
        return 1;
    }
    return 0;
}

static void sendShift(int keyPress, int shiftRequired)
{
    if( shiftRequired && !shiftDown ) // need to press shift
        postEvent( keyPress ? BiStateOn : BiStateOff, 136, 1 );
    else if( !shiftRequired && shiftDown ) // need to release shift
        postEvent( !keyPress ? BiStateOn : BiStateOff, 136, 1 );
}

static int keyEvent(int keyCode, char ch, int down)
{
    switch( keyCode )
    {
    case SDLK_BACKSPACE:
        return postEvent( down ? BiStateOn : BiStateOff, 8, 1 );
    case SDLK_TAB:
        return postEvent( down ? BiStateOn : BiStateOff, 9, 1 );
        // NOTE: line feed	10 not supported
    case SDLK_RETURN:
        return postEvent( down ? BiStateOn : BiStateOff, 13, 1 );
    case SDLK_ESCAPE:
        return postEvent( down ? BiStateOn : BiStateOff, 27, 1 );
    case SDLK_SPACE:
        return postEvent( down ? BiStateOn : BiStateOff, 32, 1 );
    case SDLK_DELETE:
        return postEvent( down ? BiStateOn : BiStateOff, 127, 1 );
        // NOTE: right shift	137
    case SDLK_LSHIFT:
    case SDLK_RSHIFT:
        shiftDown = down;
        return postEvent( down ? BiStateOn : BiStateOff, 136, 1);
    case SDLK_LCTRL:
    case SDLK_RCTRL:
        ctrlDown = down;
        return postEvent( down ? BiStateOn : BiStateOff, 138, 1 );
    case SDLK_CAPSLOCK:
        capsLockDown = down;
        return postEvent( down ? BiStateOn : BiStateOff, 139, 1 );
    case SDLK_LEFT:
        // ← ASCII 95 0x5f _
        return postEvent( down ? BiStateOn : BiStateOff, 95, 1 );
    case SDLK_UP:
        // ↑ ASCII 94 0x5e ^
        return postEvent( down ? BiStateOn : BiStateOff, 94, 1 );
    }
    if( ch >= '!' && ch <= '~' )
    {
        if( isAltoLower( ch ) )
        {
            if( down )
                sendShift( 1, 0 );
            const int res = postEvent( down ? BiStateOn : BiStateOff, ch, 1 );
            if( !down )
                sendShift( 0, 0 );
            return res;
        }else if( ( ch = toAltoUpper( ch ) ) )
        {
            if( down )
                sendShift( 1, 1 );
            const int res = postEvent( down ? BiStateOn : BiStateOff, ch, 1 );
            if( !down )
                sendShift( 0, 1 );
            return res;
        }
    }
    return 0;
}

static int decodeUtf8char( char* encoded )
{
    // https://rosettacode.org/wiki/UTF-8_encode_and_decode#PureBasic
    int c = (unsigned char)encoded[0];
    if( c <= 0x7f ) // 01111111
        return c;
    if( c >= 0xc0 && c <= 0xdf ) // 11000000..11011111
        return ((c & 0x1f) << 6) | ( (unsigned char)encoded[1] & 0x3f);
    return 0;
}

static void updateTexture()
{
    SDL_Rect r;
    r.x = 0;
    r.y = 0;
    r.w = WIDTH;
    r.h = HEIGHT;

    // TODO: partial update
    const int PixPerWord = 16;
    const int pixLineWidth = ( ( WIDTH + PixPerWord - 1 ) / PixPerWord ) * PixPerWord; // line width is a multiple of 16
    const int sw = pixLineWidth / 8;
    const int dw = WIDTH * 4; // four bytes per pixel in SDL_PIXELFORMAT_ARGB8888
    const uint8_t *src_data = buffer;
    uint8_t *dest_data = pixelBuf;
    const int ax = 0;
    const int aw = WIDTH;
    const int axaw = ax + aw;
    const int ah = HEIGHT;
    const int ay = 0;

    src_data += sw * ay;
    dest_data += dw * ay;
    int x, y;
    for( y = 0; y < ah; y++ )
    {
        uint*p = (uint*)dest_data;
        p += ax;
        for( x = ax; x < axaw; x++ )
            *p++ = ((src_data[x>>3] >> (7 - (x & 7))) & 1) ? 0xff000000 : 0xffffffff;
        src_data += sw;
        dest_data += dw;
    }

    SDL_UpdateTexture(texture, &r, pixelBuf, r.w * 4);
}

static void mousePressReleaseImp(int press, int button)
{
    const EventType t = press ? BiStateOn : BiStateOff;

    switch( button )
    {
    case SDL_BUTTON_LEFT:
        if( ctrlDown == 0 && shiftDown == 1 )
            postEvent( t, LeftButton, 1 );
        else if( ctrlDown)
            postEvent( t, RightButton, 1 );
        else if( ctrlDown || shiftDown )
            postEvent( t, MidButton, 1 );
        break;
    case SDL_BUTTON_RIGHT:
        if( shiftDown )
            postEvent( t, MidButton, 1 );
        else
            postEvent( t, RightButton, 1 );
        break;
    case SDL_BUTTON_MIDDLE:
        postEvent( t, MidButton, 1 );
        break;
    default:
        break;
    }
}

DllExport int PAL2_processEvents(int sleep)
{
    SDL_Event e;
    BOOL  down;
    int time;
    SDL_Rect r;

    if( window == 0 )
        return 1;

    sleepTime = sleep;
    down = 0;
    if(SDL_WaitEventTimeout(&e,sleep) == 1)
    {
        switch( e.type)
        {
        case SDL_QUIT:
        case SDL_APP_TERMINATING:
            return 1;
        case SDL_WINDOWEVENT:
            if( e.window.event == SDL_WINDOWEVENT_CLOSE )
                return 1;
            break;
        case SDL_MOUSEMOTION: {
                const int dx = e.motion.x - x;
                const int dy = e.motion.y - y;
                x = e.motion.x;
                y = e.motion.y;
                if( (x >= 0 && x < WIDTH ) || ( y >= 0 && y < HEIGHT) )
                    SDL_ShowCursor( SDL_DISABLE );
                else
                    SDL_ShowCursor( SDL_ENABLE );

                x = MAX(x, 0);
                x = MIN(x, WIDTH-1);
                y = MAX(y, 0);
                y = MIN(y, HEIGHT-1);
                const uint32_t diff = PAL2_getTime() - lastEvent;
                if( diff >= msPerFrame && dx )
                    postEvent( XLocation, x, 1 );
                if( diff >= msPerFrame && dy )
                    postEvent( YLocation, y, 1 );
                break;
            }
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP: {
                down = (e.button.state == SDL_PRESSED);
                mousePressReleaseImp(down, e.button.button);
                break;
            }
        case SDL_TEXTINPUT:
            keyEvent(0, decodeUtf8char(e.text.text), 1);
            break;
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            down = (e.key.state == SDL_PRESSED);
            if( e.key.keysym.sym == SDLK_q && down && (e.key.keysym.mod & KMOD_LCTRL) )
                return 1;
            else
                keyEvent(e.key.keysym.sym, 0, 1);
            break;
        }
    }
    time = SDL_GetTicks();
    if( ( time - lastUpdate ) > 30 ) // 20 good for runtime, too slow for debugger
    {
        lastUpdate = time;
        updateTexture();
        SDL_RenderClear(renderer);
        r.x = 0;
        r.y = 0;
        r.w = WIDTH;
        r.h = HEIGHT;
        SDL_RenderCopy(renderer, texture, &r, &r);
        SDL_RenderPresent(renderer);
        if( idler )
            idler();
    }
    return 0;
}

DllExport int PAL2_nextEvent()
{
    return dequeue();
}


///////////////////////////////////////////////////////////////////////////////////////////

#include <time.h>

#if defined(_WIN32) && !defined(__GNUC__)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <direct.h>

// Source: https://stackoverflow.com/questions/10905892/equivalent-of-gettimeday-for-windows/26085827

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#else
#include <sys/time.h>
#include <unistd.h>
#endif

static struct timeval start;

DllExport int32_t PAL2_getTime()
{
    static struct timeval now;
    gettimeofday(&now, 0);
    const long seconds = now.tv_sec - start.tv_sec;
    const long microseconds = now.tv_usec - start.tv_usec;
    return seconds*1000000 + microseconds;
}

static void time_init()
{
    gettimeofday(&start, 0);
}



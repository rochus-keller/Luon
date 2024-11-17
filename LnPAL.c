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
static unsigned int* buffer = 0;
static int length = 0;
static int pixelBuf[1000*1000];
static char queue[QueueLen];
static int head = 0, tail = 0, count = 0;
static int sleepTime = 0, x = 0, y = 0, lastUpdate = 0;
static BOOL shift = 0, ctrl = 0, left = 0, mid = 0, right = 0;
static int WIDTH, HEIGHT;
static void (*idler)() = 0;

DllExport void PAL_setIdle(void (*tick)() )
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

DllExport int PAL_init(unsigned int* b, int l, int w, int h)
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
    length = l;
    return 1;
}

DllExport int PAL_deinit()
{
    disposeWindow();
    buffer = 0;
    length = 0;
    return 0;
}

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static void enqueue( char c)
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

static char dequeue()
{
    char res = 0;
    if(count == 0)
        return 0;
    count--;
    res = queue[tail];
    tail = (tail + 1) % QueueLen;
    return res;
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
    unsigned int out_idx, b, pixels, line_start;
    int line, col;

    r.x = 0;
    r.y = 0;
    r.w = WIDTH;
    r.h = HEIGHT;
    out_idx = 0;
    for(line = HEIGHT-1; line >= 0; line--)
    {
        line_start = line * (WIDTH / 32);
        for( col = 0;  col < (WIDTH / 32); col++ )
        {
            pixels = buffer[line_start + col];
            for(b = 0; b < 32; b++ )
            {
                if( pixels & 1 )
                    pixelBuf[out_idx] = WHITE;
                else
                    pixelBuf[out_idx] = BLACK;
                pixels = pixels >> 1;
                out_idx++;
            }
        }
    }
    SDL_UpdateTexture(texture, &r, pixelBuf, r.w * 4);
}

DllExport int PAL_processEvents(int sleep)
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
                break;
            }
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP: {
                down = (e.button.state == SDL_PRESSED);
                switch(e.button.button)
                {
                case SDL_BUTTON_LEFT:
                    if( ctrl && shift )
                        right = down;
                    else if(ctrl)
                        mid = down;
                    else
                        left = down;
                    break;
                case SDL_BUTTON_MIDDLE:
                    mid = down;
                    break;
                case SDL_BUTTON_RIGHT:
                    if(ctrl)
                        mid = down;
                    else
                        right = down;
                    if(!down)
                    {
                        left = 0;
                        right = 0;
                        mid = 0;
                    }
                    break;
                }
            }
        case SDL_TEXTINPUT:
            enqueue(decodeUtf8char(e.text.text));
            break;
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            down = (e.key.state == SDL_PRESSED);
            switch(e.key.keysym.sym)
            {
            case SDLK_LCTRL:
                ctrl = down;
                break;
            case SDLK_LSHIFT:
                shift = down;
                break;
            case SDLK_q:
                if( down && (e.key.keysym.mod & KMOD_LCTRL) )
                    return 1;
                break;
            case SDLK_RETURN:
                if(down)
                    enqueue(0xd); // \r
                break;
            case SDLK_BACKSPACE:
                if(down)
                    enqueue(0x8); // \b
                break;
            case SDLK_TAB:
                if(down)
                    enqueue(0x9); // \t
            case SDLK_ESCAPE:
                if(down)
                    enqueue(0x1b);
            }
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

typedef struct InputState { unsigned int keys; int x; int y; } InputState;

DllExport void PAL_getState(InputState* state)
{
    PAL_processEvents(sleepTime);
    state->x = x;
    state->y = HEIGHT - y - 1;
    state->keys = 0;
    if(left)
        state->keys = state->keys | 4;
    if(mid)
        state->keys = state->keys | 2;
    if(right)
        state->keys = state->keys | 1;
}

DllExport char PAL_nextKey()
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

DllExport int32_t PAL_getTime()
{
    static struct timeval now;
    gettimeofday(&now, 0);
    const long seconds = now.tv_sec - start.tv_sec;
    const long microseconds = now.tv_usec - start.tv_usec;
    return seconds*1000000 + microseconds;
}

#if defined(_WIN32)
#define PATH_SEP '\\'
#else
#define PATH_SEP '/'
#endif

static void getPath( char* path, size_t size )
{
    const char* env = getenv("LUON_FILE_SYSTEM_ROOT");
    if( env )
    {
        strncpy( path, env, size );
        return;
    }
    getcwd(path, size);
    const size_t len = strlen(path);
    if( len + 1 + 5 + 1 > size )
    {
        *path = 0;
        return;
    }
    path[len] = PATH_SEP;
    strcpy(path+len+1,"Files");
}

static void getFilePath( char* filePath, size_t size, const char* fileName )
{
    getPath(filePath, size);
    const size_t len1 = strlen(filePath);
    const size_t len2 = strlen(fileName);
    if( len1 == 0 )
        return;
    if( len1 + 1 + len2 + 1 > size )
    {
        *filePath = 0;
        return;
    }
    filePath[len1] = PATH_SEP;
    strcpy(filePath+len1+1,fileName);
}

enum { MAX_FILES = 1000 };
static char* s_files[MAX_FILES];
static int s_count = 0;

static void clearFiles()
{
    int i;
    for( i = 0; i < s_count; i++ )
    {
        free(s_files[i]);
    }
    s_count = 0;
}

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
// source: https://stackoverflow.com/questions/2314542/listing-directory-contents-using-c-and-windows
static void readDir()
{
    clearFiles();

    WIN32_FIND_DATA fdFile;
    HANDLE hFind = NULL;

    enum { LEN = 2048 };
    char sPath[LEN];
    getPath(sPath,LEN);
    if( *sPath == 0 )
        return;

    if((hFind = FindFirstFile(sPath, &fdFile)) == INVALID_HANDLE_VALUE)
    {
        fprintf(stderr,"path not found: %s\n", sPath);
        return;
    }

    do
    {
        //Find first file will always return "."
        //    and ".." as the first two directories.
        if(strcmp(fdFile.cFileName, ".") != 0
                && strcmp(fdFile.cFileName, "..") != 0
                && !(fdFile.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) )
        {
            const int len = strlen(fdFile.cFileName);
            s_files[s_count] = malloc(len+1);
            strcpy(s_files[s_count],fdFile.cFileName);
            s_count++;
        }
    }while(FindNextFile(hFind, &fdFile)); //Find the next file.

    FindClose(hFind); //Always, Always, clean things up!
}
#else
#include <dirent.h>

static void readDir()
{
    clearFiles();

    enum { LEN = 300 };
    char path[LEN];
    getPath(path,LEN);
    if( *path == 0 )
        return;
    DIR *d;
    struct dirent *dir;
    d = opendir(path);
    if( d )
    {
        while( ( dir = readdir(d) ) != NULL && s_count < MAX_FILES)
        {
            if( dir->d_type == 8 /* DT_REG */ )
            {
                const int len = strlen(dir->d_name);
                s_files[s_count] = malloc(len+1);
                strcpy(s_files[s_count],dir->d_name);
                s_count++;
            }
        }
        closedir(d);
    }
}
#endif

DllExport int32_t PAL_listFiles()
{
    readDir();
    return s_count;
}

DllExport void PAL_fileName(int32_t i, char* name, int len)
{
    if( i < s_count )
        strncpy(name, s_files[i], len );
    else
        *name = 0;
}

static FILE* s_buffers[MAX_FILES] = {0};

static int32_t nextFreeBuffer()
{
    int i;
    for( i = 0; i < MAX_FILES; i++ )
    {
        if( s_buffers[i] == 0 )
        {
            return i;
        }
    }
    return -1;
}

DllExport int32_t PAL_openFile(const char* filename)
{
    if( filename == 0 || *filename == 0 )
        return -1;

    char path[300];
    getFilePath(path,300,filename);
    if( *path == 0 )
        return -1;

    FILE* old = fopen(path, "rb"); // must be rb, not just r, otherwise fseek fails on Windows
    if( old == 0 )
    {
        fprintf( stderr, "cannot open file for reading: %s\n", path );
        return -1;
    }

    const int32_t buf = nextFreeBuffer();
    if( buf < 0 )
    {
        fprintf( stderr, "cannot open more than %d files\n", MAX_FILES );
        return -1;
    }

    FILE* tmp = tmpfile();
    if( tmp == 0 )
    {
        fprintf( stderr, "cannot create temporary file for %s\n", filename );
        fclose(old);
        return -1;
    }

    s_buffers[buf] = tmp;

    int ch;
    while ( (ch = fgetc(old)) != EOF )
    {
        fputc(ch, tmp);
    }
    fclose(old);

    fseek(tmp, 0, SEEK_SET );
    return buf;
}

DllExport int32_t PAL_newFile()
{
    const int32_t buf = nextFreeBuffer();
    if( buf < 0 )
    {
        fprintf( stderr, "cannot open more than %d files\n", MAX_FILES );
        return -1;
    }

    FILE* tmp = tmpfile();
    if( tmp == 0 )
    {
        fprintf( stderr, "cannot create temporary file\n");
        return -1;
    }
    s_buffers[buf] = tmp;

    fseek(tmp, 0, SEEK_SET );
    return buf;
}

DllExport void PAL_freeFile(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        fclose(s_buffers[buffer]);
        s_buffers[buffer] = 0;
    }
}

DllExport int PAL_saveFile(const char* filename, int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] && filename != 0 && *filename != 0 )
    {
        char path[300];
        getFilePath(path,300,filename);
        if( *path == 0 )
            return 0;

        FILE* file = fopen(path, "wb");
        if( file == 0 )
        {
            fprintf( stderr, "cannot open file for writing: %s\n", path );
            return 0;
        }
        fseek(s_buffers[buffer], 0, SEEK_SET );
        int ch;
        while ( (ch = fgetc(s_buffers[buffer])) != EOF )
        {
            fputc(ch, file);
        }
        fclose(file);
        return 1;
    }
    return 0;
}

DllExport int PAL_removeFile(const char* filename)
{
    char path[300];
    getFilePath(path,300,filename);
    if( *path == 0 )
        return 0;

    const int res = remove(filename);
    return res == 0;
}

DllExport int PAL_renameFile(const char* oldName, const char* newName)
{
    char on[300];
    getFilePath(on,300,oldName);
    if( *on == 0 )
        return 0;
    char nn[300];
    getFilePath(nn,300,newName);
    if( *nn == 0 )
        return 0;

    const int res = rename(on, nn);
    return res == 0;
}

DllExport int32_t PAL_length(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        const int old = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], 0L, SEEK_END);
        const int pos = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], old, SEEK_SET );
        return pos;
    }
    return 0;
}

DllExport int PAL_setPos(int32_t buffer, int32_t pos)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        fseek(s_buffers[buffer], pos, SEEK_SET );
        return 1;
    }
    return 0;
}

DllExport int32_t PAL_getPos(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        return ftell(s_buffers[buffer]);
    }
    return 0;
}

DllExport int PAL_atEnd(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        const int cur = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], 0L, SEEK_END);
        const int end = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], cur, SEEK_SET );
        return cur >= end;
    }
    return 0;
}

DllExport int PAL_writeByte(int32_t buffer, int32_t byte_)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        putc(byte_,s_buffers[buffer]);
        return 1;
    }
    return 0;
}

DllExport int32_t PAL_readByte(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        return getc(s_buffers[buffer]);
    }
    return 0;
}

static void time_init()
{
    gettimeofday(&start, 0);
}



/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

// adopted from Oberon+

#include <stdint.h>
#include <assert.h>
#include <stdio.h>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

static void copystr( char* to, int len, const char* from )
{
#if 0
    QByteArray tmp = QString::fromUtf8(from).toLatin1();
    memcpy(to,tmp.constData(),qMin(len-1,tmp.size()));
    to[len-1] = 0;
#endif
}

static void printstring( const char* str )
{
    printf("%s\n", str);
}

DllExport int LuonFfi_DIV( int64_t a, int64_t b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if( a < 0 )
        return (a - b + 1) / b;
    else
        return a / b;
}

DllExport int LuonFfi_MOD( int64_t a, int64_t b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if (a < 0)
        return (b - 1) + (a - b + 1) % b;
    else
        return a % b;
}

typedef uint8_t CharArray[];


DllExport void LuonFfi_initString( CharArray data, int count, const char* utf8 ) // obsolete
{
    copystr((char*)data,count,utf8);
}

static int latin1_strcmp(const unsigned char *s1, const unsigned char *s2)
{
    while( *s1 && *s2 )
    {
        if( *s1 != *s2 )
        {
            return (*s1 > *s2) ? 1 : -1;
        }
        s1++;
        s2++;
    }

    if( *s1 )
        return 1;
    if( *s2 )
        return -1;
    return 0;
}

DllExport int LuonFfi_strRelOp( unsigned char* lhs, unsigned char* rhs, int op )
{
    const int res = latin1_strcmp(lhs,rhs);

    switch( op )
    {
    case 1: // EQ
        return res == 0;
    case 2: // NEQ
        return res != 0;
    case 3: // LT
        return res < 0;
    case 4: // LEQ
        return res <= 0;
    case 5: // GT
        return res > 0;
    case 6: // GEQ
        return res >= 0;
    }
    return 0;
}

DllExport void LuonFfi_printString( const char* str ) // obsolete
{
    printstring(str);
}

DllExport void LuonFfi_DBGTRACE( const char* str )
{
#if 0
    static QFile log( QDir::home().absoluteFilePath("trace.log") );
    if( !log.isOpen() )
        log.open(QIODevice::Append );
    log.write(str);
    log.write("\n");
    log.flush();
#endif
}

DllExport void LuonFfi_TRACE( const char* str )
{
    //qDebug() << "TRACE:" << str;
}

DllExport void LuonFfi_NOP()
{
    //qDebug() << "NOP";

    // works, but ends pcall with error:
    //lua_pushstring(Lua::Engine2::getInst()->getCtx(),"hello from nop");
    //lua_error(Lua::Engine2::getInst()->getCtx());
}

DllExport void LuonFfi_CRASH(int x)
{
    *(int*)0 = 0;
}

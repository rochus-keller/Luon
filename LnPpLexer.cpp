/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Luon language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// adopted from FreePascal and merged with preprocessing from Oberon+ parser

#include "LnPpLexer.h"
#include "LnTokenType.h"
#include <QBuffer>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QtDebug>
using namespace Ln;


PpLexer::PpLexer():d_sloc(0)
{
}

PpLexer::~PpLexer()
{
}

bool PpLexer::reset(const QByteArrayList& options)
{
    d_buffer.clear();
    d_sloc = 0;
    d_options.clear();
    foreach( const QByteArray& o, options )
        d_options[ Token::getSymbol(o).constData() ] = true;
    return true;
}

void PpLexer::setStream(QIODevice*d, const QString& sourcePath, const QDateTime& ts)
{
    d_lex.setStream(d, sourcePath, ts);
}

bool PpLexer::setStream(const QString& sourcePath)
{
    d_lex.setStream(sourcePath);
    return true;
}

Token PpLexer::nextToken()
{
    Token t;
    if( !d_buffer.isEmpty() )
    {
        t = d_buffer.first();
        d_buffer.pop_front();
    }else
        t = nextTokenImp();
    Q_ASSERT( t.d_type != Tok_LtStar && t.d_type != Tok_Comment );
    return t;
}

Token PpLexer::peekToken(quint8 lookAhead)
{
    Q_ASSERT( lookAhead > 0 );
    while( d_buffer.size() < lookAhead )
    {
        Token t = nextTokenImp();
        Q_ASSERT( t.d_type != Tok_Comment && t.d_type != Tok_Comment );
        d_buffer.push_back( t );
    }
    return d_buffer[ lookAhead - 1 ];
}

void PpLexer::ppcmd()
{
    Token t = d_lex.peekToken();
    switch(t.d_type)
    {
    case Tok_ident:
        {
            Token name = d_lex.nextToken();
            t = d_lex.nextToken();
            switch( t.d_type )
            {
            case Tok_Plus:
                d_options[name.d_val.constData()] = true;
                t = d_lex.nextToken();
                break;
            case Tok_Minus:
                d_options[name.d_val.constData()] = false;
                t = d_lex.nextToken();
                break;
            default:
                raise(name, "expecting '+' or '-' after identifier" );
                while( t.d_type != Tok_StarGt && t.d_type != Tok_Eof )
                    t = d_lex.nextToken();
                break;
            }
        }
        break;
    case Tok_IF:
        d_lex.nextToken();
        {
            const bool cond = ppexpr();
            d_conditionStack.append( ppstatus(false) );
            ppsetthis( ppouter().open && cond );
            t = d_lex.nextToken();
            if( t.d_type != Tok_THEN )
                raise( t, "expecting 'THEN'" );
            else
                t = d_lex.nextToken();
        }
        break;
    case Tok_ELSIF:
        d_lex.nextToken();
        if( ppthis().elseSeen || d_conditionStack.isEmpty() )
        {
            raise(t, "ELSIF directive not expected here");
            while( t.d_type != Tok_StarGt && t.d_type != Tok_Eof )
                t = d_lex.nextToken();
        }else
        {
            const bool cond = ppexpr();
            ppsetthis( ppouter().open && cond && !ppthis().openSeen );
            t = d_lex.nextToken();
            if( t.d_type != Tok_THEN )
                raise( t, "expecting 'THEN'" );
            else
                t = d_lex.nextToken();
        }
        break;
    case Tok_ELSE:
        d_lex.nextToken();
        if( ppthis().elseSeen || d_conditionStack.isEmpty() )
        {
            raise(t, "ELSE directive not expected here");
            while( t.d_type != Tok_StarGt && t.d_type != Tok_Eof )
                t = d_lex.nextToken();
        }else
        {
            ppsetthis( ppouter().open && !ppthis().openSeen, true );
            t = d_lex.nextToken();
        }
        break;
    case Tok_END:
        d_lex.nextToken();
        if( d_conditionStack.isEmpty() )
            raise(t, "spurious END directive");
        else
            d_conditionStack.pop_back();
        t = d_lex.nextToken();
        break;
    }
    if( t.d_type != Tok_StarGt )
    {
        raise(t, "expecting '*>'" );
        while( t.d_type != Tok_StarGt && t.d_type != Tok_Eof )
            t = d_lex.nextToken();
    }
}

bool PpLexer::ppexpr()
{
    bool res = ppterm();
    Token t = d_lex.peekToken();
    while( t.d_type == Tok_OR ) // check all, otherwise not all tokens are eaten
    {
        t = d_lex.nextToken();
        res = ppterm() || res; // order significant
        t = d_lex.peekToken();
    }
    return res;
}

bool PpLexer::ppterm()
{
    bool res = ppfactor();
    Token t = d_lex.peekToken();
    while( t.d_type == Tok_Amp || t.d_type == Tok_AND )
    {
        t = d_lex.nextToken();
        res = ppfactor() && res;
        t = d_lex.peekToken();
    }
    return res;
}

bool PpLexer::ppfactor()
{
    Token t = d_lex.nextToken();
    switch( t.d_type )
    {
    case Tok_ident:
        return d_options.value(t.d_val.constData());
    case Tok_Lpar:
        {
            const bool res = ppexpr();
            t = d_lex.nextToken();
            if( t.d_type != Tok_Rpar )
                error( t, "expecting ')'");
            return res;
        }
    case Tok_Tilde:
    case Tok_NOT:
        return !ppfactor();
    }
    return false;
}

Token PpLexer::nextTokenImp()
{
    Token t = d_lex.peekToken();
    while( t.d_type == Tok_LtStar )
    {
        while( t.d_type == Tok_LtStar )
        {
            const Token start = d_lex.nextToken();
            t = d_lex.peekToken();
            if( t.d_type != Tok_StarGt && t.d_type != Tok_Eof )
            {
                try{ ppcmd(); }
                catch(...){ return d_err; }
            }

            if( t.d_type == Tok_Eof )
                return error( start, "non-terminated source code directive" );
            else
                t = d_lex.peekToken();
        }

        if( !ppthis().open )
        {
            t = d_lex.peekToken();
            while( t.d_type != Tok_LtStar && t.d_type != Tok_Eof )
            {
                t = d_lex.nextToken();
                t = d_lex.peekToken();
            }
        }
    }
    t = d_lex.nextToken();
    if( t.d_type == Tok_Eof && !d_conditionStack.isEmpty() )
        return error( t, "expecting END directive" );
    return t;
}

Token PpLexer::error(const Token& t, const QByteArray& msg)
{
    d_err = t;
    d_err.d_type = Tok_Invalid;
    d_err.d_val = msg;
    return d_err;
}

void PpLexer::raise(const Token& t, const QByteArray& msg)
{
    error(t,msg);
    throw "";
}


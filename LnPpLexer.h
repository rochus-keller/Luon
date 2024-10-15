#ifndef PPLEXER_H
#define PPLEXER_H

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

#include "LnLexer.h"
#include "LnRowCol.h"
#include <QStringList>
#include <QHash>
#include <QLinkedList>

class QIODevice;

namespace Ln
{
class PpLexer
{
public:
    PpLexer();
    ~PpLexer();

    bool reset(const QByteArrayList& options);
    void setStream( QIODevice*, const QString& sourcePath, const QDateTime& ts = QDateTime() );
    bool setStream(const QString& sourcePath);

    Token nextToken();
    Token peekToken(quint8 lookAhead = 1);
    quint32 getSloc() const { return d_sloc; }
    QString getSource() const { return d_lex.getSource(); }
    const QHash<QString,Ranges>& getMutes() const { return d_mutes; }
protected:
    Token nextTokenImp();
    Token error(const Token& t, const QByteArray& msg);
    void raise(const Token& t, const QByteArray& msg);
    void ppcmd();
    bool ppexpr();
    bool ppterm();
    bool ppfactor();

    struct ppstatus
    {
        bool open; // this is the open condition which renders tokens
        bool openSeen; // at least one true condition seen
        bool elseSeen; // there was already an else part
        ppstatus(bool o = true):open(o),openSeen(false),elseSeen(false){}
    };

    ppstatus ppouter()
    {
        ppstatus res;
        if( d_conditionStack.size() >= 2 )
            res = d_conditionStack[d_conditionStack.size()-2];
        return res;
    }
    ppstatus ppthis()
    {
        ppstatus res;
        if( !d_conditionStack.isEmpty() )
            res = d_conditionStack.back();
        return res;
    }
    void ppsetthis(bool open, bool thisIsElse = false)
    {
        if( !d_conditionStack.isEmpty() )
        {
            ppstatus& stat = d_conditionStack.back();
            stat.open = open;
            if( thisIsElse )
                stat.elseSeen = true;
            if( open )
                stat.openSeen = true;
        }
    }
private:
    Lexer d_lex;
    QList<Token> d_buffer;
    Token d_err;
    quint32 d_sloc; // number of lines of code without empty or comment lines
    QList<ppstatus> d_conditionStack;
    QHash<const char*,bool> d_options;
    QHash<QString,Ranges> d_mutes;
    RowCol d_startMute;
};
}

#endif // PPLEXER_H

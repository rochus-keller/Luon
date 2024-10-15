#ifndef LUONLEXER_H
#define LUONLEXER_H

/*
* Copyright 2019-2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon language project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
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

// Adopted from Oberon+

#include <QObject>
#include <LnToken.h>
#include <QDateTime>

class QIODevice;

namespace Ln
{
    class Lexer : public QObject
    {
    public:
        explicit Lexer(QObject *parent = 0);

        void setStream( QIODevice*, const QString& sourcePath, const QDateTime& ts = QDateTime() );
        bool setStream(const QString& sourcePath);
        void setIgnoreComments( bool b ) { d_ignoreComments = b; }
        void setPackComments( bool b ) { d_packComments = b; }
        void setEnableExt( bool b ) { d_enableExt = b; }
        bool isEnabledExt() const { return d_enableExt; }
        void setSensExt( bool b ) { d_sensExt = b; }
        const QDateTime& getTimeStamp() const { return d_when; }

        Token nextToken();
        Token peekToken(quint8 lookAhead = 1);
        QList<Token> tokens( const QString& code );
        QList<Token> tokens( const QByteArray& code, const QString& path = QString() );
        quint32 getSloc() const { return d_sloc; }
        QString getSource() const { return d_sourcePath; }

        static void parseComment( const QByteArray& str, int& pos, int& level );

        static QByteArray extractText(QIODevice*); // recognizes Oberon file format and ASCII, returns Latin-1 UTF-8
        static bool skipBom( QIODevice* );

        static bool isUtf8(const QByteArray& str);
        static bool isAscii( const QByteArray& str );
        static bool isValidIdent( const QByteArray& str );
    protected:
        Token nextTokenImp();
        int skipWhiteSpace();
        void nextLine();
        int lookAhead(int off = 1) const;
        Token token(TokenType tt, int len = 1, const QByteArray &val = QByteArray());
        Token ident();
        Token number();
        Token comment();
        Token string();
        Token hexstring();
        bool isHexstring(int off = 1) const;
        void countLine();
    private:
        QIODevice* d_in;
        quint32 d_lineNr;
        quint16 d_colNr;
        QString d_sourcePath;
        QDateTime d_when;
        QByteArray d_line;
        QList<Token> d_buffer;
        quint32 d_sloc; // number of lines of code without empty or comment lines
        Token d_lastToken;
        bool d_ignoreComments;  // don't deliver comment tokens
        bool d_packComments;    // Only deliver one Tok_Comment for (*...*) instead of Tok_Latt and Tok_Ratt
        bool d_enableExt; // Allow for both uppercase and lowercase keywords and for idents with underscores as in C
        bool d_sensExt; // Autosense language extension (first keyword MODULE, module, DEFINITION, definition)
        bool d_sensed;
        bool d_lineCounted;
    };
}

#endif // LUONLEXER_H

/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QCoreApplication>
#include <QFile>
#include <QStringList>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QElapsedTimer>
#include <LnPpLexer.h>
#include <LnParser.h>
#include <LnParser2.h>
#include <LnSynTree.h>
using namespace Ln;

QStringList collectFiles( const QDir& dir, const QStringList& suffix )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ), suffix );

    files = dir.entryList( suffix, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append(dir.absoluteFilePath(f));
    }
    return res;
}

static QString root;

#if 0
static void dump(QTextStream& out, const SynTree* node, int level)
{
    QByteArray str;
    if( node->d_tok.d_type == Tok_Invalid )
        level--;
    else if( node->d_tok.d_type < SynTree::R_First )
    {
        if( tokenTypeIsKeyword( node->d_tok.d_type ) )
            str = tokenTypeString(node->d_tok.d_type);
        else if( node->d_tok.d_type > TT_Specials )
            str = QByteArray("\"") + node->d_tok.d_val + QByteArray("\"");
        else
            str = QByteArray("\"") + tokenTypeString(node->d_tok.d_type) + QByteArray("\"");

    }else
        str = SynTree::rToStr( node->d_tok.d_type );
    if( !str.isEmpty() )
    {
        str += QByteArray("\t") + QFileInfo(node->d_tok.d_sourcePath).baseName().toUtf8() +
                ":" + QByteArray::number(node->d_tok.d_lineNr) +
                ":" + QByteArray::number(node->d_tok.d_colNr);
        QByteArray ws;
        for( int i = 0; i < level; i++ )
            ws += "|  ";
        str = ws + str;
        out << str.data() << endl;
    }
    foreach( SynTree* sub, node->d_children )
        dump( out, sub, level + 1 );
}
#endif

class Lex : public Scanner
{
public:
    PpLexer lex;
    Token next()
    {
        return lex.nextToken();
    }

    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
};

class Lex2 : public Scanner2
{
public:
    PpLexer lex;
    Token next()
    {
        return lex.nextToken();
    }
    Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
    QString source() const { return lex.getSource(); }
};

static void checkParser(const QStringList& files)
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Lex lex;
        lex.lex.setStream(file);
        Parser p(&lex);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        p.RunParser();
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
            // break;
        }else
        {
            ok++;
            qDebug() << "ok";
        }
#if 0
        QFile out(file + ".st");
        out.open(QIODevice::WriteOnly);
        QTextStream s(&out);
        dump(s,&p.root,0);
#else
        QTextStream s(stdout);
        //dump(s,&p.root,0);
#endif
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << files.size() << "files" << "in" << timer.elapsed() << " [ms]";
}

static void checkParser2(const QStringList& files)
{
    int ok = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Lex2 lex;
        lex.lex.setStream(file);
        QFile out;
        out.open(stdout, QIODevice::WriteOnly);
        //IlAsmRenderer ar(&out);
        //MilEmitter e(&ar);
        AstModel mdl;
        Parser2 p(&mdl,&lex);
        qDebug() << "**** parsing" << file.mid(root.size()+1);
        p.RunParser(MetaActualList());
        if( !p.errors.isEmpty() )
        {
            foreach( const Parser2::Error& e, p.errors )
                qCritical() << e.path.mid(root.size()+1) << e.row << e.col << e.msg;
            // break;
        }else
        {
            ok++;
            qDebug() << "ok";
        }
    }
    qDebug() << "#### finished with" << ok << "files ok of total" << files.size() << "files" << "in" << timer.elapsed() << " [ms]";
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    if( a.arguments().size() <= 1 )
        return -1;

    QStringList files;
    QFileInfo info(a.arguments()[1]);
    if( info.isDir() )
    {
        files = collectFiles(info.filePath(), QStringList() << "*.luon" << "*.ln");
        root = info.filePath();
    }else
        files.append(info.filePath());
#if 0
    foreach( const QString& f, files )
    {
        Lex lex;
        lex.lex.setStream(f);
        Token t = lex.lex.nextToken();
        while( !t.isEof() )
        {
            qDebug() << t.getString() << t.d_val.constData();
            t = lex.lex.nextToken();
        }
    }
#else
    // checkParser(files);
    checkParser2(files);
#endif

    return 0;
}

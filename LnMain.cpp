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
#include <LnParser2.h>
#include <LnLexer.h>
#include <LnToken.h>
#include <LnValidator.h>
#include <QBuffer>
#include <QCommandLineParser>

class Lex2 : public Ln::Scanner2
{
public:
    QString sourcePath;
    Ln::PpLexer lex;
    Ln::Token next()
    {
        return lex.nextToken();
    }
    Ln::Token peek(int offset)
    {
        return lex.peekToken(offset);
    }
    QString source() const { return sourcePath; }
};

static QByteArray getModuleName(const QString& file)
{
    Ln::Lexer lex;
    lex.setStream(file);
    Ln::Token t = lex.nextToken();
    while( t.isValid() && t.d_tokenType != Ln::Tok_MODULE )
        t = lex.nextToken();
    if( t.d_tokenType == Ln::Tok_MODULE )
    {
        t = lex.nextToken();
        if( t.d_tokenType == Ln::Tok_ident )
            return t.d_val;
    }
    return QByteArray();
}

struct ModuleSlot
{
    Ln::Import imp;
    QString file;
    Ln::Declaration* decl;
    ModuleSlot():decl(0) {}
    ModuleSlot( const Ln::Import& i, const QString& f, Ln::Declaration* d):imp(i),file(f),decl(d){}
};

static bool operator==(const Ln::Import& lhs, const Ln::Import& rhs)
{
    if( lhs.path != rhs.path )
        return false;
    if( lhs.metaActuals.size() != rhs.metaActuals.size() )
        return false;
    for( int i = 0; i < lhs.metaActuals.size(); i++ )
    {
        if( lhs.metaActuals[i]->kind != rhs.metaActuals[i]->kind )
            return false;
        if( lhs.metaActuals[i]->type != rhs.metaActuals[i]->type )
            return false;
        if( lhs.metaActuals[i]->val != rhs.metaActuals[i]->val )
            return false;
   }
    return true;
}

class Manager : public Ln::Importer {
public:
    typedef QList<ModuleSlot> Modules;
    Modules modules;
    QList<QDir> searchPath;
    QString rootPath;

    Manager() {}
    ~Manager() {
        Modules::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
            Ln::Declaration::deleteAll((*i).decl);
    }

    ModuleSlot* find(const Ln::Import& imp)
    {
        for(int i = 0; i < modules.size(); i++ )
        {
            if( modules[i].imp == imp )
                return &modules[i];
        }
        return 0;
    }

    QByteArray modulePath( const QByteArrayList& path )
    {
        return path.join('$');
    }

    QByteArray moduleSuffix( const Ln::MetaActualList& ma )
    {
        // TODO: this is an intermediate solution assuming everything is built from sources in full everytime.
        if( ma.isEmpty() )
            return QByteArray();
        else
            return "$" + QByteArray::number(modules.size());
    }

    Ln::Declaration* loadModule( const Ln::Import& imp )
    {
        ModuleSlot* ms = find(imp);
        if( ms != 0 )
            return ms->decl;

        QString file = toFile(imp);
        if( file.isEmpty() )
        {
            qCritical() <<  "cannot find source file of module" << imp.path.join('.');
            modules.append(ModuleSlot(imp,QString(),0));
            return 0;
        }

        // immediately add it so that circular module refs lead to an error
        modules.append(ModuleSlot(imp,file,0));
        ms = &modules.back();

        Lex2 lex;
        lex.sourcePath = file; // to keep file name if invalid
        lex.lex.setStream(file);
        if( imp.metaActuals.isEmpty() )
            qDebug() << "**** compiling" << QFileInfo(file).fileName();
        else
            qDebug() << "**** instantiating" << QFileInfo(file).fileName();
        //Ln::MilEmitter e(&r);
        Ln::AstModel mdl;
        Ln::Parser2 p(&mdl,&lex);
        p.RunParser();
        Ln::Declaration* module = 0;
        if( !p.errors.isEmpty() )
        {
            foreach( const Ln::Parser2::Error& e, p.errors )
                qCritical() << QFileInfo(e.path).fileName() << e.pos.d_row << e.pos.d_col << e.msg;
        }else
        {
            module = p.takeResult();
            Ln::Validator v(&mdl, this);

            if( !v.validate(module, imp) )
            {
                foreach( const Ln::Validator::Error& e, v.errors )
                    qCritical() << QFileInfo(e.path).fileName() << e.pos.d_row << e.pos.d_col << e.msg;
                Ln::Declaration::deleteAll(module);
                module = 0;
                ms->imp = Ln::Import();
            }else
            {
            }
        }
        // TODO: uniquely extend the name of generic module instantiations

        ms->decl = module;

        return module;
    }

    QString toFile(const Ln::Import& imp)
    {
        const QString path = imp.path.join('/') + ".luon";
        foreach( const QDir& dir, searchPath )
        {
            const QString tmp = dir.absoluteFilePath(path);
            if( QFile::exists(tmp) )
                return tmp;
        }
        if( !modules.isEmpty() )
        {
            // if the file is not in the search path, look in the directory of the caller assuming
            // that the required module path is relative to the including module
            QFileInfo info( modules.back().file );
            const QString tmp = info.absoluteDir().absoluteFilePath(path);
            if( QFile::exists(tmp) )
                return tmp;
            // TODO: in this case we have to adjust the local path of the imported module to the full path
        }
        return QString();
    }
};

static void process(const QStringList& files, const QStringList& searchPaths, bool run, bool dump)
{
    int ok = 0;
    int all = 0;
    QElapsedTimer timer;
    timer.start();
    foreach( const QString& file, files )
    {
        Manager mgr;
        QFileInfo info(file);
        mgr.rootPath = info.absolutePath();
        mgr.searchPath.append(info.absoluteDir());
        for( int i = 0; i < searchPaths.size(); i++ )
        {
            const QString path = searchPaths[i];
            mgr.searchPath.append(path);
        }

        Ln::Import imp;
        imp.path.append(Ln::Token::getSymbol(info.baseName().toUtf8()));
        Ln::Declaration* module = mgr.loadModule(imp); // recursively compiles all imported files

        all += mgr.modules.size();
        foreach( const ModuleSlot& m, mgr.modules )
            ok += m.decl ? 1 : 0;
    }
    Ln::AstModel::cleanupGlobals();
    qDebug() << "#### finished with" << ok << "files ok of total" << all << "files" << "in" << timer.elapsed() << " [ms]";
}


int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    QCommandLineParser cp;
    cp.setApplicationDescription("Luon compiler");
    cp.addHelpOption();
    cp.addVersionOption();
    cp.addPositionalArgument("main", "the main module of the application");
    QCommandLineOption sp("I", "add a path where to look for modules", "path");
    cp.addOption(sp);
    QCommandLineOption run("r", "run in interpreter");
    cp.addOption(run);
    QCommandLineOption dump("d", "dump MIL code");
    cp.addOption(dump);

    cp.process(a);
    const QStringList args = cp.positionalArguments();
    if( args.isEmpty() )
        return -1;
    const QStringList searchPaths = cp.values(sp);

    process(args, searchPaths, cp.isSet(run), cp.isSet(dump));

    return 0;
}

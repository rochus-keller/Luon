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

// Adopted from the Oberon+ IDE

#include "LnProject.h"
#include "LnParser2.h"
#include "LnPpLexer.h"
#include <QBuffer>
#include <QDir>
#include <QtDebug>
#include <QSettings>
#include <QCoreApplication>
#include <qdatetime.h>
using namespace Ln;

struct HitTest
{
    quint32 line, col;
    QList<Declaration*> scopes;
    Declaration* scopeHit;

    HitTest():scopeHit(0){}

    QVariant findHit( Declaration* module, int row, int col )
    {
        this->col = row;
        this->line = line;
        try
        {
            visitDecl(module);
        }catch( Expression* e )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(e);
        }catch( Declaration* d )
        {
            Q_ASSERT( !scopes.isEmpty() );
            scopeHit = scopes.back();
            return QVariant::fromValue(d);
        }catch(...)
        {

        }
        return 0;
    }

    void test( Declaration* d )
    {
        if( line == d->pos.d_row && col >= d->pos.d_col && col <= d->pos.d_col + d->name.size() )
            throw d;
    }

    void test(Expression* e)
    {
        if( e == 0 )
            return;
        if( e->pos.d_row > line )
            return;
        Declaration* n = e->val.value<Declaration*>();
        if( n == 0 )
            return;
        if( line == e->pos.d_row && col >= e->pos.d_col && col <= e->pos.d_col + n->name.size() )
            throw e;
    }

    void visitDecl(Declaration* d)
    {
        test(d);
    }
};

Project::Project(QObject *parent) : QObject(parent),d_dirty(false),d_useBuiltInOakwood(false),
    d_useBuiltInObSysInner(false)
{
    d_suffixes << ".luon";
}

Project::~Project()
{
    clearModules();
}

void Project::clear()
{
    clearModules();
    d_groups.clear();
    d_filePath.clear();
    d_files.clear();
}

void Project::createNew()
{
    clear();
    d_filePath.clear();
    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
}

bool Project::initializeFromDir(const QDir& dir, bool recursive)
{
    clear();
    d_dirty = false;

    QStringList files = findFiles(dir, recursive);
    foreach( const QString& filePath, files )
        addFile(filePath);
    emit sigRenamed();
    return true;
}

void Project::setSuffixes(const QStringList& s)
{
    d_suffixes = s;
    touch();
}

void Project::setMain(const Project::ModProc& mp)
{
    d_main = mp;
    touch();
}

QString Project::renderMain() const
{
    QString res = d_main.first;
    if( !d_main.second.isEmpty() )
        res += "." + d_main.second;
    return res;
}

void Project::setUseBuiltInOakwood(bool on)
{
    d_useBuiltInOakwood = on;
    touch();
}

void Project::setUseBuiltInObSysInner(bool on)
{
    d_useBuiltInObSysInner = on;
    touch();
}

bool Project::addFile(const QString& filePath, const QByteArrayList& package)
{
    if( d_files.contains(filePath) )
        return false;
    int pos = findPackage(package);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = package;
    }
    FileGroup& fg = d_groups[pos];
    FileRef ref( new File() );
    fg.d_files.append(ref.data());
    ref->d_group = &fg;
    ref->d_filePath = filePath;
    ref->d_name = QFileInfo(filePath).baseName().toUtf8();
    d_files.insert(filePath,ref);
    touch();
    return true;
}

bool Project::addPackagePath(const QByteArrayList& path)
{
    int pos = findPackage(path);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = path;
        touch();
        return true;
    }else
        return false;
}

bool Project::removeFile(const QString& filePath)
{
    FileHash::iterator i = d_files.find(filePath);
    if( i == d_files.end() )
        return false;
    const int pos = findPackage( i.value()->d_group->d_package );
    Q_ASSERT( pos != -1 );
    d_groups[pos].d_files.removeAll(i.value().data());
    d_files.erase(i);
    touch();
    return true;
}

bool Project::removePackagePath(const QByteArrayList& path)
{
    if( path.isEmpty() )
        return false;
    int pos = findPackage(path);
    if( pos == -1 )
        return false;
    if( !d_groups[pos].d_files.isEmpty() )
        return false;
    d_groups.removeAt(pos);
    touch();
    return true;
}

const Project::FileGroup* Project::getRootFileGroup() const
{
    return findFileGroup(QByteArrayList());
}

const Project::FileGroup* Project::findFileGroup(const QByteArrayList& package) const
{
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == package )
            return &d_groups[i];
    }
    return 0;
}

Symbol* Project::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Declaration** scopePtr) const
{
    File* f = findFile(file);
    if( f == 0 || f->d_mod == 0 )
        return 0;

    return findSymbolBySourcePos(f->d_mod,line,col, scopePtr);
}

Symbol* Project::findSymbolBySourcePos(Declaration* m, quint32 line, quint16 col, Declaration** scopePtr) const
{
    Q_ASSERT(m);
    const ModuleSlot* module = findModule(m);
    if( module == 0 || module->xref.syms == 0 )
        return 0;
    Symbol* s = module->xref.syms;
    do
    {
        if( line == s->pos.d_row && col >= s->pos.d_col && col <= s->pos.d_col + s->len )
            return s;
        s = s->next;
    }while( s && s != module->xref.syms );
    return 0;
}

Project::File* Project::findFile(const QString& file) const
{
    return d_files.value(file).data();
}

void Project::addPreload(const QByteArray& name, const QByteArray& code)
{
    for( int i = 0; i < preloads.size(); i++ )
    {
        if( preloads[i]->d_name == name )
        {
            preloads[i]->d_cache = code;
            return;
        }
    }
    // else
    File* f = new File();
    f->d_name = name;
    f->d_cache = code;
    preloads.append(FileRef(f));
}

Project::UsageByMod Project::getUsage(Declaration* n) const
{
    UsageByMod res;
    for( int i = 0; i < modules.size(); i++ )
    {
        const SymList& syms = modules[i].xref.uses.value(n);
        if( !syms.isEmpty() )
            res << qMakePair(modules[i].decl, syms);
    }

    return res;
}

Symbol*Project::getSymbolsOfModule(Declaration* module) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == module )
            return modules[i].xref.syms;
    }
    return 0;
}

DeclList Project::getSubs(Declaration* d) const
{
    return subs.value(d);
}

QString Project::getWorkingDir(bool resolved) const
{
    if( d_workingDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().path();
        else
            return QCoreApplication::applicationDirPath();
    }
    else if( !resolved )
        return d_workingDir;
    // else
    QString wd = d_workingDir;
    wd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    wd.replace("%APPDIR%", path );
    return wd;
}

void Project::setWorkingDir(const QString& wd)
{
    d_workingDir = wd;
    touch();
}

QString Project::getBuildDir(bool resolved) const
{
    if( d_buildDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().absoluteFilePath("build");
        else
            return QCoreApplication::applicationDirPath() + "/build";
    }
    else if( !resolved )
        return d_buildDir;
    // else
    QString bd = d_buildDir;
    bd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    if( cur.path().endsWith("/Contents/MacOS") )
    {
        // we're in a bundle
        cur.cdUp();
        cur.cdUp();
        cur.cdUp();
    }
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    bd.replace("%APPDIR%", path );
    return bd;
}

void Project::setBuildDir(const QString& bd)
{
    d_buildDir = bd;
    touch();
}

void Project::setOptions(const QByteArrayList& o)
{
    d_options = o;
    touch();
}

bool Project::printTreeShaken(const QString& module, const QString& fileName)
{
#if 0
    // TODO
    FileRef f = d_files.value(module);
    if( f->d_mod.isNull() )
        return false;
    Declaration* m = d_mdl->treeShaken(f->d_mod.data());
    QFile out(fileName);
    if( !out.open(QIODevice::WriteOnly) )
        return false;


    ObxModuleDump dump;
    dump.out.setDevice( &out );
    m->accept(&dump);
#endif
    return true;
}

static inline QByteArray escapeDot(QByteArray name)
{
    return "\"" + name + "\"";
}

#if 0
static void removeRedundantImports(Declaration* cur, QSet<Declaration*>& imports )
{
    foreach( Import* i, cur->d_imports )
    {
        imports.remove( i->d_mod.data() );
        removeRedundantImports( i->d_mod.data(), imports );
    }
}

static QList<Declaration*> removeRedundantImports(Declaration* m)
{
    QSet<Declaration*> imports;
    foreach( Import* i, m->d_imports )
    {
        if( !i->d_mod->isFullyInstantiated() || i->d_mod->d_synthetic )
            continue;
        imports << i->d_mod.data();
    }
    foreach( Import* i, m->d_imports )
        removeRedundantImports(i->d_mod.data(), imports);
    return imports.toList();
}
#endif

bool Project::printImportDependencies(const QString& fileName, bool pruned)
{
    QFile f(fileName);
    if( !f.open(QIODevice::WriteOnly) )
        return false;
    QTextStream s(&f);

    s << "digraph \"Import Dependency Tree\" {" << endl;
    if( !pruned )
        s << "    graph [splines=ortho]" << endl;
    s << "    node [shape=box]" << endl;

#if 0
    // TODO
    QList<Declaration*> mods = getModulesToGenerate(true);
    foreach( Declaration* m, mods )
    {
        if( !m->isFullyInstantiated() || m->d_synthetic )
            continue;
        QSet<QByteArray> names;
        if( pruned )
        {
            QList<Declaration*> imports = removeRedundantImports(m);
            foreach( Declaration* i, imports )
                names << escapeDot(i->getFullName());
        }else
        {
            foreach( Import* i, m->d_imports )
            {
                if( i->d_mod.isNull() || i->d_mod->d_synthetic )
                    continue;
                names << escapeDot(i->d_mod->getFullName());
            }
        }
        const QByteArray name = escapeDot(m->getFullName());
        // s << "    " << name << " [shape=box];" << endl;
        s << "    " << name << " -> {";
        bool first = true;
        foreach( const QByteArray& to, names )
        {
            if( !first )
                s << ", ";
            first = false;
            s << to;
        }
        s << "};" << endl;
    }
#endif

    s << "}";
    return true;
}

void Project::addError(const QString& file, const RowCol& pos, const QString& msg)
{
    errors << Error(msg, pos, file);
}

QStringList Project::findFiles(const QDir& dir, bool recursive)
{
    QStringList res;
    QStringList files;

    if( recursive )
    {
        files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

        foreach( const QString& f, files )
            res += findFiles( QDir( dir.absoluteFilePath(f) ), recursive );
    }

    QStringList suff = d_suffixes;
    for(int i = 0; i < suff.size(); i++ )
        suff[i] = "*" + suff[i];
    files = dir.entryList( suff, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

void Project::touch()
{
    if( !d_dirty )
    {
        d_dirty = true;
        emit sigModified(d_dirty);
    }
}

int Project::findPackage(const QByteArrayList& path) const
{
    int pos = -1;
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == path )
        {
            pos = i;
            break;
        }
    }
    return pos;
}

QByteArray Project::modulePath(const QByteArrayList& path)
{
    return path.join('$');
}

QByteArray Project::moduleSuffix(const MetaActualList& ma)
{
    // TODO: this is an intermediate solution assuming everything is built from sources in full everytime.
    if( ma.isEmpty() )
        return QByteArray();
    else
        return "$" + QByteArray::number(modules.size());
}

static inline QByteArray failWhen(const Import& imp)
{
    QByteArray str;
    QTextStream out(&str);
    out << "when importing " << imp.path.join('.') << " from ";
    if( imp.importer )
    {
        out << imp.importer->data.value<ModuleData>().path.join('.')
            << " at " << imp.importedAt.d_row << ":" << imp.importedAt.d_col;
    }else
        out << "top level";
    out.flush();
    return str;
}

Declaration*Project::loadModule(const Import& imp)
{
    ModuleSlot* ms = find(imp);
    if( ms != 0 )
        return ms->decl;

    File* file = toFile(imp);
    if( file == 0 )
    {
        QString importer;
        if( imp.importer )
            importer = imp.importer->data.value<ModuleData>().source;
        errors << Error("cannot find source file of imported module", imp.importedAt, importer);
        modules.append(ModuleSlot(imp,QString(),0));
        return 0;
    }

    // immediately add it so that circular module refs lead to an error
    modules.append(ModuleSlot(imp,file->d_filePath,0));
    ms = &modules.back();

    class Lex2 : public Scanner2
    {
    public:
        QString sourcePath;
        PpLexer lex;
        Token next()
        {
            return lex.nextToken();
        }
        Token peek(int offset)
        {
            return lex.peekToken(offset);
        }
        QString source() const { return sourcePath; }
    };

    Lex2 lex;
    lex.sourcePath = file->d_filePath; // to keep file name if invalid
    QBuffer buf;
    if( !file->d_cache.isEmpty() )
    {
        buf.setData(file->d_cache);
        buf.open(QIODevice::ReadOnly);
        lex.lex.setStream(&buf, file->d_filePath);
    }else
        lex.lex.setStream(file->d_filePath);
    AstModel mdl;
    Parser2 p(&mdl,&lex);
    p.RunParser();
    Ln::Declaration* module = 0;
    if( !p.errors.isEmpty() )
    {
        foreach( const Parser2::Error& e, p.errors )
            errors << Error(e.msg, e.pos, e.path);
        qDebug() << "### parser failed" << failWhen(imp).constData();
    }else
    {
        module = p.takeResult();
        Validator v(&mdl, this, true);
        Import imp2 = imp;
        if( file->d_group )
        {
            // Take care that the new module is in the package where it was actually found
            // So that e.g. Interface is in som.Interface, even when imported from Vector
            imp2.path = file->d_group->d_package;
            imp2.path.append(imp.path.back());
        }
        if( !v.validate(module, imp2) )
        {
            foreach( const Validator::Error& e, v.errors )
                errors << Error(e.msg, e.pos, e.path);
            Declaration::deleteAll(module);
            module = 0;
            ms->imp = Import();
            qDebug() << "### validator failed" << failWhen(imp).constData();
        }else
        {
            if( imp.metaActuals.isEmpty() )
                file->d_mod = module;
            ms->xref = v.takeXref();
            QHash<Declaration*,DeclList>::const_iterator i;
            for( i = ms->xref.subs.begin(); i != ms->xref.subs.end(); ++i )
                subs[i.key()] += i.value();
        }
    }

    ms->decl = module;

    return module;
}

static bool operator==(const Import& lhs, const Import& rhs)
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

Project::ModuleSlot*Project::find(const Import& imp)
{
    for(int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].imp == imp )
            return &modules[i];
    }
    return 0;
}

Project::File* Project::toFile(const Import& imp)
{
    QByteArrayList path = imp.path;
    path.pop_back();
    const QByteArray name = imp.path.back();
    const Project::FileGroup* fg = findFileGroup(path);
    if( fg != 0 )
    {
        for( int i = 0; i < fg->d_files.size(); i++ )
        {
            if( fg->d_files[i]->d_name == name )
                return fg->d_files[i];
        }
        if( imp.importer )
        {
            path = imp.importer->data.value<ModuleData>().path;
            path.pop_back();
            path += imp.path;
            path.pop_back();
            fg = findFileGroup(path);
            if( fg != 0 )
            {
                for( int i = 0; i < fg->d_files.size(); i++ )
                {
                    if( fg->d_files[i]->d_name == name )
                        return fg->d_files[i];
                }
            }
        }
    }
    for( int i = 0; i < preloads.size(); i++ )
    {
        if( preloads[i]->d_name == name )
            return preloads[i].data();
    }
    return 0;
}

void Project::clearModules()
{
    errors.clear();
    Modules::const_iterator i;
    for( i = modules.begin(); i != modules.end(); ++i )
    {
        Symbol::deleteAll((*i).xref.syms);
        Declaration::deleteAll((*i).decl);
    }
    modules.clear();
    subs.clear();
}

const Project::ModuleSlot*Project::findModule(Declaration* m) const
{
    for( int i = 0; i < modules.size(); i++ )
    {
        if( modules[i].decl == m )
            return &modules[i];
    }
    return 0;
}

bool Project::parse()
{
    clearModules();

    int all = 0, ok = 0;

    for( int i = 0; i < d_groups.size(); i++ )
    {
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            QFileInfo info(d_groups[i].d_files[j]->d_filePath);
            Import imp;
            imp.path = d_groups[i].d_package;
            imp.path.append(Token::getSymbol(info.baseName().toUtf8()));
            Declaration* module = loadModule(imp); // recursively compiles all imported files
            all++;
            if( module )
                ok++;
        }
    }

    emit sigReparsed();
    return all == ok;
}

bool Project::save()
{
    if( d_filePath.isEmpty() )
        return false;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings out(d_filePath,QSettings::IniFormat);
    if( !out.isWritable() )
        return false;

    out.setValue("Suffixes", d_suffixes );
    out.setValue("BuiltInOakwood", d_useBuiltInOakwood );
    out.setValue("BuiltInObSysInner", d_useBuiltInObSysInner );
    out.setValue("MainModule", d_main.first );
    out.setValue("MainProc", d_main.second );
    out.setValue("WorkingDir", d_workingDir );
    out.setValue("BuildDir", d_buildDir );
    out.setValue("Options", d_options.join(' ') );

    const FileGroup* root = getRootFileGroup();
    out.beginWriteArray("Modules", root->d_files.size() ); // nested arrays don't work
    for( int i = 0; i < root->d_files.size(); i++ )
    {
        const QString absPath = root->d_files[i]->d_filePath;
        const QString relPath = dir.relativeFilePath( absPath );
        out.setArrayIndex(i);
        out.setValue("AbsPath", absPath );
        out.setValue("RelPath", relPath );
    }
    out.endArray();

    out.beginWriteArray("Packages", d_groups.size() );
    for( int i = 0; i < d_groups.size(); i++ )
    {
        out.setArrayIndex(i);
        out.setValue("Name", d_groups[i].d_package.join('.') ); // '/' in key gives strange effects
    }
    out.endArray();

    for( int i = 0; i < d_groups.size(); i++ )
    {
        if(d_groups[i].d_package.isEmpty())
            continue;
        out.beginWriteArray(QString::fromUtf8("." + d_groups[i].d_package.join('.')), d_groups[i].d_files.size() );
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            const QString absPath = d_groups[i].d_files[j]->d_filePath;
            const QString relPath = dir.relativeFilePath( absPath );
            out.setArrayIndex(j);
            out.setValue("AbsPath", absPath );
            out.setValue("RelPath", relPath );
        }
        out.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    return true;
}

bool Project::loadFrom(const QString& filePath)
{
    clear();

    d_filePath = filePath;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_useBuiltInObSysInner = in.value("BuiltInObSysInner").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();
    d_workingDir = in.value("WorkingDir").toString();
    d_buildDir = in.value("BuildDir").toString();
    d_options = in.value("Options").toByteArray().split(' ');

    int count = in.beginReadArray("Modules");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString absPath = in.value("AbsPath").toString();
        const QString relPath = in.value("RelPath").toString();
        if( QFileInfo(absPath).exists() )
            addFile(absPath);
        else
        {
            absPath = dir.absoluteFilePath(relPath);
            if( QFileInfo(absPath).exists() )
                addFile(absPath);
            else
                qCritical() << "Could not open module" << relPath;
        }
    }
    in.endArray();

    QList<QByteArrayList> paths;
    count = in.beginReadArray("Packages");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString name = in.value("Name").toString();
        paths << name.toLatin1().split('.');
        addPackagePath( paths.back() );
    }
    in.endArray();

    for( int j = 0; j < paths.size(); j++ )
    {
        count = in.beginReadArray(QString::fromUtf8("." + paths[j].join('.')));
        for( int i = 0; i < count; i++ )
        {
            in.setArrayIndex(i);
            QString absPath = in.value("AbsPath").toString();
            const QString relPath = in.value("RelPath").toString();
            if( QFileInfo(absPath).exists() )
                addFile(absPath, paths[j]);
            else
            {
                absPath = dir.absoluteFilePath(relPath);
                if( QFileInfo(absPath).exists() )
                    addFile(absPath, paths[j]);
                else
                    qCritical() << "Could not open module" << relPath;
            }
        }
        in.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
    return true;
}

bool Project::saveTo(const QString& filePath)
{
    d_filePath = filePath;
    const bool res = save();
    emit sigRenamed();
    return res;
}


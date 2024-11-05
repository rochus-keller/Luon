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

#include "LnLjRuntime.h"
#include <LjTools/Engine2.h>
#include "LnRowCol.h"
#include "LnProject.h"
#include "LnLjbcGen.h"
#include <QFile>
#include <QDir>
#include <QBuffer>
#include <QtDebug>
#include <QTime>
using namespace Ln;

static void printLoadError(Lua::Engine2* lua, const QByteArray& what)
{
    Lua::Engine2::ErrorMsg msg = Lua::Engine2::decodeRuntimeMessage(lua->getLastError());
    QString str;
    if( msg.d_line )
    {
        if( RowCol::isPacked(msg.d_line) )
            str = QString("%1:%2: %3").arg(RowCol::unpackRow(msg.d_line)).arg(RowCol::unpackCol(msg.d_line))
                    .arg(msg.d_message.constData());
    }else
        str = msg.d_message;
    qCritical() << "error loading" << what << str.toUtf8().constData();
}

static void loadLuaLib( Lua::Engine2* lua, const QByteArray& path, QByteArray name = QByteArray() )
{
    QFile lib( QString(":/runtime/%1.lua").arg(path.constData()) );
    if( !lib.open(QIODevice::ReadOnly) )
        qCritical() << "cannot find" << path;
    if( name.isEmpty() )
        name = path;
    if( !lua->addSourceLib( lib.readAll(), name ) )
        printLoadError( lua, path );
}

static bool preloadLib( Project* pro, const QByteArray& name )
{
    QFile f( QString(":/oakwood/%1.luon" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown preload" << name;
        return false;
    }
    pro->addPreload(name, f.readAll());
    return true;
}

LjRuntime::LjRuntime(QObject*p):QObject(p), d_jitEnabled(true),d_buildErrors(false)
{
    d_pro = new Project(this);

    d_lua = new Lua::Engine2(this);
    Lua::Engine2::setInst(d_lua);
    prepareEngine();
}

bool LjRuntime::compile(bool doGenerate)
{
    if( d_pro->useBuiltInOakwood() )
    {
        preloadLib(d_pro,"In");
        preloadLib(d_pro,"Out");
        preloadLib(d_pro,"Files");
        preloadLib(d_pro,"Input");
        preloadLib(d_pro,"Math");
        preloadLib(d_pro,"Strings");
        preloadLib(d_pro,"XYPlane");
    }
    const quint32 errCount = d_pro->getErrors().size();
    const QTime start = QTime::currentTime();
    if( !d_pro->parse() )
        return false;
    qDebug() << "recompiled in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    if( doGenerate )
        generate();
    return errCount == d_pro->getErrors().size();
}

bool LjRuntime::run()
{
    if( !compile(true) )
        return false;

    if( !loadLibraries() )
        return false;

    if( !loadBytecode() )
        return false;

    return executeMain();
}

bool LjRuntime::loadLibraries()
{
    if( d_pro->useBuiltInOakwood() )
    {
        loadLuaLib(d_lua,"In");
        loadLuaLib(d_lua,"Out");
        loadLuaLib(d_lua,"Files");
        loadLuaLib(d_lua,"Input");
        loadLuaLib(d_lua,"Math");
        loadLuaLib(d_lua,"Strings");
        loadLuaLib(d_lua,"XYPlane");
    }

    if( d_pro->useBuiltInObSysInner() )
    {
#ifdef QT_GUI_LIB_
        loadLuaLib(d_lua,"Obs/Input", "Input");
        loadLuaLib(d_lua,"Obs/Kernel", "Kernel");
        loadLuaLib(d_lua,"Obs/Display", "Display");
        loadLuaLib(d_lua,"Obs/Modules", "Modules");
        loadLuaLib(d_lua,"Obs/FileDir", "FileDir");
        loadLuaLib(d_lua,"Obs/Files", "Files");
#else
        qCritical() << "this version doesn't support the Oberon System backend modules";
        return false;
#endif
    }
    const QString root = d_pro->getWorkingDir(true);
    // TODO Obs::Files::setFileSystemRoot(root);
    if( d_pro->useBuiltInObSysInner() )
        d_lua->print(QString("Oberon file system root: %1\n").arg(root).toUtf8().constData());
    return true;
}

bool LjRuntime::loadBytecode()
{
    if( d_byteCode.isEmpty() )
    {
        qWarning() << "nothing to load";
        return true;
    }

    bool hasErrors = false;
    try
    {
        for( int i = 0; i < d_byteCode.size(); i++ )
        {
            const QByteArray name = d_byteCode[i].first->name;
            qDebug() << "loading" << name;
            if( !d_lua->addSourceLib( d_byteCode[i].second, name ) )
            {
                printLoadError(d_lua,name);
                hasErrors = true;
            }
            if( d_lua->isAborted() )
            {
                return true;
            }
        }
    }catch(...)
    {
        hasErrors = true;
        qCritical() << "LuaJIT crashed"; // doesn't help if the JIT crashes!
    }

    return !hasErrors;
}

bool LjRuntime::executeMain()
{
    Project::ModProc main = d_pro->getMain();
    if( main.first.isEmpty() )
    {
        Q_ASSERT( main.second.isEmpty() ); // we either need module or module.proc
        return true; // nothing to do
    }

    QByteArray src;
    QTextStream out(&src);

    out << "local " << main.first << " = require '" << main.first << "'" << endl;
    if( !main.second.isEmpty() )
        out << main.first << "." << main.second << "()" << endl;
    out.flush();
    return d_lua->executeCmd(src,"terminal");
}

bool LjRuntime::restartEngine()
{
    if( !d_lua->restart() )
        return false;
    prepareEngine();
    d_lua->setJit(d_jitEnabled);
    return true;
}

QByteArray LjRuntime::findByteCode(Declaration* m) const
{
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        if( d_byteCode[i].first == m )
            return d_byteCode[i].second;
    }
    return QByteArray();
}

LjRuntime::BytecodeList LjRuntime::findByteCode(const QString& filePath) const
{
    BytecodeList res;
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        ModuleData md = d_byteCode[i].first->data.value<ModuleData>();
        if( md.source == filePath )
            res.append(d_byteCode[i]);
    }
    return res;
}

bool LjRuntime::saveBytecode(const QString& outPath, const QString& suffix) const
{
    QDir dir(outPath);
    if( !dir.exists() && !dir.mkpath(outPath) )
    {
        qCritical() << "cannot create directory for writing bytecode files" << outPath;
        return false;
    }
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        QString path = dir.absoluteFilePath(d_byteCode[i].first->name + suffix);
        QFile out(path);
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot open file for writing bytecode" << path;
            return false;
        }
        out.write(d_byteCode[i].second);
    }
    return true;
}

void LjRuntime::setJitEnabled(bool on)
{
    d_jitEnabled = on;
    d_lua->setJit(on);
}

void LjRuntime::generate()
{
    QList<Declaration*> mods = d_pro->getModulesToGenerate();
    d_byteCode.clear();
    d_buildErrors = false;

    const quint32 errCount = d_pro->getErrors().size();
    foreach( Declaration* m, mods )
        LjbcGen::allocateModuleSlots(m);
    foreach( Declaration* m, mods )
        generate(m);

    d_buildErrors = d_pro->getErrors().size() != errCount;
}

void LjRuntime::generate(Declaration* m)
{
    //Q_ASSERT( m && m->isFullyInstantiated() );
    //qDebug() << "generating" << m->getName();
    QBuffer buf;
    buf.open(QIODevice::WriteOnly);
    LjbcGen gen;
    gen.translate(m, &buf, false );
    buf.close();
    d_byteCode << qMakePair(m,buf.buffer());
}

void LjRuntime::prepareEngine()
{
    // TODO LibFfi::install(d_lua->getCtx());
#ifdef QT_GUI_LIB_
    Obs::Display::install(d_lua->getCtx());
#endif
    d_lua->addStdLibs();
    d_lua->addLibrary(Lua::Engine2::PACKAGE);
    d_lua->addLibrary(Lua::Engine2::IO);
    d_lua->addLibrary(Lua::Engine2::BIT);
    d_lua->addLibrary(Lua::Engine2::JIT);
    d_lua->addLibrary(Lua::Engine2::FFI);
    d_lua->addLibrary(Lua::Engine2::OS);
    // d_lua->setJit(false); // must be called after addLibrary! doesn't have any effect otherwise
    loadLuaLib( d_lua, "LUON" );
}

